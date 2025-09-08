setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ML-based imputation for dashboard data using XGBoost with spatiotemporal features
# We train on clean data and validate imputation quality using a holdout first

library(tidyverse)
library(xgboost)
library(sf)
library(spdep)
library(zoo)

# ---- 1. DATA PREP ----

# remove week 53 (incomplete for most years using ISO weeks)
dashboard_data <- read_csv("Dashboard.csv") %>% filter(week != 53)

# Load shapefile and standardise names
thailand_sf <- st_read("gadm41_THA_1.shp")
name_fixes <- c("Bangkok Metropolis"="Bangkok", "Bueng Kan"="Bungkan", 
                "Phra Nakhon Si Ayutthaya"="P.Nakhon S.Ayutthaya",
                "Buri Ram"="Buriram", "Chon Buri"="Chonburi", 
                "Lop Buri"="Lopburi", "Phangnga"="Phang Nga", 
                "Prachin Buri"="Prachinburi")
for(old in names(name_fixes)) {
  thailand_sf$NAME_1[thailand_sf$NAME_1 == old] <- name_fixes[old]}

# Create spatial neighbours
nb <- poly2nb(thailand_sf, queen = TRUE)

# Manual fix as Phuket island has no natural neighbours, link to nearest mainland
phuket_idx <- which(thailand_sf$NAME_1 == "Phuket")
phangnga_idx <- which(thailand_sf$NAME_1 == "Phang Nga")
nb[[phuket_idx]] <- c(nb[[phuket_idx]], phangnga_idx)
nb[[phangnga_idx]] <- c(nb[[phangnga_idx]], phuket_idx)

# Convert to named list for easier lookups
province_neighbours <- setNames(
  lapply(nb, function(x) thailand_sf$NAME_1[x]),
  thailand_sf$NAME_1
)

# Create complete time series grid and identify anomalies (eg. decimals are scraping artefacts)
data_complete <- expand.grid(
  province = unique(dashboard_data$province),
  year = unique(dashboard_data$year),
  week = 1:52) %>%
  left_join(dashboard_data, by = c("province", "year", "week")) %>%
  mutate(
    original_value = value,
    is_anomalous = !is.na(value) & (value == 0 | value != floor(value)),
    value_clean = ifelse(is_anomalous, NA, value)
  )

# Flagged dataset
data_flagged <- data_complete %>%
  mutate(
    original_value = value,
    is_anomalous = !is.na(value) & (value == 0 | value != floor(value)),
    value_clean = ifelse(is_anomalous, NA, value)
  )

cat(sprintf("Flagged %d anomalous values (%.1f%%)\n", 
            sum(data_complete$is_anomalous, na.rm=TRUE),
            mean(data_complete$is_anomalous, na.rm=TRUE)*100))



# ---- 2. FEATURE ENGINEERING ----

# Historical patterns for each province-week across years
historical_medians <- data_flagged %>%
  filter(!is.na(value_clean)) %>%
  group_by(province, week) %>%
  summarise(
    week_median = median(value_clean, na.rm = TRUE),
    .groups = "drop"
  )

# Spatial diffusion features: disease burden in neighbouring provinces
spatial_features <- data_flagged %>%
  group_by(year, week) %>%  # Group by time point
  mutate(
    neighbour_mean = map_dbl(province, function(p) {
      neighbs <- province_neighbours[[p]]
      # Get values of neighbours in same year-week
      neighbour_vals <- value_clean[province %in% neighbs]
      if (any(!is.na(neighbour_vals))) {
        mean(neighbour_vals, na.rm = TRUE)
      } else {
        NA_real_
      }
    }),
    # Neighbour max to check for nearby outbreaks that might spread
    neighbour_max = map_dbl(province, function(p) {
      neighbs <- province_neighbours[[p]]
      # Get max value of neighbours in same year-week
      neighbour_vals <- value_clean[province %in% neighbs]
      if (any(!is.na(neighbour_vals))) {
        max(neighbour_vals, na.rm = TRUE)
      } else {
        NA_real_
      }
    })
  ) %>%
  ungroup() %>%
  dplyr::select(province, year, week, neighbour_mean, neighbour_max)

# Combine all temporal, spatial and seasonal features
data_with_features <- data_flagged %>%
  arrange(province, year, week) %>%
  group_by(province) %>%
  mutate(
    # Temporal lags/leads within each province
    value_prev_week = lag(value_clean, 1),
    value_next_week = lead(value_clean, 1), # Uses future for *imputation only*
    value_prev_2weeks = lag(value_clean, 2),
    value_next_2weeks = lead(value_clean, 2), # Uses future for *imputation only*
    # Rolling average centered on current week to reduce noise
    rolling_mean_4w = zoo::rollmean(value_clean, 4, fill = NA, align = "center")
  ) %>%
  ungroup() %>%
  # Year-on-year comparison for annual cycles
  left_join(
    data_flagged %>%
      dplyr::select(province, year, week, value_prev_year = value_clean) %>%
      mutate(year = year + 1),  # Shift year forward to align
    by = c("province", "year", "week")
  ) %>%
  # Add prev computed features
  left_join(historical_medians, by = c("province", "week")) %>%
  left_join(spatial_features, by = c("province", "year", "week")) %>%
  # Cyclical to avoid discontinuit between last week and 1st week
  mutate(
    week_sin = sin(2 * pi * week / 52), # Sin
    week_cos = cos(2 * pi * week / 52), # Cos
    year_scaled = (year - min(year)) / (max(year) - min(year))
  )

# All features for XGBoost
feature_cols <- c("week", "year_scaled", "week_sin", "week_cos",
                  "value_prev_week", "value_next_week", 
                  "value_prev_2weeks", "value_next_2weeks",
                  "value_prev_year", "week_median",
                  "rolling_mean_4w", "neighbour_mean", "neighbour_max")


# Keep rows where with anomalous values to impute later
model_data <- data_with_features %>%
  filter(!is.na(value_clean) | is_anomalous)



# ---- 3. MODEL TRAINING & VALIDATION ----

set.seed(7)

# Hold-out 5% of clean data to assess imputation quality
good_indices <- which(!is.na(data_with_features$value_clean))
val_indices <- sample(good_indices, round(length(good_indices) * 0.05))

# Prepare training data by masking validation data to simulate missing values
train_data <- data_with_features
train_data$value_clean[val_indices] <- NA
train_rows <- which(!is.na(train_data$value_clean))

# Feature imputation helper (because XGBoost cant handle NA)
prepare_matrix <- function(data, rows, features) {
  X <- as.matrix(data[rows, features])
  for(col in features) {
    X[is.na(X[,col]), col] <- median(X[,col], na.rm=TRUE)
  }
  X
}

X_train <- prepare_matrix(train_data, train_rows, feature_cols)
y_train <- train_data$value_clean[train_rows]
X_val <- prepare_matrix(data_with_features, val_indices, feature_cols)
y_val <- data_with_features$value_clean[val_indices]

# Train XGBoost 
# Hyperparameters tuned to prevent overfitting on small dataset
xgb_model <- xgb.train(
  params = list(
    objective = "count:poisson", # Poisson objective for count data
    eta = 0.03, # low learning rate
    max_depth = 6, # interactions captured
    subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 6,
    reg_lambda = 2, # penalty on leaf weights
    max_delta_step = 1
  ),
  data = xgb.DMatrix(X_train, label = y_train),
  nrounds = 10000,
  watchlist = list(val = xgb.DMatrix(X_val, label = y_val)),
  early_stopping_rounds = 1000,
  verbose = 0
)

# Evaluate
# Accuracy defined as within 10% OR within 20 cases
val_pred <- pmax(round(predict(xgb_model, xgb.DMatrix(X_val))), 0)
errors <- abs(y_val - val_pred)
accuracy <- (errors/y_val <= 0.1) | (errors <= 20)
Accuracy_Rate <- mean(accuracy) * 100

cat(sprintf("Validation: MAE=%.1f, RMSE=%.1f, accuracy=%.1f%%\n",
            mean(errors), sqrt(mean((y_val-val_pred)^2)), Accuracy_Rate))

# Validation: MAE=12.8, RMSE=26.9, accuracy=90.5% (stored for later use in graphs)
val_mae <- mean(errors)
val_rmse <- sqrt(mean((y_val-val_pred)^2))
val_accuracy <- Accuracy_Rate


# ---- 4. IMPUTATION ----

# Impute anomalous values
impute_idx <- which(data_with_features$is_anomalous)
X_impute <- prepare_matrix(data_with_features, impute_idx, feature_cols)
imputed <- pmax(round(predict(xgb_model, xgb.DMatrix(X_impute))), 0)

data_with_features$value_final <- data_with_features$value_clean
data_with_features$value_final[impute_idx] <- imputed
  
cat(sprintf("Imputed %d values\n", length(impute_idx)))

# Save cleaned data
dashboard_cleaned <- data_with_features %>%
  dplyr::select(province, year, week, value = value_final) %>%
  filter(!is.na(value))

### Uncomment to save the imputed dataset:
write_csv(dashboard_cleaned, "Dashboard_MLcleaned.csv")


# ----------------------------------------------------------------------- #


# ---- PART B: FEATURE ABLATION STUDY (using clean imputed data) ----

# Fresh train/test split
set.seed(777)
clean_data <- data_with_features %>% filter(!is.na(value_final))
val_idx_ablation <- sample(nrow(clean_data), round(nrow(clean_data) * 0.05))
train_idx_ablation <- setdiff(1:nrow(clean_data), val_idx_ablation)

# Standardised evaluation metrics
evaluate_model <- function(predictions, actual) {
  errors <- abs(actual - predictions)
  list(
    mae = mean(errors),
    rmse = sqrt(mean((actual - predictions)^2)),
    #mape = mean(errors/actual * 100),
    accuracy = mean((errors/actual <= 0.1) | (errors <= 20)) * 100
  )
}

# XGBoost training function w/ same hyperparameters as in Part A
train_xgb <- function(features, train_idx, val_idx, data) {
  X_tr <- prepare_matrix(data, train_idx, features)
  X_vl <- prepare_matrix(data, val_idx, features)
  model <- xgb.train(
    params = list(
      objective = "count:poisson", 
      eta = 0.03,
      max_depth = 6,
      subsample = 0.8, 
      colsample_bytree = 0.8, 
      min_child_weight = 6,
      reg_lambda = 2,
      max_delta_step = 1
    ),
    data = xgb.DMatrix(X_tr, label = data$value_final[train_idx]),
    nrounds = 10000,
    watchlist = list(val = xgb.DMatrix(X_vl, label = data$value_final[val_idx])),
    early_stopping_rounds = 1000,
    verbose = 0
  )
  pmax(round(predict(model, xgb.DMatrix(X_vl))), 0)
}

# Get actual values for validation set
y_val_ablation <- clean_data$value_final[val_idx_ablation]

results <- list()

# Baseline models

# 1. Last observation carry forward
carry_forward <- sapply(val_idx_ablation, function(i) {
  prov <- clean_data$province[i]
  wk <- clean_data$week[i]
  yr <- clean_data$year[i]
  prev <- clean_data %>%
    filter(province == prov, (year < yr | (year == yr & week < wk))) %>%
    arrange(desc(year), desc(week)) %>%
    dplyr::slice(1) %>%
    pull(value_final)
  ifelse(length(prev) > 0, prev, median(clean_data$value_final[clean_data$province == prov]))
})
results$carry_forward <- evaluate_model(carry_forward, y_val_ablation)

# 2. Historical same-week median (seasonality)
same_week <- sapply(val_idx_ablation, function(i) {
  median(clean_data$value_final[clean_data$province == clean_data$province[i] & 
                                  clean_data$week == clean_data$week[i]], na.rm=TRUE)
})
results$same_week <- evaluate_model(same_week, y_val_ablation)

# 3. Current neighbour mean
neighbour_mean <- sapply(val_idx_ablation, function(i) {
  neighbs <- province_neighbours[[clean_data$province[i]]]
  vals <- clean_data$value_final[clean_data$province %in% neighbs & 
                                   clean_data$week == clean_data$week[i] & 
                                   clean_data$year == clean_data$year[i]]
  ifelse(length(vals) > 0, mean(vals, na.rm=TRUE), 
         median(clean_data$value_final[clean_data$province == clean_data$province[i]]))
})
results$neighbour <- evaluate_model(neighbour_mean, y_val_ablation)

# 4. Temporal-spatial hybrid (equally weighted)
results$hybrid <- evaluate_model((carry_forward + neighbour_mean)/2, y_val_ablation)


# Ablation models

# Define feature sets (*no* future peeking)
features_past_spatial <- c("value_prev_week", "value_prev_2weeks", "value_prev_year",
                           "neighbour_mean", "neighbour_max",
                           "week", "week_sin", "week_cos", "week_median", "year_scaled")
features_past_only <- c("value_prev_week", "value_prev_2weeks", "value_prev_year",
                        "week", "week_sin", "week_cos", "week_median", "year_scaled")
features_spatial_only <- c("neighbour_mean", "neighbour_max", 
                           "week", "week_sin", "week_cos")

# Train ablation models
pred_spatiotemporal <- train_xgb(features_past_spatial, train_idx_ablation, val_idx_ablation, clean_data)
pred_temporal_only <- train_xgb(features_past_only, train_idx_ablation, val_idx_ablation, clean_data)
pred_spatial_only <- train_xgb(features_spatial_only, train_idx_ablation, val_idx_ablation, clean_data)

results$ml_spatiotemporal <- evaluate_model(pred_spatiotemporal, y_val_ablation)
results$ml_temporal_only <- evaluate_model(pred_temporal_only, y_val_ablation)
results$ml_spatial_only <- evaluate_model(pred_spatial_only, y_val_ablation)

# Results summary
results_df <- bind_rows(results, .id = "model") %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  arrange(desc(accuracy))
print(results_df)

# Statistical significance test for spatial contribution
# Paired t-test comparing prediction errors with vs w/o spatial features
errors_with <- abs(y_val_ablation - pred_spatiotemporal)
errors_without <- abs(y_val_ablation - pred_temporal_only)
t_test <- t.test(errors_without, errors_with, paired = TRUE)

# Spatial contribution
improvement_pct <- (mean(errors_without) - mean(errors_with)) / mean(errors_without) * 100

# % MAE reduction from spatial features
improvement_pct
# Statistical significance
t_test


# ----------------------------------------------------------------------- #


# ---- PART C: VISUALISING RESULTS ----

# Load plotting libraries
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)

# Convert results to dataframes for plotting
# Baseline models vs XGBoost
table1_data <- data.frame(
  Model = c("Carry-forward", "Same-week median", "Neighbour mean", 
            "Spatiotemporal", "Full model"),
  MAE = c(results$carry_forward$mae, results$same_week$mae, 
          results$neighbour$mae, results$hybrid$mae, 
          results$ml_spatiotemporal$mae),
  RMSE = c(results$carry_forward$rmse, results$same_week$rmse,
           results$neighbour$rmse, results$hybrid$rmse,
           results$ml_spatiotemporal$rmse),
  Accuracy_Rate = c(results$carry_forward$accuracy, results$same_week$accuracy,
                     results$neighbour$accuracy, results$hybrid$accuracy,
                     results$ml_spatiotemporal$accuracy)
)

# Feature ablation analysis using validation metrics
table2_data <- data.frame(
  Model = c("Past only", "Spatial only", "Past + Spatial", "Full model"),
  MAE = c(results$ml_temporal_only$mae, results$ml_spatial_only$mae,
          results$ml_spatiotemporal$mae, val_mae),
  RMSE = c(results$ml_temporal_only$rmse, results$ml_spatial_only$rmse,
           results$ml_spatiotemporal$rmse, val_rmse),
  Accuracy_Rate = c(results$ml_temporal_only$accuracy, results$ml_spatial_only$accuracy,
                     results$ml_spatiotemporal$accuracy, val_accuracy)
)

# Theme
theme_1 <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      strip.text = element_text(size = 11, face = "bold", color = "black"),
      strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10))
    )
}



# Validation model vs baselines

# Create comparison data for validation model vs baselines only
baseline_comparison <- data.frame(
  Model = c("Carry-forward", "Same-week\nmedian", "Neighbour mean", 
            "Hybrid", "XGBoost (Full)"),
  MAE = c(results$carry_forward$mae, results$same_week$mae, 
          results$neighbour$mae, results$hybrid$mae, val_mae),
  RMSE = c(results$carry_forward$rmse, results$same_week$rmse,
           results$neighbour$rmse, results$hybrid$rmse, val_rmse),
  Accuracy = c(results$carry_forward$accuracy, results$same_week$accuracy,
               results$neighbour$accuracy, results$hybrid$accuracy, val_accuracy)
)

# Calculate relative improvements
baseline_comparison <- baseline_comparison %>%
  mutate(
    MAE_improvement = (MAE[1] - MAE) / MAE[1] * 100,  # % improvement vs carry-forward
    RMSE_improvement = (RMSE[1] - RMSE) / RMSE[1] * 100,
    Accuracy_gain = Accuracy - Accuracy[1],  # Percentage point gain
    Is_XGBoost = Model == "XGBoost (Full)"
  )

# Panel A: Combined error metrics with simplified legend
p_errors <- baseline_comparison %>%
  dplyr::select(Model, MAE, RMSE) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = reorder(Model, -Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
  scale_fill_manual(
    values = c("MAE" = "#440154", "RMSE" = "#31688e"),
    name = "Metric"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(baseline_comparison$RMSE) * 1.1)) +
  theme_1() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    legend.position = "top",
    legend.title = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Error Metrics: XGBoost vs Baselines",
    x = NULL,
    y = "Error Value (cases)"
  )

# Panel B: Accuracy comparison 
p_accuracy <- baseline_comparison %>%
  ggplot(aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Is_XGBoost)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%")), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = "#440154", "TRUE" = "#31688e"), 
                    guide = "none") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme_1() +
  theme(plot.margin = margin(15, 15, 15, 15)) +
  labs(
    title = "Prediction Accuracy Comparison",
    x = NULL,
    y = NULL
  )


# Composite layout (1x2 grid)
validation_comparison <- (p_errors | p_accuracy) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Model Performance Comparison",
    subtitle = "Comparison of machine learning model against naive baselines",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey50"),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

# Display the plot
print(validation_comparison)

# Create plots directory
dir.create("plots")

# Save the plot to the plots directory
ggsave("plots/validation_vs_baselines.png", validation_comparison, width = 12, height = 5, dpi = 300)



# Feature ablation
feature_panel_errors <- table2_data %>%
  dplyr::select(Model, MAE, RMSE) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = reorder(Model, -Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
  scale_fill_manual(values = c("MAE" = "#440154", "RMSE" = "#31688e")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(table2_data$RMSE) * 1.05)) +
  theme_1() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    legend.position = "top",
    legend.title = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Error Metrics: Feature Ablation",
    x = NULL,
    y = "Error Value (cases)"
  )

feature_panel_accuracy <- table2_data %>%
  ggplot(aes(x = reorder(Model, Accuracy_Rate), y = Accuracy_Rate)) +
  geom_col(fill = "#35b779", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = paste0(round(Accuracy_Rate, 1), "%")), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme_1() +
  theme(plot.margin = margin(15, 15, 15, 15)) +
  labs(
    title = "Feature Ablation: Accuracy Comparison",
    x = NULL,
    y = "Accuracy (%)"
  )

# Combine into 1x2 layout
feature_ablation_plot <- (feature_panel_errors | feature_panel_accuracy) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Contribution of Temporal vs Spatial Features",
    subtitle = "Comparison of model performance with different feature combinations",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey50"),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

print(feature_ablation_plot)
ggsave("plots/feature_ablation_analysis.png", feature_ablation_plot, width = 12, height = 5, dpi = 300)




# Feature ablation - Dot Plot
feature_ablation_data <- table2_data %>%
  dplyr::select(Model, MAE, RMSE, Accuracy_Rate) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = factor(Metric, levels = c("MAE", "RMSE", "Accuracy_Rate"),
                    labels = c("MAE", "RMSE", "Accuracy Rate (%)"))
  )

feature_ablation_dot <- ggplot(feature_ablation_data, aes(x = Value, y = reorder(Model, Value))) +
  geom_segment(aes(x = 0, xend = Value, yend = reorder(Model, Value)), 
               color = "grey60", size = 0.8) +
  geom_point(aes(color = Model), size = 3.5, alpha = 0.9) +
  facet_wrap(~Metric, scales = "free_x", ncol = 2) +
  scale_color_viridis_d(option = "plasma") +
  theme_1() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "Feature Analysis Metrics",
    x = "Value",
    y = NULL
  )

print(feature_ablation_dot)
ggsave("plots/feature_ablation_dot.png", feature_ablation_dot, width = 12, height = 4, dpi = 300)
