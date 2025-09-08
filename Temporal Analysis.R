# Temporal Analysis of Thai Diarrhoea Data (2012-2023)
# =====================================================

# Load required libraries
library(tidyverse)
library(forecast)
library(Rbeast)
library(lmtest)
library(tseries)
library(readxl)

# ---- DATA IMPORT AND PREPARATION ----

# Load monthly data (2012-2019)
monthly_2012_2019 <- read.delim("diarrhoea_cases_monthly.txt", sep = " ")
# Load weekly data (2020-2023)
weekly_2020_2023 <- read_xlsx("weekly.xlsx", sheet = 1)

# Convert weekly to monthly using weighted allocation
convert_weekly_to_monthly <- function(weekly_data, year) {
  days_in_month <- c(31, 28 + (year %% 4 == 0), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  monthly_cases <- numeric(12)
  # Track week
  week_counter <- 1
  days_used <- 0
  for (month in 1:12) {
    days_needed <- days_in_month[month]
    days_allocated <- 0
    while (days_allocated < days_needed && week_counter <= 52) {
      # days left in current week
      days_left_in_week <- 7 - days_used
      # days to take from current week
      days_to_take <- min(days_left_in_week, days_needed - days_allocated)
      # Allocate proportional cases
      if (week_counter <= nrow(weekly_data)) {
        weekly_cases <- weekly_data[[as.character(year)]][week_counter]
        monthly_cases[month] <- monthly_cases[month] + 
          (weekly_cases * days_to_take / 7)
      }
      days_allocated <- days_allocated + days_to_take
      days_used <- days_used + days_to_take
      # Move to next week if current week is done
      if (days_used >= 7) {
        week_counter <- week_counter + 1
        days_used <- 0
      }
    }
  }
  return(monthly_cases)
}

# Convert weekly 2020-2023 to monthly
monthly_2020_2023 <- data.frame(
  Year = 2020:2023,
  Jan = NA, Feb = NA, Mar = NA, Apr = NA, May = NA, Jun = NA,
  Jul = NA, Aug = NA, Sep = NA, Oct = NA, Nov = NA, Dec = NA
)

for (i in 1:4) {
  year <- 2019 + i
  monthly_values <- convert_weekly_to_monthly(weekly_2020_2023, year)
  monthly_2020_2023[i, 2:13] <- round(monthly_values)
}

# Add total column
monthly_2020_2023$Total <- rowSums(monthly_2020_2023[, 2:13])

# Combine all monthly data
all_monthly <- rbind(monthly_2012_2019, monthly_2020_2023)

# Pivot to long format
monthly_long <- all_monthly %>%
  pivot_longer(cols = -c(Year, Total), 
               names_to = "Month", 
               values_to = "Cases") %>%
  mutate(Month_num = match(Month, month.abb)) %>%
  arrange(Year, Month_num)

# Create time series object
ts_monthly <- ts(monthly_long$Cases, 
                 start = c(2012, 1), 
                 frequency = 12)



# ---- Exploratory analysis ----

# Calculate year-on-year changes
yearly_totals <- all_monthly %>%
  filter(Year >= 2012) %>%
  select(Year, Total)

yoy_change <- yearly_totals %>%
  mutate(
    YoY_Change = (Total - lag(Total)) / lag(Total) * 100,
    Avg_Monthly = Total / 12
  )

print(yoy_change)

# average annual decline from 2012 to 2019
mean(yoy_change$YoY_Change[yoy_change$Year %in% 2013:2019], na.rm = TRUE)

# Plot with annotations
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
plot(ts_monthly, 
     main = "Monthly Diarrhoea Cases (Jan 2012 - Dec 2023)",
     ylab = "Cases",
     xlab = "Year",
     type = "l",
     lwd = 2)

# Add event markers
abline(v = 2020, col = "red", lty = 2, lwd = 2)
text(2020, max(monthly_long$Cases) * 0.9, "2020", col = "red", pos = 4)

# Add YoY change annotations
for (i in 2:nrow(yoy_change)) {
  if (!is.na(yoy_change$YoY_Change[i])) {
    text(yoy_change$Year[i], 
         max(monthly_long$Cases) * (0.85 - 0.03 * (i %% 4)), 
         sprintf("%.1f%%", yoy_change$YoY_Change[i]),
         cex = 0.7,
         col = ifelse(yoy_change$YoY_Change[i] < 0, "darkred", "darkgreen"))
  }
}



# ---- 1. STL DECOMPOSITION ----

# Run STL with evolving seasonality
stl_result <- stl(ts_monthly, s.window = "periodic", # 12-month window
                  robust = TRUE, # Robust to outliers
                  na.action = na.exclude)

# Plot decomposition
plot(stl_result, main = "STL Decomposition (s.window = 12, robust = TRUE)")

# Extract components
seasonal <- stl_result$time.series[, "seasonal"]
trend <- stl_result$time.series[, "trend"]
remainder <- stl_result$time.series[, "remainder"]

# Diagnostic tests on residuals

# Ljung-Box test at 1-year lag and 2-year lag
lb_12 <- Box.test(remainder, lag = 12, type = "Ljung-Box")
lb_24 <- Box.test(remainder, lag = 24, type = "Ljung-Box")
# Shapiro-Wilk normality test
sw_test <- shapiro.test(remainder)

# Results
cat(sprintf("  Lag 12: χ² = %.2f, p = %.3f\n", lb_12$statistic, lb_12$p.value))
cat(sprintf("  Lag 24: χ² = %.2f, p = %.3f\n", lb_24$statistic, lb_24$p.value))
cat(sprintf("\nShapiro-Wilk Test: W = %.3f, p = %.3f\n", sw_test$statistic, sw_test$p.value))



# ---- 2. BEAST SENSITIVITY ANALYSIS ----

# BEAST changepoint detection with increased chains for robustness
beast_result <- beast(ts_monthly,
                      season = "harmonic",
                      period = 12,
                      mcmc.seed = 777,
                      mcmc.samples = 10000, # Increased from default
                      mcmc.chains = 100, # 100 independent chains
                      mcmc.thinning = 5,
                      mcmc.burnin = 1000, # Longer burn-in
                      print.progress = FALSE)

# Plot and save the results
plot(beast_result, main = "BEAST Changepoint Detection")

png("plots/beastplot.png", width = 16, height = 9, units = "in", res = 320)
plot(beast_result, main = "BEAST Changepoint Detection")
dev.off()

# Extract MAP (Maximum A Posteriori) changepoints
# These are the most likely changepoints shown on the plot
trend_cp <- beast_result$trend$cp
trend_cp_clean <- trend_cp[!is.na(trend_cp)]

# Get probabilities for changepoints
trend_cp_pr <- beast_result$trend$cpPr
trend_cp_pr_clean <- trend_cp_pr[!is.na(trend_cp_pr)]

# Show changepoint candidate dates w/ confidence
for(i in 1:length(trend_cp_clean)) {
  year <- floor(trend_cp_clean[i])
  month <- round((trend_cp_clean[i] - year) * 12) + 1
  cat(sprintf("%d. %s %d (%.0f%% probability)\n", 
              i, month.abb[month], year, 
              trend_cp_pr_clean[i] * 100))
}



# ---- 3. IDENTIFY BREAKPOINT WITH CHOW TEST ----

# Test the STL trend component for breaks
trend_values <- as.numeric(trend) # STL trend component
n <- length(trend_values)
time_var <- 1:n

# Test one year on each side of 2020
test_months <- which(time(ts_monthly) >= 2019 & time(ts_monthly) <= 2021)

chow_results <- data.frame(
  month = test_months,
  f_stat = NA,
  p_value = NA
)

model_restricted <- lm(as.numeric(ts_monthly) ~ time_var + 
                         sin(2*pi*time_var/12) + cos(2*pi*time_var/12))

for(i in seq_along(test_months)) {
  break_idx <- test_months[i]
  dummy_test <- c(rep(0, break_idx), rep(1, n - break_idx))
  
  model_test <- lm(as.numeric(ts_monthly) ~ time_var + dummy_test + 
                     sin(2*pi*time_var/12) + cos(2*pi*time_var/12))
  
  RSS_r <- sum(residuals(model_restricted)^2)
  RSS_u <- sum(residuals(model_test)^2)
  F_stat <- ((RSS_r - RSS_u) / 1) / (RSS_u / (n - 4))
  
  chow_results$f_stat[i] <- F_stat
  chow_results$p_value[i] <- pf(F_stat, 1, n-4, lower.tail = FALSE)
}

# Get the breakpoint with the max F-statistic
best_break <- test_months[which.max(chow_results$f_stat)]
break_date <- time(ts_monthly)[best_break]
break_year <- floor(break_date)
break_month <- round((break_date - break_year) * 12) + 1

# Plot showing Feb 2020 is likeliest breakpoint as per Chow'S Test
plot(time(ts_monthly)[chow_results$month], chow_results$f_stat, 
     type = "l", lwd = 2, col = "darkblue",
     xlab = "Year", ylab = "F-statistic", 
     main = "Structural Break Test Statistics",
     xaxt = "n", panel.first = {
       abline(v = 2019:2021, col = "gray80", lwd = 1.5)
       abline(v = seq(2019, 2021, 1/12), col = "gray90", lwd = 0.5)
       abline(h = axTicks(2), col = "gray95", lwd = 0.5)
     })
axis(1, at = 2019:2021, labels = 2019:2021)
abline(v = time(ts_monthly)[chow_results$month[which.max(chow_results$f_stat)]], 
       col = "red", lty = 2, lwd = 2)



# ---- 4. NICE LOOKING STL PLOT ----

# Tidy data
stl_df <- tibble(
  Year      = as.numeric(time(ts_monthly)),
  Data      = as.numeric(ts_monthly),
  Trend     = as.numeric(stl_result$time.series[, "trend"]),
  Seasonal  = as.numeric(stl_result$time.series[, "seasonal"]),
  Remainder = as.numeric(stl_result$time.series[, "remainder"])
) |>
  pivot_longer(c(Data, Trend, Seasonal, Remainder),
               names_to = "Component", values_to = "Value") |>
  mutate(Component = factor(Component, levels = c("Data","Trend","Seasonal","Remainder")))

# Plot
p_stl <- ggplot(stl_df, aes(Year, Value)) +
  geom_line(linewidth = 0.6, colour = "black",
            data = dplyr::filter(stl_df, Component != "Remainder")) +
  geom_col(data = dplyr::filter(stl_df, Component == "Remainder"),
           aes(Year, Value),
           fill = "grey30", width = 0.02) +
  facet_grid(rows = vars(Component), scales = "free_y", switch = "y") +
  geom_hline(aes(yintercept = 0),
             data = dplyr::filter(stl_df, Component == "Remainder"),
             linewidth = 0.4, colour = "black") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "STL decomposition of monthly diarrhoea cases",
    x = "Year", y = NULL
  ) +
  theme_grey(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey85", colour = NA),
    strip.text.y.left = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.spacing = grid::unit(0.6, "lines")
  )

print(p_stl)

ggsave("plots/stl_diarrhoea_2012_2023.png", p_stl, width = 9, height = 5.8, dpi = 320)



# ---- 5. NICE CHOW PLOT ----

# Data
chow_df <- tibble(
  Year = time(ts_monthly)[chow_results$month],
  F    = chow_results$f_stat
) |>
  arrange(Year)

# Peak location -> label as calendar month
peak_idx  <- which.max(chow_df$F)
peak_year <- chow_df$Year[peak_idx]
peak_F    <- chow_df$F[peak_idx]
peak_y    <- floor(peak_year)
peak_frac <- peak_year - peak_y
peak_m    <- floor(peak_frac * 12) + 1
peak_lab  <- paste0("Peak: ", month.abb[peak_m], " ", peak_y)

# Axes/grid
year_min <- floor(min(chow_df$Year))
year_max <- ceiling(max(chow_df$Year))
month_grid <- seq(year_min, year_max, by = 1/12)
year_grid  <- seq(year_min, year_max, by = 1)

# Plot
p_chow <- ggplot(chow_df, aes(Year, F)) +
  # month and year grid (year grid slightly darker)
  geom_vline(xintercept = month_grid, colour = "grey92", linewidth = 0.25) +
  geom_vline(xintercept = year_grid,  colour = "grey85", linewidth = 0.35) +
  # series
  geom_area(alpha = 0.18, linewidth = 0) +
  geom_line(linewidth = 0.5) +
  # peak label
  annotate("label",
           x = peak_year, y = peak_F,
           label = peak_lab, vjust = -0.4, hjust = 0,
           label.size = 0, size = 3) +
  scale_x_continuous(breaks = year_grid,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(
    breaks = function(x) {
      rng <- range(x, na.rm = TRUE)
      by  <- max(5, round(diff(rng) / 6))  # coarse, integer ticks
      seq(0, ceiling(max(x, na.rm = TRUE) / by) * by, by = by)
    },
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  labs(title = "Chow test F-statistics, 2019-2021", x = "Year", y = "F-statistic") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6))
  )

print(p_chow)

ggsave("plots/chow.png", p_chow, width = 9, height = 5, dpi = 320)
