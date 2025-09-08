library(tidyverse)
library(readxl)

# Import datasets
pop_df <- read_csv("pop_info_2023.csv", show_col_types = FALSE)
pop_df$Province[pop_df$Province == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
pop_df$Province[pop_df$Province == "Bueng Kan"] <- "Bungkan"

poverty_data <- read_xlsx("Poverty.xlsx")
poverty_data[[1]][poverty_data[[1]] == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
poverty_data[[1]][poverty_data[[1]] == "Phattalung"] <- "Phatthalung"

personnel_data <- read_xlsx("Personnel.xlsx")
personnel_data[[1]][personnel_data[[1]] == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
personnel_data[[1]][personnel_data[[1]] == "Phattalung"] <- "Phatthalung"

# Create dataset for 2016-2019
scatter_data <- map_dfr(2016:2019, function(year) {
  poverty_data %>%
    select(Province = 1, Poverty = as.character(year)) %>%
    left_join(personnel_data %>% select(Province = 1, Personnel = as.character(year)), by = "Province") %>%
    left_join(pop_df %>% select(Province, Population = Total), by = "Province") %>%
    mutate(
      Year = year,
      Poverty = as.numeric(Poverty),
      Personnel = as.numeric(Personnel),
      Personnel_per_100k = Personnel / Population * 100000
    )
}) %>%
  filter(complete.cases(.))

# Calculate means
mean_poverty <- mean(scatter_data$Poverty)
mean_personnel <- mean(scatter_data$Personnel_per_100k)

# Equity assessment (minimal)
model <- lm(Personnel_per_100k ~ Poverty, data = scatter_data)
slope <- coef(model)[2]
p_val <- summary(model)$coefficients[2, 4]

# Slope label (β per 1% poverty), with compact p-format
slope_lab <- sprintf("\u03B2 = %.2f per 1%% poverty (p < 0.01)", slope)

# Place label in top-right
xr <- range(scatter_data$Poverty, na.rm = TRUE)
yr <- range(scatter_data$Personnel_per_100k, na.rm = TRUE)
x_pos <- xr[2] - 0.02 * diff(xr)
y_pos <- yr[2] - 0.35 * diff(yr)

# Create scatterplot
p <- ggplot(scatter_data, aes(x = Poverty, y = Personnel_per_100k, colour = factor(Year))) +
  geom_vline(xintercept = mean_poverty, linetype = "dashed", colour = "grey50", alpha = 0.5) +
  geom_hline(yintercept = mean_personnel, linetype = "dashed", colour = "grey50", alpha = 0.5) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, colour = "red", aes(group = 1)) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.10))) +
  annotate(
    "label", x = x_pos, y = y_pos, label = slope_lab,
    hjust = 1, vjust = 1, size = 3.6,
    label.size = 0, fill = "white", alpha = 0.45
  ) +
  scale_colour_viridis_d() +
  labs(
    title = "Healthcare personnel vs poverty by province (2016-2019)",
    x = "Poverty (%)",
    y = "Healthcare personnel per 100k population",
    colour = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save the plot
p
ggsave("plots/personnel_vs_poverty.png", plot = p, width = 10, height = 6, dpi = 300)

# Print correlation
cor_val <- cor(scatter_data$Poverty, scatter_data$Personnel_per_100k, use = "complete.obs")
cor_val

# Gini coefficient (inter-provincial)
gini <- function(x) {
  n <- length(x)
  x <- sort(x)
  2 * sum(x * 1:n) / (n * sum(x)) - (n + 1) / n
}
gini(scatter_data$Personnel_per_100k)

# Quadrant analysis
scatter_data <- scatter_data %>%
  mutate(quadrant = case_when(
    Poverty > mean_poverty & Personnel_per_100k < mean_personnel ~ "High poverty, Low personnel",
    Poverty > mean_poverty & Personnel_per_100k >= mean_personnel ~ "High poverty, High personnel",
    Poverty <= mean_poverty & Personnel_per_100k < mean_personnel ~ "Low poverty, Low personnel",
    TRUE ~ "Low poverty, High personnel"
  ))


# Personnel ratio comparison between poorest and richest quartiles
q1_pov <- quantile(scatter_data$Poverty, 0.25)
q3_pov <- quantile(scatter_data$Poverty, 0.75)
mean_poor <- mean(scatter_data$Personnel_per_100k[scatter_data$Poverty >= q3_pov])
mean_rich <- mean(scatter_data$Personnel_per_100k[scatter_data$Poverty <= q1_pov])

# Equity summary
cat(sprintf("Slope: %.2f personnel per poverty %% \n", slope))
cat(sprintf("Personnel in poorest quartile: %.1f per 100k\n", mean_poor))
cat(sprintf("Personnel in richest quartile: %.1f per 100k\n", mean_rich))
