# Libraries

library(tidyverse)
library(sf)
library(spdep)


# ---- Section 1: Import data and clean the shapefile ----

# Diarrhoea data
diarrhoea_case_data <- read_csv("Diarrhoea_cases_2012-2023.csv")

# Population stratification data by province
pop_df <- read_csv("pop_info_2023.csv")
# Clean up province names in pop_df to match those in the main df
pop_df$Province[pop_df$Province == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
pop_df$Province[pop_df$Province == "Bueng Kan"] <- "Bungkan"

# Shapefile data (.dbf and .shx files are also needed in the directory)
thailand_shapefile <- sf::st_read("gadm41_THA_1.shp")
# Clean up province names in shapefile to match those in the csvs above 
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Bangkok Metropolis"] <- "Bangkok"
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Bueng Kan"] <- "Bungkan"
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"


# Verify that all province names match
names_in_csv <- diarrhoea_case_data$Province
names_in_pop <- pop_df$Province
names_in_shp <- thailand_shapefile$NAME_1

setdiff(names_in_csv, names_in_shp)
setdiff(names_in_csv, names_in_pop)


# Creating a long format of the data with provincial rate per 100,000
diarrhoea_case_data_long <- diarrhoea_case_data %>%
  pivot_longer(cols = -Province,
               names_to  = "Year",
               values_to = "Cases") %>%
  mutate(Year = as.numeric(Year)) %>% # Yeartreated as numeric
  left_join(
    # Extract provincial population no.
    pop_df %>% dplyr::select(Province, pop_total = Total),
    by = "Province"
  ) %>%
  mutate(Rate = Cases / pop_total * 1e5)      



# ---- Section 2: Choropleth config ----

# Calculate common ceiling for consistent scales across all maps
max_rate_all_years <- max(diarrhoea_case_data_long$Rate, na.rm = TRUE)
colour_ceiling <- ceiling(max_rate_all_years / 1000) * 1000 # Round up to nearest 1,000

# Colour breaks for all maps using 6 breaks 
colour_breaks <- seq(0, colour_ceiling, length.out = 6)

# Plot theme settings
theme_set(theme_void()) # Clean map theme
map_theme <- theme(
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "grey40"),
  legend.position = "right",
  legend.title = element_text(size = 11, face = "bold"),
  legend.text = element_text(size = 10),
  legend.key.height = unit(1.2, "cm"),
  legend.key.width = unit(0.4, "cm")
)



# ---- Section 3: Spatial Analysis Setup ----

# Create neighbour topology
nb <- spdep::poly2nb(thailand_shapefile, queen = TRUE)

# There was an issue with the analysis as Phuket had 0 neighbours (it's an island)
# Fix zero neighbours for Phuket island (connecting to nearby Phangnga)
phuket_index <- which(thailand_shapefile$NAME_1 == "Phuket")
phangnga_index <- which(thailand_shapefile$NAME_1 == "Phangnga")
nb[[phuket_index]] <- unique(c(nb[[phuket_index]], phangnga_index))
nb[[phangnga_index]] <- unique(c(nb[[phangnga_index]], phuket_index))
nb[[phuket_index]] <- nb[[phuket_index]][nb[[phuket_index]] != 0]

# Transform binary contiguity matrix (nb) into standardised weights matrix for Moran's I
lw <- spdep::nb2listw(nb, style = "W")



# ---- Section 4: Annual Choropleths with Moran's I ----

# Years to process
years <- 2012:2023

# Storage for Moran's I results
moran_results <- data.frame(
  Year = years, 
  Morans_I = NA, 
  P_value = NA,
  Interpretation = NA
)

# Create output directory
dir.create("choropleth_outputs")

# Log scale breaks covering full range
log_breaks <- c(100, 300, 1000, 3000, 10000, 20000)
log_labels <- c("100", "300", "1k", "3k", "10k", "20k")

# Generate choropleth for each year
for (i in seq_along(years)) {
  current_year <- years[i]
  
  # Join current year data to shapefile
  current_sf <- thailand_shapefile %>%
    left_join(
      diarrhoea_case_data_long %>%
        filter(Year == current_year) %>%
        dplyr::select(Province, Rate),
      by = c("NAME_1" = "Province")
    ) %>%
    # Add small constant to handle zeros for log scale
    mutate(Rate_log = pmax(Rate, 1))
  
  # Calculate Moran's I statistic
  moran_test <- spdep::moran.test(current_sf$Rate, lw, na.action = na.omit)
  moran_results$Morans_I[i] <- round(moran_test$estimate[["Moran I statistic"]], 3)
  moran_results$P_value[i] <- moran_test$p.value
  moran_results$Significant[i] <- moran_test$p.value < 0.05
  
  # Create choropleth with log scale
  plot <- ggplot(data = current_sf) +
    geom_sf(aes(fill = Rate_log), colour = "white", size = 0.15) +
    scale_fill_viridis_c(
      option = "plasma",           
      trans = "log10",
      breaks = log_breaks,
      labels = log_labels,
      limits = c(200, 20000), # Squished limits
      oob = scales::squish,
      name = "Rate per 100k\n(log scale)"
    ) +
    labs(
      title = paste0(current_year),
      caption = if(moran_results$Significant[i]) 
        paste0("I = ", moran_results$Morans_I[i]) else NULL
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 9, colour = "grey40", hjust = 0.95),
      legend.position = "right",
      legend.title = element_text(size = 9, colour = "grey20"),
      legend.text = element_text(size = 8),
      legend.key.height = unit(1, "cm"),
      legend.key.width = unit(0.3, "cm"),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  # Save plot
  ggsave(
    filename = sprintf("choropleth_outputs/choropleth_%d.png", current_year),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}

# Moran's I summary
print(moran_results)



# ---- Section 5: Small Multiples with Enhanced Design ----

# Data with log transformation
sf_multiples <- diarrhoea_case_data_long %>%
  left_join(thailand_shapefile, by = c("Province" = "NAME_1")) %>%
  st_as_sf() %>%
  mutate(Rate_log = pmax(Rate, 1))

# Small multiples plot 
small_multiples <- ggplot(sf_multiples) +
  geom_sf(aes(fill = Rate_log), colour = "white", size = 0.02) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    breaks = log_breaks,
    labels = log_labels,
    limits = c(200, 20000),
    oob = scales::squish,
    name = "Rate per 100,000 (log scale)"
  ) +
  facet_wrap(~Year, nrow = 2, ncol = 6) +
  labs(title = "Spatiotemporal Evolution") +
  theme_void() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.title = element_text(size = 9, colour = "grey30"),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              margin = margin(b = 20)), # Added bottom margin
    plot.margin = margin(10, 10, 10, 10),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(
  "choropleth_outputs/small_multiples_all_years.png",
  small_multiples,
  width = 12,
  height = 9,
  dpi = 300,
  bg = "white"
)



# ---- Section 6: Hotspot Analysis ----

# Highest mean incidence (2012-2023)
mean_incidence <- diarrhoea_case_data_long %>%
  group_by(Province) %>%
  summarise(mean_rate = mean(Rate, na.rm = TRUE)) %>%
  arrange(desc(mean_rate))

# Highest mean incidence provinces
print(head(mean_incidence, 5))
# Lowest mean incidence provinces
print(tail(mean_incidence, 5))

# Most consistent hotspots (top 5 appearances across years)
yearly_top5 <- map_dfr(2012:2023, function(yr) {
  diarrhoea_case_data_long %>%
    filter(Year == yr) %>%
    slice_max(Rate, n = 5) %>%
    mutate(year = yr) %>%
    select(Province, year)
})

hotspot_consistency <- yearly_top5 %>%
  count(Province, sort = TRUE) %>%
  rename(appearances = n)

# Most consistent appearances in yearly top 5
print(hotspot_consistency)


