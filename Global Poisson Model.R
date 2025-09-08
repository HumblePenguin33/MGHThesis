### Global Poisson Model

# Load packages
library(tidyverse)
library(sf)
library(spdep)
library(MASS)
library(spgwr)
library(ggplot2)

# ---- DATA IMPORT AND CLEANING ----

# Import shapefile
thailand_shapefile <- sf::st_read("gadm41_THA_1.shp")
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Bangkok Metropolis"] <- "Bangkok"
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Bueng Kan"] <- "Bungkan"
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"

# Import datasets
diarrhoea_case_data <- read_csv("Diarrhoea_cases_2012-2023.csv")
pop_df <- read_csv("pop_info_2023.csv")
pop_df$Province[pop_df$Province == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
pop_df$Province[pop_df$Province == "Bueng Kan"] <- "Bungkan"

poverty_data <- readxl::read_xlsx("Poverty.xlsx")
poverty_data[[1]][poverty_data[[1]] == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
poverty_data[[1]][poverty_data[[1]] == "Phattalung"] <- "Phatthalung"

personnel_data <- readxl::read_xlsx("Personnel.xlsx")
personnel_data[[1]][personnel_data[[1]] == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
personnel_data[[1]][personnel_data[[1]] == "Phattalung"] <- "Phatthalung"

# ---- Data panel for 2016-2019 ----

panel_data <- map_dfr(2016:2019, function(year) {
  thailand_shapefile %>%
    st_drop_geometry() %>%
    mutate(Year = factor(year, levels = 2016:2019)) %>%
    left_join(diarrhoea_case_data %>% dplyr::select(Province, Cases = as.character(year)), 
              by = c("NAME_1" = "Province")) %>%
    left_join(poverty_data %>% dplyr::select(1, Poverty = as.character(year)), 
              by = c("NAME_1" = names(poverty_data)[1])) %>%
    left_join(personnel_data %>% dplyr::select(1, Personnel = as.character(year)), 
              by = c("NAME_1" = names(personnel_data)[1])) %>%
    left_join(pop_df %>% dplyr::select(Province, Population = Total), 
              by = c("NAME_1" = "Province")) %>%
    mutate(
      Province = NAME_1,
      Cases = as.numeric(Cases),
      Poverty = as.numeric(Poverty),
      Personnel = as.numeric(Personnel),
      Personnel_per_100k = Personnel / Population * 100000,
      log_pop = log(Population)
    ) %>%
    dplyr::select(Province, Year, Cases, Poverty, Personnel_per_100k, Population, log_pop)
}) %>%
  filter(complete.cases(.))



# Fit Poisson GLM
poisson_model <- glm(
  Cases ~ Poverty + Personnel_per_100k + Year + offset(log_pop),
  data = panel_data,
  family = poisson
)

# Poisson model summary
print(summary(poisson_model))

# Calculate overdispersion parameter φ
phi_poisson <- deviance(poisson_model) / df.residual(poisson_model)
phi_poisson

# Residual check
poisson_resid <- residuals(poisson_model, type = "deviance")
min(poisson_resid)
max(poisson_resid)
sd(poisson_resid)



