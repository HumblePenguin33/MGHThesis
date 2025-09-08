# This script builds a province-year panel for Thailand (2016–2019) with covariate 
# data and fits negative binomial regression models of diarrhoea incidence using INLA:
#   A) Base NB with covariates + year
#   B) Add IID province random effects
#   C) Add BYM2 (structured + unstructured) spatial random effects
# It compares model metrics (WAIC/DIC/LPML) and reports key parameters, interprets the
# Poverty:Personnel interaction via IRRs, maps province random effects, computes
# the Median Rate Ratio (MRR), and checks residual spatial autocorrelation by year.

library(INLA)
library(tidyverse)
library(sf)
library(spdep)
library(ggplot2)


# ---- IMPORT DATA ----

thailand_shapefile <- sf::st_read("gadm41_THA_1.shp", quiet = TRUE)
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Bangkok Metropolis"] <- "Bangkok"
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Bueng Kan"] <- "Bungkan"
thailand_shapefile$NAME_1[thailand_shapefile$NAME_1 == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"

# Case counts by province
diarrhoea_case_data <- read_csv("Diarrhoea_cases_2012-2023.csv", show_col_types = FALSE)

# Population totals to calc incidence via exposure
pop_df <- read_csv("pop_info_2023.csv", show_col_types = FALSE)
pop_df$Province[pop_df$Province == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
pop_df$Province[pop_df$Province == "Bueng Kan"] <- "Bungkan"

# Poverty (% of population)
poverty_data <- readxl::read_xlsx("Poverty.xlsx")
poverty_data[[1]][poverty_data[[1]] == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
poverty_data[[1]][poverty_data[[1]] == "Phattalung"] <- "Phatthalung"

# Health personnel
personnel_data <- readxl::read_xlsx("Personnel.xlsx")
personnel_data[[1]][personnel_data[[1]] == "Phra Nakhon Si Ayutthaya"] <- "P.Nakhon S.Ayutthaya"
personnel_data[[1]][personnel_data[[1]] == "Phattalung"] <- "Phatthalung"



# ---- CREATE DATA PANEL ----

# Build a province-year panel with covariates and offsets
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
      # Personnel density adjusted to per 100k
      Personnel_per_100k = Personnel / Population * 100000,
      log_pop = log(Population)
    ) %>%
    dplyr::select(Province, Year, Cases, Poverty, Personnel_per_100k, Population, log_pop)
}) %>%
  filter(complete.cases(.))

panel_data$ProvinceID <- match(panel_data$Province, thailand_shapefile$NAME_1)

# Standardise covariates (mean 0, SD 1)
panel_data$Poverty_std <- scale(panel_data$Poverty)[,1]
panel_data$Personnel_std <- scale(panel_data$Personnel_per_100k)[,1]



# ---- SPATIAL STRUCTURE ----

nb <- poly2nb(thailand_shapefile, queen = TRUE)

# Fix Phuket
phuket_idx <- which(thailand_shapefile$NAME_1 == "Phuket")
phangnga_idx <- which(thailand_shapefile$NAME_1 == "Phangnga")
nb[[phuket_idx]] <- unique(c(nb[[phuket_idx]], phangnga_idx))
nb[[phangnga_idx]] <- unique(c(nb[[phangnga_idx]], phuket_idx))
nb[[phuket_idx]] <- nb[[phuket_idx]][nb[[phuket_idx]] != 0]

nb2INLA("thailand.adj", nb)
g <- inla.read.graph("thailand.adj")



# ---- FIT MODELS ----

# All models use negative binomial with population as exposure (E) to get incidence

# Model A: Base NB
model_base <- inla(
  Cases ~ Poverty_std * Personnel_std + Year,
  family = "nbinomial",
  E = panel_data$Population,
  data = panel_data,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE),
  verbose = FALSE
)

# Model B: IID province effects 
model_iid <- inla(
  Cases ~ Poverty_std * Personnel_std + Year + 
    f(ProvinceID, model = "iid",
      hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))), # Prior on precision
  family = "nbinomial",
  E = panel_data$Population,
  data = panel_data,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE),
  verbose = FALSE
)

# Model C: BYM2 spatial
model_bym2 <- inla(
  Cases ~ Poverty_std * Personnel_std + Year + 
    f(ProvinceID, model = "bym2", graph = g, scale.model = TRUE,
      hyper = list(
        prec = list(prior = "pc.prec", param = c(1, 0.01)), # Prior on precision
        phi = list(prior = "pc", param = c(0.5, 0.5)))),
  family = "nbinomial",
  E = panel_data$Population,
  data = panel_data,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE),
  verbose = FALSE
)



# ---- MODEL COMPARISON ----

# Aggregate WAIC/DIC/LPML for quick side-by-side comparison
results <- data.frame(
  Model = c("Base NB", "NB + IID", "NB + BYM2"),
  WAIC = c(model_base$waic$waic, model_iid$waic$waic, model_bym2$waic$waic),
  DIC = c(model_base$dic$dic, model_iid$dic$dic, model_bym2$dic$dic),
  LPML = c(sum(log(model_base$cpo$cpo), na.rm = TRUE),
           sum(log(model_iid$cpo$cpo), na.rm = TRUE),
           sum(log(model_bym2$cpo$cpo), na.rm = TRUE))
)

# Differences relative to the base model
results$ΔWAIC <- results$WAIC - results$WAIC[1]
results$ΔDIC <- results$DIC - results$DIC[1]
results$ΔLPML <- results$LPML - results$LPML[1]

print(results)



# ---- KEY PARAMETERS ----

# IID was the most parsimonious model
best_model <- model_iid

# NB size for the nbinomial observations (1/overdispersion)
# Large term means that the distribution approaches Poisson
rownames(best_model$summary.hyperpar)

# > rownames(best_model$summary.hyperpar)
# [1] "size for the nbinomial observations (1/overdispersion)"
# [2] "Precision for ProvinceID" 

nb_overdisp <- best_model$summary.hyperpar["size for the nbinomial observations (1/overdispersion)", c("mean", "0.025quant", "0.975quant")]
sprintf("NB size parameter: %.2f [%.2f–%.2f]", nb_overdisp["mean"], nb_overdisp["0.025quant"], nb_overdisp["0.975quant"])

# Province random-effect variance
province_var <- 1/best_model$summary.hyperpar["Precision for ProvinceID", "mean"]
province_sd <- sqrt(province_var)

province_var
province_sd

# BYM2 mixing parameter φ w/ 95% CrI
rho_mean <- model_bym2$summary.hyperpar["Phi for ProvinceID", "mean"]
rho_ci <- c(model_bym2$summary.hyperpar["Phi for ProvinceID", "0.025quant"],
            model_bym2$summary.hyperpar["Phi for ProvinceID", "0.975quant"])

sprintf("BYM2 spatial proportion: %.3f [%.3f, %.3f]", rho_mean, rho_ci[1], rho_ci[2])



# ---- FIXED EFFECTS IRRs with 95% CI ----

# Fixed effects to IRRs to interpret as per 1 SD change in standardised covariate
bfix <- best_model$summary.fixed
sd_pov <- sd(panel_data$Poverty, na.rm = TRUE)
sd_per <- sd(panel_data$Personnel_per_100k, na.rm = TRUE)

# Main effects (conditionally at mean of other variable)
pov_irr <- exp(bfix["Poverty_std", c("0.025quant", "mean", "0.975quant")])
per_irr <- exp(bfix["Personnel_std", c("0.025quant", "mean", "0.975quant")])
int_irr <- exp(bfix["Poverty_std:Personnel_std", c("0.025quant", "mean", "0.975quant")])

sprintf("Poverty: %.3f [%.3f-%.3f]", pov_irr[2], pov_irr[1], pov_irr[3])
sprintf("Personnel: %.3f [%.3f-%.3f]", per_irr[2], per_irr[1], per_irr[3])
# Interaction is per SD², unlike the other two!
sprintf("Interaction: %.3f [%.3f-%.3f]", int_irr[2], int_irr[1], int_irr[3])

# Year effects vs 2016 baseline
yr_rows <- grep("^Year", rownames(bfix))
for(i in yr_rows) {
  yr <- sub("Year", "", rownames(bfix)[i])
  irr <- exp(bfix[i, c("0.025quant", "mean", "0.975quant")])
  # Needed "cat" in front here
  cat(sprintf("Year %s vs 2016: %.3f [%.3f-%.3f]\n", yr, irr[2], irr[1], irr[3]))
}



# ---- INTERACTION ----

# Interaction (per SD²): 1.016 [0.985-1.048]

# Poverty effect at different personnel levels
pov_coef <- bfix["Poverty_std", "mean"]
int_coef <- bfix["Poverty_std:Personnel_std", "mean"]

# Personnel at -1 SD
pov_effect_low <- exp(pov_coef - int_coef)
pov_effect_low
# Personnel at mean
pov_effect_mean <- exp(pov_coef)
pov_effect_mean
# Personnel at +1 SD
pov_effect_high <- exp(pov_coef + int_coef)
pov_effect_high


# Personnel effect across poverty levels
per_coef <- bfix["Personnel_std", "mean"]

# Poverty at -1 SD
per_effect_low <- exp(per_coef - int_coef)
per_effect_low
# Poverty at mean
per_effect_mean <- exp(per_coef)
per_effect_mean
# Poverty at +1 SD
per_effect_high <- exp(per_coef + int_coef)
per_effect_high



# ---- PROVINCE EFFECTS MAP ----

province_effects <- best_model$summary.random$ProvinceID
province_effects$Province <- thailand_shapefile$NAME_1[province_effects$ID]

map_data <- thailand_shapefile %>%
  left_join(province_effects[, c("Province", "mean")], 
            by = c("NAME_1" = "Province"))

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = mean), colour = "white", size = 0.15) +
  scale_fill_viridis_c(
    option = "plasma",
    limits = c(-3, 3),
    oob = scales::squish,
    breaks = seq(-3, 3, 1),
    labels = seq(-3, 3, 1),
    name = "Random Effect (log scale)"
  ) +
  labs(
    title = "IID province random effects",
    caption = NULL
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

print(p_map)

ggsave("plots/IID_random_effects.png", p_map, width = 8, height = 6, dpi = 300, bg = "white")



# ---- MRR (Median Rate Ratio) ----

# MRR is the median (over the random effects) of the incidence rate ratio between 
# two randomly chosen provinces with identical covariates.
# eg. If MRR = 1.30, holding covariates equal, the median difference due to
# province heterogeneity is would be 30%.

prec_marg <- best_model$marginals.hyperpar[["Precision for ProvinceID"]]
sd_marg <- inla.tmarginal(function(x) sqrt(1/x), prec_marg)
mrr_marg <- inla.tmarginal(function(s) exp(0.6745*sqrt(2)*s), sd_marg)
mrr_q <- inla.qmarginal(c(0.025, 0.5, 0.975), mrr_marg)

sprintf("MRR: %.2f [%.2f-%.2f]", mrr_q[2], mrr_q[1], mrr_q[3])



# ---- RESIDUAL SPATIAL AUTOCORRELATION ----

# Extract Pearson residuals
theta <- best_model$summary.hyperpar["size for the nbinomial observations (1/overdispersion)", "mean"]
# Test Moran's I each year ob extracted residuals
fitted <- best_model$summary.fitted.values$mean[1:nrow(panel_data)]
panel_data$pearson_resid <- (panel_data$Cases - fitted * panel_data$Population) / sqrt(fitted * 
                                                                                         panel_data$Population * (1 + fitted * panel_data$Population / theta))


# Create spatial weights
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Test each year
for(year in 2016:2019) {
  # Get residuals for this year, ordered by shapefile
  year_resid <- panel_data$pearson_resid[panel_data$Year == year]
  prov_order <- match(thailand_shapefile$NAME_1, panel_data$Province[panel_data$Year == year])
  year_resid_ordered <- year_resid[prov_order]
  
  # Moran's I test
  moran_result <- moran.test(year_resid_ordered, lw, na.action = na.omit)
  cat(sprintf("Year %d: I = %.3f, p = %.3f\n", year, moran_result$estimate[1], moran_result$p.value))
}

# Quickly check correlation between observed counts and fitted counts to make sure there's no
# double scaling with the way pop exposure was handled
cor(panel_data$Cases, best_model$summary.fitted.values$mean[1:nrow(panel_data)] * panel_data$Population)




