# Load required library
library(tidyverse)

# Read the weekly data
weekly_data <- read_csv("Dashboard_MLcleaned.csv")

# Convert Thai years to Western years and aggregate by province and year
annual_data <- weekly_data %>%
  # Convert Thai year to Western year
  mutate(year_western = year - 543) %>%
  # Group by province and year and sum the weekly values
  group_by(province, year_western) %>%
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
  # Pivot to wide format with years as columns
  pivot_wider(
    names_from = year_western,
    values_from = total_cases,
    values_fill = 0  # Fill missing values with 0
  ) %>%
  # Rename province column to match
  rename(Province = province)

# Write output file
write_csv(annual_data, "Diarrhoea_cases_2020-2023.csv")


# Read both datasets
cases_2012_2019 <- read_csv("Diarrhoea_cases_2012-2019.csv")
cases_2020_2023 <- read_csv("Diarrhoea_cases_2020-2023.csv")

# Extract unique province names from each dataset
provinces_2012_2019 <- unique(cases_2012_2019$Province)
provinces_2020_2023 <- unique(cases_2020_2023$Province)

# Find provinces that don't match
in_2020_not_2012 <- setdiff(provinces_2020_2023, provinces_2012_2019)
in_2012_not_2020 <- setdiff(provinces_2012_2019, provinces_2020_2023)

# Display mismatches
print(in_2020_not_2012)
print(in_2012_not_2020)

# Correct mapping based on the mismatches found
province_mapping <- c(
  "Bueng Kan" = "Bungkan",
  "Buriram" = "Buri Ram",
  "Chonburi" = "Chon Buri",
  "Lopburi" = "Lop Buri",
  "Phang Nga" = "Phangnga",
  "Phra Nakhon Si Ayutthaya" = "P.Nakhon S.Ayutthaya",
  "Prachinburi" = "Prachin Buri"
)

# Apply the renaming to the 2020-2023 dataset
cases_2020_2023_renamed <- cases_2020_2023 %>%
  mutate(Province = case_when(
    Province %in% names(province_mapping) ~ province_mapping[Province],
    TRUE ~ Province
  ))

# Verify the renaming worked
remaining_mismatches <- setdiff(unique(cases_2020_2023_renamed$Province), 
                                provinces_2012_2019)
length(remaining_mismatches)

# Overwrite the original file with corrected names
write_csv(cases_2020_2023_renamed, "Diarrhoea_cases_2020-2023.csv")


# Join datasets

# Read both datasets
cases_2012_2019 <- read_csv("Diarrhoea_cases_2012-2019.csv")
cases_2020_2023 <- read_csv("Diarrhoea_cases_2020-2023.csv")

# Combine them by joining on Province
cases_2012_2023 <- cases_2012_2019 %>%
  left_join(cases_2020_2023, by = "Province") %>%
  # Reorder columns chronologically
  dplyr::select(Province, `2012`:`2023`)

# Write the combined file
write_csv(cases_2012_2023, "Diarrhoea_cases_2012-2023.csv")


