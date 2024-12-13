# Load Required Libraries ----
library(tidyverse)
library(readxl)
library(countrycode)
library(glue)

# Read Raw Data ----
raw_code <- countrycode::codelist %>% 
  select(country_name = country.name.en, country = eurostat)

# Use relative path
file_path <- "data/Electricity_generation_statistics_2019.xlsx"
raw_excel <- read_excel(file_path, sheet = 3)

# Clean and Transform Data ----
raw_excel_clean <- raw_excel %>% 
  filter(!is.na(...4)) %>% 
  mutate(
    country = str_remove_all(...4, "[:digit:]"), 
    .before = ...1
  ) %>% 
  mutate(
    country = if_else(
      str_length(country) > 1, 
      country, 
      NA_character_
    ),
    country = str_extract(country, "[:alpha:]+")
  ) %>% 
  fill(country) %>% 
  select(-c(...1, ...2, ...14:...18))

# Prepare Row Statistics ----
row_stat <- read_excel(
  path = file_path, 
  sheet = 3,
  range = "C48:C61",
  col_names = FALSE
)[[1]][c(1, 3:14)] %>%   
  str_remove("[:digit:]") %>% 
  str_remove("of which: ") %>% 
  str_remove("\\.") %>% 
  str_trim()

# Create Country Range ----
country_range <- tibble(
  row_start = seq(from = 46, to = 454, by = 34), 
  row_end = seq(from = 61, to = 469, by = 34)
) %>% 
  mutate(
    col1 = 4, 
    col2 = col1 + 5, 
    col3 = col2 + 5
  ) %>% 
  pivot_longer(
    cols = col1:col3, 
    names_to = "col_var", 
    values_to = "col_start"
  ) %>% 
  mutate(col_end = col_start + 2) %>% 
  select(-col_var) %>% 
  slice(-n(), -(n() - 1)) %>% 
  mutate(row_stat = list(row_stat))

# Define Supporting Functions ----
get_country_stats <- function(row_start, row_end, col_start, col_end, row_stat) {
  col_range <- glue("{LETTERS[col_start]}{row_start}:{LETTERS[col_end]}{row_end}")
  raw_data <- suppressMessages(read_excel(
    file_path, sheet = 3, col_names = FALSE, range = col_range
  ))
  country_data <- raw_data %>% 
    set_names(nm = c(2016:2018)) %>% 
    filter(!is.na(`2016`), `2016` != "2016") %>% 
    mutate(
      country = if_else(
        is.na(`2017`), 
        `2016`, 
        NA_character_
      ), 
      .before = `2016`
    ) %>% 
    fill(country) %>% 
    filter(!is.na(`2017`)) %>% 
    mutate(
      type = row_stat, 
      .after = country, 
      level = c("Total", "Level 1", "Level 1", "Level 1", "Level 2", 
                "Level 1", "Level 1", "Level 1", "Level 1", "Total", 
                "Total", "Total", "Total")
    )%>% 
    mutate(across(c(`2016`:`2018`), as.double))
  return(country_data)
}

# Calculate Summary Tables ----
all_countries <- country_range %>% 
  pmap_dfr(get_country_stats) %>% 
  left_join(raw_code, by = "country") %>% 
  select(country, country_name, everything())

country_totals <- all_countries %>% filter(level == "Total")
energy_types <- all_countries %>% filter(level != "Total")

# Save to CSV ----
write.csv(country_totals, "data/country_totals.csv", row.names = FALSE)
write.csv(energy_types, "data/energy_types.csv", row.names = FALSE)
