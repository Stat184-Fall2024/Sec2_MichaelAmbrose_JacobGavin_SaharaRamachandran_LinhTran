# Load Required Libraries 
library(ggplot2)
library(dplyr)
library(eurostat)
library(tidyr)

# Filter Renewable Energy Types 
renewable_countries <- energy_types %>%
  filter(type %in% c("Wind", "Solar", "Hydro", "Geothermal & Other", "Nuclear", "Conventional thermal")) %>%
  select(country) %>%
  distinct()  # Get unique countries producing relevant energy types

# Calculate Trade Balances 
renewable_trade_balance <- country_totals %>%
  filter(country %in% renewable_countries$country) %>%
  filter(type == "Imports" | type == "Exports") %>%
  pivot_wider(  # Reshape to separate Import and Export columns
    names_from = type, 
    values_from = c(`2016`, `2017`, `2018`)
  ) %>%
  mutate(
    net_trade_balance_2018 = `2018_Exports` - `2018_Imports`  # Calculate Net Trade Balance for 2018
  ) %>%
  select(country, net_trade_balance_2018)  # Keep only relevant columns

# Load Geospatial Data 
europe_map <- get_eurostat_geospatial(resolution = "60", nuts_level = 0)

# Merge Geospatial Data with Trade Balances 
europe_map <- europe_map %>%
  left_join(renewable_trade_balance, by = c("CNTR_CODE" = "country"))

# Map for Net Trade Balance 
ggplot(europe_map) +
  geom_sf(aes(fill = net_trade_balance_2018), color = "white", size = 0.2) + 
  scale_fill_gradientn(
    colors = c("#d13819", "#e8df11", "#09a804"),  # Gradient from red (negative) to green (positive)
    name = "Net Trade Balance (GWh)"
  ) +
  labs(
    title = "Net Trade Balance of Renewable Energy by Country",
    subtitle = "2018 Data: Exports - Imports",
    caption = "Source: Eurostat"
  ) +
  coord_sf(
    xlim = c(-25, 50),  # Set map longitude limits
    ylim = c(30, 75),   # Set map latitude limits
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center and style the title
    legend.position = "bottom"  # Position the legend at the bottom
  )
