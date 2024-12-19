# BOAST Coding Style
# Load Required Libraries 
library(tidyverse)
library(readxl)
library(countrycode)
library(glue)
library(ggplot2)
library(dplyr)
library(eurostat)
library(tidyr)

# Set Working Directory 
setwd("C:/Users/linht/Github/Sec2__FP_MichaelAmbrose_JacobGavin_SahanaRamachandran_LinhTran_YuvBoghani")

# Load and Prepare Raw Data ----
raw_code <- countrycode::codelist %>% 
  select(country_name = country.name.en, country = eurostat)

file_path <- "data/Electricity_generation_statistics_2019.xlsx"
raw_excel <- read_excel(file_path, sheet = 3)

# Clean and Transform Raw Data 
raw_excel_clean <- raw_excel %>%
  filter(!is.na(...4)) %>%  # Remove rows with missing data
  mutate(
    country = str_remove_all(...4, "[:digit:]"),  # Extract country names
    .before = ...1
  ) %>%
  mutate(
    country = if_else(
      str_length(country) > 1, country, NA_character_  # Validate country names
    ),
    country = str_extract(country, "[:alpha:]+")  # Retain only alphabetic characters
  ) %>%
  fill(country) %>%  # Fill down country names
  select(-c(...1, ...2, ...14:...18))  # Drop irrelevant columns

# Extract Row Statistics
row_stat <- read_excel(
  path = file_path, 
  sheet = 3,
  range = "C48:C61",
  col_names = FALSE
)[[1]][c(1, 3:14)] %>% 
  str_remove("[:digit:]") %>%
  str_remove("of which: ") %>%
  str_remove("\\.") %>%
  str_trim()  # Clean string data

# Create Country Ranges 
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

# Define Function for Processing Country Data 
get_country_stats <- function(row_start, row_end, col_start, col_end, row_stat) {
  col_range <- glue("{LETTERS[col_start]}{row_start}:{LETTERS[col_end]}{row_end}")
  raw_data <- suppressMessages(read_excel(
    file_path, sheet = 3, col_names = FALSE, range = col_range
  ))
  country_data <- raw_data %>%
    set_names(nm = c(2016:2018)) %>%
    filter(!is.na(`2016`), `2016` != "2016") %>%
    mutate(
      country = if_else(is.na(`2017`), `2016`, NA_character_),
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
    ) %>%
    mutate(across(c(`2016`:`2018`), as.double))  # Convert columns to numeric
  return(country_data)
}

# Process and Organize Data 
all_countries <- country_range %>%
  pmap_dfr(get_country_stats) %>%
  left_join(raw_code, by = "country") %>%
  select(country, country_name, everything())

country_totals <- all_countries %>%
  filter(level == "Total")  # Filter for total production data

energy_types <- all_countries %>%
  filter(level != "Total")  # Filter for specific energy types

# Save Processed Data 
write.csv(country_totals, "data/country_totals.csv", row.names = FALSE)
write.csv(energy_types, "data/energy_types.csv", row.names = FALSE)

# Compute Summary Statistics 
summary_table <- read.csv("data/country_totals.csv") %>%
  filter(type == "Total net production") %>%  # Filter for 'Total net production'
  rename_with(make.names) %>%  # Ensure column names are syntactically valid
  summarise(
    Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
    Year_2016 = summary(.$X2016)[1:6],
    Year_2017 = summary(.$X2017)[1:6],
    Year_2018 = summary(.$X2018)[1:6]
  )

# Calculate Energy Categories and Combine Data 
combinedEnergyData <- bind_rows(
  energy_types %>%
    filter(type %in% c("Hydro", "Wind", "Solar", "Geothermal")) %>% # Select renewable sources
    pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "production") %>% # Reshape years into long format
    group_by(year) %>%
    summarize(totalProduction = sum(production, na.rm = TRUE)) %>% # Calculate total renewable production by year
    mutate(category = "Renewable"),
  energy_types %>%
    filter(!type %in% c("Hydro", "Wind", "Solar", "Geothermal")) %>% # Select non-renewable sources
    pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "production") %>% # Reshape years into long format
    group_by(year) %>%
    summarize(totalProduction = sum(production, na.rm = TRUE)) %>% # Calculate total non-renewable production by year
    mutate(category = "Non-Renewable"),
  country_totals %>%
    filter(type == "Total net production") %>%
    pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "production") %>% # Reshape years into long format
    group_by(year) %>%
    summarize(totalProduction = sum(production, na.rm = TRUE)) %>% # Calculate total net production by year
    mutate(category = "Net Production")
) %>%
  mutate(category = factor(category, levels = c("Renewable", "Non-Renewable", "Net Production"))) # Define category order for plotting

# Plot Bar Chart 
## Energy Production by Category and Year (2016–2018)
ggplot(combinedEnergyData, aes(x = year, y = totalProduction, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + # Create a grouped bar chart
  geom_text(
    aes(label = scales::comma(totalProduction)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("Renewable" = "#f3a712", "Non-Renewable" = "#db2b39", "Net Production" = "#29335c")) + # Set custom colors
  scale_y_continuous(
    limits = c(0, 4000000),  # Define y-axis limits
    breaks = seq(0, 4000000, by = 1000000),  # Define axis intervals
    labels = scales::comma_format() # Format y-axis labels as comma-separated numbers
  ) +
  labs(
    title = "Energy Production by Category and Year (2016–2018)", # Add chart title and axis labels
    x = "Year",
    y = "Total Production (GWh)",
    fill = "Energy Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5), # Center and style the title
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Prepare Data 
energy_bar_data <- energy_types %>%
  filter(type != "Pumped hydro power") %>%  # Exclude "Pumped hydro power" as it's not a primary source
  mutate(
    type = ifelse(type %in% c("Geothermal", "Other"), "Geothermal & Other", type)  # Group "Geothermal" and "Other" into one category
  ) %>%
  group_by(type) %>% # Group by energy source
  summarise(
    `2016` = sum(`2016`, na.rm = TRUE), # Aggregate production for each year
    `2017` = sum(`2017`, na.rm = TRUE),
    `2018` = sum(`2018`, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(`2016`, `2017`, `2018`),  # Convert year columns to long format
    names_to = "Year",
    values_to = "Production"
  )
# Create Small Multiples Charts 
## Energy Production Trends by Source (2016–2018)
ggplot(energy_bar_data, aes(x = Year, y = Production, group = type)) +
  geom_line(size = 1.2, color = "#003f5c") +  # Dark blue for lines
  geom_point(size = 3, color = "#1ca3ec") +  # Lighter blue for points
  facet_wrap(~ type, scales = "free_y") +  
  labs(
    title = "Energy Production Trends by Source (2016–2018)", 
    x = "Year",
    y = "Production (GWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# Define Custom Color Palette
custom_colors <- c(
  "Hydro" = "#006d77",            
  "Wind" = "#0d3b66",          
  "Solar" = "#ffc857",            
  "Geothermal & Other" = "#db3a34",  
  "Conventional thermal" = "#9966cc",
  "Nuclear" = "#6a0572"  
)

# Boxplot for Selected Renewable Energy Sources 
selected_energy_data <- energy_types %>%
  filter(type %in% c("Hydro", "Wind", "Solar")) %>%  # Filter only "Hydro," "Wind," and "Solar"
  pivot_longer(cols = c(`2016`, `2017`, `2018`), 
               names_to = "Year", values_to = "Production")


# Visualize with Boxplot 
ggplot(selected_energy_data, aes(x = type, y = Production, fill = type)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Comparison of Renewable Energy Production by Source",
    x = "Energy Source",
    y = "Production (GWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Net Trade Balance Calculation and Plotting 
data_clean <- country_totals %>%
  filter(type %in% c("Imports", "Exports")) %>%
  pivot_longer(
    cols = c(`2016`, `2017`, `2018`), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = type, 
    values_from = Value
  ) %>%
  mutate(Net_Trade_Balance = Exports - Imports) %>%
  filter(Year == "2018")

ggplot(data_clean, aes(x = reorder(country, Net_Trade_Balance), y = Net_Trade_Balance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Net Trade Balance by Country (2018)",
    x = "Country",
    y = "Net Trade Balance (GWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Geospatial Mapping for Net Trade Balance 
renewable_countries <- energy_types %>%
  filter(type %in% c("Wind", "Solar", "Hydro", "Geothermal & Other", "Nuclear", "Conventional thermal")) %>%
  select(country) %>%
  distinct()

renewable_trade_balance <- country_totals %>%
  filter(country %in% renewable_countries$country) %>%
  filter(type == "Imports" | type == "Exports") %>%
  pivot_wider(
    names_from = type, 
    values_from = c(`2016`, `2017`, `2018`)
  ) %>%
  mutate(
    net_trade_balance_2018 = `2018_Exports` - `2018_Imports`
  ) %>%
  select(country, net_trade_balance_2018)

europe_map <- get_eurostat_geospatial(resolution = "60", nuts_level = 0)

europe_map <- europe_map %>%
  left_join(renewable_trade_balance, by = c("CNTR_CODE" = "country"))

ggplot(europe_map) +
  geom_sf(aes(fill = net_trade_balance_2018), color = "white", size = 0.2) + 
  scale_fill_gradientn(
    colors = c("#d13819", "#e8df11", "#09a804"),  
    name = "Net Trade Balance (GWh)"
  ) +
  labs(
    title = "Net Trade Balance of Renewable Energy by Country",
    subtitle = "2018 Data: Exports - Imports",
    caption = "Source: Eurostat"
  ) +
  coord_sf(
    xlim = c(-25, 50),  
    ylim = c(30, 75),   
    expand = FALSE      
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.position = "bottom"
  )

# Summarize renewable energy production
data_clean <- energy_types %>%
  mutate(type = ifelse(type %in% c("Geothermal", "Other"), "Geothermal & Other", type)) %>%
  filter(type %in% renewable_sources) %>%
  group_by(country, type) %>%
  summarise(
    `2016` = sum(`2016`, na.rm = TRUE),
    `2017` = sum(`2017`, na.rm = TRUE),
    `2018` = sum(`2018`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(`2016`, `2017`, `2018`),
    names_to = "Year",
    values_to = "Production"
  )

# Total renewable production per country
total_renewable_production <- data_clean %>%
  filter(Year == "2018") %>%
  group_by(country) %>%
  summarise(Total_Production = sum(Production, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Production))

# Top 5 and bottom 5 countries for renewable production
top_5_countries <- total_renewable_production %>% slice_head(n = 5)
bottom_5_countries <- total_renewable_production %>% slice_tail(n = 5)

data_top_5 <- data_clean %>% filter(country %in% top_5_countries$country, Year == "2018")
data_bottom_5 <- data_clean %>% filter(country %in% bottom_5_countries$country, Year == "2018")

# Plot renewable energy trends
top_5_most_plot <- ggplot(data_top_5, aes(x = country, y = Production, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Top 5 Countries with the Highest Renewable Energy Production (2018)",
    x = "Country",
    y = "Total Renewable Energy Production (GWh)",
    fill = "Renewable Energy Source"
  ) +
  theme_minimal()

top_5_least_plot <- ggplot(data_bottom_5, aes(x = country, y = Production, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Top 5 Countries with the Lowest Renewable Energy Production (2018)",
    x = "Country",
    y = "Total Renewable Energy Production (GWh)",
    fill = "Renewable Energy Source"
  ) +
  theme_minimal()

# Display plots
print(top_5_most_plot)
print(top_5_least_plot)

# Merge renewable production with trade balance
combined_data <- total_renewable_production %>%
  left_join(net_trade_data, by = "country") %>%
  drop_na()

# Correlation and Visualizations
# Correlation between total renewable production and net trade balance
correlation_renewable <- cor(combined_data$Total_Production, combined_data$Net_Trade_Balance)
print(paste("Correlation between Total Renewable Energy Production and Net Trade Balance: ", round(correlation_renewable, 3)))

# Scatter plot: Net trade balance vs renewable production
plot_renewable_vs_trade <- ggplot(combined_data, aes(x = Total_Production, y = Net_Trade_Balance)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Net Trade Balance vs Total Renewable Energy Production (2018)",
    x = "Total Renewable Energy Production (GWh)",
    y = "Net Trade Balance (GWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )

# Correlation between imports and net trade balance
data_imports <- country_totals %>%
  filter(type %in% c("Imports", "Exports")) %>%
  pivot_longer(
    cols = c(`2016`, `2017`, `2018`),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  filter(Year == "2018") %>%
  pivot_wider(
    names_from = type,
    values_from = Value
  ) %>%
  mutate(Net_Trade_Balance = Exports - Imports) %>%
  select(country, Net_Trade_Balance, Imports)

correlation_imports <- cor(data_imports$Net_Trade_Balance, data_imports$Imports, use = "complete.obs")
print(paste("Correlation between Imports and Net Trade Balance: ", round(correlation_imports, 3)))

# Scatter plot: Net trade balance vs imports
plot_imports_vs_trade <- ggplot(data_imports, aes(x = Imports, y = Net_Trade_Balance)) +
  geom_point(color = "black", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Net Trade Balance vs Imports (2018)",
    x = "Imports (GWh)",
    y = "Net Trade Balance (GWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )

# Display Plots
print(plot_renewable_vs_trade)
print(plot_imports_vs_trade)

