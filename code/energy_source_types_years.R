# Load Required Libraries 
library(tidyverse)

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
  scale_fill_manual(values = c("Renewable" = "#09a804", "Non-Renewable" = "#eac81f", "Net Production" = "#95bb00")) + # Set custom colors
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