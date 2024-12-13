# Load Required Libraries ----
library(tidyverse)

# Calculate Energy Categories and Combine Data ----
combinedEnergyData <- bind_rows(
  energy_types %>%
    filter(type %in% c("Hydro", "Wind", "Solar", "Geothermal")) %>%
    pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "production") %>%
    group_by(year) %>%
    summarize(totalProduction = sum(production, na.rm = TRUE)) %>%
    mutate(category = "Renewable"),
  energy_types %>%
    filter(!type %in% c("Hydro", "Wind", "Solar", "Geothermal")) %>%
    pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "production") %>%
    group_by(year) %>%
    summarize(totalProduction = sum(production, na.rm = TRUE)) %>%
    mutate(category = "Non-Renewable"),
  country_totals %>%
    filter(type == "Total net production") %>%
    pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "production") %>%
    group_by(year) %>%
    summarize(totalProduction = sum(production, na.rm = TRUE)) %>%
    mutate(category = "Net Production")
) %>%
  mutate(category = factor(category, levels = c("Renewable", "Non-Renewable", "Net Production")))

# Plot Bar Chart ----
ggplot(combinedEnergyData, aes(x = year, y = totalProduction, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = scales::comma(totalProduction)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("Renewable" = "green", "Non-Renewable" = "red", "Net Production" = "blue")) +
  scale_y_continuous(
    limits = c(0, 4e+06),
    breaks = seq(0, 4e+06, by = 1e+06),
    labels = scales::label_scientific()
  ) +
  labs(
    title = "Energy Production by Category and Year (2016–2018)",
    x = "Year",
    y = "Total Production (GWh)",
    fill = "Energy Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
