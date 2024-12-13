# Load Required Libraries ----
library(tidyverse)

# Reshape Data ----
# Transform the data to long format and filter for "Total net production"
totalNetProduction <- country_totals %>%
  pivot_longer(
    cols = c(`2016`, `2017`, `2018`),  # Specify year columns 
    names_to = "year",                 # Create a new column for years
    values_to = "totalProduction"      # Store production values 
  ) %>%
  filter(type == "Total net production") %>%  # Only "Total net production"
  group_by(year) %>%                          # Group data by year
  summarize(
    totalProduction = sum(totalProduction, na.rm = TRUE)  # Summarize by year
  )

# Plot Total Net Energy Production ----
ggplot(data = totalNetProduction, aes(x = year, y = totalProduction, group = 1)) +
  geom_line(
    color = "blue",  # Line color set to blue
    size = 1.2       # Line thickness
  ) +
  geom_point(
    color = "red",   # Point color set to red
    size = 3         # Point size
  ) +
  geom_text(
    aes(label = scales::comma(totalProduction)),
    vjust = 0,                                    
    size = 3.5                                    
  ) +
  labs(
    title = "Total Net Energy Production Across Europe (2016–2018)",  
    x = "Year",                                                      
    y = "Total Energy Production (GWh)"                             
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  
    axis.title = element_text(size = 14),                             
    axis.text = element_text(size = 12)                               
  )

