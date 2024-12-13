# Load Required Libraries ----
library(tidyverse)

# Reshape Data ----
# Transform the data to long format and filter for "Total net production"
totalNetProduction <- country_totals %>%
  pivot_longer(
    cols = c(`2016`, `2017`, `2018`),  
    names_to = "year",              
    values_to = "totalProduction"     
  ) %>%
  filter(type == "Total net production") %>%  
  group_by(year) %>%                        
  summarize(
    totalProduction = sum(totalProduction, na.rm = TRUE)  
  )

# Plot Total Net Energy Production ----
ggplot(data = totalNetProduction)+
  aes(x = year, y = totalProduction, group = 1) +
  geom_line(
    color = "forestgreen",  
    size = 1.2       
  ) +
  geom_point(
    color = "red",   
    size = 3         
  ) +
  geom_text(
    aes(
      label = scales::comma(totalProduction),
      vjust = if_else(year == "2017", -0.5, 1.5)  
    ),
    size = 3            
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