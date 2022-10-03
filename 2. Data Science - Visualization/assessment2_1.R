library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Question 1
temp_carbon %>% .$year %>% max() # 2018
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% max() # 2014
# temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(year) - error
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% .$year %>% max() # 2014
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(.$year)

# Question 2
emissions <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% arrange(year, descending = TRUE) %>%
  select(year, carbon_emissions)

# Question 3
temp_anom <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% arrange(year, descending = TRUE) %>%
  select(year, temp_anomaly)
temp_anom

# Question 4
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) +
  geom_line()
p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p

# Question 5
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

# Question 7
p + geom_line(aes(year, ocean_anomaly), col = "blue") + 
  geom_line(aes(year, land_anomaly), col = "red") + 
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
