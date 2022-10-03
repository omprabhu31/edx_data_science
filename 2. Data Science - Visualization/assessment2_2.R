library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Question 8
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# Question 10
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% ggplot(aes(year, carbon_emissions)) +
  geom_line()

# Question 11
co2_time <- historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(year, co2, col = source)) +
  geom_line()
co2_time

# Question 12
co2_time + xlim(-800000, -775000)
co2_time + xlim(-375000, -330000)
co2_time + xlim(-140000, -120000)
co2_time + xlim(-3000, 2018)
