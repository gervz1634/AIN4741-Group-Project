library(tidyverse)
library(easystats)


# Data --------------------------------------------------------------------

df_hour <- read_csv("bike+sharing+dataset/hour.csv")
df_day <- read_csv("bike+sharing+dataset/day.csv")


glimpse(df_hour)
glimpse(df_day)

Data_detail(df_hour)
Data_detail(df_day)


df_hour$cnt %>%
  sort() %>% 
  plot(type = "l")

df_day$cnt %>%
  sort() %>% 
  plot(type = "l")




# EDA ---------------------------------------------------------------------

df_day |>
  correlation::correlation() |>
  summary() %>% 
  plot()
