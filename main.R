library(tidyverse)
library(easystats)


# Data --------------------------------------------------------------------

df_hour <- read_csv("bike+sharing+dataset/hour.csv")
df_day <- read_csv("bike+sharing+dataset/day.csv")


glimpse(df_hour)
glimpse(df_day)

Data_detail(df_hour)
Data_detail(df_day)

data_codebook(df_hour)
data_codebook(df_day)


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


df_day %>%
  ggplot() +
  geom_boxplot(aes(x = cnt, y = temp))


# Linear Model ------------------------------------------------------------

lm_baseline_model <- lm(cnt ~ ., df_hour)
lm_baseline_model |> check_model()
lm_baseline_model |> check_collinearity()
lm_baseline_model |> check_outliers()
lm_baseline_model |> influence.measures()
lm_baseline_model |> vcov()
lm_baseline_model |> cooks.distance()
