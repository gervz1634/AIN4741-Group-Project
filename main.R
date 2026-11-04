library(tidyverse)
library(easystats)
library(forecast)
library(tseries)
source("utils_for_group_project.R")


# Data --------------------------------------------------------------------

df_day <- read_csv("bike+sharing+dataset/day.csv")[, -1]


glimpse(df_day)


data_codebook(df_day)


# Data Preprocessing ------------------------------------------------------
cat_col <- c(
  "season", "holiday", "workingday", "weathersit",
  "yr", "mnth", "weekday"
)
num_col <- c(
  "temp", "atemp", "hum", "windspeed"
) # No casual and registered


df_pre <- train_test_split(df_day)
Training <- df_pre$train
Testing <- df_pre$test


# EDA --------------------------------------------------------------------  -

Training %>%
  correlation::correlation() %>%
  summary() %>%
  plot()


Training %>%
  ggplot() +
  geom_boxplot(aes(x = cnt, y = temp))


Training %>%
  ggplot(aes(x = cnt)) +
  geom_density(colour = "red") +
  geom_histogram()


# 123 ---------------------------------------------------------------------

Training.2nd <- Training %>%
  select(
    -c(
      casual, registered, temp,
      dteday, mnth
    )
  )


Training.3rd <- Training.2nd %>%
  select(
    -c()
  )


# Linear Model -----------------------------------------------------------  -

lm_baseline_model <- lm(cnt ~ ., Training)

lm_baseline_model %>% model_parameters()
lm_baseline_model %>% check_model()
lm_baseline_model %>% model_performance()
lm_baseline_model %>%
  check_collinearity() %>%
  plot() # variable selection through VIF
lm_baseline_model %>% influence.measures()
lm_baseline_model %>% vcov()
lm_baseline_model %>% cooks.distance()


lm_second_model <- lm(cnt ~ ., Training.2nd)

lm_second_model %>% model_parameters()
lm_second_model %>% check_model()
lm_second_model %>%
  select_parameters() %>% # stepwise base on AIC
  get_parameters()


lm_third_model <- lm(cnt ~ ., Training.3rd)


model_performance(lm_baseline_model)


# Time series -------------------------------------------------------------

ts_df <- ts(df_day$cnt, frequency = 7, start = 1)
ts_df.diff1 <- ts_df %>% diff()


ts_df %>%
  adf.test() # non-stationary
ts_df.diff1 %>%
  adf.test()
ts_df %>%
  decompose() %>%
  plot()
ts_df.diff1 %>%
  Acf()


ts_baseline_model <- auto.arima(ts_df)
ts_baseline_model %>% check_model()
ts_baseline_model %>% model_performance()
