library(tseries)
library(forecast)
library(tidyverse)
# library(zoo)
library(patchwork)


df_time <- read_csv("bike+sharing+dataset/day.csv")[, -1]
df_time$dteday <- df_time %>% ymd(dteday)


savePNG <- function(name, graph) {
  ggsave(
    filename = name,
    plot = graph,
    width = 1600 / 150, height = 1100 / 150, dpi = 150
  )
}


# Original ----------------------------------------------------------------

y_weekly <- ts(df_time$cnt, frequency = 7, start = c(1, 1))
y_weekly %>%
  autoplot(linewidth = 0.7) +
  labs(title = "The original Y", y = "cnt")
y_weekly %>%
  decompose() %>%
  autoplot(linewidth = 1) +
  labs(title = "Decomposition")
# ggseasonplot(y_weekly, year.labels = FALSE) +
#   labs(title = "Seasonal Plot of Y", x = "Day of Week")
ggAcf(y_weekly) + labs(title = "ACF of Y") +
  ggPacf(y_weekly) + labs(title = "PACF of Y")
adf.test(y_weekly)


y_weekly.diff1 <- diff(y_weekly)
y_weekly.diff1 %>%
  autoplot(linewidth = 0.7) +
  labs(title = "1 lag Differencing", y = "cnt")
y_weekly.diff1 %>%
  decompose() %>%
  autoplot(linewidth = 1) +
  labs(title = "Decomposition")
# ggseasonplot(y_weekly, year.labels = FALSE) +
#   labs(title = "Seasonal Plot of Y", x = "Day of Week")
ggAcf(y_weekly.diff1) + labs(title = "ACF of 1 lag diff Y") +
  ggPacf(y_weekly.diff1) + labs(title = "PACF of 1 lag diff Y")
adf.test(y_weekly.diff1)


# quadratic term ------------------------------------------------------------------
t <- 1:length(y_weekly)
poly(t, degree = 2, raw = FALSE)

# Model -------------------------------------------------------------------
ARIMA_model_weekly <- auto.arima(y_weekly, ic = "aic")
ARIMA_model_weekly %>% summary()
ARIMA_model_weekly %>% checkresiduals()

# arma_model <- arima()
