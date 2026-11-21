library(tseries)
library(forecast)
library(tidyverse)
library(patchwork)

df_time <- read_csv("bike+sharing+dataset/day.csv")[, -1]

savePNG <- function(name, graph) {
  ggsave(
    filename = name,
    plot = graph,
    width = 1600 / 150, height = 1100 / 150, dpi = 150
  )
}


# Original ----------------------------------------------------------------

y_weekly <- ts(df_time$cnt, frequency = 7, start = c(1, 1))
p1 <- y_weekly %>%
  autoplot() +
  labs(title = "Original Time Series of Daily Bike Rental Count", y = "cnt")
y_weekly %>%
  decompose() %>%
  autoplot(linewidth = 1) +
  labs(title = "Decomposition of Original Series")
# ggseasonplot(y_weekly, year.labels = FALSE) +
#   labs(title = "Seasonal Plot of Y", x = "Day of Week")
p2 <- ggAcf(y_weekly) +
  labs(title = "ACF Plot of Original Series")
p3 <- ggPacf(y_weekly) +
  labs(title = "PACF Plot of Original Series")
p1 / (p2 | p3)
adf.test(y_weekly)


# 1st-order ordinary differencing
y_weekly.diff1 <- diff(y_weekly)
p1 <- y_weekly.diff1 %>%
  autoplot() +
  labs(title = "1st-order Ordinary Differencing", y = "cnt")
y_weekly.diff1 %>%
  decompose() %>%
  autoplot(linewidth = 1) +
  labs(title = "Decomposition after 1st-order Differencing")
# ggseasonplot(y_weekly, year.labels = FALSE) +
#   labs(title = "Seasonal Plot of Y", x = "Day of Week")
p2 <- ggAcf(y_weekly.diff1) +
  labs(title = "ACF Plot after 1st-order Differencing")
p3 <- ggPacf(y_weekly.diff1) +

  labs(title = "PACF Plot after 1st-order Differencing")
# ACF cuts off at lag 1, while significant autocorrelation still remains at seasonal lag 7,
# indicating weekly seasonal pattern has not been removed yet
p1 / (p2 | p3)
adf.test(y_weekly.diff1)


# Seasonal differencing (lag = 7, weekly cycle)
y_weekly.diff1.seasonal <- diff(y_weekly.diff1, lag = 7)
p1 <- y_weekly.diff1.seasonal %>%
  autoplot() +
  labs(title = "1st-order Differencing and Seasonal Differencing (lag = 7)", y = "cnt")
y_weekly.diff1.seasonal %>%
  decompose() %>%
  autoplot(linewidth = 1) +
  labs(title = "Decomposition after Ordinary + Seasonal Differencing")
# ggseasonplot(y_weekly, year.labels = FALSE) +
#   labs(title = "Seasonal Plot of Y", x = "Day of Week")
p2 <- ggAcf(y_weekly.diff1.seasonal) +
  labs(title = "ACF Plot after Ordinary and Seasonal Differencing")
p3 <- ggPacf(y_weekly.diff1.seasonal) +
  labs(title = "PACF Plot after Ordinary and Seasonal Differencing")
# After combined differencing, check whether non-stationarity and weekly seasonality are eliminated;
# use ACF/PACF to identify candidate SARIMA(p,d,q)(P,D,Q)7 orders
p1 / (p2 | p3)
adf.test(y_weekly.diff1.seasonal)


# Modeling ----------------------------------------------------------------

candidate_model <- expand.grid(
  p = 0:2,
  q = 0:2,
  P = 0:1,
  Q = 0:1
)
ARIMA_model_weekly.list <- lapply(
  seq_len(nrow(candidate_model)),
  function(i) {
    Arima(y_weekly,
      order = c(candidate_model$p[i], 1, candidate_model$q[i]),
      seasonal = c(candidate_model$P[i], 1, candidate_model$Q[i])
    )
  }
)
cat("Total", length(ARIMA_model_weekly.list), "models")
# sapply(ARIMA_model_weekly.list, AIC)
target_model <- sapply(ARIMA_model_weekly.list, AIC) %>%
  which.min() %>%
  ARIMA_model_weekly.list[[.]]
cat("The", sapply(ARIMA_model_weekly.list, AIC) %>% which.min(), "model has lowest AIC")
target_model %>% arimaorder()
target_model %>% summary()
target_model %>% autoplot() + labs(title = "ARIMA(1,1,1)(0,1,1)[7]")
target_model %>% checkresiduals()
target_model %>%
  forecast() %>%
  autoplot()


ARIMA_model_weekly.auto <- auto.arima(y_weekly, ic = "aic") # The result of auto.arima() justify the choice of differencing
ARIMA_model_weekly.auto %>% arimaorder()
ARIMA_model_weekly.auto %>% summary()
ARIMA_model_weekly.auto %>% autoplot() + labs(title = "ARIMA(1,1,1)(1,0,2)[7]")
ARIMA_model_weekly.auto %>% checkresiduals()
ARIMA_model_weekly.auto %>%
  forecast() %>%
  autoplot()


AIC(ARIMA_model_weekly.auto) > AIC(target_model)


# quadratic term ------------------------------------------------------------------

ts_rand_trend <- y_weekly - decompose(y_weekly)$seasonal
t <- seq_len(length(ts_rand_trend))
lmr <- lm(ts_rand_trend ~ t + I(t^2))
ts_trend <- ts(predict.lm(lmr, newdata = list(t)), frequency = 7)
ts_rand <- ts(ts_rand_trend - ts_trend)
adf.test(ts_rand)


ARIMA_model_weekly.quadra.auto <- auto.arima(ts_rand)
ARIMA_model_weekly.quadra.auto %>% arimaorder()
ARIMA_model_weekly.quadra.auto %>% summary()
ARIMA_model_weekly.quadra.auto %>% autoplot() + labs(title = "ARIMA(1,1,1)")
ARIMA_model_weekly.quadra.auto %>% checkresiduals()
ARIMA_model_weekly.quadra.auto %>%
  forecast() %>%
  autoplot()


# Model fitness -------------------------------------------------------------------
candidate_model <- list(
  target_model,
  ARIMA_model_weekly.auto,
  ARIMA_model_weekly.quadra.auto
)
lapply(
  candidate_model,
  function(mod) {
    Box.test(mod$residuals, lag = 14, type = "Ljung-Box")
  }
) #


# Forecasting -------------------------------------------------------------

# 模型1 target_model
p1 <- autoplot(forecast(target_model, h = 10)) + ggtitle("Grid best SARIMA")
# 模型2 ARIMA_model_weekly.auto
p2 <- autoplot(forecast(ARIMA_model_weekly.auto, h = 10)) + ggtitle("Auto SARIMA")
# 模型3 带二次项SARIMAX，一定要加 xreg = t_poly2_future
p3 <- autoplot(
  forecast(ARIMA_model_weekly.quadra.auto, h = 10, xreg = t_poly2_future)
) +
  ggtitle("Auto SARIMAX + quadratic trend")

# 用你已经加载的patchwork拼合三张图
p1 + p2 + p3
