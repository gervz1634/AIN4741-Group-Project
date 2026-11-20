library(tseries)
library(forecast)
library(tidyverse)
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
# Since the ACF has cut-off in lag 1 and have significant correlation at the seasonal lags 7
# PACF has a
adf.test(y_weekly.diff1)



# Modeling ----------------------------------------------------------------

# Seasonal differencing
y_weekly.diff1.seasonal <- diff(y_weekly.diff1, lag = 7)
y_weekly.diff1.seasonal %>%
  autoplot(linewidth = 0.7) +
  labs(title = "lag=1 and lag=7 Differencing", y = "cnt")
y_weekly.diff1.seasonal %>%
  decompose() %>%
  autoplot(linewidth = 1) +
  labs(title = "Decomposition")
# ggseasonplot(y_weekly, year.labels = FALSE) +
#   labs(title = "Seasonal Plot of Y", x = "Day of Week")
ggAcf(y_weekly.diff1.seasonal) + labs(title = "ACF of lag=1 diff and lag = 7 diff Y") +
  ggPacf(y_weekly.diff1.seasonal) + labs(title = "PACF of 1 lag diff Y")
# Since the ACF has cut-off in lag 1 and have significant correlation at the seasonal lags 7
# PACF has a
adf.test(y_weekly.diff1.seasonal)


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
sapply(ARIMA_model_weekly.list, AIC)
target_model <- sapply(ARIMA_model_weekly.list, AIC) %>%
  which.min() %>%
  ARIMA_model_weekly.list[[.]]
cat("The", sapply(ARIMA_model_weekly.list, AIC) %>% which.min(), "model has lowest AIC")
target_model %>% arimaorder()
target_model %>% summary()
target_model %>% autoplot()
target_model %>% checkresiduals()
target_model %>%
  forecast() %>%
  autoplot()


ARIMA_model_weekly.auto <- auto.arima(y_weekly, ic = "aic") # The result of auto.arima() justify the choice of differencing
ARIMA_model_weekly.auto %>% arimaorder()
ARIMA_model_weekly.auto %>% summary()
ARIMA_model_weekly.auto %>% autoplot()
ARIMA_model_weekly.auto %>% checkresiduals()
ARIMA_model_weekly.auto %>%
  forecast() %>%
  autoplot()


AIC(ARIMA_model_weekly.auto) > AIC(target_model)


# quadratic term ------------------------------------------------------------------
t <- 1:length(y_weekly)
t_poly2 <- poly(t, degree = 2, raw = FALSE)
xreg_quad <- as.matrix(t_poly2)
colnames(xreg_quad) <- c("t1", "t2")

ARIMA_model_weekly.quadra.auto <- auto.arima(
  y_weekly,
  ic = "aic",
  xreg = xreg_quad
)

summary(ARIMA_model_weekly.quadra.auto)
checkresiduals(ARIMA_model_weekly.quadra.auto)


# Model fitness -------------------------------------------------------------------
candidate_model <- list(
  target_model,
  ARIMA_model_weekly.auto,
  ARIMA_model_weekly.quadra.auto
)
lapply(
  candidate_model,
  function(mod) {
    Box.test(mod$residuals, lag = 10, type = "Ljung-Box")
  }
) # ???


# Forecasting -------------------------------------------------------------

# 模型1 target_model
p1 <- autoplot(forecast(target_model, h = 10)) + ggtitle("Grid best SARIMA")
# 模型2 ARIMA_model_weekly.auto
p2 <- autoplot(forecast(ARIMA_model_weekly.auto, h = 10)) + ggtitle("Auto SARIMA")
# 模型3 带二次项SARIMAX，一定要加 xreg = t_poly2_future
p3 <- autoplot(forecast(ARIMA_model_weekly.quadra.auto, h = 10, xreg = t_poly2_future)) + ggtitle("Auto SARIMAX + quadratic trend")

# 用你已经加载的patchwork拼合三张图
p1 + p2 + p3
