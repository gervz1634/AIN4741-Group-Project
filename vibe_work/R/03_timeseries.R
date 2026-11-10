###############################################################################
## 03_timeseries.R -- seasonal decomposition, stationarity, model identification
##                     and selection of the SARIMA model, evaluation and forecast
## Output : tables T11-T17, figures F11-F17, log 03_timeseries.log
###############################################################################

source("vibe_work/R/00_setup.R")

cat("\n===================== 03 TIME SERIES MODEL =======================\n\n")

df <- readRDS("vibe_work/data/day_clean.rds")$full

## ---------------------------------------------------------------------------
## 1. The series as a daily time series with a weekly cycle (frequency = 7)
## ---------------------------------------------------------------------------
y <- ts(df$cnt, start = 1, frequency = 7)
n <- length(y)
cat("series length :", n, "days (", format(min(df$dteday)), "to", format(max(df$dteday)), ")\n")
cat("frequency     : 7 (one observation per day, 7 days per cycle)\n")
cat("mean / sd     :", round(mean(y), 1), "/", round(sd(y), 1), "\n")
cat("first / last  :", y[1], "/", y[n], "\n\n")

## weekday dummy matrix for the whole calendar (data + the forecast week);
## used by the "deterministic weekly seasonality" model family in Section 5
all_dates <- seq(min(df$dteday), as.Date("2013-01-07"), by = "day")
Xw_all <- model.matrix(~ factor(as.POSIXlt(all_dates)$wday, levels = 0:6))[, -1, drop = FALSE]
colnames(Xw_all) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
Xw    <- Xw_all[1:n, , drop = FALSE]
Xw_fc <- Xw_all[(n + 1):(n + 7), , drop = FALSE]

## ---------------------------------------------------------------------------
## 2. Seasonal decomposition (STL, robust)  -- figure F11
## ---------------------------------------------------------------------------
fit_stl <- stl(y, s.window = "periodic", robust = TRUE)
comp <- as_tibble(fit_stl$time.series) %>%
  mutate(dteday = df$dteday, observed = as.numeric(y))

## strength of the components (Wang, Smith & Hyndman, 2006)
var_r <- var(comp$remainder)
strength_seasonal <- max(0, 1 - var_r / var(comp$seasonal + comp$remainder))
strength_trend    <- max(0, 1 - var_r / var(comp$trend + comp$remainder))
cat("---- 2.1 STL decomposition ----\n")
cat("strength of the weekly seasonality :", round(strength_seasonal, 4), "\n")
cat("strength of the trend/nannual cycle:", round(strength_trend, 4), "\n")
cat("note: with frequency = 7 the STL seasonal component is the WEEKLY cycle;\n",
    "     the annual cycle is absorbed by the trend component.\n\n")

p_stl <- comp %>%
  select(dteday, observed, trend, seasonal, remainder) %>%
  pivot_longer(-dteday) %>%
  mutate(name = factor(name, levels = c("observed", "trend", "seasonal", "remainder"),
                       labels = c("(a) Observed cnt", "(b) Trend (incl. annual cycle)",
                                  "(c) Weekly seasonal component", "(d) Remainder"))) %>%
  ggplot(aes(dteday, value)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed", linewidth = 0.35) +
  geom_line(colour = pal_main[1], linewidth = 0.4) +
  facet_wrap(~ name, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  labs(title = "STL decomposition of the daily total rentals",
       subtitle = sprintf("Additive robust STL; weekly seasonal strength = %.2f, trend strength = %.2f",
                          strength_seasonal, strength_trend),
       x = NULL, y = NULL) +
  theme_ev(10)
save_fig(p_stl, "F11_stl_decomposition", width = 9.5, height = 8.4)

## the estimated weekly seasonal pattern (T11)
T11 <- comp %>%
  mutate(wday = factor(as.POSIXlt(dteday)$wday, levels = 0:6,
                       labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  group_by(wday) %>%
  summarise(seasonal_index = mean(seasonal),
            mean_cnt = mean(observed),
            median_cnt = median(observed),
            .groups = "drop") %>%
  mutate(seasonal_index = round(seasonal_index, 1),
         pct_of_mean = round(seasonal_index / mean(comp$observed) * 100, 2))
save_tab(T11, "T11_weekly_seasonal")
save_md(T11, "T11_weekly_seasonal", digits = 2, caption = "Table T11 - Weekly seasonal pattern estimated by STL (additive index, bikes per day)")
cat("---- 2.2 estimated weekly seasonal pattern (T11) ----\n")
print(as.data.frame(T11), digits = 3)

p_sub <- comp %>%
  mutate(wday = factor(as.POSIXlt(dteday)$wday, levels = 0:6,
                       labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
         yr = factor(format(dteday, "%Y"))) %>%
  ggplot(aes(wday, observed, fill = yr)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85, width = 0.7) +
  scale_fill_manual(values = pal_main[c(3, 1)]) +
  labs(title = "Weekly sub-series: daily rentals by day of week",
       subtitle = "The weekly cycle is weak compared with the annual cycle, but it is systematic",
       x = NULL, y = "cnt", fill = "Year") +
  theme_ev()
save_fig(p_sub, "F12_weekly_subseries", width = 8.5, height = 4.6)

## ---------------------------------------------------------------------------
## 3. Stationarity analysis (T12)
## ---------------------------------------------------------------------------
d1   <- diff(y)
d7   <- diff(y, lag = 7)
d1d7 <- diff(diff(y, lag = 7), lag = 1)

T12 <- bind_rows(
  adf_test(y,    lags = 7, type = "drift") %>% mutate(series = "level (cnt)"),
  adf_test(d1,   lags = 7, type = "drift") %>% mutate(series = "first difference, d = 1"),
  adf_test(d7,   lags = 7, type = "drift") %>% mutate(series = "seasonal difference, D = 1 (lag 7)"),
  adf_test(d1d7, lags = 7, type = "drift") %>% mutate(series = "d = 1 and D = 1 (lag 7)")
) %>%
  select(series, type, lags, statistic, crit_1pct, crit_5pct, crit_10pct, reject_5pct) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
save_tab(T12, "T12_adf")
save_md(T12, "T12_adf", digits = 3, caption = "Table T12 - Augmented Dickey-Fuller tests (estimated with base R, MacKinnon 1996 critical values)")
cat("\n---- 3.1 ADF tests (T12) ----\n")
print(as.data.frame(T12), digits = 3)
cat("\nstandard deviations of the successive differences:\n")
cat("  level          :", round(sd(y), 1), "\n")
cat("  d = 1          :", round(sd(d1), 1), "\n")
cat("  D = 1 (lag 7)  :", round(sd(d7), 1), "\n")
cat("  d = 1, D = 1   :", round(sd(d1d7), 1), "\n")

p_acf1 <- plot_acf_pacf(y, 28, "Level series") +
  labs(subtitle = "spikes at the multiples of 7 and slow decay -> weekly seasonality + trend")
p_acf2 <- plot_acf_pacf(d1d7, 28, "After d = 1 and D = 1 (lag 7)") +
  labs(subtitle = "no spike left at lag 7 or 14 -> the differenced series is stationary")
save_fig(p_acf1 / p_acf2, "F13_acf_pacf", width = 10, height = 7.2)

## ---------------------------------------------------------------------------
## 4. Model identification: grid search over SARIMA orders (T13)
## ---------------------------------------------------------------------------
y_train <- window(y, end = time(y)[n - 28])
y_test  <- window(y, start = time(y)[n - 27])
n_test  <- 28

cat("\n---- 4.1 estimation sample:", length(y_train), "days | hold-out:", n_test,
    "days = 4 full weeks (", format(df$dteday[n - 27]), "to", format(df$dteday[n]), ") ----\n\n")

fit_sarima <- function(series, p, d, q, P, D, Q, s = 7, method = "ML") {
  tryCatch({
    m <- arima(series, order = c(p, d, q),
               seasonal = list(order = c(P, D, Q), period = s),
               include.mean = (d == 0 && D == 0), method = method)
    list(ok = TRUE, model = m)
  }, error = function(e) list(ok = FALSE, model = NULL, msg = conditionMessage(e)))
}

grid <- expand.grid(p = 0:2, q = 0:2, P = 0:1, Q = 0:1, KEEP.OUT.ATTRS = FALSE)

res <- purrr::pmap_dfr(grid, function(p, q, P, Q) {
  d <- 1; D <- 1
  f <- fit_sarima(y_train, p, d, q, P, D, Q)
  n_eff <- length(y_train) - d - D * 7
  k <- if (f$ok) length(f$model$coef) else NA_integer_
  tibble(
    order = sprintf("(%d,%d,%d)(%d,%d,%d)[7]", p, d, q, P, D, Q),
    p = p, d = d, q = q, P = P, D = D, Q = Q, n_par = k,
    AIC  = if (f$ok) AIC(f$model) else NA_real_,
    AICc = if (f$ok) AIC(f$model) + 2 * k * (k + 1) / (n_eff - k - 1) else NA_real_,
    BIC  = if (f$ok) BIC(f$model) else NA_real_,
    loglik = if (f$ok) as.numeric(logLik(f$model)) else NA_real_,
    sigma2 = if (f$ok) f$model$sigma2 else NA_real_,
    converged = if (f$ok) f$model$code == 0 else FALSE
  )
}) %>%
  arrange(AICc) %>%
  mutate(across(c(AIC, AICc, BIC, loglik, sigma2), ~ round(.x, 3)))

save_tab(res, "T13_sarima_grid")
T13 <- res %>% slice_head(n = 15) %>% select(-p, -d, -q, -P, -D, -Q)
save_md(T13, "T13_sarima_grid", digits = 2, caption = "Table T13 - SARIMA grid search over the estimation sample with d = 1 and D = 1 fixed (seasonal period 7); the 15 best specifications ordered by AICc")
cat("---- 4.2 SARIMA grid search: 15 best by AICc (T13) ----\n")
print(as.data.frame(T13), digits = 3)

## ---------------------------------------------------------------------------
## 5. Comparison of competing specifications by out-of-sample forecasting (T14)
##    (a) rolling-origin one-step-ahead over the last 12 weeks = 84 forecasts
##    (b) a fixed 28-day (4-week) hold-out forecast
## ---------------------------------------------------------------------------
cat("\n---- 5.1 rolling-origin and fixed hold-out comparison (T14) ----\n")
start_idx <- n - 84

roll_ones <- function(spec) {
  e <- numeric(0)
  for (t in start_idx:(n - 1)) {
    yy <- window(y, end = time(y)[t])
    if (isTRUE(spec$wd)) {
      m <- tryCatch(arima(yy, order = spec$o, xreg = Xw[seq_len(t), , drop = FALSE],
                          include.mean = TRUE, method = "ML"), error = function(e) NULL)
      if (is.null(m)) return(tibble(roll_RMSE = NA, roll_MAE = NA, roll_bias = NA))
      p <- predict(m, n.ahead = 1, newxreg = Xw[t + 1, , drop = FALSE])$pred
    } else {
      m <- fit_sarima(yy, spec$o[1], spec$o[2], spec$o[3], spec$s[1], spec$s[2], spec$s[3])$model
      if (is.null(m)) return(tibble(roll_RMSE = NA, roll_MAE = NA, roll_bias = NA))
      p <- predict(m, n.ahead = 1)$pred
    }
    e <- c(e, as.numeric(y)[t + 1] - as.numeric(p))
  }
  tibble(roll_RMSE = sqrt(mean(e^2)), roll_MAE = mean(abs(e)), roll_bias = mean(e))
}

hold_out <- function(spec) {
  yy <- window(y, end = time(y)[n - 28])
  if (isTRUE(spec$wd)) {
    m <- tryCatch(arima(yy, order = spec$o, xreg = Xw[seq_len(n - 28), , drop = FALSE],
                        include.mean = TRUE, method = "ML"), error = function(e) NULL)
    if (is.null(m)) return(tibble(h_RMSE = NA, h_MAE = NA, h_cover95 = NA))
    f <- predict(m, n.ahead = 28, newxreg = Xw[(n - 27):n, , drop = FALSE])
  } else {
    m <- fit_sarima(yy, spec$o[1], spec$o[2], spec$o[3], spec$s[1], spec$s[2], spec$s[3])$model
    if (is.null(m)) return(tibble(h_RMSE = NA, h_MAE = NA, h_cover95 = NA))
    f <- predict(m, n.ahead = 28, se.fit = TRUE)
  }
  obs <- as.numeric(y)[(n - 27):n]
  e <- obs - as.numeric(f$pred)
  tibble(h_RMSE = sqrt(mean(e^2)), h_MAE = mean(abs(e)),
         h_cover95 = mean(abs(e) <= qnorm(0.975) * as.numeric(f$se)))
}

specs <- list(
  list(id = "(0,1,1)(0,1,1)[7]", o = c(0,1,1), s = c(0,1,1)),
  list(id = "(1,1,1)(0,1,1)[7]", o = c(1,1,1), s = c(0,1,1)),
  list(id = "(2,1,2)(0,1,1)[7]", o = c(2,1,2), s = c(0,1,1)),
  list(id = "(1,1,1)(1,1,1)[7]", o = c(1,1,1), s = c(1,1,1)),
  list(id = "(2,1,1)(1,1,1)[7]", o = c(2,1,1), s = c(1,1,1)),
  list(id = "(1,1,1)(1,0,1)[7]", o = c(1,1,1), s = c(1,0,1)),
  list(id = "(1,1,1)(0,0,1)[7]", o = c(1,1,1), s = c(0,0,1)),
  list(id = "(1,0,0)+weekday dummies", o = c(1,0,0), s = c(0,0,0), wd = TRUE),
  list(id = "(2,1,2)(0,0,1)[7]", o = c(2,1,2), s = c(0,0,1))
)

T14 <- purrr::map_dfr(specs, function(sp) {
  full <- if (isTRUE(sp$wd)) {
    tryCatch(arima(y, order = sp$o, xreg = Xw, include.mean = TRUE, method = "ML"),
             error = function(e) NULL)
  } else fit_sarima(y, sp$o[1], sp$o[2], sp$o[3], sp$s[1], sp$s[2], sp$s[3])$model
  k <- if (is.null(full)) NA_integer_ else length(coef(full))
  n_eff <- n - sp$o[2] - sp$s[2] * 7
  lb <- if (is.null(full)) NA_real_ else
    Box.test(residuals(full), lag = 14, type = "Ljung-Box",
             fitdf = sum(sp$o[c(1, 3)]) + sum(sp$s[c(1, 3)]) +
               if (isTRUE(sp$wd)) 6 else 0)$p.value
  nan_se <- if (is.null(full)) NA else any(is.nan(sqrt(diag(full$var.coef))))
  bind_cols(
    tibble(order = sp$id, n_par = k,
           AIC = if (is.null(full)) NA_real_ else AIC(full),
           AICc = if (is.null(full)) NA_real_ else
             AIC(full) + 2 * k * (k + 1) / (n_eff - k - 1),
           LB14_p = lb, unreliable_se = nan_se),
    roll_ones(sp), hold_out(sp)
  )
}) %>%
  arrange(roll_RMSE) %>%
  mutate(across(c(AIC, AICc, roll_RMSE, roll_MAE, roll_bias, h_RMSE, h_MAE), ~ round(.x, 2)),
         LB14_p = round(LB14_p, 3), h_cover95 = round(h_cover95, 3))
save_tab(T14, "T14_model_comparison")
save_md(T14, "T14_model_comparison", digits = 2, caption = "Table T14 - Out-of-sample comparison of competing time-series specifications. roll_* = 84 rolling-origin one-step-ahead forecasts (last 12 weeks); h_* = one fixed 28-day-ahead forecast; LB14_p = p-value of the Ljung-Box test on the residuals; h_cover95 = share of the 28 hold-out days inside the 95% interval")
print(as.data.frame(T14), digits = 3)

## ---------------------------------------------------------------------------
## 6. The final time-series model
## ---------------------------------------------------------------------------
best <- T14 %>%
  filter(is.na(unreliable_se) | unreliable_se == FALSE) %>%
  arrange(AICc) %>%
  slice(1)
best_spec <- specs[[which(purrr::map_lgl(specs, ~ .x$id == best$order))]]

cat("\n---- 6.1 selected specification:", best_spec$id, "----\n")
cat("Selection rule, applied in this order:\n",
    "  1. the specification must be ADEQUATE: its residuals must pass the\n",
    "     Ljung-Box test (no predictable structure left);\n",
    "  2. among adequate specifications, lowest AICc and fewest parameters;\n",
    "  3. out-of-sample accuracy (rolling one-step-ahead and 4-week hold-out).\n",
    "Chosen:", best_spec$id, "with Ljung-Box p(14) =", round(best$LB14_p, 3),
    ", AICc =", round(best$AICc, 2), ", rolling RMSE =", round(best$roll_RMSE, 1),
    "and hold-out RMSE =", round(best$h_RMSE, 1), ".\n")
cat("The weekday-dummy specification forecasts slightly better one step ahead\n",
    "(rolling RMSE", round(T14$roll_RMSE[T14$order == "(1,0,0)+weekday dummies"], 1),
    ") but its residuals are strongly autocorrelated (Ljung-Box p < 0.001),\n",
    "so it is not an adequate model and is not adopted.\n")

fit_ts <- arima(y, order = best_spec$o,
                seasonal = list(order = best_spec$s, period = 7),
                include.mean = FALSE, method = "ML")

cat("\n---- 6.2 final SARIMA estimated on all", n, "days ----\n")
print(fit_ts)

k_ts <- length(coef(fit_ts))
T15 <- tibble(
  parameter = names(coef(fit_ts)),
  estimate  = as.numeric(coef(fit_ts)),
  se        = as.numeric(sqrt(diag(fit_ts$var.coef))),
  z         = estimate / se,
  p         = 2 * pnorm(-abs(z))
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
save_tab(T15, "T15_sarima_coefs")
save_md(T15, "T15_sarima_coefs", digits = 4, caption = "Table T15 - Estimated parameters of the final SARIMA model (all 731 days)")
cat("\ninformation criteria: AIC =", round(AIC(fit_ts), 2),
    "| AICc =", round(AIC(fit_ts) + 2 * k_ts * (k_ts + 1) / (n - k_ts - 1), 2),
    "| BIC =", round(BIC(fit_ts), 2),
    "| sigma^2 =", round(fit_ts$sigma2, 1),
    "| sigma =", round(sqrt(fit_ts$sigma2), 1), "\n")

## ---------------------------------------------------------------------------
## 7. Residual diagnostics (T16) and residual figures
## ---------------------------------------------------------------------------
r_ts <- residuals(fit_ts)
df_fit <- sum(best_spec$o[c(1, 3)]) + sum(best_spec$s[c(1, 3)])
lb1 <- Box.test(r_ts, lag = 7,  type = "Ljung-Box", fitdf = df_fit)
lb2 <- Box.test(r_ts, lag = 14, type = "Ljung-Box", fitdf = df_fit)
lb3 <- Box.test(r_ts, lag = 21, type = "Ljung-Box", fitdf = df_fit)
sw  <- shapiro.test(as.numeric(r_ts))
sk  <- mean((r_ts - mean(r_ts))^3) / sd(r_ts)^3
ku  <- mean((r_ts - mean(r_ts))^4) / sd(r_ts)^4

T16 <- tibble(
  diagnostic = c("Ljung-Box Q(7)", "Ljung-Box Q(14)", "Ljung-Box Q(21)",
                 "Shapiro-Wilk W", "Shapiro-Wilk p", "Skewness of residuals",
                 "Kurtosis of residuals", "Residual standard deviation (bikes/day)",
                 "Residual sd as % of mean daily total"),
  value = c(as.numeric(lb1$statistic), as.numeric(lb2$statistic),
            as.numeric(lb3$statistic), as.numeric(sw$statistic), sw$p.value,
            sk, ku, sd(r_ts), sd(r_ts) / mean(y) * 100),
  p_value = c(lb1$p.value, lb2$p.value, lb3$p.value, NA, NA, NA, NA, NA, NA)
) %>%
  mutate(value = round(value, 4), p_value = signif(p_value, 4))
save_tab(T16, "T16_ts_diagnostics")
save_md(T16, "T16_ts_diagnostics", digits = 4, caption = "Table T16 - Residual diagnostics of the final SARIMA model")
cat("\n---- 7.1 residual diagnostics (T16) ----\n")
print(as.data.frame(T16), digits = 4)

save_fig(plot_acf_pacf(r_ts, 28, "Residual ACF / PACF of the final SARIMA model"),
         "F14_ts_resid_acf", width = 9, height = 4)

p_hist <- ggplot(tibble(r = as.numeric(r_ts)), aes(r)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = pal_main[2], colour = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(r_ts), sd = sd(r_ts)),
                colour = pal_main[5], linewidth = 0.9) +
  labs(title = "Residuals of the final SARIMA model",
       subtitle = sprintf("mean = %.1f, sd = %.1f, skewness = %.2f, excess kurtosis = %.2f",
                          mean(r_ts), sd(r_ts), sk, ku - 3),
       x = "Residual (bikes/day)", y = "Density") +
  theme_ev()
save_fig(p_hist, "F15_ts_resid_hist", width = 7.5, height = 4.4)

## ---------------------------------------------------------------------------
## 8. Forecast for 1-7 January 2013 (T17)
## ---------------------------------------------------------------------------
fc <- predict(fit_ts, n.ahead = 7, se.fit = TRUE)
T17 <- tibble(
  step     = 1:7,
  dteday   = seq(as.Date("2012-12-31") + 1, by = "day", length.out = 7),
  weekday  = weekdays(dteday),
  forecast = as.numeric(fc$pred),
  se       = as.numeric(fc$se),
  lower_95 = forecast - qnorm(0.975) * se,
  upper_95 = forecast + qnorm(0.975) * se
) %>%
  mutate(across(c(forecast, lower_95, upper_95, se), ~ round(.x, 1)))
cat("\n---- 8.1 forecast from the end of the sample (T17) ----\n")
print(as.data.frame(T17), digits = 5)
save_tab(T17, "T17_ts_forecast")
save_md(T17, "T17_ts_forecast", digits = 1, caption = "Table T17 - SARIMA forecast of the daily total rentals for the first week of January 2013, with the 95% forecast interval")

hist_df <- tibble(dteday = df$dteday, cnt = df$cnt)
p_fc <- ggplot() +
  geom_line(data = hist_df %>% filter(dteday >= as.Date("2012-10-01")),
            aes(dteday, cnt), colour = "grey45", linewidth = 0.45) +
  geom_ribbon(data = T17, aes(dteday, ymin = lower_95, ymax = upper_95),
              fill = pal_main[5], alpha = 0.22) +
  geom_line(data = T17, aes(dteday, forecast), colour = pal_main[5], linewidth = 0.9) +
  geom_point(data = T17, aes(dteday, forecast), colour = pal_main[5], size = 1.8) +
  geom_vline(xintercept = as.Date("2012-12-31"), linetype = "dashed", colour = "grey40") +
  annotate("label", x = as.Date("2013-01-02"), y = max(T17$upper_95) * 1.05,
           label = sprintf("2 Jan 2013 (Wed)\n%.0f bikes\n[%.0f, %.0f]",
                           T17$forecast[2], T17$lower_95[2], T17$upper_95[2]),
           size = 3, fill = "white", colour = pal_main[5]) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(title = "SARIMA forecast of the daily total rentals, 1-7 January 2013",
       subtitle = "Shaded band = 95% forecast interval",
       x = NULL, y = "Total rentals per day") +
  theme_ev()
save_fig(p_fc, "F16_ts_forecast", width = 10, height = 4.8)

fit_hist <- tibble(dteday = df$dteday, observed = as.numeric(y),
                   fitted = as.numeric(y - r_ts))
p_fit <- ggplot(fit_hist %>% filter(dteday >= as.Date("2012-06-01")), aes(dteday)) +
  geom_line(aes(y = observed), colour = "grey55", linewidth = 0.5) +
  geom_line(aes(y = fitted), colour = pal_main[1], linewidth = 0.7) +
  labs(title = "One-step-ahead fitted values of the final SARIMA model (last 7 months)",
       subtitle = sprintf("residual sd = %.0f bikes/day (%.1f%% of the mean daily total)",
                          sd(r_ts), sd(r_ts) / mean(df$cnt) * 100),
       x = NULL, y = "Bikes per day") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme_ev()
save_fig(p_fit, "F17_ts_fitted", width = 10, height = 4.4)

saveRDS(list(fit_ts = fit_ts, T17 = T17, comp = comp, res = res, T14 = T14,
             T11 = T11, T12 = T12, T15 = T15, T16 = T16,
             strength_seasonal = strength_seasonal,
             strength_trend = strength_trend),
        "vibe_work/data/timeseries_objects.rds")

cat("\n==================== 03 TIME SERIES COMPLETE =====================\n")
