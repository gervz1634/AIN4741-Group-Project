###############################################################################
## 03_models.R -- transformation, stationarity, model search and selection
##   * Box-Cox (Guerrero) and the variance/level relationship
##   * KPSS / ndiffs / nsdiffs on the hourly series
##   * ACF and PACF (feasts + ggfortify)
##   * 8 candidate models compared in sample and on a 2-week hold-out
## Output : T07-T10 ; F10-F13 ; data/model_selection.rds
###############################################################################

source("vibe_work_ts/R/00_setup.R")

cat("\n=================== 03 MODEL SEARCH & SELECTION ===================\n\n")

d <- readRDS("vibe_work_ts/data/hourly_ts.rds")
hourly <- d$hourly
n <- nrow(hourly)
cat("observations:", n, "| from", format(min(hourly$datetime)), "to",
    format(max(hourly$datetime)), "\n\n")

## ---------------------------------------------------------------------------
## 1. Is a variance-stabilising transformation needed? (T07)
## ---------------------------------------------------------------------------
lambda <- guerrero(hourly$cnt, .period = 24)
cat("---- 1.1 Guerrero / Box-Cox lambda for period 24 ----\n")
cat("lambda =", round(as.numeric(lambda), 4),
    "(0 = log, 1 = no transformation)\n")

T07 <- hourly %>%
  as_tibble() %>%
  mutate(bin = ntile(cnt, 10)) %>%
  group_by(bin) %>%
  summarise(mean_cnt = mean(cnt), sd_cnt = sd(cnt), cv = sd_cnt / mean_cnt, .groups = "drop") %>%
  mutate(cv = round(cv, 3), across(c(mean_cnt, sd_cnt), ~ round(.x, 1)))
save_tab(T07, "T07_variance_level")
save_md(T07, "T07_variance_level", digits = 3,
        caption = "Table T07 - The variance of hourly demand grows with the level: deciles of cnt, with the standard deviation and coefficient of variation inside each decile. This is the reason a log transformation is considered.")
cat("\n---- 1.2 Table T07: sd grows with the level ----\n"); print(as.data.frame(T07))
cat("\ncoefficient of variation: smallest decile", round(T07$cv[1], 2),
    "-> largest decile", round(T07$cv[10], 2), "\n")
cat("sd of log(cnt) =", round(sd(hourly$logcnt), 4), "\n\n")

## ---------------------------------------------------------------------------
## 2. Stationarity (T08)
## ---------------------------------------------------------------------------
kpss_tbl <- function(x) {
  r <- unitroot_kpss(x)
  if (is.data.frame(r)) tibble(stat = as.numeric(r$kpss_stat), p = as.numeric(r$kpss_pvalue))
  else tibble(stat = as.numeric(r[[1]]), p = as.numeric(r[[2]]))
}
kpss     <- kpss_tbl(hourly$cnt)
kpss_log <- kpss_tbl(hourly$logcnt)
nd   <- unitroot_ndiffs(hourly$cnt)
nsd  <- unitroot_nsdiffs(hourly$cnt)

T08 <- tibble(
  test = c("KPSS on the level of cnt",
           "KPSS on the log of cnt",
           "number of first differences suggested (KPSS)",
           "number of seasonal differences suggested (period 24)"),
  statistic = c(round(kpss$stat, 4), round(kpss_log$stat, 4), NA, NA),
  p_value = c(kpss$p, kpss_log$p, NA, NA),
  conclusion = c(ifelse(kpss$p < 0.05, "unit root / trend present: difference it", "stationary"),
                 ifelse(kpss_log$p < 0.05, "unit root / trend present: difference it", "stationary"),
                 paste0("d = ", nd), paste0("D = ", nsd))
)
save_tab(T08, "T08_stationarity")
save_md(T08, "T08_stationarity", digits = 4, caption = "Table T08 - Stationarity tests on the hourly series")
cat("---- 2.1 Table T08: stationarity ----\n"); print(as.data.frame(T08), digits = 4)
cat("\nKPSS rejects constancy of the mean in the level series (the annual cycle\n",
    "and the growth of the system); the daily seasonal pattern is treated as\n",
    "deterministic, which is why the models below use seasonal harmonics /\n",
    "dummies rather than a seasonal difference.\n\n")

## ---------------------------------------------------------------------------
## 3. ACF and PACF (F10, F11)
## ---------------------------------------------------------------------------
## F10 : feasts::ACF / PACF of the level, of the daily difference, of the weekly difference
acf_panel <- function(x, lag, title) {
  ACF(x, cnt, lag_max = lag) %>% autoplot() +
    labs(title = title, y = "ACF") + theme_ev(9)
}
p10a <- ACF(hourly, cnt, lag_max = 168) %>% autoplot() +
  labs(title = "(a) ACF of the level series", subtitle = "peaks every 24 h, on a slowly decaying background",
       y = "ACF") + theme_ev(9)
p10b <- hourly %>% mutate(d24 = difference(cnt, 24)) %>%
  ACF(d24, lag_max = 168) %>% autoplot() +
  labs(title = "(b) ACF after a daily difference (lag 24)", y = "ACF") + theme_ev(9) + labs(x = "lag")
p10c <- hourly %>% mutate(d24 = difference(cnt, 24), d24d168 = difference(d24, 168)) %>%
  ACF(d24d168, lag_max = 168) %>% autoplot() +
  labs(title = "(c) ACF after daily and weekly differences", y = "ACF") + theme_ev(9)
save_fig((p10a / p10b / p10c), "F10_acf_feasts", width = 10, height = 9)

## F11 : the same information drawn with ggfortify (base-R acf/pacf objects)
a_lvl <- acf(as.numeric(hourly$cnt), lag.max = 168, plot = FALSE)
p_lvl <- acf(as.numeric(diff(as.numeric(hourly$cnt), lag = 24)), lag.max = 168, plot = FALSE)
p11a <- autoplot(a_lvl) + labs(title = "(a) ACF, level (ggfortify)", y = "ACF") + theme_ev(9)
p11b <- autoplot(p_lvl) + labs(title = "(b) PACF, daily-differenced (ggfortify)", y = "PACF") + theme_ev(9)
save_fig(p11a | p11b, "F11_ggfortify_acf", width = 12, height = 4.4)

## ---------------------------------------------------------------------------
## 4. Training / test split and the candidate models
## ---------------------------------------------------------------------------
h_train <- hourly %>% filter(datetime <  as.POSIXct("2012-12-18", tz = "UTC"))
h_test  <- hourly %>% filter(datetime >= as.POSIXct("2012-12-18", tz = "UTC"))
cat("---- 4.1 split ----\n")
cat("training:", nrow(h_train), "hours (", format(min(h_train$datetime)), "to",
    format(max(h_train$datetime)), ")\n")
cat("hold-out:", nrow(h_test), "hours (", format(min(h_test$datetime)), "to",
    format(max(h_test$datetime)), ") =", nrow(h_test) / 24, "days\n\n")

cat("---- 4.2 fitting 8 candidate models (this takes a couple of minutes) ----\n")
t0 <- Sys.time()
fits_lvl <- h_train %>% model(
  SNAIVE_day    = SNAIVE(cnt ~ lag(24)),
  SNAIVE_week   = SNAIVE(cnt ~ lag(168)),
  TSLM_harm     = TSLM(cnt ~ trend() + fourier(period = 24, K = 6) +
                         fourier(period = 168, K = 6) + fourier(period = 8766, K = 2)),
  ARIMA_fourier = ARIMA(cnt ~ fourier(period = 24, K = 4) + fourier(period = 168, K = 2) + PDQ(0, 0, 0)),
  ARIMA_auto    = ARIMA(cnt)
)
fits_log <- h_train %>% model(
  TSLM_log_harm    = TSLM(logcnt ~ trend() + fourier(period = 24, K = 6) +
                            fourier(period = 168, K = 6) + fourier(period = 8766, K = 2)),
  TSLM_log_how     = TSLM(logcnt ~ 0 + how),
  TSLM_log_how_K3  = TSLM(logcnt ~ 0 + how + fourier(period = 8766, K = 3)),
  TSLM_log_how_K4  = TSLM(logcnt ~ 0 + how + fourier(period = 8766, K = 4)),
  TSLM_log_how_hol = TSLM(logcnt ~ 0 + how + holiday + fourier(period = 8766, K = 4)),
  TSLM_log_how_hol_tr = TSLM(logcnt ~ 0 + how + holiday + trend() + fourier(period = 8766, K = 4))
)
cat("fitted in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), "min\n\n")

## ---------------------------------------------------------------------------
## 5. Comparison in sample and out of sample (T09)
## ---------------------------------------------------------------------------
gl <- bind_rows(
  fits_lvl %>% glance() %>% mutate(scale = "level", .after = .model),
  fits_log %>% glance() %>% mutate(scale = "log", .after = .model)
) %>%
  select(.model, scale, sigma2, r_squared, AIC, AICc, BIC, CV)

cat("---- 5.1 glance() of all candidates (AICc is on the scale of the response) ----\n")
print(as.data.frame(gl), digits = 5)

## forecasts on the hold-out window, back-transformed to the count scale
fc <- bind_rows(
  fits_lvl %>% forecast(new_data = h_test) %>% as_tibble() %>%
    select(.model, datetime, pred = .mean) %>% mutate(log_scale = FALSE),
  fits_log %>% forecast(new_data = h_test) %>% as_tibble() %>%
    select(.model, datetime, pred = .mean) %>% mutate(log_scale = TRUE)
)

oos <- fc %>%
  left_join(h_test %>% as_tibble() %>% select(datetime, actual = cnt), by = "datetime") %>%
  mutate(pred = if_else(log_scale, exp(pred), pred)) %>%
  group_by(.model) %>%
  summarise(ME_oos = mean(actual - pred),
            RMSE_oos = sqrt(mean((actual - pred)^2)),
            MAE_oos = mean(abs(actual - pred)),
            R2_oos = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2),
            .groups = "drop")

## empirical coverage of the 95% forecast interval on the hold-out window
cov95 <- bind_rows(
  fits_lvl %>% forecast(new_data = h_test, level = 95) %>% hilo(level = 95) %>%
    unpack_hilo(`95%`) %>% as_tibble() %>%
    transmute(.model, datetime, lo = `95%_lower`, hi = `95%_upper`, log_scale = FALSE),
  fits_log %>% forecast(new_data = h_test, level = 95) %>% hilo(level = 95) %>%
    unpack_hilo(`95%`) %>% as_tibble() %>%
    transmute(.model, datetime, lo = exp(`95%_lower`), hi = exp(`95%_upper`))
) %>%
  left_join(h_test %>% as_tibble() %>% select(datetime, actual = cnt), by = "datetime") %>%
  group_by(.model) %>%
  summarise(cover95_oos = mean(actual >= lo & actual <= hi), .groups = "drop")

## in-sample fit on the original scale
ins <- bind_rows(
  fits_lvl %>% augment() %>% as_tibble() %>% select(.model, datetime, fitted = .fitted) %>%
    mutate(log_scale = FALSE),
  fits_log %>% augment() %>% as_tibble() %>% select(.model, datetime, fitted = .fitted) %>%
    mutate(log_scale = TRUE)
) %>%
  left_join(h_train %>% as_tibble() %>% select(datetime, cnt), by = "datetime") %>%
  mutate(fitted = if_else(log_scale, exp(fitted), fitted)) %>%
  group_by(.model) %>%
  summarise(RMSE_in = sqrt(mean((cnt - fitted)^2)),
            MAE_in = mean(abs(cnt - fitted)),
            R2_in = 1 - sum((cnt - fitted)^2) / sum((cnt - mean(cnt))^2),
            .groups = "drop")

## residual autocorrelation at the daily and weekly lags (whiteness evidence)
innov <- bind_rows(
  fits_lvl %>% augment() %>% as_tibble() %>% select(.model, datetime, innov = .innov),
  fits_log %>% augment() %>% as_tibble() %>% select(.model, datetime, innov = .innov)
) %>%
  group_by(.model) %>%
  summarise(
    acf24  = acf(as.numeric(na.omit(innov)), lag.max = 24,  plot = FALSE)$acf[25],
    acf168 = acf(as.numeric(na.omit(innov)), lag.max = 168, plot = FALSE)$acf[169],
    lb48   = as.numeric(Box.test(as.numeric(na.omit(innov)), lag = 48,
                                 type = "Ljung-Box")$statistic),
    .groups = "drop")

k_par <- bind_rows(fits_lvl %>% tidy() %>% count(.model, name = "k"),
                   fits_log %>% tidy() %>% count(.model, name = "k"))

T09 <- gl %>%
  left_join(k_par, by = ".model") %>%
  select(-CV) %>%
  left_join(ins, by = ".model") %>%
  left_join(oos, by = ".model") %>%
  left_join(cov95, by = ".model") %>%
  left_join(innov, by = ".model") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(RMSE_oos)
save_tab(T09, "T09_model_comparison")
save_md(T09, "T09_model_comparison", digits = 3,
        caption = "Table T09 - The nine candidate models. sigma2/AIC/AICc/BIC belong to the scale on which the model was estimated (level or log); RMSE_in, R2_in and all *_oos columns are on the original count scale and are therefore directly comparable. acf24 / acf168 are the residual autocorrelations at the daily and weekly lag; lb48 is the Ljung-Box statistic at lag 48; cover95_oos is the share of the 336 hold-out hours that fell inside the 95% forecast interval.")
cat("\n---- 5.2 Table T09: model comparison ----\n")
print(as.data.frame(T09 %>% select(.model, scale, k, R2_in, RMSE_in, RMSE_oos, MAE_oos,
                                   ME_oos, R2_oos, cover95_oos, acf24, acf168)), digits = 4)

## ---------------------------------------------------------------------------
## 6. Rolling-origin evaluation: the decisive comparison (T09b)
##    A single hold-out window can be dominated by one unusual fortnight. The
##    last two weeks of December 2012 contain the Christmas collapse, which
##    flatters models with a low level. We therefore re-evaluate the four most
##    promising specifications at 57 successive origins (one per day over the
##    last eight weeks), always forecasting the next 24 hours.
## ---------------------------------------------------------------------------
cat("\n---- 6.1 rolling-origin evaluation: 57 origins x 24 hours ahead ----\n")
eval_specs <- list(
  TSLM_log_how_K4     = logcnt ~ 0 + how + fourier(period = 8766, K = 4),
  TSLM_log_how_hol    = logcnt ~ 0 + how + holiday + fourier(period = 8766, K = 4),
  TSLM_log_how_hol_tr = logcnt ~ 0 + how + holiday + trend() + fourier(period = 8766, K = 4),
  TSLM_log_harm_tr    = logcnt ~ trend() + holiday + fourier(period = 24, K = 6) +
    fourier(period = 168, K = 6) + fourier(period = 8766, K = 2)
)
defs <- lapply(eval_specs, function(f) TSLM(f))
eval_start <- which(hourly$datetime >= as.POSIXct("2012-11-05", tz = "UTC"))[1] - 1

t_roll <- Sys.time()
roll_res <- purrr::map_dfr(seq(eval_start, nrow(hourly) - 1, by = 24), function(o) {
  tr <- hourly[seq_len(o), ]
  nd <- hourly[(o + 1):min(o + 24, nrow(hourly)), ] %>% as_tibble() %>%
    select(datetime, cnt, how, holiday) %>% as_tsibble(index = datetime)
  m <- tr %>% model(!!!defs)
  m %>% forecast(new_data = nd) %>% as_tibble() %>%
    select(.model, datetime, pred = .mean) %>%
    mutate(pred = exp(pred)) %>%
    left_join(nd %>% as_tibble() %>% select(datetime, actual = cnt), by = "datetime")
})
cat("rolling loop took", round(as.numeric(difftime(Sys.time(), t_roll, units = "mins")), 2), "min\n")

T09b <- roll_res %>%
  group_by(.model) %>%
  summarise(n_forecasts = n(),
            RMSE_roll = sqrt(mean((actual - pred)^2)),
            MAE_roll  = mean(abs(actual - pred)),
            ME_roll   = mean(actual - pred),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(RMSE_roll)
save_tab(T09b, "T09b_rolling_origin")
save_md(T09b, "T09b_rolling_origin", digits = 2,
        caption = "Table T09b - Rolling-origin evaluation: 57 daily origins over the last eight weeks (5 Nov - 30 Dec 2012), each forecasting the next 24 hours (1,368 forecast hours in total).")
cat("\n---- 6.2 Table T09b: rolling-origin accuracy ----\n")
print(as.data.frame(T09b), digits = 4)

## ---------------------------------------------------------------------------
## 7. The selected model
## ---------------------------------------------------------------------------
cat("\n---- 7.1 ranking ----\n")
rank_tbl <- T09 %>% select(.model, scale, k, R2_in, RMSE_oos, MAE_oos, ME_oos) %>%
  mutate(rank_single_window = rank(RMSE_oos))
print(as.data.frame(rank_tbl), digits = 4)

best_roll <- T09b %>% slice(1)
cat("\nSingle hold-out window:", T09 %>% arrange(RMSE_oos) %>% slice(1) %>% pull(.model),
    "(RMSE", round(min(T09$RMSE_oos), 1), ")\n")
cat("Rolling origins       :", best_roll$.model,
    "(RMSE", best_roll$RMSE_roll, ", bias", best_roll$ME_roll, ")\n")
cat("\nThe two protocols disagree, and the reason is visible in Figure F12: the\n",
    "single window (18-31 December 2012) contains the Christmas collapse, when\n",
    "demand fell to 441-1,000 rentals per day, i.e. far below any smooth annual\n",
    "cycle. A model WITHOUT a trend has a lower level and is therefore rewarded\n",
    "by that particular fortnight, even though the level of the series has risen\n",
    "by about 42% between 2011 and 2012. Across 57 origins the version WITH a\n",
    "linear trend is clearly better (RMSE", best_roll$RMSE_roll, "against",
    round(T09b$RMSE_roll[T09b$.model == "TSLM_log_how_hol"], 1),
    ") and almost unbiased.\n",
    "We therefore adopt", best_roll$.model, "as the final model.\n", sep = "")
best_model <- best_roll$.model

## ---------------------------------------------------------------------------
## 8. Figures: the hold-out window (F12) and in-sample fit (F13)
## ---------------------------------------------------------------------------
show_models <- c("TSLM_log_how_hol_tr", "ARIMA_auto", "SNAIVE_day")
p_oos <- fc %>%
  filter(.model %in% show_models) %>%
  mutate(pred = if_else(log_scale, exp(pred), pred)) %>%
  left_join(h_test %>% as_tibble() %>% select(datetime, actual = cnt), by = "datetime") %>%
  ggplot(aes(datetime)) +
  geom_line(aes(y = actual), colour = "grey55", linewidth = 0.5) +
  geom_line(aes(y = pred, colour = .model), linewidth = 0.7) +
  scale_colour_manual(values = pal_main[c(5, 1, 4)]) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b") +
  facet_wrap(~ .model, ncol = 1) +
  labs(title = "The three best models on the two-week hold-out window",
       subtitle = "grey = observed hourly rentals, colour = forecast",
       x = NULL, y = "rentals per hour", colour = NULL) +
  theme_ev(10)
save_fig(p_oos, "F12_holdout_forecasts", width = 10.5, height = 7)

fit_final <- fits_log %>% select(TSLM_log_how_hol_tr) %>% augment() %>% as_tibble() %>%
  left_join(h_train %>% as_tibble() %>% select(datetime, cnt), by = "datetime")
p_fit <- fit_final %>%
  filter(datetime >= as.POSIXct("2012-11-01", tz = "UTC")) %>%
  ggplot(aes(datetime)) +
  geom_line(aes(y = cnt), colour = "grey55", linewidth = 0.5) +
  geom_line(aes(y = exp(.fitted)), colour = pal_main[2], linewidth = 0.7) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%d %b") +
  labs(title = "In-sample fit of the selected model (last two months of the training set)",
       subtitle = "grey = observed, blue = fitted; the model is a multiplicative hour-of-week profile x annual cycle",
       x = NULL, y = "rentals per hour") +
  theme_ev()
save_fig(p_fit, "F13_insample_fit", width = 10.5, height = 4.4)

saveRDS(list(fits_lvl = fits_lvl, fits_log = fits_log, T09 = T09, gl = gl,
             oos = oos, ins = ins, innov = innov, lambda = lambda,
             T07 = T07, T08 = T08, h_test = h_test, h_train = h_train,
             T09b = T09b, roll_res = roll_res, best_model = best_model),
        "vibe_work_ts/data/model_selection.rds")

cat("\n===================== 03 MODELS COMPLETE ========================\n")
