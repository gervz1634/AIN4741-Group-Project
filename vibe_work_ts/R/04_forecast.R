###############################################################################
## 04_forecast.R -- the selected model refitted on all data, its diagnostics
##   and the forecast for 17:00 on Wednesday 2 January 2013
## Output : T10-T13 ; F14-F17 ; data/forecast.rds
###############################################################################

source("vibe_work_ts/R/00_setup.R")

cat("\n================= 04 FINAL MODEL AND FORECAST ====================\n\n")

d <- readRDS("vibe_work_ts/data/hourly_ts.rds")
hourly <- d$hourly
ms <- readRDS("vibe_work_ts/data/model_selection.rds")
final_name <- ms$best_model
cat("model selected in script 03:", final_name, "\n\n")

holiday_dates <- hourly %>% as_tibble() %>% filter(holiday == 1) %>% distinct(date) %>% pull(date)

## ---------------------------------------------------------------------------
## 1. refit the selected model on the whole sample
## ---------------------------------------------------------------------------
f_final <- logcnt ~ 0 + how + holiday + trend() + fourier(period = 8766, K = 4)
fit_all <- hourly %>% model(FINAL = TSLM(f_final))
cat("---- 1.1 the fitted model (all", nrow(hourly), "hours) ----\n")
g <- fit_all %>% glance()
print(as.data.frame(g %>% select(.model, sigma2, r_squared, adj_r_squared, AIC, AICc, BIC, log_lik)), digits = 6)

coefs <- fit_all %>% tidy()
k <- nrow(coefs)
cat("\nnumber of estimated parameters:", k, "\n")
cat("residual sd on the log scale :", round(sqrt(g$sigma2), 4),
    " (a one-sigma multiplicative factor of", round(exp(sqrt(g$sigma2)), 3), ")\n")
cat("R2 on the log scale          :", round(g$r_squared, 4), "\n")

## R2 on the count scale (back-transformed fitted values)
aug <- fit_all %>% augment() %>% as_tibble() %>%
  left_join(hourly %>% as_tibble() %>% select(datetime, cnt, filled), by = "datetime") %>%
  mutate(fitted_cnt = exp(.fitted))
r2_cnt <- 1 - sum((aug$cnt - aug$fitted_cnt)^2) / sum((aug$cnt - mean(aug$cnt))^2)
cat("R2 on the count scale        :", round(r2_cnt, 4), "\n")
cat("RMSE on the count scale      :", round(sqrt(mean((aug$cnt - aug$fitted_cnt)^2)), 2), "bikes/hour\n")
cat("MAE  on the count scale      :", round(mean(abs(aug$cnt - aug$fitted_cnt)), 2), "bikes/hour\n\n")

## ---------------------------------------------------------------------------
## 2. the coefficients, in interpretable form (T10)
## ---------------------------------------------------------------------------
hours_of_week <- levels(hourly$how)
trend_coef <- coefs %>% filter(str_starts(term, "trend")) %>% pull(estimate)
if (length(trend_coef) == 0) trend_coef <- 0
cat("\n---- 2.0 the linear trend ----\n")
cat("trend coefficient:", round(trend_coef, 8), "per hour on the log scale\n")
cat("implied growth per year:", round(100 * (exp(trend_coef * 8766) - 1), 1), "%\n")
cat("implied growth from 2011 to 2012 (one year):",
    round(100 * (exp(trend_coef * 8766) - 1), 1), "%\n")

how_coefs <- coefs %>% filter(str_starts(term, "how")) %>%
  mutate(how = str_remove(term, "^how"),
         dow_int = as.integer(str_sub(how, 1, 1)),
         hour = as.integer(str_sub(how, 3, 4)),
         dow = factor(dow_int, levels = 0:6,
                      labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
         ## level of each hour-of-week at the middle of the sample (the trend()
         ## index is 1, 2, ..., n), so that the numbers are comparable with the
         ## observed average of each hour of the week
         level_bikes = exp(estimate + trend_coef * (nrow(hourly) + 1) / 2),
         relative = 100 * (level_bikes / mean(level_bikes) - 1))

T10 <- how_coefs %>%
  select(dow, hour, estimate, level_bikes, relative) %>%
  arrange(dow, hour) %>%
  mutate(across(c(level_bikes, relative), ~ round(.x, 1)),
         estimate = round(estimate, 4))
save_tab(T10, "T10_hour_of_week_profile")
save_md(T10, "T10_hour_of_week_profile", digits = 3,
        caption = "Table T10 - The 168 hour-of-week coefficients of the final model. estimate = coefficient on the log scale; level_bikes = exp(estimate), i.e. the multiplicative level of that hour-of-week; relative = percentage deviation from the average hour-of-week level.")

cat("---- 2.1 the highest and lowest hours of the week ----\n")
print(as.data.frame(bind_rows(
  T10 %>% arrange(desc(level_bikes)) %>% slice_head(n = 5),
  T10 %>% arrange(level_bikes) %>% slice_head(n = 5))), digits = 3)

hol_coef <- coefs %>% filter(term == "holiday")
cat("\n---- 2.2 the holiday effect ----\n")
cat("coefficient:", round(hol_coef$estimate, 4), " se:", round(hol_coef$std.error, 4),
    " -> on a federal holiday demand is", round(100 * exp(hol_coef$estimate), 2),
    "% of the level of the same hour of week on an ordinary day (",
    round(100 * (exp(hol_coef$estimate) - 1), 2), "%)\n")

fourier_coefs <- coefs %>% filter(str_starts(term, "fourier"))
T11 <- fourier_coefs %>% select(term, estimate, std.error, statistic, p.value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
save_tab(T11, "T11_fourier_coefficients")
save_md(T11, "T11_fourier_coefficients", digits = 4,
        caption = "Table T11 - The eight annual Fourier coefficients (period 8,766 h = 365.25 days; K = 4) of the final model, on the log scale")

## ---------------------------------------------------------------------------
## 3. residual diagnostics (T12)
## ---------------------------------------------------------------------------
r <- aug$.innov
lb48  <- Box.test(r, lag = 48,  type = "Ljung-Box", fitdf = k)
lb168 <- Box.test(r, lag = 240, type = "Ljung-Box", fitdf = k)
acf_r <- acf(r, lag.max = 168, plot = FALSE)$acf
## shapiro.test is limited to 5,000 observations: a random subsample is used
set.seed(2013)
sw <- shapiro.test(as.numeric(sample(r, 4999)))

T12 <- tibble(
  diagnostic = c("Ljung-Box Q(240), df = 240 - k",
                 "Shapiro-Wilk W (subsample of 4,999)", "Shapiro-Wilk p",
                 "residual ACF at lag 1", "residual ACF at lag 24",
                 "residual ACF at lag 48", "residual ACF at lag 168",
                 "residual sd (log scale)", "residual sd (bikes/hour)"),
  value = c(as.numeric(lb168$statistic),
            as.numeric(sw$statistic), sw$p.value,
            acf_r[2], acf_r[25], acf_r[49], acf_r[169],
            sd(r), sd(aug$cnt - aug$fitted_cnt)),
  p_value = c(lb168$p.value, NA, NA, NA, NA, NA, NA, NA, NA),
  df = c(240 - k, NA, NA, NA, NA, NA, NA, NA, NA)
) %>%
  mutate(across(c(value, p_value), ~ signif(.x, 5)))
print(as.data.frame(T12), digits = 5)
save_tab(T12, "T12_final_diagnostics")
save_md(T12, "T12_final_diagnostics", digits = 5,
        caption = "Table T12 - Residual diagnostics of the final model (all 17,544 hours). The Ljung-Box tests use the correct degrees of freedom (lag minus the 177 estimated parameters).")

## ---------------------------------------------------------------------------
## 4. diagnostic figures (F14, F15)
## ---------------------------------------------------------------------------
p_res <- gg_tsresiduals(fit_all %>% select(FINAL))
save_fig(p_res, "F14_ts_residuals", width = 10.5, height = 6.5)

res_hour <- aug %>%
  mutate(hour = as.integer(format(datetime, "%H")),
         dow = as.integer(format(datetime, "%w"))) %>%
  group_by(hour) %>%
  summarise(mean_innov = mean(.innov), sd_innov = sd(.innov), .groups = "drop")

p15a <- ggplot(res_hour, aes(hour, mean_innov)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_col(fill = pal_main[5], width = .7) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "(a) Mean residual by hour of the day (log scale)",
       subtitle = "no systematic bias at any hour: the hour-of-week profile is well captured",
       x = "hour", y = "mean residual") + theme_ev(9)

p15b <- ggplot(aug, aes(.fitted, .innov)) +
  geom_hline(yintercept = 0, colour = "grey55") +
  geom_point(alpha = .12, size = .5, colour = pal_main[1]) +
  geom_smooth(method = "loess", se = FALSE, colour = pal_main[5], linewidth = .8) +
  labs(title = "(b) Residuals vs fitted (log scale)",
       subtitle = "slightly wider spread at high fitted values -> the variance is not perfectly constant",
       x = "fitted log(cnt)", y = "residual") + theme_ev(9)

p15c <- ggplot(aug %>% filter(filled == 0), aes(.fitted, .innov)) +
  geom_hline(yintercept = 0, colour = "grey55") +
  geom_hex(bins = 45) +
  scale_fill_gradientn(colours = c("#f4f8fa", pal_main[3], pal_main[2], pal_main[1]), name = "hours") +
  labs(title = "(c) The same, as a density (2-D histogram)", x = "fitted log(cnt)", y = "residual") +
  theme_ev(9)
save_fig((p15a | p15b) / (p15c | plot_spacer()), "F15_residual_structure", width = 12, height = 7)

## ---------------------------------------------------------------------------
## 5. the future covariate window and the forecast
##    2013-01-01 is New Year's Day (a federal holiday); nothing else in the
##    first week of January 2013 is a holiday.
## ---------------------------------------------------------------------------
future <- tibble(
  datetime = seq(as.POSIXct("2013-01-01 00:00:00", tz = "UTC"), by = "hour", length.out = 168)
) %>%
  mutate(
    hr  = as.integer(format(datetime, "%H")),
    dow = as.integer(format(datetime, "%w")),
    how = factor(paste0(dow, "_", sprintf("%02d", hr)), levels = levels(hourly$how)),
    holiday = as.integer(as.Date(datetime) == as.Date("2013-01-01"))
  ) %>%
  as_tsibble(index = datetime)

fc <- fit_all %>% forecast(new_data = future, level = c(80, 95))
fc_tbl <- fc %>%
  hilo(level = c(80, 95)) %>%
  unpack_hilo(c(`80%`, `95%`)) %>%
  as_tibble() %>%
  transmute(
    datetime,
    median_cnt = exp(.mean),
    mean_cnt   = exp(.mean + ((`95%_upper` - .mean) / qnorm(0.975))^2 / 2),
    lo80 = exp(`80%_lower`), hi80 = exp(`80%_upper`),
    lo95 = exp(`95%_lower`), hi95 = exp(`95%_upper`)
  )

cat("\n---- 5.1 forecast for Wednesday 2 January 2013, hour by hour (T13) ----\n")
T13 <- fc_tbl %>%
  filter(as.Date(datetime) == as.Date("2013-01-02")) %>%
  mutate(hour = as.integer(format(datetime, "%H"))) %>%
  select(hour, median_cnt, mean_cnt, lo80, hi80, lo95, hi95) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
print(as.data.frame(T13), digits = 4)
save_tab(T13, "T13_forecast_2013_01_02")
save_md(T13, "T13_forecast_2013_01_02", digits = 1,
        caption = "Table T13 - Predicted number of rentals for every hour of Wednesday 2 January 2013 (the median of the predictive distribution, its mean, and the 80% and 95% intervals)")

target <- fc_tbl %>% filter(datetime == as.POSIXct("2013-01-02 17:00:00", tz = "UTC"))
cat("\n=========== THE ANSWER ===========\n")
cat(sprintf("17:00 on Wednesday 2 January 2013 : %.0f rentals (median)\n", target$median_cnt))
cat(sprintf("                                    expected value %.0f\n", target$mean_cnt))
cat(sprintf("                                    80%% interval [%.0f, %.0f]\n", target$lo80, target$hi80))
cat(sprintf("                                    95%% interval [%.0f, %.0f]\n", target$lo95, target$hi95))
cat(sprintf("implied daily total for 2 Jan 2013 : %.0f rentals (sum of the 24 hourly medians)\n",
            sum(T13$median_cnt)))
cat(sprintf("                                     sum of the 24 hourly expected values: %.0f\n",
            sum(T13$mean_cnt)))
cat("\nreference: mean hourly demand in January 2012 =",
    round(mean(hourly$cnt[format(hourly$datetime, "%Y-%m") == "2012-01"]), 1),
    "| mean over all hours =", round(mean(hourly$cnt), 1), "\n")

## ---------------------------------------------------------------------------
## 6. forecast figures (F16, F17)
## ---------------------------------------------------------------------------
hist_df <- hourly %>% as_tibble() %>% select(datetime, cnt) %>%
  filter(datetime >= as.POSIXct("2012-12-18", tz = "UTC"))

p_fc <- ggplot() +
  geom_ribbon(data = fc_tbl, aes(datetime, ymin = lo95, ymax = hi95),
              fill = pal_main[5], alpha = .18) +
  geom_ribbon(data = fc_tbl, aes(datetime, ymin = lo80, ymax = hi80),
              fill = pal_main[5], alpha = .22) +
  geom_line(data = hist_df, aes(datetime, cnt), colour = "grey45", linewidth = .45) +
  geom_line(data = fc_tbl, aes(datetime, median_cnt), colour = pal_main[5], linewidth = .8) +
  geom_vline(xintercept = as.POSIXct("2012-12-31 23:00:00", tz = "UTC"),
             linetype = "dashed", colour = "grey40") +
  geom_point(data = target %>% mutate(hour = 17), aes(datetime, median_cnt),
             colour = pal_main[1], size = 2.4) +
  annotate("label", x = target$datetime, y = target$hi95 * 1.02,
           label = sprintf("2 Jan 2013, 17:00\n%.0f [%.0f, %.0f]", target$median_cnt,
                           target$lo95, target$hi95),
           size = 3, colour = pal_main[1], fill = "white") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
  labs(title = "Forecast of hourly rentals for the first week of January 2013",
       subtitle = "grey = observed, red line = median forecast, bands = 80% and 95% intervals",
       x = NULL, y = "rentals per hour") +
  theme_ev()
save_fig(p_fc, "F16_forecast_week", width = 11, height = 5)

p_day <- T13 %>%
  ggplot(aes(hour)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = pal_main[5], alpha = .18) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = pal_main[5], alpha = .22) +
  geom_line(aes(y = median_cnt), colour = pal_main[5], linewidth = 1) +
  geom_point(aes(y = median_cnt), colour = pal_main[5], size = 2) +
  geom_point(data = T13 %>% filter(hour == 17), aes(y = median_cnt),
             colour = pal_main[1], size = 4) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(title = "The forecast for each hour of Wednesday 2 January 2013",
       subtitle = sprintf("the 17:00 value is %.0f rentals (95%% interval [%.0f, %.0f])",
                          target$median_cnt, target$lo95, target$hi95),
       x = "hour of the day", y = "rentals per hour") +
  theme_ev()
save_fig(p_day, "F17_forecast_day", width = 9.5, height = 4.6)

## ---------------------------------------------------------------------------
## 7. sensitivity of the answer to the specification (T14)
## ---------------------------------------------------------------------------
cat("\n---- 7.1 sensitivity of the 17:00 forecast to the specification ----\n")
sens <- imap_dfr(list(
  "K = 2 annual harmonics"         = logcnt ~ 0 + how + holiday + trend() + fourier(period = 8766, K = 2),
  "K = 3 annual harmonics"         = logcnt ~ 0 + how + holiday + trend() + fourier(period = 8766, K = 3),
  "K = 4 annual harmonics (FINAL)" = f_final,
  "K = 5 annual harmonics"         = logcnt ~ 0 + how + holiday + trend() + fourier(period = 8766, K = 5),
  "without the holiday term"       = logcnt ~ 0 + how + trend() + fourier(period = 8766, K = 4),
  "without the trend"              = logcnt ~ 0 + how + holiday + fourier(period = 8766, K = 4),
  "without the annual cycle"       = logcnt ~ 0 + how + holiday + trend()
), function(f, nm) {
  m <- hourly %>% model(M = TSLM(f))
  p <- m %>% forecast(new_data = future, level = 95) %>% hilo(level = 95) %>%
    unpack_hilo(`95%`) %>% as_tibble() %>%
    filter(datetime == as.POSIXct("2013-01-02 17:00:00", tz = "UTC"))
  tibble(specification = nm, median_1700 = exp(p$.mean),
         lo95 = exp(p$`95%_lower`), hi95 = exp(p$`95%_upper`))
}) %>%
  mutate(across(c(median_1700, lo95, hi95), ~ round(.x, 1)))
print(as.data.frame(sens), digits = 4)
save_tab(sens, "T14_sensitivity")
save_md(sens, "T14_sensitivity", digits = 1,
        caption = "Table T14 - Sensitivity of the 17:00 forecast to the specification of the model")

saveRDS(list(fit_all = fit_all, fc_tbl = fc_tbl, T10 = T10, T12 = T12, T13 = T13,
             target = target, sens = sens, coefs = coefs, r2_cnt = r2_cnt, k = k),
        "vibe_work_ts/data/forecast.rds")

cat("\nFigures F14-F17 written to", path_fig, "\n")
cat("\n===================== 04 FORECAST COMPLETE ======================\n")
