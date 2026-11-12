###############################################################################
## 02_decomposition.R -- seasonal decomposition of the hourly series
##   * multi-seasonal STL (24 h and 168 h) with feasts
##   * seasonal strength measures, seasonal indices
##   * seasonal / subseries plots (feasts) and the classical decomposition
##     (stats::decompose drawn with ggfortify)
## Output : T05, T06, T07 ; F05 - F09
###############################################################################

source("vibe_work_ts/R/00_setup.R")

cat("\n================== 02 SEASONAL DECOMPOSITION =====================\n\n")

d <- readRDS("vibe_work_ts/data/hourly_ts.rds")
hourly <- d$hourly
cat("series:", nrow(hourly), "hourly observations,",
    format(min(hourly$datetime)), "to", format(max(hourly$datetime)), "\n\n")

## ---------------------------------------------------------------------------
## 1. multi-seasonal STL: a fixed (periodic) weekly profile is the natural
##    object for an additive seasonal decomposition of calendar-driven demand
## ---------------------------------------------------------------------------
t0 <- Sys.time()
fit_stl <- hourly %>%
  model(STL(cnt ~ season(period = 24, window = "periodic") +
              season(period = 168, window = "periodic")))
cmp <- components(fit_stl) %>% as_tibble() %>%
  left_join(hourly %>% as_tibble() %>% select(datetime, hour, dow, date), by = "datetime")
cat("multi-seasonal STL fitted in",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s\n")
cat("identity check  trend + season24 + season168 + remainder == cnt :",
    isTRUE(all.equal(cmp$trend + cmp$season_24 + cmp$season_168 + cmp$remainder, cmp$cnt)),
    "\n\n")

## a second version with the automatic (smoothly varying) seasonal windows
fit_stl_auto <- hourly %>%
  model(STL(cnt ~ season(period = 24) + season(period = 168)))
cmp_auto <- components(fit_stl_auto) %>% as_tibble()

## ---------------------------------------------------------------------------
## 2. how much of the variation does each component carry? (T05)
## ---------------------------------------------------------------------------
share <- function(x, y = cmp$cnt) round(100 * var(x) / var(y), 2)
T05 <- tibble(
  component = c("trend (annual cycle + growth)", "daily seasonal (period 24)",
                "weekly seasonal (period 168)", "remainder"),
  var_share_periodic = c(share(cmp$trend), share(cmp$season_24),
                         share(cmp$season_168), share(cmp$remainder)),
  var_share_automatic = c(share(cmp_auto$trend), share(cmp_auto$season_24),
                          share(cmp_auto$season_168), share(cmp_auto$remainder)),
  sd_periodic = c(sd(cmp$trend), sd(cmp$season_24), sd(cmp$season_168), sd(cmp$remainder)),
  sd_automatic = c(sd(cmp_auto$trend), sd(cmp_auto$season_24),
                   sd(cmp_auto$season_168), sd(cmp_auto$remainder))
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

strength <- tibble(
  measure = c("strength of the daily seasonality (period 24)",
              "strength of the weekly seasonality (period 168)",
              "strength of the trend"),
  periodic = c(season_strength(cmp$remainder, cmp$season_24),
               season_strength(cmp$remainder, cmp$season_168),
               max(0, 1 - var(cmp$remainder) / var(cmp$trend + cmp$remainder))),
  automatic = c(season_strength(cmp_auto$remainder, cmp_auto$season_24),
                season_strength(cmp_auto$remainder, cmp_auto$season_168),
                max(0, 1 - var(cmp_auto$remainder) / var(cmp_auto$trend + cmp_auto$remainder)))
) %>%
  mutate(across(c(periodic, automatic), ~ round(.x, 4)))

T05 <- bind_rows(T05, tibble(component = "--- strength measures (0-1) ---",
                             var_share_periodic = NA, var_share_automatic = NA,
                             sd_periodic = NA, sd_automatic = NA))
T05b <- strength %>% rename(component = measure, var_share_periodic = periodic,
                            var_share_automatic = automatic)
save_tab(T05, "T05_stl_shares")
save_md(T05, "T05_stl_shares", digits = 3,
        caption = "Table T05a - Share of the variance of cnt carried by each STL component (percent), for the periodic and the automatic seasonal windows")
save_tab(T05b, "T05b_stl_strength")
save_md(T05b, "T05b_stl_strength", digits = 4,
        caption = "Table T05b - Strength of the seasonal components (Wang, Smith & Hyndman): max(0, 1 - Var(remainder)/Var(component + remainder))")

cat("---- 2.1 Table T05a: variance shares (percent) ----\n"); print(as.data.frame(T05), digits = 3)
cat("\n---- 2.2 Table T05b: strength of seasonality ----\n"); print(as.data.frame(T05b), digits = 4)

## ---------------------------------------------------------------------------
## 3. the seasonal indices (T06)
## ---------------------------------------------------------------------------
T06 <- tibble(
  hour = 0:23,
  seasonal_index_24h = as.numeric(tapply(cmp$season_24, cmp$hour, mean)),
  mean_cnt = as.numeric(tapply(cmp$cnt, cmp$hour, mean))
) %>%
  mutate(pct_of_mean = round(100 * seasonal_index_24h / mean(cmp$cnt), 2),
         seasonal_index_24h = round(seasonal_index_24h, 1),
         mean_cnt = round(mean_cnt, 1))
save_tab(T06, "T06_daily_seasonal_index")
save_md(T06, "T06_daily_seasonal_index", digits = 1,
        caption = "Table T06 - The 24-hour seasonal index from the STL decomposition (additive, bikes per hour)")
cat("\n---- 3.1 Table T06: daily seasonal index ----\n"); print(as.data.frame(T06), digits = 1)

T06b <- tibble(
  dow_int = cmp$dow, hour = cmp$hour, season_168 = cmp$season_168
) %>%
  group_by(dow_int, hour) %>% summarise(index = mean(season_168), .groups = "drop") %>%
  mutate(dow = factor(dow_int, levels = 0:6,
                      labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
weekly_offset <- T06b %>% group_by(dow) %>%
  summarise(weekly_index = mean(index), .groups = "drop") %>%
  mutate(weekly_index = round(weekly_index, 1))
save_tab(weekly_offset, "T06b_weekly_seasonal_index")
save_md(weekly_offset, "T06b_weekly_seasonal_index", digits = 1,
        caption = "Table T06b - The weekly part of the seasonal component (average over the 24 hours of each weekday)")

## ---------------------------------------------------------------------------
## 4. figures
## ---------------------------------------------------------------------------
## F05 : the four components, each at its natural time scale
p_tr <- ggplot(cmp, aes(datetime, trend)) +
  geom_line(colour = pal_main[1], linewidth = 0.5) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b %y") +
  labs(title = "(a) Trend component (annual cycle + growth of the system)",
       x = NULL, y = "trend") + theme_ev(10)

zoom3 <- cmp %>% filter(datetime >= as.POSIXct("2012-09-10", tz = "UTC"),
                        datetime <  as.POSIXct("2012-09-13", tz = "UTC"))
p_s24 <- ggplot(zoom3, aes(datetime, season_24)) +
  geom_line(colour = pal_main[4], linewidth = 1) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%H:%M") +
  labs(title = "(b) Daily seasonal component (3 days)",
       x = NULL, y = "season_24") + theme_ev(10)

zoom21 <- cmp %>% filter(datetime >= as.POSIXct("2012-09-03", tz = "UTC"),
                         datetime <  as.POSIXct("2012-09-24", tz = "UTC"))
p_s168 <- ggplot(zoom21, aes(datetime, season_168)) +
  geom_line(colour = pal_main[2], linewidth = 0.8) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%d %b") +
  labs(title = "(c) Weekly seasonal component (3 weeks)",
       x = NULL, y = "season_168") + theme_ev(10)

p_rem <- ggplot(cmp, aes(datetime, remainder)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(colour = pal_main[5], linewidth = 0.3) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b %y") +
  labs(title = "(d) Remainder (weather, holidays, special events)",
       x = NULL, y = "remainder") + theme_ev(10)

save_fig(p_tr / p_s24 / p_s168 / p_rem +
           plot_annotation(title = "Multi-seasonal STL decomposition of the hourly series",
                           subtitle = "periodic windows of 24 h and 168 h, additive",
                           caption = "trend + season_24 + season_168 + remainder = cnt (verified)"),
         "F05_stl_components", width = 10, height = 9)

## F06 : the fitted profile -- hour of day and hour of week
prof <- T06b %>%
  mutate(dow = factor(dow, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
p_wk <- ggplot(prof, aes(hour, index, colour = dow)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = pal_main[c(1, 2, 3, 4, 5, 6, 1)]) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "(b) Weekly seasonal component by weekday",
       subtitle = "weekdays share one shape, Saturday and Sunday another",
       x = "hour of the day", y = "season_168", colour = NULL) + theme_ev(10)

p_d24 <- ggplot(T06, aes(hour, seasonal_index_24h)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_col(fill = pal_main[4], width = 0.75) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "(a) Daily seasonal index (period 24)",
       x = "hour of the day", y = "season_24 (bikes/hour)") + theme_ev(10)
save_fig(p_d24 / p_wk, "F06_seasonal_indices", width = 9.5, height = 7)

## F07 : feasts::gg_season on a four-week window (day and week periods)
window4w <- hourly %>% filter(datetime >= as.POSIXct("2012-09-03", tz = "UTC"),
                              datetime <  as.POSIXct("2012-10-01", tz = "UTC"))
p_seas_day <- suppressWarnings(gg_season(window4w %>% as_tsibble(index = datetime), cnt, period = "day")) +
  labs(title = "(a) gg_season, period = 'day'", subtitle = "one line per day, four weeks") +
  theme_ev(9)
p_seas_wk <- suppressWarnings(gg_season(window4w %>% as_tsibble(index = datetime), cnt, period = "week")) +
  labs(title = "(b) gg_season, period = 'week'", subtitle = "one line per week, four weeks") +
  theme_ev(9)
save_fig(p_seas_day | p_seas_wk, "F07_gg_season", width = 12, height = 4.6)

## F08 : feasts::gg_subseries on an eight-week window (24 panels, one per hour)
window8w <- hourly %>% filter(datetime >= as.POSIXct("2012-06-04", tz = "UTC"),
                              datetime <  as.POSIXct("2012-07-30", tz = "UTC"))
p_sub <- suppressWarnings(gg_subseries(window8w %>% as_tsibble(index = datetime), cnt)) +
  labs(title = "gg_subseries: one panel per hour of the day (eight weeks in June-July 2012)",
       subtitle = "the 08:00 and 17:00-18:00 commuting peaks appear only on working days; the horizontal line is the mean of each hour",
       y = "cnt") +
  theme_ev(8) +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))
save_fig(p_sub, "F08_gg_subseries", width = 13, height = 6.4)

## F09 : classical decomposition drawn with ggfortify
class_win <- hourly %>% filter(datetime >= as.POSIXct("2012-12-01", tz = "UTC"),
                               datetime <  as.POSIXct("2012-12-29", tz = "UTC"))
ts_win <- ts(as.numeric(class_win$cnt), start = 1, frequency = 24)
dec <- decompose(ts_win, type = "additive")
p_class <- autoplot(dec) +
  labs(title = "Classical additive decomposition (stats::decompose, drawn with ggfortify)",
       subtitle = "four weeks of December 2012, period 24; the trend is a centred 24-hour moving average",
       x = "hour index within the window") +
  theme_ev(10)
save_fig(p_class, "F09_classical_decomposition", width = 10, height = 6)

## ggfortify on the plain ts object (cross-check that the tsibble is what we think)
p_ts <- autoplot(ts(as.numeric(hourly$cnt), frequency = 24), main = NULL) +
  labs(title = "The hourly series as a base-R ts object, drawn with ggfortify::autoplot",
       subtitle = sprintf("%d observations, frequency 24", nrow(hourly)),
       x = "hour index", y = "cnt") +
  theme_ev(10)
save_fig(p_ts, "F09b_ggfortify_ts", width = 10, height = 4)

saveRDS(list(fit_stl = fit_stl, cmp = cmp, cmp_auto = cmp_auto, strength = strength,
             T05 = T05, T05b = T05b, T06 = T06, T06b = T06b, weekly_offset = weekly_offset),
        "vibe_work_ts/data/decomposition.rds")

cat("\nFigures F05-F09 written to", path_fig, "\n")
cat("\n=================== 02 DECOMPOSITION COMPLETE ====================\n")
