###############################################################################
## 01_data_prep.R -- load hour.csv, build a regular hourly tsibble, describe it
## Output : T01 (gaps), T02 (descriptives), T03 (hourly profile), T04 (heatmap)
##          F01-F04
###############################################################################

source("vibe_work_ts/R/00_setup.R")

cat("\n=================== 01 DATA PREPARATION (hourly) ==================\n\n")

## ---------------------------------------------------------------------------
## 1. read and check
## ---------------------------------------------------------------------------
raw <- readr::read_csv(path_raw, show_col_types = FALSE)
readr::write_csv(raw, path_data)

cat("raw dimensions :", nrow(raw), "rows x", ncol(raw), "columns\n")
cat("date range     :", format(min(raw$dteday)), "to", format(max(raw$dteday)), "\n")
cat("hour range     :", paste(range(raw$hr), collapse = " - "), "\n")
cat("missing values :", sum(is.na(raw)), "\n")
cat("cnt            : min", min(raw$cnt), " max", max(raw$cnt),
    " mean", round(mean(raw$cnt), 2), "\n")
cat("casual + registered == cnt ?", max(abs(raw$cnt - raw$casual - raw$registered)) == 0, "\n")
cat("hours are in chronological order:", all(diff(as.numeric(
  as.POSIXct(paste(raw$dteday, sprintf("%02d:00:00", raw$hr)), tz = "UTC"))) > 0), "\n\n")

## ---------------------------------------------------------------------------
## 2. build the regular hourly tsibble
##    The file stores local clock hours (0-23) for every day; a naive UTC index
##    is therefore used so that no day is shortened or lengthened by daylight
##    saving time -- the index is a "clock time" axis, which is what the
##    provider aggregated on.
## ---------------------------------------------------------------------------
hourly_raw <- raw %>%
  mutate(datetime = as.POSIXct(paste(dteday, sprintf("%02d:00:00", hr)), tz = "UTC")) %>%
  select(datetime, cnt, casual, registered, season, yr, mnth, hr, holiday, weekday,
         workingday, weathersit, temp, atemp, hum, windspeed) %>%
  as_tsibble(index = datetime)

cat("--- 2.1 calendar completeness ---\n")
full_seq <- seq(min(hourly_raw$datetime), max(hourly_raw$datetime), by = "hour")
missing  <- setdiff(full_seq, hourly_raw$datetime)
cat("hours in the calendar span :", length(full_seq), "\n")
cat("hours actually observed    :", nrow(hourly_raw), "\n")
cat("missing hours              :", length(missing),
    sprintf("(%.2f%% of the span)\n", 100 * length(missing) / length(full_seq)))

miss_tbl <- tibble(datetime = missing) %>%
  mutate(date = as.Date(datetime), month = format(datetime, "%Y-%m"),
         hour = as.integer(format(datetime, "%H")))

T01 <- tibble(
  item = c("calendar span (hours)", "hours observed", "hours missing",
           "share missing", "longest single gap (hours)",
           "missing hours 00:00-05:59", "days with at least one missing hour"),
  value = c(length(full_seq), nrow(hourly_raw), length(missing),
            sprintf("%.2f%%", 100 * length(missing) / length(full_seq)),
            max(as.numeric(diff(sort(hourly_raw$datetime)), units = "hours")),
            sum(miss_tbl$hour < 6),
            n_distinct(miss_tbl$date))
)
save_tab(T01, "T01_completeness")
save_md(T01, "T01_completeness", caption = "Table T01 - Completeness of the hourly series")
cat("\n---- Table T01: completeness ----\n"); print(as.data.frame(T01))

cat("\nmissing hours by year-month:\n")
print(as.data.frame(count(miss_tbl, month)))
cat("\nmissing hours by hour of the day:\n")
print(as.data.frame(count(miss_tbl, hour)))

## ---------------------------------------------------------------------------
## 3. a complete, regular hourly tsibble
##    * the seven calendar variables are deterministic functions of the
##      timestamp, so they are recomputed from the date (and verified against
##      the file);
##    * the 165 missing hours of the continuous variables are filled by linear
##      interpolation between the neighbouring observations (0.94% of the rows).
## ---------------------------------------------------------------------------
holiday_dates <- hourly_raw %>% as_tibble() %>%
  mutate(date = as.Date(datetime)) %>%
  filter(holiday == 1) %>% distinct(date) %>% pull(date)
cat("number of distinct holiday dates in the file:", length(holiday_dates), "\n")

calendar <- tibble(datetime = seq(min(hourly_raw$datetime), max(hourly_raw$datetime), by = "hour")) %>%
  mutate(
    date    = as.Date(datetime),
    hour    = as.integer(format(datetime, "%H")),
    dow     = as.integer(format(datetime, "%w")),   # 0 = Sunday, as in the file
    mnth_d  = as.integer(format(datetime, "%m")),
    yr_d    = as.integer(format(datetime, "%Y")),
    day     = as.integer(format(datetime, "%d")),
    holiday_d    = as.integer(date %in% holiday_dates),
    weekday_d    = dow,                             # the file uses 0 = Sunday too
    workingday_d = as.integer(dow %in% 1:5 & !(date %in% holiday_dates)),
    ## The file's `season` is not the calendar quarter: cross-tabulating month
    ## against season shows 22 days of December in level 1 and 22 days of March
    ## in level 2, i.e. the four levels are the astronomical quarters
    ##   1 = 21 Dec - 20 Mar (winter), 2 = 21 Mar - 20 Jun (spring),
    ##   3 = 21 Jun - 20 Sep (summer), 4 = 21 Sep - 20 Dec (autumn).
    ## This is confirmed by the data (mean temperature by level: 12.2, 22.3,
    ## 29.0 and 17.3 degC), and it means that the readme's labels
    ## "1 spring, 2 summer, 3 fall, 4 winter" are shifted by one level.
    md = mnth_d * 100 + day,
    season_d = case_when(md >= 1221 | md <= 320 ~ 1L,
                         md <= 620            ~ 2L,
                         md <= 920            ~ 3L,
                         TRUE                 ~ 4L)
  )

obs <- hourly_raw %>% as_tibble() %>%
  select(-weekday, -workingday, -holiday, -season, -yr, -mnth)
## the file's own season/yl/mnth values are kept wherever they exist (season is
## a descriptive variable here, never a model input); the few hours that belong
## to a day with no observation at all fall back on the derived value
season_lookup <- hourly_raw %>% as_tibble() %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(season_file = first(season), yr_file = first(yr), mnth_file = first(mnth),
            .groups = "drop")

hourly <- calendar %>%
  left_join(obs, by = "datetime") %>%
  left_join(season_lookup, by = "date") %>%
  mutate(
    filled = as.integer(is.na(cnt)),
    across(c(cnt, casual, registered, temp, atemp, hum, windspeed),
           ~ approx(as.numeric(datetime), .x, xout = as.numeric(datetime), rule = 1)$y),
    cnt = as.double(cnt), casual = as.double(casual), registered = as.double(registered),
    season = coalesce(season_file, season_d),
    yr = coalesce(yr_file, yr_d - 2011L),
    mnth = coalesce(mnth_file, mnth_d),
    holiday = holiday_d, weekday = weekday_d, workingday = workingday_d,
    year = yr_d, month = mnth_d,
    how = factor(paste0(dow, "_", sprintf("%02d", hour)),
                 levels = as.vector(t(outer(0:6, sprintf("%02d", 0:23), paste, sep = "_")))),
    logcnt = log(cnt)
  ) %>%
  select(datetime, cnt, casual, registered, season, yr, mnth, hour, holiday, weekday,
         workingday, weathersit, temp, atemp, hum, windspeed, date, dow, month, year,
         how, logcnt, filled) %>%
  as_tsibble(index = datetime)

## verification: the recomputed calendar variables must agree with the file
chk <- hourly %>% as_tibble() %>% filter(filled == 0) %>% select(datetime, holiday, weekday, workingday)
raw_chk <- hourly_raw %>% as_tibble() %>% select(datetime, holiday, weekday, workingday)
cmp_chk <- chk %>% left_join(raw_chk, by = "datetime", suffix = c("_new", "_old"))
mismatch <- sapply(c("holiday", "weekday", "workingday"),
                   function(v) sum(cmp_chk[[paste0(v, "_new")]] != cmp_chk[[paste0(v, "_old")]]))
cat("mismatches between the recomputed calendar and the file:",
    paste(names(mismatch), mismatch, sep = "=", collapse = ", "), "\n")
cat("the astronomical-season rule reproduces the file's season on",
    round(100 * mean(calendar$season_d == hourly$season[match(calendar$datetime, hourly$datetime)]), 1),
    "% of the hours (the remaining",
    sum(calendar$season_d != hourly$season[match(calendar$datetime, hourly$datetime)]),
    "hours are the equinox / solstice dates, which move by one day between 2011 and 2012)\n")
cat("hours in the frame:", nrow(hourly), "| observed:", sum(hourly$filled == 0),
    "| interpolated:", sum(hourly$filled == 1), "\n")
cat("any remaining missing value in the modelling columns:",
    any(is.na(hourly %>% as_tibble() %>%
                select(cnt, holiday, weekday, workingday, how, logcnt))), "\n")
cat("weathersit has", sum(is.na(hourly$weathersit)), "missing hours (not used by the models)\n\n")

## the temperature evidence for the season coding, reported later in the text
season_temp <- hourly %>% as_tibble() %>%
  group_by(season) %>%
  summarise(mean_temp_C = mean(temp, na.rm = TRUE) * 41,
            mean_cnt = mean(cnt), .groups = "drop") %>%
  mutate(across(c(mean_temp_C, mean_cnt), ~ round(.x, 1)))
cat("mean temperature and demand by season level:\n"); print(as.data.frame(season_temp))

codebook <- tribble(
  ~variable,     ~description,                                                          ~type,        ~role,
  "datetime",    "date + local clock hour, 2011-01-01 00:00 .. 2012-12-31 23:00",        "hourly index", "time index",
  "cnt",         "total rentals in that hour (TARGET), 1 .. 977",                        "count",      "response",
  "casual",      "rentals by occasional users in that hour",                             "count",      "descriptive only (part of the target)",
  "registered",  "rentals by registered users in that hour",                             "count",      "descriptive only (part of the target)",
  "season",      "astronomical quarter: 1 = 21 Dec-20 Mar (winter), 2 = 21 Mar-20 Jun (spring), 3 = 21 Jun-20 Sep (summer), 4 = 21 Sep-20 Dec (autumn) -- see the note in Section 1.2", "categorical", "descriptive only",
  "yr",          "0 = 2011, 1 = 2012",                                                   "binary",     "descriptive",
  "mnth",        "month 1..12",                                                          "categorical", "descriptive",
  "hr",          "hour of the day 0..23",                                                "cyclical",   "daily seasonality",
  "holiday",     "federal holiday (0/1)",                                                "binary",     "candidate covariate",
  "weekday",     "day of week 0 = Sunday .. 6 = Saturday",                               "categorical", "weekly seasonality",
  "workingday",  "1 if neither weekend nor holiday",                                     "binary",     "profile split (exploratory)",
  "weathersit",  "1 clear .. 4 heavy rain/snow/fog (level 4 never occurs)",             "ordinal",    "descriptive",
  "temp",        "normalised temperature (degC / 41)",                                   "continuous", "descriptive",
  "atemp",       "normalised feeling temperature (degC / 50)",                           "continuous", "descriptive",
  "hum",         "normalised humidity (/100)",                                           "continuous", "descriptive",
  "windspeed",   "normalised wind speed (/67)",                                          "continuous", "descriptive",
  "filled",      "1 if the hour had to be interpolated (165 hours), 0 if observed",        "binary",     "data-quality flag"
)
save_tab(codebook, "T00_codebook")
save_md(codebook, "T00_codebook", caption = "Table T00 - The 17 variables of hour.csv and their role in this study")

## ---------------------------------------------------------------------------
## 4. descriptive statistics
## ---------------------------------------------------------------------------
T02 <- hourly_raw %>%
  as_tibble() %>%
  select(cnt, casual, registered, temp, atemp, hum, windspeed) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(n = n(), mean = mean(value), sd = sd(value), min = min(value),
            q1 = quantile(value, .25), median = median(value),
            q3 = quantile(value, .75), max = max(value),
            skewness = mean((value - mean(value))^3) / sd(value)^3, .groups = "drop") %>%
  mutate(across(c(mean, sd, min, q1, median, q3, max, skewness), ~ round(.x, 3)))
save_tab(T02, "T02_descriptives")
save_md(T02, "T02_descriptives", digits = 3, caption = "Table T02 - Descriptive statistics of the hourly variables (n = 17,379 observed hours)")
cat("\n---- Table T02: descriptives ----\n"); print(as.data.frame(T02), digits = 3)

cat("\ncategorical levels:\n")
for (v in c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit")) {
  cat("  ", v, ":", paste(names(table(raw[[v]])), table(raw[[v]])), "\n", sep = " ")
}

## ---------------------------------------------------------------------------
## 5. the hourly profile (T03) and the hour x weekday matrix (T04)
## ---------------------------------------------------------------------------
T03 <- hourly_raw %>%
  as_tibble() %>%
  mutate(wd = if_else(workingday == 1, "work", "nonwork")) %>%
  group_by(hr, wd) %>%
  summarise(mean_cnt = mean(cnt), median_cnt = median(cnt), .groups = "drop") %>%
  pivot_wider(id_cols = hr, names_from = wd, values_from = c(mean_cnt, median_cnt)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
names(T03) <- c("hr", "mean_nonworking", "mean_working", "median_nonworking", "median_working")
save_tab(T03, "T03_hourly_profile")
save_md(T03, "T03_hourly_profile", digits = 1, caption = "Table T03 - Mean number of rentals by hour of the day, separately for working and non-working days")
cat("\n---- Table T03: hourly profile ----\n"); print(as.data.frame(T03), digits = 1)

T04 <- hourly_raw %>%
  as_tibble() %>%
  group_by(wd = weekday, hr) %>%
  summarise(mean_cnt = mean(cnt), .groups = "drop") %>%
  mutate(wd = factor(wd, levels = 0:6, labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  mutate(mean_cnt = round(mean_cnt, 1))
save_tab(T04, "T04_week_hour_matrix")
save_md(T04, "T04_week_hour_matrix", digits = 1, caption = "Table T04 - Mean rentals by day of week and hour of day (the 7 x 24 profile matrix)")

## ---------------------------------------------------------------------------
## 6. figures
## ---------------------------------------------------------------------------
## F01 : overview -- daily totals over two years + a three-week hourly zoom
daily <- hourly %>% as_tibble() %>% group_by(date) %>%
  summarise(total = sum(cnt), .groups = "drop")

p_a <- ggplot(daily, aes(date, total)) +
  geom_line(colour = pal_main[2], linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, colour = pal_main[5], linewidth = 0.8) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  labs(title = "(a) Daily totals (sum of the 24 hourly values)",
       subtitle = "Annual cycle + growth of the system", x = NULL, y = "rentals per day") +
  theme_ev(10)

zoom <- hourly %>% filter(datetime >= as.POSIXct("2012-09-03", tz = "UTC"),
                          datetime <  as.POSIXct("2012-09-24", tz = "UTC"))
p_b <- ggplot(zoom, aes(datetime, cnt)) +
  geom_line(colour = pal_main[1], linewidth = 0.45) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%d %b") +
  labs(title = "(b) Three weeks of hourly data (3 - 23 September 2012)",
       subtitle = "Working-day double peak vs weekend single hump", x = NULL, y = "rentals per hour") +
  theme_ev(10)
save_fig(p_a / p_b, "F01_series_overview", width = 10, height = 6.6)

## F02 : hourly profile, working vs non-working day
p_prof <- hourly_raw %>%
  as_tibble() %>%
  mutate(wd = factor(workingday, levels = c(0, 1), labels = c("Non-working day", "Working day"))) %>%
  group_by(hr, wd) %>%
  summarise(mean_cnt = mean(cnt), se = sd(cnt) / sqrt(n()), .groups = "drop") %>%
  ggplot(aes(hr, mean_cnt, colour = wd, fill = wd)) +
  geom_ribbon(aes(ymin = mean_cnt - 1.96 * se, ymax = mean_cnt + 1.96 * se), alpha = .18, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.7) +
  scale_colour_manual(values = pal_main[c(5, 1)]) +
  scale_fill_manual(values = pal_main[c(5, 1)]) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(title = "The hourly shape of demand depends on the type of day",
       subtitle = "Mean rentals per hour (95% CI); working days peak at 08:00 and 17:00-18:00, non-working days at midday",
       x = "hour of the day", y = "mean rentals per hour", colour = NULL, fill = NULL) +
  theme_ev()
save_fig(p_prof, "F02_hourly_profile", width = 9.5, height = 5)

## F03 : hour x weekday heatmap
p_heat <- T04 %>%
  mutate(wd = factor(wd, levels = rev(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))) %>%
  ggplot(aes(hr, wd, fill = mean_cnt)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#f4f8fa", pal_main[3], pal_main[2], pal_main[1], pal_main[4]),
                       breaks = c(100, 200, 300, 400, 500), name = "mean\nrentals",
                       guide = guide_colourbar(barwidth = 11, barheight = 1)) +
  scale_x_continuous(breaks = seq(0, 23, 2), expand = c(0, 0)) +
  labs(title = "The 7 x 24 profile of demand",
       subtitle = "Commuting peaks on weekdays, a single broad afternoon hump at the weekend",
       x = "hour of the day", y = NULL) +
  theme_ev() +
  theme(panel.grid = element_blank(), legend.position = "bottom")
save_fig(p_heat, "F03_week_hour_heatmap", width = 9.5, height = 4.6)

## F04 : seasonality at the three time scales
p_m <- hourly %>% as_tibble() %>% group_by(month, year) %>%
  summarise(mean_cnt = mean(cnt), .groups = "drop") %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(month, mean_cnt, colour = year)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = pal_main[c(3, 1)]) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Monthly mean rentals per hour", x = NULL, y = "rentals/hour", colour = "year") +
  theme_ev(10)

p_w <- hourly %>% as_tibble() %>%
  mutate(wd = factor(as.POSIXlt(date)$wday, levels = 0:6,
                     labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  group_by(wd) %>% summarise(mean_cnt = mean(cnt), se = sd(cnt) / sqrt(n()), .groups = "drop") %>%
  ggplot(aes(wd, mean_cnt)) +
  geom_col(fill = pal_main[2], width = .7) +
  geom_errorbar(aes(ymin = mean_cnt - 1.96 * se, ymax = mean_cnt + 1.96 * se), width = .2) +
  labs(title = "Weekly profile", x = NULL, y = "rentals/hour") +
  theme_ev(10)

p_h <- hourly %>% as_tibble() %>% group_by(hour) %>%
  summarise(mean_cnt = mean(cnt), .groups = "drop") %>%
  ggplot(aes(hour, mean_cnt)) +
  geom_col(fill = pal_main[1], width = .8) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Daily profile", x = "hour", y = "rentals/hour") +
  theme_ev(10)

save_fig((p_m | p_w | p_h) + plot_annotation(
  title = "Three seasonal scales in the hourly series",
  caption = "monthly, weekly and daily means of the same series"),
  "F04_seasonal_scales", width = 13, height = 4.4)

saveRDS(list(hourly = hourly, daily = daily, T02 = T02, T03 = T03, T04 = T04,
             missing_tbl = miss_tbl, codebook = codebook),
        "vibe_work_ts/data/hourly_ts.rds")

cat("\nFigures F01-F04 written to", path_fig, "\n")
cat("\n==================== 01 DATA PREP COMPLETE =======================\n")
