###############################################################################
## 01_data_prep.R -- load, clean and describe the daily Capital Bikeshare data
## Output : vibe_work/output/tables/T01_codebook.csv, T02_descriptives.csv,
##          T03_correlations.csv
##          vibe_work/output/figures/F01_cnt_series.png, F02_distributions.png,
##          F03_corr_heatmap.png, F04_temp_atemp.png
##          vibe_work/data/day_clean.rds  (processed data for later scripts)
###############################################################################

source("vibe_work/R/00_setup.R")

cat("\n======================= 01 DATA PREPARATION =======================\n\n")

## ---- 1. read ---------------------------------------------------------------
df_raw <- readr::read_csv(path_raw, show_col_types = FALSE)
readr::write_csv(df_raw, path_data)                     # working copy

cat("Raw dimensions :", nrow(df_raw), "rows x", ncol(df_raw), "columns\n")
cat("Date range     :", format(min(df_raw$dteday)), "to", format(max(df_raw$dteday)), "\n")
cat("Missing values :", sum(is.na(df_raw)), "\n")
cat("Duplicated rows:", sum(duplicated(df_raw)), "\n")

## is the recording complete (no gaps in dates)?
all_dates <- seq(min(as.Date(df_raw$dteday)), max(as.Date(df_raw$dteday)), by = "day")
cat("Calendar days expected:", length(all_dates),
    "| observed:", nrow(df_raw), "| gaps:", length(setdiff(all_dates, as.Date(df_raw$dteday))), "\n\n")

## target decomposition: cnt must equal casual + registered
cat("max |cnt - (casual + registered)| =",
    max(abs(df_raw$cnt - (df_raw$casual + df_raw$registered))), "\n\n")

## ---- 2. clean --------------------------------------------------------------
##  * instant  : record index, carries no explanatory information -> removed
##  * casual / registered : they are the two components of the target
##    (cnt = casual + registered).  Using them would be target leakage.
##  * dteday   : kept in a separate object for the time-series part only.
target <- "cnt"

df <- df_raw %>%
  mutate(
    dteday = as.Date(dteday),
    across(c(season, yr, mnth, holiday, weekday, workingday, weathersit), as.factor)
  ) %>%
  select(-instant, -casual, -registered)

## Give every categorical variable self-explanatory factor labels.  They are
## written back into the canonical columns so that every model output, table
## and figure of the report is directly readable (e.g. "season [Winter]").
df <- df %>%
  mutate(
    season     = factor(season, levels = 1:4,
                        labels = c("Spring", "Summer", "Fall", "Winter")),
    yr         = factor(yr, levels = 0:1, labels = c("2011", "2012")),
    mnth       = factor(mnth, levels = 1:12, labels = month.abb),
    holiday    = factor(holiday, levels = 0:1, labels = c("No", "Yes")),
    weekday    = factor(weekday, levels = 0:6,
                        labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
    workingday = factor(workingday, levels = 0:1,
                        labels = c("Non-working", "Working")),
    weathersit = factor(weathersit, levels = 1:4,
                        labels = c("Clear", "Mist", "RainSnow", "Heavy"))
  )

## Which categories actually occur?  weathersit = 4 (heavy rain / snow / fog)
## never occurs in the 731 days, so the empty level is dropped (otherwise lm()
## would have to carry an all-zero dummy and report an aliased coefficient).
cat("occurrence of each categorical level:\n")
for (v in c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit")) {
  cat("  ", v, ":", paste(names(table(df_raw[[v]])), table(df_raw[[v]])),
      sep = " ", collapse = " | ")
  cat("\n")
}
df <- droplevels(df)

## physical (de-normalised) versions, using the divisors documented on the
## dataset web page: temp/41, atemp/50, hum/100, windspeed/67
df <- df %>%
  mutate(
    temp_C     = temp * 41,
    atemp_C    = atemp * 50,
    hum_pct    = hum * 100,
    wind_kmh   = windspeed * 67
  )

saveRDS(list(full = df, target = target), "vibe_work/data/day_clean.rds")

cat("Analysis data  :", nrow(df), "rows x", ncol(df), "columns after cleaning\n")
cat("Predictors kept:", paste(setdiff(names(df_raw)[-1], c("casual", "registered", "cnt")),
                             collapse = ", "), "\n\n")

## ---- 3. codebook (T01) -----------------------------------------------------
codebook <- tribble(
  ~variable,     ~description,                                                  ~type,       ~role,
  "instant",     "Record index (1..731)",                                       "ID",        "Dropped - identifies the row only",
  "dteday",      "Date (2011-01-01 .. 2012-12-31)",                             "date",      "Used for the time-series analysis only",
  "season",      "Season: 1 spring, 2 summer, 3 fall, 4 winter",                "categorical", "Predictor",
  "yr",          "Year: 0 = 2011, 1 = 2012",                                    "categorical", "Predictor (trend / level shift)",
  "mnth",        "Month 1..12",                                                 "categorical", "Predictor (collinear with season)",
  "holiday",     "Day is a US federal holiday (1) or not (0)",                  "binary",    "Predictor",
  "weekday",     "Day of week, 0 = Sunday .. 6 = Saturday",                     "categorical", "Predictor",
  "workingday",  "1 if neither weekend nor holiday, else 0",                    "binary",    "Predictor (exact function of weekday x holiday)",
  "weathersit",  "1 clear/partly cloudy, 2 mist/cloudy, 3 light rain or snow, 4 heavy rain/snow/fog", "ordinal", "Predictor",
  "temp",        "Normalised temperature, Celsius divided by 41",               "continuous", "Predictor",
  "atemp",       "Normalised feeling temperature, Celsius divided by 50",       "continuous", "Predictor (collinear with temp, r > 0.99)",
  "hum",         "Normalised humidity, divided by 100",                         "continuous", "Predictor",
  "windspeed",   "Normalised wind speed, divided by 67",                        "continuous", "Predictor",
  "casual",      "Count of casual users",                                       "count",     "Dropped - part of the target (leakage)",
  "registered",  "Count of registered users",                                   "count",     "Dropped - part of the target (leakage)",
  "cnt",         "Total rentals = casual + registered  (TARGET)",               "count",     "Response variable"
)
save_tab(codebook, "T01_codebook")
save_md(codebook, "T01_codebook", caption = "Table T01 - Variables in day.csv and their role in the analysis")
cat("Table T01 (codebook) written.\n")

## ---- 4. descriptive statistics (T02) --------------------------------------
num_vars <- c("cnt", "casual", "registered", "temp", "atemp", "hum", "windspeed")

desc_num <- df_raw %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n        = sum(!is.na(value)),
    mean     = mean(value),
    sd       = sd(value),
    min      = min(value),
    q1       = quantile(value, 0.25),
    median   = median(value),
    q3       = quantile(value, 0.75),
    max      = max(value),
    skewness = mean((value - mean(value))^3) / sd(value)^3,
    .groups  = "drop"
  ) %>%
  mutate(across(c(mean, sd, min, q1, median, q3, max, skewness), ~ round(.x, 3)))

desc_cat <- tibble(
  variable = c("season", "mnth", "weekday", "weathersit", "yr", "holiday", "workingday"),
  n_levels = c(4, 12, 7, 4, 2, 2, 2),
  levels   = c("1-4", "1-12", "0-6", "1-4", "0/1", "0/1", "0/1")
)

T02_cat <- desc_cat
save_tab(T02_cat, "T02b_categories")

T02 <- desc_num %>% mutate(kind = "numeric", .after = variable)
save_tab(T02, "T02_descriptives")
save_md(T02, "T02_descriptives", digits = 3, caption = "Table T02a - Descriptive statistics of the numeric variables in day.csv")
save_md(T02_cat, "T02b_categories", caption = "Table T02b - Categorical variables and their coding")
cat("\n---- Table T02a: descriptive statistics ----\n")
print(as.data.frame(T02), digits = 3)

## ---- 5. correlations among the predictors (T03) ---------------------------
cor_res <- df %>%
  select(temp, atemp, hum, windspeed) %>%
  correlation()

T03 <- as.data.frame(cor_res) %>%
  select(Parameter1, Parameter2, r, CI_low, CI_high, t, p) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
save_tab(T03, "T03_correlations")
T03_md <- T03 %>% mutate(p = formatC(p, format = "e", digits = 2))
save_md(T03_md, "T03_correlations", caption = "Table T03 - Pearson correlation matrix of the continuous predictors (all 731 days)")

cat("\n---- Table T03: correlations among continuous predictors ----\n")
print(as.data.frame(T03), digits = 3)

## ---- 6. EDA figures -------------------------------------------------------
## F01 : the target over time
p_series <- df %>%
  ggplot(aes(dteday, cnt)) +
  geom_line(colour = pal_main[2], linewidth = 0.45) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE,
              colour = pal_main[5], linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2012-01-01"), linetype = "dashed",
             colour = "grey50") +
  annotate("text", x = as.Date("2012-01-05"), y = max(df$cnt) * 0.97,
           label = "2012", hjust = 0, colour = "grey40", size = 3.5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  labs(title = "Total daily bike rentals, Washington D.C. (2011-2012)",
       subtitle = "Red line = local (loess) trend; the level rises visibly in 2012",
       x = NULL, y = "Total rentals per day (cnt)") +
  theme_ev()
save_fig(p_series, "F01_cnt_series", width = 10, height = 4.8)

## F02 : distribution of the target and of the main weather variables
p_a <- df %>%
  ggplot(aes(cnt)) +
  geom_histogram(bins = 40, fill = pal_main[2], colour = "white") +
  labs(title = "(a) Daily total rentals", x = "cnt", y = "Days") + theme_ev(10)
p_b <- df %>%
  ggplot(aes(temp_C)) +
  geom_histogram(bins = 30, fill = pal_main[1], colour = "white") +
  labs(title = "(b) Mean temperature", x = "Temperature (C)", y = NULL) + theme_ev(10)
p_c <- df %>%
  ggplot(aes(cnt, fill = weathersit)) +
  geom_density(alpha = 0.45, colour = NA, adjust = 0.8) +
  scale_fill_manual(values = pal_main[1:4]) +
  labs(title = "(c) Rentals by weather situation", x = "cnt", y = NULL, fill = NULL) +
  theme_ev(10)
p_d <- df %>%
  ggplot(aes(temp_C, cnt)) +
  geom_point(alpha = 0.45, colour = pal_main[1], size = 1.1) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), colour = pal_main[5],
              fill = pal_main[5], alpha = 0.2) +
  labs(title = "(d) Rentals vs temperature", x = "Temperature (C)", y = "cnt") +
  theme_ev(10)
save_fig((p_a | p_b) / (p_c | p_d) +
           plot_annotation(title = "Exploratory view of the daily data",
                           caption = "Source: day.csv, Capital Bikeshare, Washington D.C."),
         "F02_distributions", width = 10.5, height = 7.6)

## F03 : correlation heatmap (built by hand so that no extra package is needed)
cor_long <- df %>%
  select(temp, atemp, hum, windspeed, cnt) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  mutate(var1 = factor(var1, levels = c("cnt", "temp", "atemp", "hum", "windspeed")),
         var2 = factor(var2, levels = rev(c("cnt", "temp", "atemp", "hum", "windspeed"))))

p_corr <- ggplot(cor_long, aes(var1, var2, fill = r)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", r), colour = abs(r) > 0.6), size = 3.4) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"), guide = "none") +
  scale_fill_gradient2(low = pal_main[5], mid = "white", high = pal_main[1],
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  coord_equal() +
  labs(title = "Correlation structure of the continuous variables",
       subtitle = "temp and atemp are practically the same variable (r = 0.99)",
       x = NULL, y = NULL) +
  theme_ev()
save_fig(p_corr, "F03_corr_heatmap", width = 7.6, height = 5.6)

## F04 : temp vs atemp, the collinearity problem in a single picture
r_ta <- cor(df$temp, df$atemp)
p_ta <- ggplot(df, aes(temp_C, atemp_C)) +
  geom_point(alpha = 0.5, colour = pal_main[1], size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  labs(title = "Normalised temperature vs normalised feeling temperature",
       subtitle = sprintf("r = %.3f on the raw scale; the two variables carry the same information", r_ta),
       x = "temp x 41  (deg C)", y = "atemp x 50  (deg C)") +
  theme_ev()
save_fig(p_ta, "F04_temp_atemp", width = 7.5, height = 5)

cat("\nFigure F01-F04 written to", path_fig, "\n")
cat("\n==================== 01 DATA PREP COMPLETE ========================\n")
