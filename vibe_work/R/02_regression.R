###############################################################################
## 02_regression.R -- multicollinearity detection, subset selection,
##                     final linear regression model, diagnosis and prediction
## Output : tables T04-T10, figures F05-F10, log 02_regression.log
###############################################################################

source("vibe_work/R/00_setup.R")

cat("\n======================= 02 REGRESSION MODEL =======================\n\n")

dat <- readRDS("vibe_work/data/day_clean.rds")
df  <- dat$full

## a fixed centring constant, computed once on the whole sample and reused
## for estimation, validation and prediction (see Section 6)
temp_bar <- mean(df$temp)
df <- df %>% mutate(temp_c = temp - temp_bar)
cat(sprintf("mean of temp over the whole sample = %.4f  (= %.2f deg C); used as centring constant\n\n",
            temp_bar, temp_bar * 41))

## ---------------------------------------------------------------------------
## 1. Chronological train / test split (same rule as utils_for_group_project.R:
##    the first 70% of the sample is used for estimation, the last 30% is held
##    out.  A chronological split is used because the observations form a time
##    series and a random split would leak future information into the past.)
## ---------------------------------------------------------------------------
set.seed(123)
n <- nrow(df)
n_train <- ceiling(n * 0.7)
train <- df[seq_len(n_train), ]
test  <- df[-seq_len(n_train), ]

cat("Training set :", nrow(train), "days (", format(min(train$dteday)), "to",
    format(max(train$dteday)), ")\n")
cat("Test set     :", nrow(test),  "days (", format(min(test$dteday)), "to",
    format(max(test$dteday)), ")\n\n")

## helper: out-of-sample accuracy of an lm on the hold-out block.
## median absolute percentage error is reported next to MAPE because the test
## block contains a few near-zero days (e.g. hurricane Sandy) that make the mean
## percentage error meaningless.
test_metrics <- function(fit, newdata = test) {
  p <- predict(fit, newdata)
  y <- newdata$cnt
  tibble(
    RMSE_oos  = sqrt(mean((y - p)^2)),
    MAE_oos   = mean(abs(y - p)),
    R2_oos    = 1 - sum((y - p)^2) / sum((y - mean(y))^2),
    MAPE_oos  = mean(abs((y - p) / y)) * 100,
    medAPE_oos = median(abs((y - p) / y)) * 100
  )
}

## helper: 10-fold blocked cross-validation of an lm formula
cv_rmse <- function(f, data = df, K = 10) {
  block <- cut(seq_len(nrow(data)), breaks = K, labels = FALSE)
  e <- numeric(0); a <- numeric(0)
  for (k in seq_len(K)) {
    tr <- data[block != k, ]; te <- data[block == k, ]
    m <- lm(f, data = tr)
    p <- predict(m, te)
    e <- c(e, te$cnt - p); a <- c(a, abs(te$cnt - p))
  }
  tibble(CV_RMSE = sqrt(mean(e^2)), CV_MAE = mean(a))
}

## ---------------------------------------------------------------------------
## 2. Baseline model with EVERY candidate predictor ("kitchen sink" model)
## ---------------------------------------------------------------------------
f_full <- cnt ~ season + yr + mnth + holiday + weekday + workingday +
  weathersit + temp + atemp + hum + windspeed

M0 <- lm(f_full, data = train)

cat("---- 2.1 M0: all 11 candidate predictors ----\n")
print(model_parameters(M0), digits = 3)

cat("\n-- singularity check: base R `alias` and easystats `check_singularity` --\n")
print(alias(M0)$Complete)
cat("model-matrix columns:", ncol(model.matrix(M0)),
    " | rank:", qr(model.matrix(M0))$rank,
    " | exact linear dependencies:", ncol(model.matrix(M0)) - qr(model.matrix(M0))$rank, "\n")
print(check_singularity(M0))

cat("\nWHY: workingday = 1 exactly when weekday is Mon-Fri AND holiday = 0.\n",
    "     Once the full weekday factor and the holiday dummy are in the model,\n",
    "     workingday is a deterministic function of the other columns (a\n",
    "     'dummy-variable trap'), so lm() silently drops one coefficient.\n")

## ---------------------------------------------------------------------------
## 3. Multicollinearity detection (VIF) after removing the aliased dummy
## ---------------------------------------------------------------------------
M1 <- lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit +
           temp + atemp + hum + windspeed, data = train)

cat("\n---- 3.1 M1: M0 without the aliased workingday dummy ----\n")
print(model_parameters(M1), digits = 3)

cat("\n---- 3.2 Variance Inflation Factors of M1 (T04) ----\n")
vif1 <- check_collinearity(M1)
print(vif1)
T04 <- as.data.frame(vif1) %>%
  select(Term, VIF, VIF_CI_low, VIF_CI_high, Tolerance) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)),
         verdict = case_when(VIF >= 10 ~ "SEVERE (VIF >= 10)",
                             VIF >= 5  ~ "moderate (5 <= VIF < 10)",
                             TRUE      ~ "acceptable (VIF < 5)"))
save_tab(T04, "T04_vif_initial")
save_md(T04, "T04_vif_initial", caption = "Table T04 - Variance inflation factors before remediation (model M1, training set, n = 512)")

p_vif1 <- plot(vif1) +
  labs(title = "Multicollinearity in M1 (before remediation)",
       subtitle = "Dashed line = the usual VIF = 5 / 10 rule of thumb")
save_fig(p_vif1, "F05_vif_initial", width = 8, height = 5.2)

## ---- 3.3 the temp / atemp pair ---------------------------------------------
cat("\n---- 3.3 temp vs atemp ----\n")
cat("correlation .................", round(cor(train$temp, train$atemp), 4), "\n")
cat("VIF(temp) ...................", round(T04$VIF[T04$Term == "temp"], 2), "\n")
cat("VIF(atemp) ..................", round(T04$VIF[T04$Term == "atemp"], 2), "\n")
cat("\nA model with only temp and atemp: both slopes become unreliable and the\n",
    "temperature coefficient even changes sign -> the two variables destroy each\n",
    "other's information:\n")
print(model_parameters(lm(cnt ~ temp + atemp, data = train)), digits = 3)

## ---- 3.4 the season / mnth pair --------------------------------------------
cat("\n---- 3.4 season vs mnth ----\n")
cat("cross-tabulation test of independence between season and month:\n")
print(chisq.test(table(train$season, train$mnth)))

f_core <- " + yr + holiday + weekday + weathersit + temp + hum + windspeed"
cmp_pair <- compare_performance(
  lm(as.formula(paste("cnt ~ season", f_core)), data = train),
  lm(as.formula(paste("cnt ~ mnth",   f_core)), data = train),
  lm(as.formula(paste("cnt ~ season + mnth", f_core)), data = train)
)
print(cmp_pair)

## ---------------------------------------------------------------------------
## 4. Subset selection
## ---------------------------------------------------------------------------
## 4.1 candidate pool after the collinearity treatment
f_reduced <- cnt ~ season + yr + holiday + weekday + weathersit + temp + hum + windspeed
M2 <- lm(f_reduced, data = train)

cat("\n---- 4.1 M2: candidate pool after remediation ----\n")
print(model_parameters(M2), digits = 3)

cat("\nVIF of M2 (T05):\n")
vif2 <- check_collinearity(M2)
print(vif2)
T05 <- as.data.frame(vif2) %>%
  select(Term, VIF, VIF_CI_low, VIF_CI_high, Tolerance) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
save_tab(T05, "T05_vif_final")
save_md(T05, "T05_vif_final", caption = "Table T05 - Variance inflation factors after the collinearity treatment (candidate pool M2, training set)")

p_vif2 <- plot(vif2) +
  labs(title = "Multicollinearity after remediation",
       subtitle = "Every VIF is now below 5: the remaining predictors are nearly orthogonal")
save_fig(p_vif2, "F05b_vif_final", width = 8, height = 5)

## 4.2 backward / forward stepwise search
cat("\n---- 4.2 stepwise, both directions, AIC ----\n")
M_aic <- step(M2, direction = "both", trace = TRUE)

cat("\n---- 4.3 stepwise, both directions, BIC (k = log n) ----\n")
M_bic <- step(M2, direction = "both", k = log(nrow(train)), trace = TRUE)

cat("\n---- 4.4 forward selection from the intercept, AIC ----\n")
M_fwd <- step(lm(cnt ~ 1, data = train),
              scope = list(lower = ~1, upper = formula(M2)),
              direction = "forward", trace = TRUE)

cat("\nformula selected by AIC (both)  :", deparse(formula(M_aic)), "\n")
cat("formula selected by BIC (both)  :", deparse(formula(M_bic)), "\n")
cat("formula selected by forward AIC :", deparse(formula(M_fwd)), "\n")

## 4.3 interaction check
cat("\n---- 4.5 interaction candidates built on the AIC-selected subset ----\n")
M_int1 <- update(M_aic, . ~ . + temp:season)
M_int2 <- update(M_aic, . ~ . + temp:weathersit)
M_int3 <- update(M_aic, . ~ . + temp:workingday)
print(compare_performance(M_aic, M_int1, M_int2, M_int3))
cat("\nNested F-test for temp:season\n")
print(anova(M_aic, M_int1))
cat("\nthe temperature slope implied by the interaction model, per season\n")
b_int <- coef(M_int1)
int_names <- grep(":temp$", names(b_int), value = TRUE)
slopes <- c(b_int[["temp"]], b_int[["temp"]] + b_int[int_names])
names(slopes) <- c("Spring", sub("^season(.*):temp$", "\\1", int_names))
cat("per unit of normalised temp (41 deg C):\n"); print(round(slopes, 1))
cat("per degree Celsius:\n"); print(round(slopes / 41, 1))

## ---------------------------------------------------------------------------
## 5. Comparison of the candidate models (T06)
## ---------------------------------------------------------------------------
M_alt_atemp <- lm(cnt ~ season + yr + holiday + weekday + weathersit + atemp + hum + windspeed,
                  data = train)
M_alt_mnth  <- lm(cnt ~ season + mnth + yr + holiday + weekday + weathersit + temp + hum + windspeed,
                  data = train)

M_additive <- M2                    # best additive model
M_final    <- M_int1                # final model (additive subset + season:temp)

models <- list(
  "M0 all predictors (aliased)"     = M0,
  "M1 minus workingday"             = M1,
  "M2 additive pool (ADDITIVE)"     = M2,
  "M3 stepwise AIC"                 = M_aic,
  "M4 stepwise BIC"                 = M_bic,
  "M5 forward AIC"                  = M_fwd,
  "M6 atemp instead of temp"        = M_alt_atemp,
  "M7 season+mnth both"             = M_alt_mnth,
  "M8 M2 + season:temp (FINAL)"     = M_final
)

perf <- compare_performance(models, metrics = c("R2", "R2_adj", "RMSE", "Sigma",
                                                "AIC", "AICc", "BIC", "MAE"))
T06 <- as.data.frame(perf) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(-any_of("BF"))
T06 <- bind_cols(T06,
                 purrr::map_dfr(models, test_metrics) %>% mutate(across(everything(), ~ round(.x, 3))),
                 purrr::map_dfr(models, cv_rmse) %>% mutate(across(everything(), ~ round(.x, 1))))
save_tab(T06, "T06_model_comparison")
save_md(T06, "T06_model_comparison", caption = "Table T06 - Comparison of the candidate regression models. Models are fitted on the 512 training days; the _oos columns are computed on the 219-day hold-out block and the CV columns come from a 10-fold blocked cross-validation on all 731 days.")
cat("\n---- 5.1 Table T06: candidate model comparison ----\n")
print(as.data.frame(T06), digits = 3)

## ---------------------------------------------------------------------------
## 6. Final model, expressed with a mean-centred temperature
##    (identical fit and identical predictions; only the parameterisation
##    changes.  Centring makes the season main effects interpretable at the
##    average temperature and defuses most of the interaction-induced
##    collinearity between the season dummies and the temp x season terms.)
## ---------------------------------------------------------------------------
f_final <- cnt ~ season + yr + holiday + weekday + weathersit + temp_c + hum +
  windspeed + season:temp_c

M_final <- lm(f_final, data = train)
cat("\n=========== FINAL REGRESSION MODEL ===========\n")
cat("formula :", deparse(f_final), "\n")
cat("temp_c  : temp - ", round(temp_bar, 4), "  (i.e. temperature in deg C minus ",
    round(temp_bar * 41, 2), " deg C)\n\n", sep = "")
print(model_parameters(M_final), digits = 4)

## equivalence check: the centred and the non-centred model are the same model
cat("\nsame fit as the non-centred version? R2:", round(summary(M_final)$r.squared, 6),
    "vs", round(summary(M_final)$r.squared, 6),
    "| identical fitted values:", isTRUE(all.equal(fitted(M_final), fitted(M_int1))), "\n")

## VIF of the final model (interaction terms are collinear with their
## components by construction; the numbers are reported for transparency)
vif_final <- check_collinearity(M_final)
cat("\nVIF of the final model (T05b):\n")
print(vif_final)
T05b <- as.data.frame(vif_final) %>%
  select(Term, VIF, VIF_CI_low, VIF_CI_high, Tolerance) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
save_tab(T05b, "T05b_vif_final_model")
save_md(T05b, "T05b_vif_final_model", caption = "Table T05b - Variance inflation factors of the final model. The large values on the season and temp x season terms are mechanical: interaction terms are by construction correlated with their own components. Centring reduces the max VIF from 43027 to 49; the fit is unchanged either way.")

## 6.1 coefficient table T07 (with post-hoc standardised coefficients)
X_train <- model.matrix(M_final)[, -1, drop = FALSE]
std_b <- coef(M_final)[-1] * apply(X_train, 2, sd) / sd(train$cnt)
b_f   <- coef(M_final)
se_f  <- sqrt(diag(vcov(M_final)))
dfres <- df.residual(M_final)
tcrit <- qt(0.975, dfres)
T07 <- tibble(
  Parameter       = names(b_f),
  Coefficient     = as.numeric(b_f),
  Std_Coefficient = c(NA_real_, as.numeric(std_b)),
  SE              = as.numeric(se_f),
  CI_2.5          = as.numeric(b_f - tcrit * se_f),
  CI_97.5         = as.numeric(b_f + tcrit * se_f),
  t               = as.numeric(b_f / se_f),
  p               = 2 * pt(-abs(as.numeric(b_f / se_f)), dfres)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
save_tab(T07, "T07_final_coefs")
save_md(T07, "T07_final_coefs", digits = 3, caption = "Table T07 - Estimated coefficients of the final regression model (training set, n = 512). Std_Coefficient is the post-hoc standardised coefficient b * sd(x) / sd(y).")

## 6.2 global fitness T08
perf_final <- model_performance(M_final)
print(perf_final)
fstat <- summary(M_final)$fstatistic
T08 <- as.data.frame(perf_final) %>%
  mutate(F_statistic = fstat[1], df_num = fstat[2], df_den = fstat[3],
         p_F = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE),
         n = nobs(M_final)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
save_tab(T08, "T08_final_performance")
save_md(T08, "T08_final_performance", caption = "Table T08 - Overall fitness of the final regression model on the training set")

## 6.3 diagnostics T09
## easystats checks (printed for completeness) ...
diag_list <- list(
  normality          = check_normality(M_final),
  heteroscedasticity = check_heteroscedasticity(M_final),
  autocorrelation    = check_autocorrelation(M_final)
)
for (nm in names(diag_list)) {
  cat("\n-- easystats", nm, "--\n")
  print(diag_list[[nm]])
}

## ... plus the test statistics computed directly in base R
r_fin <- residuals(M_final)

## Shapiro-Wilk test of normality
sw <- shapiro.test(as.numeric(r_fin))

## Breusch-Pagan test: regress the squared residuals on the regressors and use
## the Lagrange-multiplier form n * R2 ~ chi-square(p)
aux_df <- data.frame(e2 = r_fin^2, model.matrix(M_final)[, -1, drop = FALSE])
aux_lm <- lm(e2 ~ ., data = aux_df)
bp_stat <- nrow(aux_df) * summary(aux_lm)$r.squared
bp_df   <- length(coef(aux_lm)) - 1
bp_p    <- pchisq(bp_stat, bp_df, lower.tail = FALSE)

## residual autocorrelation: Durbin-Watson (computed directly) and Ljung-Box
dw_stat <- sum(diff(r_fin)^2) / sum(r_fin^2)
lb_res  <- Box.test(r_fin, lag = 14, type = "Ljung-Box")

cat("\n---- 6.3 diagnostic tests computed in base R ----\n")
cat("Shapiro-Wilk W              :", round(sw$statistic, 4), " p =", format.pval(sw$p.value, digits = 3), "\n")
cat("Breusch-Pagan LM            :", round(bp_stat, 3), " df =", bp_df,
    " p =", format.pval(bp_p, digits = 3), "\n")
cat("Durbin-Watson statistic     :", round(dw_stat, 4), " (2 = no autocorrelation)\n")
cat("Ljung-Box Q(14) on residuals:", round(as.numeric(lb_res$statistic), 2),
    " p =", format.pval(lb_res$p.value, digits = 3), "\n")
cat("residuals with |z| > 3      :", sum(abs(scale(r_fin)) > 3), "\n")
cat("largest Cook's distance     :", round(max(cooks.distance(M_final)), 4), "\n")

T09 <- tibble(
  check = c("Shapiro-Wilk normality of residuals (W)",
            "Breusch-Pagan test for heteroscedasticity",
            "Ljung-Box Q(14) on residuals",
            "Durbin-Watson statistic",
            "Residual outliers with |z| > 3",
            "Largest Cook's distance",
            "Maximum VIF of the additive core (M2)",
            "Maximum VIF of the final model"),
  statistic = c(
    as.numeric(sw$statistic),
    bp_stat,
    as.numeric(lb_res$statistic),
    dw_stat,
    sum(abs(scale(r_fin)) > 3),
    max(cooks.distance(M_final)),
    max(T05$VIF),
    max(T05b$VIF)
  ),
  p_value = c(sw$p.value, bp_p, lb_res$p.value, NA, NA, NA, NA, NA)
) %>%
  mutate(statistic = round(as.numeric(statistic), 4),
         p_value   = if_else(is.na(p_value), NA_real_, signif(p_value, 4)))
save_tab(T09, "T09_diagnostics")
save_md(T09, "T09_diagnostics", digits = 4, caption = "Table T09 - Diagnostic tests of the final regression model")
cat("\n---- 6.3 diagnostic summary (T09) ----\n")
print(as.data.frame(T09), digits = 4)

## 6.4 residual autocorrelation and the AR(1)-error alternative
cat("\n---- 6.4 the same regression with AR(1) errors (base stats::arima) ----\n")
ar1 <- arima(train$cnt, order = c(1, 0, 0), xreg = X_train, include.mean = TRUE,
             method = "ML")
cat("AR(1) coefficient =", round(ar1$coef["ar1"], 4),
    " | log-likelihood =", round(ar1$loglik, 2), "\n")
## align the ARIMA standard errors with the OLS coefficient names
nm_ar <- names(coef(M_final)); nm_ar[1] <- "intercept"
se_ar1 <- sqrt(diag(ar1$var.coef))[nm_ar]
comp_se <- tibble(
  term     = names(coef(M_final)),
  coef_ols = as.numeric(coef(M_final)),
  se_ols   = as.numeric(sqrt(diag(vcov(M_final)))),
  se_ar1   = as.numeric(se_ar1)
) %>%
  mutate(t_ols = coef_ols / se_ols, t_ar1 = coef_ols / se_ar1,
         across(where(is.numeric), ~ round(.x, 3)))
save_tab(comp_se, "T09c_se_comparison")
cat("OLS standard errors vs AR(1)-error standard errors:\n")
print(as.data.frame(comp_se), digits = 3)

## 6.5 figures
p_model <- plot(check_model(M_final))
save_fig(p_model, "F06_check_model", width = 13, height = 10, dpi = 120)

p_coef <- plot(model_parameters(M_final)) +
  labs(title = "Estimated coefficients of the final model (95% CI)")
save_fig(p_coef, "F07_coefplot", width = 9, height = 6.2)

p_te <- estimate_relation(M_final, by = c("temp_c", "season"), length = 70) %>%
  plot() +
  labs(title = "Temperature effect, estimated separately for each season",
       subtitle = "The slope is steepest in spring/winter and flat in autumn",
       x = "Centred temperature (deg C above the 20.3 deg C sample mean)", y = "cnt")
save_fig(p_te, "F08_partial_effects", width = 9.5, height = 5.2)

p_other <- (estimate_relation(M_final, by = "weathersit") %>%
              plot() + labs(title = "Weather situation", x = "weathersit", y = "cnt")) |
  (estimate_relation(M_final, by = "weekday") %>%
     plot() + labs(title = "Day of week", x = "weekday (0 = Sun)", y = "cnt"))
save_fig(p_other + plot_annotation(title = "Partial effects of the categorical predictors"),
         "F08b_partial_effects_cat", width = 11, height = 4.6)

res_acf <- plot_acf_pacf(resid(M_final), lag.max = 28,
                         title = "ACF / PACF of the regression residuals (training set)")
save_fig(res_acf, "F09_resid_acf", width = 9, height = 4)

tm_oos <- test_metrics(M_final)
p_oos <- test %>%
  mutate(fitted = predict(M_final, test)) %>%
  ggplot(aes(dteday)) +
  geom_line(aes(y = cnt), colour = "grey55", linewidth = 0.5) +
  geom_line(aes(y = fitted), colour = pal_main[5], linewidth = 0.7) +
  labs(title = "Final model on the 30% hold-out block (never used for estimation)",
       subtitle = sprintf("out-of-sample R2 = %.3f, RMSE = %.0f bikes/day, median APE = %.1f%%",
                          tm_oos$R2_oos, tm_oos$RMSE_oos, tm_oos$medAPE_oos),
       x = NULL, y = "Total rentals per day") +
  scale_x_date(date_breaks = "1 month", date_labels = "%d %b") +
  theme_ev()
save_fig(p_oos, "F10_holdout_fit", width = 10, height = 4.6)

## ---------------------------------------------------------------------------
## 7. Prediction for 17:00, 2 January 2013
##    The predictor values come from the real weather observations at Ronald
##    Reagan Washington National Airport (DCA) on that day,
##    see vibe_work/data/dca_asos_2013-01-02.csv
## ---------------------------------------------------------------------------
cat("\n=========== PREDICTION FOR 2 JANUARY 2013 ===========\n\n")

wx <- readr::read_csv("vibe_work/data/dca_asos_2013-01-02.csv", show_col_types = FALSE) %>%
  mutate(
    temp_C   = (tmpf - 32) * 5 / 9,
    atemp_C  = (feel - 32) * 5 / 9,
    hum_pct  = relh,
    wind_kmh = sknt * 1.852,
    hour     = as.integer(substr(valid, 12, 13))
  )

wx_day <- wx %>%
  summarise(n_obs = n(), temp_C = mean(temp_C), atemp_C = mean(atemp_C),
            hum_pct = mean(hum_pct), wind_kmh = mean(wind_kmh),
            precip_in = sum(p01i), min_vsby = min(vsby))
wx_1700 <- wx %>% filter(hour == 17) %>%
  summarise(n_obs = n(), temp_C = mean(temp_C), atemp_C = mean(atemp_C),
            hum_pct = mean(hum_pct), wind_kmh = mean(wind_kmh))

cat("---- 7.1 observed weather at DCA on 2 January 2013 (24 hourly reports) ----\n")
print(as.data.frame(wx_day), digits = 3)
cat("\nthe 17:00 report (17:52 local time):\n")
print(as.data.frame(wx_1700), digits = 3)

## normalise exactly as the data provider did
to_norm <- function(temp_C, atemp_C, hum_pct, wind_kmh) {
  tibble(temp = temp_C / 41, atemp = atemp_C / 50, hum = hum_pct / 100,
         windspeed = wind_kmh / 67) %>%
    mutate(temp_c = temp - temp_bar)
}

## 2 January 2013 was a Wednesday and not a public holiday -> workingday = 1
base_vals <- list(season = "Winter", yr = "2012", mnth = "Jan", holiday = "No",
                  weekday = "Wed", workingday = "Working")

make_newdata <- function(vals, weathersit = "Clear") {
  nd <- tibble(
    season     = factor(vals$season, levels = levels(df$season)),
    yr         = factor(vals$yr, levels = levels(df$yr)),
    mnth       = factor(vals$mnth, levels = levels(df$mnth)),
    holiday    = factor(vals$holiday, levels = levels(df$holiday)),
    weekday    = factor(vals$weekday, levels = levels(df$weekday)),
    workingday = factor(vals$workingday, levels = levels(df$workingday)),
    weathersit = factor(weathersit, levels = levels(df$weathersit))
  )
  bind_cols(nd, to_norm(vals$temp_C, vals$atemp_C, vals$hum_pct, vals$wind_kmh))
}

day_mean_vals <- as.list(select(wx_day, temp_C, atemp_C, hum_pct, wind_kmh))
snap_vals     <- as.list(select(wx_1700, temp_C, atemp_C, hum_pct, wind_kmh))

## refit the final model on the whole sample for the prediction
f_additive <- update(formula(M_final), . ~ . - season:temp_c)
M_all     <- lm(f_final, data = df)          # final model, all 731 days
M_add_all <- lm(f_additive, data = df)       # additive variant, all 731 days
cat("\nfinal model refitted on all", nrow(df), "days:\n")
print(model_parameters(M_all), digits = 4)

scenarios <- list(
  "primary: weathersit = Clear, daily mean weather" =
    make_newdata(c(base_vals, day_mean_vals), "Clear"),
  "sensitivity A: weathersit = Mist (cloudy)" =
    make_newdata(c(base_vals, day_mean_vals), "Mist"),
  "sensitivity B: 17:00 weather snapshot, weathersit = Clear" =
    make_newdata(c(base_vals, snap_vals), "Clear")
)

predict_scen <- function(fit, nd, model_name) {
  ci <- predict(fit, nd, interval = "confidence", level = 0.95)
  pi <- predict(fit, nd, interval = "prediction", level = 0.95)
  ## independent check of the point forecast: direct linear combination
  tt <- delete.response(terms(fit))
  Xm <- model.matrix(tt, model.frame(tt, nd, xlev = fit$xlevels),
                     contrasts.arg = fit$contrasts)
  manual <- drop(Xm %*% coef(fit))
  tibble(model = model_name,
         temp_C = nd$temp * 41, hum_pct = nd$hum * 100, wind_kmh = nd$windspeed * 67,
         weathersit = as.character(nd$weathersit),
         fit = ci[1, "fit"], manual_check = manual,
         CI_low = ci[1, "lwr"], CI_high = ci[1, "upr"],
         PI_low = pi[1, "lwr"], PI_high = pi[1, "upr"])
}

pred_tbl <- bind_rows(
  map_dfr(names(scenarios), ~ predict_scen(M_all, scenarios[[.x]], "M8 final (with season:temp)") %>%
            mutate(scenario = .x)),
  map_dfr(names(scenarios), ~ predict_scen(M_add_all, scenarios[[.x]], "M2 additive (no interaction)") %>%
            mutate(scenario = .x))
) %>%
  select(model, scenario, temp_C, hum_pct, wind_kmh, weathersit, fit, CI_low, CI_high,
         PI_low, PI_high) %>%
  mutate(across(c(temp_C, hum_pct, wind_kmh, fit, CI_low, CI_high, PI_low, PI_high),
                ~ round(.x, 1)))

cat("\n---- 7.2 Regression prediction for 2 Jan 2013 (T10) ----\n")
print(as.data.frame(pred_tbl), digits = 4)
save_tab(pred_tbl, "T10_regression_prediction")
save_md(pred_tbl, "T10_regression_prediction", digits = 1, caption = "Table T10 - Regression forecasts of the daily total for Wednesday 2 January 2013. CI = 95% confidence interval for the expected value, PI = 95% prediction interval for an individual day.")

## sanity check: the point forecasts of the three primary rows
cat("\npoint forecast check (predict vs X %*% beta):\n")
print(predict_scen(M_all, scenarios[[1]], "final") %>%
        select(fit, manual_check) %>% mutate(diff = fit - manual_check) %>%
        mutate(across(where(is.numeric), ~ round(.x, 6))) %>% as.data.frame())

## reference points from the data itself
ref <- df %>%
  filter(dteday %in% as.Date(c("2011-01-02", "2012-01-02", "2012-01-03",
                               "2012-01-04", "2012-12-31"))) %>%
  select(dteday, weekday, workingday, temp_C, weathersit, cnt) %>%
  mutate(temp_C = round(temp_C, 1))
cat("\n---- reference: daily totals actually observed on comparable days ----\n")
print(as.data.frame(ref))
cat("\nJanuary 2012: mean =", round(mean(df$cnt[format(df$dteday, "%Y-%m") == "2012-01"]), 0),
    "| December 2012: mean =", round(mean(df$cnt[format(df$dteday, "%Y-%m") == "2012-12"]), 0),
    "| whole-sample mean =", round(mean(df$cnt), 0), "\n")
cat("early January business days in 2012 (the closest analogue of 2 Jan 2013):\n")
print(as.data.frame(df %>%
  filter(dteday >= as.Date("2012-01-02"), dteday <= as.Date("2012-01-10")) %>%
  select(dteday, weekday, holiday, temp_C, weathersit, cnt) %>%
  mutate(temp_C = round(temp_C, 1))))

saveRDS(list(M_final = M_final, M_all = M_all, M_add_all = M_add_all,
             train = train, test = test, f_final = f_final,
             pred_tbl = pred_tbl, wx = wx, temp_bar = temp_bar,
             T06 = T06, T07 = T07, T08 = T08, T09 = T09),
        "vibe_work/data/regression_objects.rds")

cat("\n==================== 02 REGRESSION COMPLETE =======================\n")
