# KC Housing Price Appreciation - full analysis pipeline

library(tidyverse)
library(readr)
library(janitor)
library(tigris)
library(sf)
library(broom)
library(gt)
library(stargazer)
library(spdep)
library(spatialreg)
library(knitr)
library(kableExtra)

options(tigris_use_cache = TRUE)

## Data Prep

crosswalk <- read_csv("target_zips.csv") %>%
  mutate(RegionName = as.character(RegionName))

zillow_prices <- read_csv("zillow_sfh_price.csv")

prices_kc <- zillow_prices %>%
  semi_join(crosswalk, by = "RegionName") %>%
  select(-RegionID, -SizeRank, -RegionType, -StateName, -State, -City, -CountyName, -Metro)

prices_kc_long <- prices_kc %>%
  pivot_longer(cols = -RegionName, names_to = "Date", values_to = "prices_kc")

prices_kc_yearly <- prices_kc_long %>%
  filter(month(Date) == 1) %>%
  mutate(Year = year(Date), ln_price = log(prices_kc))

beta_data <- prices_kc_yearly %>%
  select(RegionName, Year, ln_price) %>%
  pivot_wider(names_from = Year, values_from = ln_price, names_prefix = "log_price_") %>%
  mutate(
    growth_2020_2026 = log_price_2026 - log_price_2020,
    growth_2018_2026 = log_price_2026 - log_price_2018,
    growth_2012_2026 = log_price_2026 - log_price_2012,
    growth_2001_2026 = log_price_2026 - log_price_2001
  )

missing <- prices_kc_yearly %>%
  group_by(Year) %>%
  summarize(
    n_zip = n(),
    years_missing = sum(is.na(prices_kc)),
    pct_missing = mean(is.na(prices_kc))
  )

## Coverage Maps

zips <- zctas(year = 2020)

map_data <- zips %>%
  inner_join(beta_data, by = c("ZCTA5CE20" = "RegionName"))

coverage_map <- function(data, year_var) {
  ggplot(data) +
    geom_sf(aes(fill = is.na(.data[[year_var]]))) +
    scale_fill_manual(values = c("steelblue", "red"))
}

na2001_map <- coverage_map(map_data, "log_price_2001")
na2012_map <- coverage_map(map_data, "log_price_2012")
na2018_map <- coverage_map(map_data, "log_price_2018")
na2020_map <- coverage_map(map_data, "log_price_2020")

## Beta Convergence

beta2001_2026 <- lm(growth_2001_2026 ~ log_price_2001, data = beta_data)
beta2012_2026 <- lm(growth_2012_2026 ~ log_price_2012, data = beta_data)
beta2018_2026 <- lm(growth_2018_2026 ~ log_price_2018, data = beta_data)
beta2020_2026 <- lm(growth_2020_2026 ~ log_price_2020, data = beta_data)

stargazer(
  beta2001_2026, beta2012_2026, beta2018_2026, beta2020_2026,
  type = "text",
  title = "Beta Convergence in Kansas City ZIP Codes",
  dep.var.labels = "Log Home Price Growth",
  column.labels = c("2001-2026", "2012-2026", "2018-2026", "2020-2026"),
  covariate.labels = rep("Initial Log Home Price", 4),
  digits = 3,
  align = TRUE,
  omit.stat = c("f", "ser")
)

# Annualize raw beta coefficients so they're comparable across unequal study windows
get_beta_se <- function(model, varname) {
  tidy(model) %>% filter(term == varname)
}

beta_2001 <- get_beta_se(beta2001_2026, "log_price_2001")
beta_2012 <- get_beta_se(beta2012_2026, "log_price_2012")
beta_2018 <- get_beta_se(beta2018_2026, "log_price_2018")
beta_2020 <- get_beta_se(beta2020_2026, "log_price_2020")

delta_method_se <- function(beta, se_beta, T) {
  b <- -log(1 + beta) / T
  se_b <- se_beta / ((1 + beta) * T)
  tibble(b = b, se_b = se_b, ci_low = b - 1.96 * se_b, ci_high = b + 1.96 * se_b)
}

results <- bind_rows(
  delta_method_se(beta_2001$estimate, beta_2001$std.error, 25) %>% mutate(period = "2001-2026"),
  delta_method_se(beta_2012$estimate, beta_2012$std.error, 14) %>% mutate(period = "2012-2026"),
  delta_method_se(beta_2018$estimate, beta_2018$std.error, 8)  %>% mutate(period = "2018-2026"),
  delta_method_se(beta_2020$estimate, beta_2020$std.error, 6)  %>% mutate(period = "2020-2026")
)

# Pairwise significance tests between annualized speeds
z_test_b <- function(b1, se1, b2, se2) {
  z <- (b1 - b2) / sqrt(se1^2 + se2^2)
  tibble(z = z, p = 2 * (1 - pnorm(abs(z))))
}

get_row <- function(period) results %>% filter(period == !!period)

comparisons <- bind_rows(
  z_test_b(get_row("2012-2026")$b, get_row("2012-2026")$se_b, get_row("2018-2026")$b, get_row("2018-2026")$se_b) %>% mutate(pair = "2012-2026 vs 2018-2026"),
  z_test_b(get_row("2012-2026")$b, get_row("2012-2026")$se_b, get_row("2020-2026")$b, get_row("2020-2026")$se_b) %>% mutate(pair = "2012-2026 vs 2020-2026"),
  z_test_b(get_row("2018-2026")$b, get_row("2018-2026")$se_b, get_row("2020-2026")$b, get_row("2020-2026")$se_b) %>% mutate(pair = "2018-2026 vs 2020-2026"),
  z_test_b(get_row("2012-2026")$b, get_row("2012-2026")$se_b, get_row("2001-2026")$b, get_row("2001-2026")$se_b) %>% mutate(pair = "2012-2026 vs 2001-2026"),
  z_test_b(get_row("2018-2026")$b, get_row("2018-2026")$se_b, get_row("2001-2026")$b, get_row("2001-2026")$se_b) %>% mutate(pair = "2018-2026 vs 2001-2026"),
  z_test_b(get_row("2020-2026")$b, get_row("2020-2026")$se_b, get_row("2001-2026")$b, get_row("2001-2026")$se_b) %>% mutate(pair = "2020-2026 vs 2001-2026")
)

# Export tables
results %>%
  mutate(
    b = scales::percent(b, accuracy = 0.01),
    ci = paste0("[", scales::percent(ci_low, accuracy = 0.01), ", ", scales::percent(ci_high, accuracy = 0.01), "]")
  ) %>%
  select(period, b, ci) %>%
  gt() %>%
  cols_label(period = "Period", b = "Annualized speed (b)", ci = "95% CI") %>%
  as_raw_html() %>%
  writeLines("tables/standardized_convergence.html")

comparisons %>%
  mutate(z = round(z, 2), p_value = if_else(p < 0.001, "< 0.001", as.character(round(p, 3)))) %>%
  select(pair, z, p_value) %>%
  gt() %>%
  cols_label(pair = "Comparison", z = "z", p_value = "p-value") %>%
  as_raw_html() %>%
  writeLines("tables/pvalue_standardized_convergence.html")

# Data coverage by model specification
coverage_table <- missing %>%
  filter(Year %in% c(2001, 2012, 2018, 2020)) %>%
  mutate(
    Model = 1:4,
    Years = c("2001–2026", "2012–2026", "2018–2026", "2020–2026"),
    N = c(nobs(beta2001_2026), nobs(beta2012_2026), nobs(beta2018_2026), nobs(beta2020_2026)),
    `Missing (%)` = scales::percent(pct_missing, accuracy = 1)
  ) %>%
  select(Model, Years, N, `Missing (%)`)

coverage_table %>%
  gt() %>%
  cols_label(Model = "Model", Years = "Study Period", N = "Regression N", `Missing (%)` = "Missing at Start") %>%
  fmt_number(columns = N, decimals = 0) %>%
  gtsave("tables/beta_coverage_table.html")

## Sigma Convergence

sigma_data <- prices_kc_yearly %>%
  group_by(Year) %>%
  summarize(
    sigma = sd(ln_price, na.rm = TRUE),
    mean_ln_price = mean(ln_price, na.rm = TRUE),
    n = sum(!is.na(ln_price))
  )

sigma_map1 <- ggplot(sigma_data, aes(x = Year, y = sigma)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Sigma Convergence in Kansas City ZIP Codes",
       x = "Year", y = "Standard Deviation of Log Home Prices") +
  theme_minimal()

sigma2000_2026 <- lm(sigma ~ Year, data = sigma_data)
sigma2000_2006 <- lm(sigma ~ Year, data = filter(sigma_data, Year >= 2000, Year <= 2006))
sigma2007_2014 <- lm(sigma ~ Year, data = filter(sigma_data, Year >= 2007, Year <= 2014))
sigma2015_2026 <- lm(sigma ~ Year, data = filter(sigma_data, Year >= 2015))

stargazer(
  sigma2000_2026, sigma2000_2006, sigma2007_2014, sigma2015_2026,
  type = "text",
  title = "Sigma Convergence in Kansas City ZIP Codes",
  dep.var.labels = "Standard Deviation of Log Home Prices",
  column.labels = c("2000-2026", "2000-2006", "2007-2014", "2015-2026"),
  covariate.labels = "Year (Annual Change in σ)",
  digits = 3,
  align = TRUE,
  omit.stat = c("f", "ser")
)

sigma_data <- sigma_data %>%
  mutate(regime = case_when(
    Year <= 2006 ~ "2000–2006",
    Year <= 2014 ~ "2007–2014",
    TRUE ~ "2015–2026"
  ))

sigma_map2 <- ggplot(sigma_data, aes(x = Year, y = sigma)) +
  geom_point(size = 2) +
  geom_line() +
  geom_smooth(aes(color = regime), method = "lm", se = FALSE, linewidth = 1.2) +
  labs(title = "Sigma Convergence in Kansas City ZIP Codes",
       x = "Year", y = "Standard Deviation of Log Home Prices", color = "Period") +
  theme_minimal()

## Convergence Trajectory Charts

baseline_year <- 2012

tercile_colors <- c(
  "Lowest-priced ZIPs (2012)"  = "#5B7FA6",
  "Highest-priced ZIPs (2012)" = "#0B2E59",
  "MSA Average"                = "black"
)
tercile_linewidths <- c(
  "Lowest-priced ZIPs (2012)"  = 1.3,
  "Highest-priced ZIPs (2012)" = 1.3,
  "MSA Average"                = 0.7
)

zip_groups <- prices_kc_long %>%
  filter(year == baseline_year) %>%
  mutate(price_tercile = ntile(prices_kc, 3)) %>%
  select(RegionName, price_tercile)

log_prices <- prices_kc_long %>%
  mutate(log_price = log(prices_kc))

group_trajectories <- log_prices %>%
  inner_join(zip_groups, by = "RegionName") %>%
  group_by(year, price_tercile) %>%
  summarise(avg_log_price = mean(log_price, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = case_when(
    price_tercile == 1 ~ "Lowest-priced ZIPs (2012)",
    price_tercile == 3 ~ "Highest-priced ZIPs (2012)",
    TRUE ~ "Middle tercile"
  )) %>%
  filter(group != "Middle tercile")

overall_avg <- log_prices %>%
  group_by(year) %>%
  summarise(avg_log_price = mean(log_price, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = "MSA Average")

plot_data <- bind_rows(group_trajectories, overall_avg)

wide_trajectories <- group_trajectories %>%
  select(year, group, avg_log_price) %>%
  pivot_wider(names_from = group, values_from = avg_log_price) %>%
  rename(high = `Highest-priced ZIPs (2012)`, low = `Lowest-priced ZIPs (2012)`) %>%
  mutate(gap = high - low)

# Chart 1: gap over time
ggplot(wide_trajectories, aes(x = year, y = gap)) +
  geom_line(color = "#0B2E59", linewidth = 1.3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  labs(
    title = "The Price Gap Between KC's Cheapest and Priciest ZIPs Is Shrinking",
    subtitle = "Difference in average log home price, highest vs. lowest 2012 tercile",
    x = NULL, y = "Log Price Gap"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16), plot.title.position = "plot", panel.grid.minor = element_blank())

# Chart 2: three-line trajectory with shaded gap ribbon
ggplot() +
  geom_ribbon(data = wide_trajectories, aes(x = year, ymin = low, ymax = high), fill = "#0B2E59", alpha = 0.08) +
  geom_line(data = plot_data, aes(x = year, y = avg_log_price, color = group, linewidth = group)) +
  scale_color_manual(values = tercile_colors) +
  scale_linewidth_manual(values = tercile_linewidths, guide = "none") +
  labs(
    title = "Convergence in Log Home Prices: Lowest vs. Highest Priced ZIPs",
    subtitle = "Groups fixed by 2012 price tercile — shaded region shows the narrowing gap",
    x = NULL, y = "Average Log Home Price", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15), plot.title.position = "plot",
        panel.grid.minor = element_blank(), legend.position = "top")

## Spatial Autocorrelation

spatial_df <- readRDS("map_data.rds")
spatial_df <- st_set_crs(spatial_df, NA)
spatial_df <- st_set_crs(spatial_df, 4269)      # NAD83 geographic
spatial_df <- st_transform(spatial_df, 26915)   # project to working UTM CRS

spatial_df_2012 <- spatial_df %>% filter(!is.na(growth_2012_2026))
beta_model <- lm(growth_2012_2026 ~ log_price_2012, data = spatial_df_2012)

# Weights matrix
nb_queen_2012 <- poly2nb(spatial_df_2012, queen = TRUE)
which(card(nb_queen_2012) == 0)  # confirm no islands

lw_queen_2012 <- nb2listw(nb_queen_2012, style = "W", zero.policy = TRUE)

# Moran's I on residuals
spatial_df_2012$resid <- residuals(beta_model)
moran_test <- moran.test(spatial_df_2012$resid, lw_queen_2012, zero.policy = TRUE)

# LM diagnostics: lag vs. error
lm_tests <- lm.LMtests(beta_model, lw_queen_2012, test = "all", zero.policy = TRUE)
lm_summary <- summary(lm_tests)

# Spatial Error Model
error_model_2012 <- errorsarlm(
  growth_2012_2026 ~ log_price_2012,
  data = spatial_df_2012,
  listw = lw_queen_2012,
  zero.policy = TRUE
)

# Export weights matrix sample
W_mat <- listw2mat(lw_queen_2012)
subset_ids <- c("64108", "64109", "64110", "64111", "64112")
idx <- match(subset_ids, spatial_df_2012$ZCTA5CE20)
W_chunk <- round(W_mat[idx, idx], 2)
rownames(W_chunk) <- subset_ids
colnames(W_chunk) <- subset_ids

kable(W_chunk, format = "html") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, background = "#0B2E59", color = "white") %>%
  save_kable("tables/weight_matrix_sample.html")

# Export Moran's I table
moran_table <- data.frame(
  Statistic = c("Moran's I", "Expected I", "Variance", "Standard Deviate (z)", "p-value"),
  Value = c(
    round(moran_test$estimate[["Moran I statistic"]], 3),
    round(moran_test$estimate[["Expectation"]], 4),
    round(moran_test$estimate[["Variance"]], 4),
    round(moran_test$statistic, 3),
    format.pval(moran_test$p.value, digits = 3, eps = 0.001)
  )
)

kable(moran_table, format = "html", align = "lr") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, background = "#0B2E59", color = "white") %>%
  save_kable("tables/moran_I_test.html")

# Export LM test table
test_names <- c("RSerr", "RSlag", "adjRSerr", "adjRSlag", "SARMA")
lm_table <- data.frame(
  Test = c("LM Error", "LM Lag", "Robust LM Error", "Robust LM Lag", "SARMA"),
  Statistic = sapply(test_names, function(t) round(as.numeric(lm_summary[[t]]$statistic), 3)),
  `p-value` = sapply(test_names, function(t) format.pval(lm_summary[[t]]$p.value, digits = 3, eps = 0.001)),
  check.names = FALSE,
  row.names = NULL
)

kable(lm_table, format = "html", align = "lcc") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, background = "#0B2E59", color = "white") %>%
  save_kable("tables/lm_test.html")

# Export OLS vs. SEM comparison table
comparison_table <- data.frame(
  Term = c("Intercept", "Initial Log Home Price", "Lambda (λ)", "AIC", "Observations"),
  OLS = c(
    round(coef(beta_model)[1], 3),
    paste0(round(coef(beta_model)[2], 3), " (", round(summary(beta_model)$coefficients[2, 2], 3), ")"),
    "—",
    round(AIC(beta_model), 2),
    nobs(beta_model)
  ),
  `Spatial Error` = c(
    round(coef(error_model_2012)[1], 3),
    paste0(round(coef(error_model_2012)[2], 3), " (", round(summary(error_model_2012)$Coef[2, 2], 3), ")"),
    paste0(round(error_model_2012$lambda, 3), "***"),
    round(AIC(error_model_2012), 2),
    nrow(spatial_df_2012)
  ),
  check.names = FALSE
)

kable(comparison_table, format = "html", align = "lcc") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, background = "#0B2E59", color = "white") %>%
  save_kable("tables/ols_vs_sem.html")
