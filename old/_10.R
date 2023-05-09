library(tidyverse)
library(timetk)
library(lubridate)

google_analytics_summary_tbl <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/google_analytics_summary_hourly.csv")
google_analytics_summary_tbl 

mailchimp_users_tbl  <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/mailchimp_users.csv")
mailchimp_users_tbl 

transactions_tbl  <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/transactions_weekly.csv")
transactions_tbl

# --------------------------------------

google_analytics_summary_long_tbl <- google_analytics_summary_tbl %>% 
  mutate(date = ymd_h(dateHour)) %>% 
  select(-dateHour) %>% 
  pivot_longer(-date) %>% 
  group_by(name)

subs_daily_ybl <- mailchimp_users_tbl %>% 
  summarise_by_time(optin_time, .by = 'day', optins = n()) %>% 
  pad_by_time(.pad_value = 0, .start_date = '2018-06-01')

# --------------------------------------

subs_daily_ybl %>% 
  plot_time_series(.date_var = optin_time, .value = optins)

subs_daily_ybl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = optins ~ as.numeric(optin_time) +
      wday(optin_time, label = TRUE) +
      month(optin_time, label = TRUE),
    .show_summary = TRUE
  )

# log

subs_daily_ybl %>% 
  plot_time_series(
    .date_var = optin_time, .value = log(optins)
  )

subs_daily_ybl %>% 
  plot_time_series(
    .date_var = optin_time, .value = log1p(optins)
  )

subs_daily_ybl %>% 
  plot_time_series(
    .date_var = optin_time, .value = log1p(optins) %>% expm1()
  )

subs_daily_ybl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = log1p(optins) ~ as.numeric(optin_time) +
      wday(optin_time, label = TRUE) +
      month(optin_time, label = TRUE),
    .show_summary = TRUE
  )

google_analytics_summary_long_tbl %>% 
  plot_time_series(.date_var = date,
                   .value = log1p(value),
                   .color_var = name)

google_analytics_summary_long_tbl %>% 
  plot_time_series(.date_var = date,
                   .value = log1p(value) %>% expm1(),
                   .color_var = name)

# Box-Cox

subs_daily_ybl %>% 
  plot_time_series(optin_time, box_cox_vec(optins + 1, lambda = 'auto'))

subs_daily_ybl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = box_cox_vec(optins + 1) ~ as.numeric(optin_time) +
      wday(optin_time, label = TRUE) +
      month(optin_time, label = TRUE)
  )

# Box Cox inversion

subs_daily_ybl %>% 
  plot_time_series(
    .date_var = optin_time,
    .value = box_cox_vec(optins + 1, lambda = -0.189538732777345) %>% 
      box_cox_inv_vec(lambda = -0.189538732777345)
  )


google_analytics_summary_long_tbl %>% 
  plot_time_series(
    .date = date,
    .value = box_cox_vec(value),
    .color_var = name
  )

google_analytics_summary_long_tbl %>% 
  mutate(value_trans = box_cox_vec(value)) %>% 
  group_split() %>% 
  map2(.y = 1:3, .f = function(df, idx){
    if(idx == 1) lambda <- 0.441313194936969
    if(idx == 2) lambda <- -0.0023793550944814
    if(idx == 3) lambda <- -0.116626712183629
  
    df %>% 
      mutate(value_trans_inv = box_cox_inv_vec(x = value_trans, lambda = lambda))
    }) %>% 
  bind_rows() %>% 
  group_by(name) %>% 
  plot_time_series(
    .date_var = date,
    .value = value_trans_inv
  )

# ------------------------------------------------

google_analytics_summary_long_tbl %>% 
  mutate(value_roll = slidify_vec(
    .x = value,
    .f = mean,
    .period = 24 * 7,
    .align = 'center'
  )) %>% 
  pivot_longer(contains('value'), names_repair = 'unique') %>% 
  rename(names = `name...2`,
         names_2 = `name...3`) %>% 
  group_by(names) %>% 
  plot_time_series(
    .date_var = date,
    .value = value,
    .color_var = names_2,
    .smooth = FALSE
  )

google_analytics_summary_long_tbl %>% 
  mutate(value_smooth = smooth_vec(x = value, period = 24 * 7)) %>% 
  pivot_longer(contains('value'), names_repair = 'unique') %>% 
  rename(names = `name...2`,
         names_2 = `name...3`) %>% 
  group_by(names) %>% 
  plot_time_series(
    .date_var = date,
    .value = value,
    .color_var = names_2,
    .smooth = FALSE
  )

# -------------------------------------------------

rolling_cor_24_7 <- slidify(
  .f = ~ cor(.x, .y, use = 'pairwise.complete.obs'),
  .period = 24 * 7,
  .align = 'center'
)

google_analytics_summary_tbl %>% 
  mutate(roll_cor_page_org = rolling_cor_24_7(pageViews, organicSearches)) %>% 
  mutate(date = ymd_h(dateHour)) %>% 
  plot_time_series(
    .date_var = date,
    .value = roll_cor_page_org
  )

# -------------------------------------------------

transactions_tbl %>% 
  mutate(mavg_8 = slidify_vec(
    .x = revenue,
    .f = ~ mean(.x, na.rm = TRUE),
    .period = 8,
    .align = 'center'
  )) %>% 
  bind_rows(
    future_frame(., .length_out = 8)
  ) %>% 
  fill(mavg_8, .direction = 'down') %>% 
  pivot_longer(-purchased_at) %>% 
  plot_time_series(
    .date_var = purchased_at,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )

# -------------------------------------------------

# standatize + normalize

google_analytics_summary_long_tbl %>% 
  mutate(value = normalize_vec(value)) %>% 
  ungroup() %>% 
  plot_time_series(
    .date_var = date,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )

google_analytics_summary_long_tbl %>% 
  mutate(value = standardize_vec(value)) %>% 
  ungroup() %>% 
  plot_time_series(
    .date_var = date,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )

# -------------------------------------------------

subs_daily_ybl %>% 
  mutate(optins_lag_1 = lag_vec(optins, lag = 1),
         optins_lag_2 = lag_vec(optins, lag = 2))

subs_daily_ybl %>% 
  plot_acf_diagnostics(
    .date_var = optin_time,
    .value = log1p(optins)
  )

subs_daily_ybl %>% 
  tk_augment_lags(
    .value = optins,
    .lags = c(1, 2, 6, 14)
  ) %>% 
  drop_na() %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = log1p(optins) ~ as.numeric(optin_time) +
      log1p(optins_lag1) +
      log1p(optins_lag2) +
      log1p(optins_lag6) +
      log1p(optins_lag14),
    .show_summary = TRUE
  )

subs_daily_ybl %>% 
  mutate(total_optins = cumsum(optins),
         optins_diff_1 = diff_vec(total_optins, lag = 1),
         optins_diff_2 = diff_vec(total_optins, lag = 1, difference = 2)) %>% 
  pivot_longer(-optin_time) %>% 
  group_by(name) %>% 
  plot_time_series(
    optin_time, log1p(value), .color_var = name, .smooth = FALSE)


google_analytics_summary_diff_tbl <- google_analytics_summary_tbl %>% 
  mutate(across(pageViews:sessions, .fns = diff_vec))

google_analytics_summary_diff_tbl %>% 
  mutate(dateHour = ymd_h(dateHour)) %>% 
  pivot_longer(-dateHour) %>% 
  plot_time_series(dateHour, value, name, .smooth = FALSE)

google_analytics_summary_diff_tbl %>% 
  mutate(pageViews = diff_inv_vec(pageViews, initial_values = 79))

# -------------------------------------------------

subs_daily_ybl %>% 
  mutate(sin7_k1 = fourier_vec(optin_time, period = 14, K = 1, type = 'sin'),
         cos7_k1 = fourier_vec(optin_time, period = 7, K = 1, type = 'cos')) %>% 
  select(-optins) %>% 
  pivot_longer(-optin_time) %>% 
  plot_time_series(
    optin_time,
    value,
    name,
    .smooth = FALSE
  )

subs_daily_ybl %>% 
  tk_augment_fourier(optin_time, .periods = c(7, 14, 30, 90, 365), .K = 2) %>% 
  plot_time_series_regression(
    optin_time,
    .formula = log1p(optins) ~ as.numeric(optin_time) + . - optin_time,
    .show_summary = TRUE
  )

# -------------------------------------------------

1:10

val_trans_vec <- log_interval_vec(1:10, limit_lower = 0, limit_upper = 15)

val_trans_vec %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 15)

new_val_trans_vec <- c(val_trans_vec, c(0.75, 1.5, 2, 10))

new_val_trans_vec %>% plot()

new_val_trans_vec %>% log_interval_inv_vec(0, 15) %>% plot()

limit_lower <- 0
limit_upper <- 3650
offset <- 1

subs_daily_ybl %>% 
  plot_time_series(
    optin_time,
    log_interval_vec(
      optins,
      limit_lower = limit_lower,
      limit_upper = limit_upper,
      offset = offset
    )
  )

# -------------------------------------------------

fourier_periods <- c(6,14,30,90,365)
fourier_order <- 5

data_transformed_tbl <- subs_daily_ybl %>%
  mutate(optins_trans = log_interval_vec(optins, limit_lower = limit_lower,
                                         limit_upper = limit_upper,
                                         offset = offset)) %>%
  tk_augment_fourier(
    .date_var  = optin_time,
    .periods = fourier_periods,
    .K = fourier_order
  ) %>%
  select(-optins)


model_formula <- as.formula(optins_trans ~ as.numeric(optin_time) +
                              wday(optin_time) +
                              month(optin_time) +
                              . - optin_time)

data_transformed_tbl %>%
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = model_formula,
    .show_summary = TRUE
  )

model_fit_lim <- lm(formula = model_formula, data = data_transformed_tbl)

model_fit_lim %>% 
  summary()

# * Create Future Data ----
future_tbl <- data_transformed_tbl %>%
  future_frame(
    .date_var = optin_time,
    .length_out = "6 months"
  ) %>%
  tk_augment_fourier(
    .date_var = optin_time,
    .periods  = fourier_periods,
    .K      = fourier_order
  )

future_tbl

# * Predict ----
predictions <- predict(
  object = model_fit_lim,
  newdata = future_tbl
) %>%
  as.vector()

# * Combine data ----
conf_interval <- 0.95
residuals <- model_fit_lim$residuals %>% as.vector()
alpha <- (1 - conf_interval)/2
abs_margin_error <- abs(qnorm(alpha) * sd(residuals))

forecast_tbl <- data_transformed_tbl %>%
  select(optin_time, optins_trans) %>%
  add_column(type = "actual") %>%
  bind_rows(
    future_tbl %>%
      select(optin_time) %>%
      mutate(
        optins_trans = predictions,
        type       = "prediction"
      ) %>%
      mutate(
        conf_lo   = optins_trans - abs_margin_error
        , conf_hi = optins_trans + abs_margin_error
      )
  )

forecast_tbl %>%
  pivot_longer(cols = c(optins_trans, conf_lo, conf_hi)) %>%
  plot_time_series(
    .date_var = optin_time,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )

# * Invert Transformation ----

forecast_tbl %>%
  pivot_longer(cols = c(optins_trans, conf_lo, conf_hi)) %>%
  plot_time_series(
    .date_var = optin_time,
    .value = log_interval_inv_vec(
      x = value,
      limit_lower = limit_lower,
      limit_upper = limit_upper,
      offset      = offset
    ),
    .color_var = name,
    .smooth    = FALSE
  )

