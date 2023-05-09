library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# Data
google_analytics_summary_tbl <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/google_analytics_summary_hourly.csv")

learning_labs_tbl <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/learning_labs.csv") 

subscribers_tbl   <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/mailchimp_users.csv")


# DATA PREPARATION ----
# - Apply Preprocessing to Target
data_prepared_tbl <- subscribers_tbl %>%
  summarize_by_time(
    optin_time,
    .by = "day",
    optins = n()
  ) %>%
  pad_by_time(
    .date_var = optin_time,
    .pad_value = 0
  ) %>%
  
  # Preprocess
  mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
  mutate(optins_trans = standardize_vec(optins_trans)) %>%
  # fix missing values at beginning of series
  filter_by_time(.start_date = "2018-07-03") %>%
  
  # Cleaning
  mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
  mutate(optins_trans = ifelse(optin_time %>% between_time("2018-11-18","2018-11-20"),
                               optins_trans_cleaned,
                               optins_trans)) %>%
  select(-optins, -optins_trans_cleaned)

data_prepared_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(
    .date_var    = optin_time,
    .value     = value,
    .color_var = name
  )

# ----------------------------------------------------------

# tk_augment_timeseries_signature()

data_prepared_signature_tbl <- data_prepared_tbl %>% 
  tk_augment_timeseries_signature() %>% 
  select(-diff, -contains('.iso'), -contains('.xts'),
         -matches('(hour)|(minute)|(second)|(am.pm)'))

data_prepared_signature_tbl %>% 
  glimpse()

data_prepared_signature_tbl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = optins_trans ~ .
  )

data_prepared_signature_tbl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = optins_trans ~ splines::bs(index.num, degree = 3)
  )

data_prepared_signature_tbl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = optins_trans ~ splines::ns(index.num, df = 3) + .
  )

# ----------------------------------------------------------

formula_seasonality <- as.formula(
  optins_trans ~ splines::ns(index.num, knots = quantile(
    index.num, prob = c(0.025, 0.5)
  )) + wday.lbl + month.lbl + .
)

data_prepared_signature_tbl %>% 
  plot_time_series_regression(
    optin_time,
    optins_trans ~ splines::ns(index.num, df = 3) +
      wday.lbl + month.lbl
  )

data_prepared_signature_tbl %>% 
  plot_time_series_regression(
    optin_time,
    formula_seasonality
  )

# ----------------------------------------------------------

data_prepared_signature_tbl %>% 
  plot_acf_diagnostics(
    optin_time,
    optins_trans
  )


data_prep_fourier_tbl <- data_prepared_signature_tbl %>% 
  tk_augment_fourier(
    optin_time,
    .periods = c(7, 14, 30, 90, 365),
    .K = 2
  )

data_prep_fourier_tbl

formula <- as.formula(
  optins_trans ~ splines::ns(index.num, knots = quantile(
    index.num, prob = c(0.025, 0.5)
  )) + wday.lbl + month.lbl + (as.factor(week2) * wday.lbl) + .
)

data_prep_fourier_tbl %>% 
  plot_time_series_regression(
    optin_time,
    formula
  )

# ----------------------------------------------------------

# tk_augment_lags()

data_prep_lags_tbl <- data_prep_fourier_tbl %>% 
  tk_augment_lags(
    .value = optins_trans,
    .lags = c(57, 63, 70)
  ) %>% 
  drop_na()

model_formula <- as.formula(
  optins_trans ~ splines::ns(index.num,
                             knots = quantile(
                               index.num,
                               prob = c(.025, .50)
                             ))
  + .
  + (as.factor(week2) * wday.lbl)
)
  
data_prep_lags_tbl %>% 
  plot_time_series_regression(
    optin_time,
    model_formula,
    .show_summary = TRUE
  )

# ----------------------------------------------------------

learning_labs_tbl

learning_labs_daily_tbl <- learning_labs_tbl %>%
  mutate(event_date = ymd_hms(event_date)) %>%
  summarise_by_time(.date_var = event_date, .by = "day", event = n())

data_prep_events_tbl <- data_prep_lags_tbl %>%
  left_join(learning_labs_daily_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event))

data_prep_events_tbl

plot <- data_prep_events_tbl %>% 
  plot_time_series(optin_time, optins_trans, .interactive = FALSE) +
  geom_point(color = 'red', data = . %>% filter(event == 1))

ggplotly(plot)

data_prep_events_tbl %>% 
  plot_time_series_regression(
    optin_time,
    model_formula,
    .show_summary = TRUE
  )

# ----------------------------------------------------------

google_analytics_summary_tbl

google_analytics_prep_tbl <- google_analytics_summary_tbl %>%
  mutate(date = ymd_h(dateHour)) %>%
  summarise_by_time(
    .date_var = date,
    .by = "day",
    across(pageViews:sessions, .fns = sum)
  ) %>%
  mutate(across(pageViews:sessions, .fns = log1p)) %>%
  mutate(across(pageViews:sessions, .fns = standardize_vec))

google_analytics_prep_tbl

data_prep_google_tbl <- data_prep_events_tbl %>%
  left_join(google_analytics_prep_tbl, by = c("optin_time"="date"))

data_prep_google_tbl <- data_prep_google_tbl %>% 
  drop_na()

data_prep_google_tbl %>% 
  plot_acf_diagnostics(
    optin_time,
    optins_trans,
    .ccf_vars = pageViews:sessions,
    .show_ccf_vars_only = TRUE
  )

data_prep_google_tbl %>% 
  plot_time_series_regression(
    optin_time,
    model_formula,
    .show_summary = TRUE
  )

data_prep_google_tbl %>%
  select(optin_time, optins_trans, pageViews) %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(
    .date_var = optin_time,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )
