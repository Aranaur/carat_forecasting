library(tidyverse)
library(timetk)
library(lubridate)
library(DataExplorer)

google_analytics_tbl <- read_csv('https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/google_analytics_summary_hourly.csv')

mailchimp_users_tbl <- read_csv('https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/mailchimp_users.csv')

transactions_tbl <- read_csv('https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/transactions_weekly.csv')

google_analytics_tbl
mailchimp_users_tbl
transactions_tbl

# -----------------------------------------------------

subscribers_daily_tbl <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "day",
    optins = n()
  )

mailchimp_users_tbl %>% 
  summarise_by_time(
    .date_var = optin_time,
    .by = 'day',
    optin = n()
  )

mailchimp_users_tbl %>% 
  group_by(member_rating) %>% 
  summarise_by_time(
    .date_var = optin_time,
    .by = 'day',
    optin = n()
  ) %>% 
  plot_time_series(
    .date_var = optin_time,
    .value = optin,
    .interactive = FALSE
  )

mailchimp_users_tbl %>% 
  summarise_by_time(
    .date_var = optin_time,
    .by = 'week',
    optin = n()
  )

transactions_tbl %>% 
  summarise_by_time(
    .date_var = purchased_at,
    .by = 'month',
    revenue = sum(revenue)
  )

transactions_tbl %>% 
  summarise_by_time(
    .date_var = purchased_at,
    .by = 'month',
    .type = 'ceiling',
    revenue = sum(revenue)
  ) %>% 
  mutate(purchased_at = purchased_at %-time% '1 day')

transactions_tbl %>% 
  summarise_by_time(
    .date_var = purchased_at,
    .by = 'month',
    .type = 'round',
    revenue = sum(revenue)
  )

# -----------------------------------------------------

subscribers_day_tbl %>% 
  pad_by_time(
    .date_var = optin_time,
    .by = 'day',
    .pad_value = 0
  )
  
transactions_tbl %>% 
  pad_by_time(
    .date_var = purchased_at,
    .by = 'day',
    .start_date = '2018-06',
    .pad_value = NA
  ) %>% 
  mutate_by_time(
    .by = 'week',
    revenue = sum(revenue / 7, na.rm = TRUE)
  ) %>% 
  plot_time_series(
    .date_var = purchased_at,
    .value = revenue
  )

transactions_tbl %>% 
  plot_time_series(
    .date_var = purchased_at,
    .value = revenue
  )

# -----------------------------------------------------

transactions_tbl %>% 
  filter_by_time(
    .date_var = purchased_at,
    .start_date = "2019-12"
  ) %>% 
  plot_time_series(
    .date_var = purchased_at,
    .value = revenue
  )

transactions_tbl %>% 
  filter_by_time(
    .date_var = purchased_at,
    .start_date = "2018",
    .end_date = '2018'
  ) %>% 
  plot_time_series(
    .date_var = purchased_at,
    .value = revenue
  )

transactions_tbl %>% 
  filter_by_time(
    .date_var = purchased_at,
    .start_date = "2018-01",
    .end_date = "2018-12" %+time% "8 weeks"
  ) %>% 
  plot_time_series(
    .date_var = purchased_at,
    .value = revenue
  )

# -----------------------------------------------------

transactions_tbl %>% 
  mutate_by_time(
    .date_var = purchased_at,
    .by = '3 month',
    rev_avg = mean(revenue, na.rm = TRUE),
    rev_md = median(revenue),
    rev_max = max(revenue),
    rev_min = min(revenue)
  ) %>% 
  pivot_longer(cols = contains('rev')) %>% 
  plot_time_series(
    .date_var = purchased_at,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )

# -----------------------------------------------------

subscribers_daily_padded_tbl <- subscribers_daily_tbl %>% 
  pad_by_time(
    .date_var = optin_time,
    .pad_value = 0,
    .start_date = '2018-06'
  )

google_analytcs_summary_daily_tbl <- google_analytics_tbl %>%
  mutate(dateHour = ymd_h(dateHour)) %>%
  summarise_by_time(
    .date_var = dateHour,
    .by = "day",
    across(pageViews:sessions, .fns = sum)
  )

google_analytcs_summary_daily_tbl %>%
  pivot_longer(
    pageViews:sessions
  ) %>%
  plot_time_series(
    .date_var = dateHour,
    .value = value,
    .facet_vars = name
  )

subscribers_daily_joined_daily_tbl <- subscribers_daily_padded_tbl %>% 
  left_join(google_analytcs_summary_daily_tbl,
            by = c('optin_time' = 'dateHour'))

subscribers_daily_joined_daily_tbl %>% 
  plot_missing()

subscribers_daily_joined_daily_tbl %>% 
  tk_summary_diagnostics()

subscribers_daily_joined_daily_tbl %>% 
  pivot_longer(-optin_time) %>% 
  plot_time_series(
    .date_var = optin_time,
    .value = value,
    .color_var = name
  )

log_std_subscribers_daily_joined_daily_tbl <- subscribers_daily_joined_daily_tbl %>% 
  drop_na() %>% 
  mutate(across(.cols = optins:sessions, .fns = log1p)) %>% 
  mutate(across(.cols = optins:sessions, .fns = standardize_vec))

log_std_subscribers_daily_joined_daily_tbl %>% 
  pivot_longer(-optin_time) %>% 
  plot_time_series(
    .date_var = optin_time,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )

log_std_subscribers_daily_joined_daily_tbl %>% 
  plot_acf_diagnostics(
    .date_var = optin_time,
    .value = optins,
    .ccf_vars = pageViews:sessions,
    .show_ccf_vars_only = TRUE
  )

# -----------------------------------------------------

subscribers_daily_padded_tbl %>% 
  tk_index(optin_time)

tibble(
  date = tk_make_timeseries('2023', length_out = 100, by = 'month'),
  values = 1:100
)

tk_make_holiday_sequence(
  start_date = '2022',
  end_date = '2023',
  calendar = 'NYSE'
) %>% 
  tk_get_holiday_signature() %>% 
  glimpse()

tk_make_timeseries('2023') %>% 
  tk_get_holiday_signature()
  
tk_make_timeseries('2023', by = 'quarter') %+time% '1 year'

tk_make_timeseries('2023', by = 'quarter') %>% 
  tk_make_future_timeseries(length_out = 4)

# -----------------------------------------------------

future_tbl <- google_analytcs_summary_daily_tbl %>% 
  future_frame(
    .date_var = dateHour,
    .length_out = '2 month'
  )

lm_fit <- lm(
  pageViews ~ as.numeric(dateHour) + 
    wday(dateHour, label = TRUE) +
    month(dateHour),
  data = google_analytcs_summary_daily_tbl
)

predict_vec <- predict(lm_fit, newdata = future_tbl) %>% 
  as.vector()

google_analytcs_summary_daily_tbl %>% 
  select(dateHour, pageViews) %>% 
  add_column(type = 'actual') %>% 
  bind_rows(
    future_tbl %>% 
      mutate(
        pageViews = predict_vec,
        type = 'forecast'
      )
  ) %>% 
  plot_time_series(
    .date_var = dateHour,
    .value = pageViews,
    .color_var = type,
    .smooth = FALSE
  )
