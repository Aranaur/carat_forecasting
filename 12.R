# ADVANCED FEATURE ENGINEERING

# ЦІЛІ ----
# - РЕАЛІЗАЦІЯ РЕЦЕПТІВ
# - ЗАСТОСУВАЙТЕ РОБОЧИЙ ПРОЦЕС MODELTIME ТА ВІЗУАЛІЗУЙТЕ РЕЗУЛЬТАТИ
# - ПОРІВНЯЙТЕ СПЛАЙН-МОДЕЛЬ З ЛАГ-МОДЕЛЮ

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(lubridate)
library(timetk)

# Дані
subscribers_tbl   <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/mailchimp_users.csv")
learning_labs_tbl <- read_csv("https://raw.githubusercontent.com/Aranaur/datasets/main/time_series/learning_labs.csv") 

# Обробка даних
subscribers_prep_tbl <- subscribers_tbl %>%
  summarise_by_time(optin_time, .by = "day", optins = n()) %>%
  pad_by_time(.pad_value = 0)
subscribers_prep_tbl

learning_labs_prep_tbl <- learning_labs_tbl %>%
  mutate(event_date = ymd_hms(event_date)) %>%
  summarise_by_time(event_date, .by = "day", event = n())
learning_labs_prep_tbl

# Перетворення даних
subscribers_transformed_tbl <- subscribers_prep_tbl %>%
  
  # Обробка цільової змінної
  mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
  mutate(optins_trans = standardize_vec(optins_trans)) %>%
  select(-optins) %>%
  
  # Виправлення пропущених значень на початку серії
  filter_by_time(.start_date = "2018-07-03") %>%
  
  # Очищення
  mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
  mutate(optins_trans = ifelse(optin_time %>% between_time("2018-11-18", "2018-11-20"), 
                               optins_trans_cleaned,
                               optins_trans)) %>%
  select(-optins_trans_cleaned)

subscribers_transformed_tbl

# Збереження ключових параметрів
limit_lower <- 0
limit_upper <- 3650.8
offset      <- 1
std_mean    <- -5.25529020756467
std_sd      <- 1.1109817111334

# 1.0 КРОК 1 - СТВОРИТИ ПОВНИЙ НАБІР ДАНИХ ----
# - Розширити до майбутнього вікна
# - Додайте будь-які затримки до повного набору даних
# - Додайте будь-які зовнішні регресори до повного набору даних

horizon <- 8*7
lag_period <- 8*7
rolling_periods <- c(30, 60, 90)

data_prepared_full_tbl <- subscribers_transformed_tbl %>%
  
  # Додавання вікна прогнозу
  bind_rows(
    future_frame(., .date_var = optin_time, .length_out = horizon)
  ) %>%
  
  # Додавання автокореляційнихї лагів
  tk_augment_lags(optins_trans, .lags = lag_period) %>%
  
  # Додавання віконних фічей
  tk_augment_slidify(
    .value   = optins_trans_lag56,
    .f       = mean,
    .period  = rolling_periods,
    .align   = "center",
    .partial = TRUE
  ) %>%
  
  # Додавання зовнішніх регресорів
  left_join(learning_labs_prep_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event)) %>%
  
  # Форматування стовпців
  rename(lab_event = event) %>%
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .))

data_prepared_full_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(
    optin_time,
    .value = value,
    .color_var = name,
    .smooth = FALSE
  )

# 2.0 Крок 2 - Поділ на модельні та прогнозовані дані ----
data_prepared_tbl <- data_prepared_full_tbl %>%
  filter(!is.na(optins_trans))

forecast_tbl <- data_prepared_full_tbl %>%
  filter(is.na(optins_trans))

# 3.0 TRAIN/TEST (модельні дані) ----
splits <- data_prepared_tbl %>%
  time_series_split(assess = horizon, cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)

# 4.0 РЕЦЕПТИ ----
# - Сігнатури часових рядів - додає масу фічей на основі часу
# - Перетворення сплайна до index.num
# - Взаємодія: wday.lbl:week2
# - Ряди Фур'є
model_fit_best_lm <- read_rds("00_models/model_fit_best_lm.rds")
model_fit_best_lm %>% summary()
model_fit_best_lm$terms %>% formula()

recipe_spec_base <- recipe(optins_trans ~ ., data = training(splits)) %>%
  
  # Сігнатури часових рядів
  step_timeseries_signature(optin_time) %>%
  step_rm(matches("(.iso)|(.xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  
  # Стандартизація
  step_normalize(matches("(index.num)|(year)|(yday)")) %>%
  
  # Фіктивні змінні (one hot encoding)
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  
  # Взаємодії / Фур'є
  step_interact(terms = ~ matches("week2") * matches("wday.lbl")) %>%
  step_fourier(optin_time, period = c(7,14,30,90,365), K = 2)

recipe_spec_base %>% prep() %>% bake(new_data = NULL) %>% glimpse()

# 5.0 СПЛАЙН МОДЕЛЬ ----

# * Специфікація LM моделі  ----
model_spec_lm <- linear_reg() %>%
  set_engine("lm")

# * Специфікація рецепта сплайну ----
recipe_spec_1 <- recipe_spec_base %>%
  step_rm(optin_time) %>%
  step_ns(ends_with("index.num"), deg_free = 2) %>%
  step_rm(starts_with("lag_"))

recipe_spec_1 %>% prep() %>% bake(new_data = NULL) %>% glimpse()

# * Робочий процес сплайну ----
workflow_fit_lm_1_spline <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(recipe = recipe_spec_1) %>%
  fit(training(splits))

# 6.0 MODELTIME  ----
calibration_tbl <- modeltime_table(
  workflow_fit_lm_1_spline
) %>%
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()

calibration_tbl %>%
  modeltime_accuracy()

# 7.0 LAG MODEL ----

# * Lag рецепт ----
recipe_spec_2 <- recipe_spec_base %>%
  step_rm(optin_time) %>%
  step_naomit(starts_with("lag_"))

recipe_spec_2 %>% prep() %>% bake(new_data = NULL) %>% glimpse()

# * Lag робочий процес ----
workflow_fit_lm_2_lag <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

workflow_fit_lm_2_lag %>% extract_fit_parsnip() %>% pluck("fit") %>% summary()

# * Порівняння з Modeltime -----
calibration_tbl <- modeltime_table(
  workflow_fit_lm_1_spline,
  workflow_fit_lm_2_lag
) %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  modeltime_forecast(
    new_data      = testing(splits),
    actual_data = data_prepared_tbl
  ) %>%
  plot_modeltime_forecast()

calibration_tbl %>%
  modeltime_accuracy()

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prepared_tbl)

# 8.0 ПРОГНОЗ НА МАЙБУТНЄ ----
refit_tbl %>%
  modeltime_forecast(
    new_data = forecast_tbl,
    actual_data = data_prepared_tbl
  ) %>%
  # Зворотне перетворення
  mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  ))) %>%
  mutate(across(.value:.conf_hi, .fns = ~ log_interval_inv_vec(
    x           = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  ))) %>%
  plot_modeltime_forecast()

# 9.0 Збереження артефактів ----

feature_engineering_artifacts_lst <- list(
  # Дані
  data = list(
    data_prepared_tbl  = data_prepared_tbl,
    forecast_tbl     = forecast_tbl
  ),
  # рецепти
    recipes = list(
    recipe_spec_base = recipe_spec_base,
    recipe_spec_1    = recipe_spec_1,
    recipe_spec_2    = recipe_spec_2
  ),
  # Моделі / Робочі процеси
    models = list(
    workflow_fit_lm_1_spline = workflow_fit_lm_1_spline,
    workflow_fit_lm_2_lag  = workflow_fit_lm_2_lag   
  ),
  # Параметри інверсії
    standardize = list(
    std_mean  = std_mean,
    std_sd  = std_sd
  ),
    log_interval = list(
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )
)

feature_engineering_artifacts_lst %>%
  write_rds(
    "00_models/feature_engineering_artifacts_list.rds"
  )
read_rds("00_models/feature_engineering_artifacts_list.rds")
