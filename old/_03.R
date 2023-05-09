library(fpp3)
library(timetk)

aus_production

dcmp <- aus_production %>% 
  model(STL(Gas ~ trend() + season() + lowpass(), robust = TRUE)) %>% 
  components() %>% 
  select(-.model)

fit_dcmp <- aus_production %>% 
  model(stlf = decomposition_model(
    STL(Gas ~ trend() + season() + lowpass(), robust = TRUE),
    RW(season_adjust ~ drift())
  )) 

fit_dcmp %>% 
  forecast() %>% 
  autoplot(aus_production)

fit_dcmp %>% 
  augment()

fit_dcmp %>% 
  gg_tsresiduals()

# train/test
test <- aus_production %>% 
  filter(year(Quarter) >= 2000)

train <- aus_production %>% 
  filter(year(Quarter) < 2000)

train_fit <- train %>% 
  model(
    stlf_1 = decomposition_model(
      STL(Gas ~ trend() + season() + lowpass(), robust = TRUE),
      RW(season_adjust ~ drift())),
    snaive = SNAIVE(Gas))

forecast <- train_fit %>% 
  forecast(h = nrow(test))

train_fit %>% 
  forecast(h = nrow(test)) %>% 
  autoplot(train, level = NULL) + 
  autolayer(test %>% select(Gas), alpha = 0.2)

forecast %>%
  accuracy(test)

# cross

train_cv <- train %>%
  stretch_tsibble(.init = 100, .step = 4)


train_cv %>%
  model(
    stlf_1 = decomposition_model(
      STL(Gas ~ trend() + season() + lowpass(), robust = TRUE),
      RW(season_adjust ~ drift())),
    snaive = SNAIVE(Gas)) %>%
  forecast(h = 1) %>% 
  accuracy(train)

train %>% 
  model(
    stlf_1 = decomposition_model(
      STL(Gas ~ trend() + season() + lowpass(), robust = TRUE),
      RW(season_adjust ~ drift())),
    snaive = SNAIVE(Gas)) %>%
  accuracy()
  
  