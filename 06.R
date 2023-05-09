library(fpp3)

aus_production_long <- aus_production %>% 
  pivot_longer(!Quarter, names_to = 'Name', values_to = 'Value')

beer <- aus_production_long %>% 
  filter(Name == 'Beer')

beer %>% 
  autoplot()

beer %>% 
  autoplot(log(Value))

beer %>% 
  autoplot(log(Value) %>% 
             difference(4))

beer %>% 
  autoplot(log(Value) %>% 
             difference(4) %>% 
             difference(1))

beer %>%
  features(Value, unitroot_kpss)

beer %>%
  features(Value, unitroot_ndiffs(.period = 4))

beer %>% mutate(log_Value = log(Value)) %>%
  features(log_Value, list(unitroot_nsdiffs, feat_stl)) # Fs>0,64

beer %>% mutate(log_Value = log(Value)) %>%
  features(log_Value, unitroot_nsdiffs)

beer %>% mutate(d_log_Value = difference(log(Value), 4)) %>%
  features(d_log_Value, unitroot_ndiffs)

##########################################

fit <- beer %>% 
  model(ARIMA(Value))
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  features(.innov, ljung_box)

fit %>% forecast(h=10) %>%
  autoplot(beer) +
  labs(y = "megalitres", title = "Beer production")

beer %>% ACF(Value) %>% autoplot()
beer %>% PACF(Value) %>% autoplot()

beer %>% 
  gg_tsdisplay(Value, plot_type = 'partial')

##########################################

fit1 <- beer %>% 
  model(arima400 = ARIMA(Value ~ pdq(4,0,0)),
        arima111 = ARIMA(Value ~ pdq(1,1,1)),
        stepwise = ARIMA(Value))
report(fit1)

fit1 %>% pivot_longer(!Name,
                      names_to = "Model name",
                      values_to = "Orders")

glance(fit1) %>% arrange(BIC) %>% select(.model:BIC)

fit1 %>% select(stepwise) %>% gg_tsresiduals()

augment(fit1) %>%
  filter(.model=='stepwise') %>%
  features(.innov, ljung_box)

fit1 %>% 
  forecast(h=5) %>%
  filter(.model == 'stepwise') %>% 
  autoplot(beer)

##########################################

fit2 <- beer %>% 
  model(best = ARIMA(log(Value), stepwise = FALSE,
                     approximation = FALSE,
                     trace = TRUE,
                     order_constraint = p + q + P + Q <= 9))
report(fit2)

##########################################

beer %>% 
  model(best = ARIMA(log(Value), stepwise = TRUE,
                     approximation = FALSE,
                     order_constraint = p + q + P + Q <= 9),
        ets = ETS(Value),
        arima_fur = ARIMA(Value ~ PDQ(0, 0, 0) + pdq(d=0) +
                fourier(K = 1)),
        arima = ARIMA(Value)) %>% 
  forecast(h = 10) %>% 
  autoplot(beer)

fit3 <- progressr::with_progress(beer %>% 
  stretch_tsibble(.init = 100, .step = 4) %>% 
  model(ets = ETS(Value),
        arima = ARIMA(Value),
        arima_fur = ARIMA(Value ~ PDQ() + pdq() + fourier(K = 2))))

fit3 %>% 
  forecast(h = 1) %>% 
  accuracy(beer) %>%
  select(.model, ME:RMSSE) %>% 
  arrange(-MAPE)

final_fit <- beer %>% 
  model(
    arima_fur = ARIMA(Value ~ PDQ() + pdq() + fourier(K = 2))
  )

final_fit %>% 
  forecast(new_data = new_data(beer, 2*8)) %>% 
  autoplot(beer)

##########################################

beer %>% 
  model(
    ARIMA(Value ~ PDQ(0, 0, 0) + pdq(d=0) +
            fourier(K = 2))
  ) %>% 
    forecast(h = 10) %>% 
  accuracy(beer)
