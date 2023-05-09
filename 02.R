library(tidyverse)
library(fpp3)
library(timetk)

global_economy %>% 
  filter(Country == "Australia") %>% 
  autoplot(GDP / Population)

aus_production %>% 
  autoplot(Gas)

aus_production %>% 
  autoplot(log(Gas))

lembda <- aus_production %>% 
  features(Gas, features = guerrero) %>% 
  pull(lambda_guerrero)

aus_production %>% 
  autoplot(box_cox(Gas, lembda)) +
  labs(
    y = "Gas",
    title = latex2exp::TeX(paste0(
      "Перетворений часовий ряд з $\\lambda$ = ",
      round(lembda, 2)
    ))
  )

aus_production_lambda <- aus_production %>% 
  mutate(Gas_lambda = box_cox(Gas, lembda))

aus_production_lambda %>% 
  model(
    classical_decomposition(Gas_lambda, type = 'additive')
  ) %>% 
  components() %>% 
  autoplot()

aus_production_lambda %>% 
  model(
    x_11 = X_13ARIMA_SEATS(Gas_lambda ~ x11())
  ) %>% 
  components() %>% 
  autoplot()

STL <- aus_production_lambda %>% 
  model(
    STL(Gas_lambda)
  ) %>% 
  components() 
  

STL %>% 
  gg_subseries(season_year)

STL %>% 
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas_lambda, colour = "Real data")) +
  geom_line(aes(y = season_adjust, colour = 'SAj')) +
  geom_line(aes(y = trend, color = 'Trend'))










