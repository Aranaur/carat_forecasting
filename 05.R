library(fpp3)
library(tidyquant)
library(timetk)
library(gridExtra)
library(ggrepel)

aus_production_long <- aus_production %>% 
  pivot_longer(!Quarter, names_to = 'Name', values_to = 'Value')

aus_fit <- aus_production_long %>% 
  filter(Name == 'Beer') %>% 
  model(holt_winters = ETS(Value ~ error('A') + trend('A') + season('A')),
        holt_winters_m = ETS(Value ~ error('M') + trend('A') + season('A')),
        hw_m_d = ETS(Value ~ error('M') + trend('Ad') + season('A')))

report(aus_fit)

components(aus_fit) %>% 
  autoplot()

components(aus_fit) %>% 
  left_join(fitted(aus_fit), by = c('.model', 'Quarter', 'Name'))

aus_fit %>% 
  forecast(h = 8) %>% 
  autoplot(aus_production_long)

fit <- aus_production_long %>% 
  model(ets = ETS(Value))

fit %>% 
  forecast(h = 8) %>% 
  autoplot(aus_production_long)
