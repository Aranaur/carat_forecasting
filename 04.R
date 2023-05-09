library(fpp3)
library(tidyquant)
library(timetk)
library(gridExtra)
library(ggrepel)

aus_production_long <- aus_production %>% 
  pivot_longer(!Quarter, names_to = 'Name', values_to = 'Value')

aus_production_long %>% 
  autoplot() +
  facet_wrap(~ Name, scales = 'free')

aus_production_long %>% 
  features(Value, features = list(~ mean(., na.rm = TRUE)))

my_func <- function(x) {
  m <- mean(x, na.rm = TRUE)
  v <- var(x, na.rm = TRUE)
  return(c(avg = m, var = v))
}

aus_production_long %>% 
  features(Value, features = list(~ my_func(.)))

aus_production_long %>% 
  features(Value, list(coef_hurst)) # > 0.5 - зберігають тенденцію

aus_production_long %>% 
  features(Value, list(feat_stl))

all_features <- aus_production_long %>% 
  features(Value, feature_set(pkgs = "feasts"))

#################

aus_prod_trend <- aus_production_long %>% 
  features(Value, feature_set(tags = "stl"))

min <- aus_prod_trend %>% 
  filter(trend_strength == min(trend_strength)) %>% 
  select(Name) %>% 
  left_join(aus_production_long, by = 'Name') %>% 
  ggplot(aes(Quarter, Value)) +
  geom_line()

max <- aus_prod_trend %>% 
  filter(trend_strength == max(trend_strength)) %>% 
  select(Name) %>% 
  left_join(aus_production_long, by = 'Name') %>% 
  ggplot(aes(Quarter, Value)) +
  geom_line()

grid.arrange(min, max, ncol = 2)

#################

aus_prod_trend %>% 
  ggplot(., aes(trend_strength, 
                seasonal_strength_year, label = Name)) +
  geom_point() + 
  geom_text_repel(force = 10, segment.color = "gray60") +
  theme_minimal()



















