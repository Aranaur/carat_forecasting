library(tidyverse)
library(fpp3)
library(timetk)

# tsibble
walmart_sales_weekly_tsb <- walmart_sales_weekly %>% 
  select(id, Date, Weekly_Sales) %>% 
  as_tsibble(
    index = Date,
    key = id
  )

walmart_sales_weekly_tsb %>% 
  autoplot(Weekly_Sales) +
  facet_wrap(vars(id), scales = "free")

markdown_tsb <- walmart_sales_weekly %>% 
  select(id, Date, 11:15) %>% 
  pivot_longer(!c(id, Date),
               names_to = "Markdown",
               values_to = "Value") %>% 
  as_tsibble(index = Date,
             key = c(id, Markdown))

markdown_tsb %>% 
  filter_index(. ~ "2012-08") %>% 
  autoplot() +
  facet_wrap(vars(Markdown), scales = 'free') +
  theme(legend.position = 'none')

markdown_tsb %>% 
  drop_na() %>% 
  has_gaps()

markdown_tsb %>% 
  drop_na() %>% 
  scan_gaps()


markdown_tsb %>% 
  drop_na() %>% 
  count_gaps()

markdown_tsb %>% 
  drop_na() %>% 
  fill_gaps(Value = median(Value)) %>% 
  autoplot() +
  facet_wrap(vars(Markdown), scales = 'free') +
  scale_y_log10() +
  theme(legend.position = 'none')

markdown_tsb %>% 
  drop_na() %>% 
  fill_gaps() %>% 
  fill(Value, .direction = "down") %>% 
  autoplot() +
  facet_wrap(vars(Markdown), scales = 'free') +
  scale_y_log10() +
  theme(legend.position = 'none')

markdown_tsb_fill <- markdown_tsb %>% 
  drop_na() %>% 
  fill_gaps(Value = median(Value))


markdown_fill <- markdown_tsb %>% 
  drop_na() %>% 
  group_by_key() %>% 
  fill_gaps(Value = median(Value))


markdown_fill %>% 
  index_by(Year = ~ year(.)) %>% 
  summarise(mean_value = mean(Value))

markdown_fill %>% 
  index_by(Year = ~ yearmonth(.)) %>% 
  summarise(mean_value = mean(Value))


walmart_sales <- walmart_sales_weekly %>% 
  select(id, Date, Weekly_Sales, Temperature) %>% 
  as_tsibble(
    index = Date,
    key = id
  )

walmart_sales %>% 
  autoplot(Weekly_Sales) +
  geom_smooth(se = FALSE) + 
  facet_wrap(~ id, scales = 'free')


walmart_sales %>% 
  filter(id == '1_1') %>% 
  gg_season(period = "year")

walmart_sales %>% 
  pivot_wider(values_from = "Weekly_Sales",
              names_from = 'id') %>% 
  GGally::ggpairs(columns = 3:9)
  

walmart_sales %>% 
  filter(id == '1_1') %>% 
  ACF(lag_max = 500) %>% 
  autoplot()