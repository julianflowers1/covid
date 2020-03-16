## get uk data from tweets and coronavirus tracker

library(rtweet)
#remotes::install_github("tidyverse/dplyr")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

covid <- get_timeline("DHSCgovuk", n = 3200, retryonratelimit = TRUE )

## extract daily tests and positives and deaths from DHSC daily tweets

covid_stats <- covid %>%
  filter(str_detect(text, "UPDATE")) %>% 
  mutate(date = floor_date(as.Date(created_at), unit = "days")) %>%
  select(date, text) %>%
  mutate(total = str_extract(text, "total of \\d.*|\\d.*concluded"), 
         pos  = str_extract(text, "\\d.*positive"), 
         total = parse_number(total), 
         pos = parse_number(pos), 
         deaths = parse_number(str_extract(text, ("\\d.*died") )))%>%
  filter(!is.na(total)) %>%
  mutate( 
         ref_date = date - 1, 
         lag_tot = lead(total), 
         lag_pos = lead(pos) + 0.5, 
         daily_change = -lag_tot + total, 
         new_cases = pos- lag_pos, 
         tests_per_case = daily_change/new_cases,
         cum_tests_per_case = total/pos, 
         daily_deaths = -lead(deaths) + deaths, 
         death_rate = daily_deaths/ new_cases, 
         cum_death_rate = deaths/pos * 100,
         deaths_tests = deaths/total * 1000
         ) %>%
  select(-text)

covid_stats

covid_deaths <- covid %>%
  filter(str_detect(text, "[Dd]ie")) %>% 
  mutate(date = floor_date(as.Date(created_at), unit = "days")) %>%
  select(date, text) %>%
  mutate(total = str_extract(text, "total of \\d.*|\\d.*concluded"), 
         pos  = str_extract(text, "\\d.*positive"), 
         total = parse_number(total), 
         pos = parse_number(pos))

covid_deaths %>%
  View()

a <- covid_stats %>% 
  filter(ref_date >= "2020-02-25") %>% 
  ggplot(aes(ref_date, cum_tests_per_case)) +
  geom_col() +
  geom_smooth(se = FALSE) +
  #theme(plot.background = element_blank()) +
  ggtitle("COVID-19 detection rate (tests per postive case per day)") +
  theme(plot.title.position = "plot") +
  theme_minimal() +
  scale_x_date(breaks = "week") +
  geom_text(aes(ref_date, cum_tests_per_case, label = cum_tests_per_case), color = "white", hjust = 3, size = rel(2.7)) +


b <- covid_stats %>%
  filter(ref_date >= "2020-02-25") %>%
  ggplot(aes(date, pos)) +
  geom_col() +
  geom_smooth(se = FALSE, method = "loess") +
  theme(plot.background = element_blank()) 


d <- covid_stats %>%
  filter(ref_date >= "2020-02-25") %>%
  ggplot(aes(date, cum_death_rate)) +
  geom_col() +
  geom_smooth(se = FALSE, method = "gam") +
  theme(plot.background = element_blank()) 


c + (a / b / d ) 

