## scrape world data from wikipedia

library(pacman)
p_load(myScrapers, tidyverse, rvest, coronavirus, googlesheets4)

read_sheet("https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/edit#gid=0")

coronavirus::coronavirus
glimpse(coronavirus)

cases <- read_csv("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

head(cases)


coronavirus %>%
  group_by(date, Country.Region, type) %>%
  summarise(totals = sum(cases)) %>%
  ggplot(aes(date, log(totals), group = type, colour = type)) +
  geom_line(aes(group = type)) +
  facet_wrap(~Country.Region)

url <- "https://en.wikipedia.org/wiki/2019%E2%80%9320_coronavirus_outbreak"

cov <- read_html(url)

table <- cov %>%
  html_table(fill = TRUE) %>%
  .[3] %>%
  data.frame() %>%
  slice(2:nrow(.)) 

View(table)

title <- cov %>%
  html_table(fill = TRUE) %>%
  .[3] %>%
  data.frame() %>%
  slice(1) 

covid_daily <- table %>%
  select(-1) %>%
  rename(country  =1, 
         cases = 2, 
         deaths = 3, 
         recovered = 4) %>%
mutate(data_accessed = Sys.Date())

tail(covid_daily)
  
c <- covid_daily %>%
  slice(2:122) %>%
  mutate(country = reorder(country, cases), cases = parse_number(cases)) %>%
  filter(cases > 29) %>%
  ggplot() +
  geom_col(aes(reorder(country, cases), cases, label = cases)) +
  geom_text(aes(country, cases, label = cases), color = "white", hjust = 3, size = rel(2.7)) +
  coord_flip() +
  scale_y_log10() +
  ggtitle(paste("Country cases as at", Sys.Date()), "(Cases >= 30)") +
  theme(plot.title.position = "plot") +
  labs(x = "")

c

