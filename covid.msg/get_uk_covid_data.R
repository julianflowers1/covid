get_uk_covid_data <- function(){
  
  ## extract daily tests, positives and deaths from DHSC daily tweets
  
  covid <- get_timeline("DHSCgovuk", n = 3200, retryonratelimit = TRUE )
  
  
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
}