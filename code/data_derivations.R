#####
## This code take the data and makes a row for every day that there is no data
## available for, ie when I did not run
#####
install.packages("zoo")
daily_dat <- app_dat %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    daily_miles = sum(distance_miles, na.rm = TRUE),
    daily_time  = sum(elapsed_minutes, na.rm = TRUE),
    daily_runs  = n(),
    avg_pace    = weighted.mean(avg_pace_mile, distance_miles, na.rm = TRUE)
  ) %>%
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(daily_miles = 0, daily_time = 0, daily_runs = 0)
           ) |> 
  arrange(date) %>%
  ### Add load metrtics
  mutate(
    acute_load   = zoo::rollmean(daily_miles, 7, fill = NA, align = "right"),
    chronic_load = zoo::rollmean(daily_miles, 28, fill = NA, align = "right"),
    load_ratio   = acute_load / chronic_load
  ) |> 
  ### Training Structure metrics
  mutate(
    # monotony is giving -Inf for some and it is causing issues for readiness calculation
    # monotony = acute_load / zoo::rollapply(daily_miles, 7, sd, fill = NA, align = "right"),
    # These 2 features come from Foster's training monotony
    # "How repetitive was my last week?"
    rolling_sd = zoo::rollapply(daily_miles, 7, sd, fill = NA, align = "right"),
    monotony = ifelse(rolling_sd == 0, NA, acute_load / rolling_sd),
    
    # this is also soemthing from Foster:
    # "How big was the week and how stressful was the structure?"
    strain   = acute_load * monotony,
    # "How fast is training increasing week to week"
    ramp_rate = acute_load / dplyr::lag(acute_load, 7)
  ) |> 
  ### Readiness
  mutate(
    readiness = scale(chronic_load) -
      scale(load_ratio) +
      # The reason we negate monotony is to flip the direction, and indicate that
      # high monotony is bad according to Foster research
      scale(-monotony),
    insight = case_when(
      load_ratio > 1.5 ~ "High ramp risk",
      chronic_load > quantile(chronic_load, .9, na.rm=TRUE) ~ "Peak training load",
      readiness > 1 ~ "High readiness window",
      TRUE ~ "Normal training load"
    ),
    year = lubridate::year(date)
  )


# 
# output$status_table <- renderTable({
#   daily_dat %>%
#     arrange(desc(date)) %>%
#     select(date, daily_miles, acute_load, chronic_load, readiness, insight) %>%
#     head(14)
# })


