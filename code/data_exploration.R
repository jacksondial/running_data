# Read in the data from strava

pacman::p_load(
  dplyr,
  lubridate,
  ggplot2
)

activity_dat <- read.csv("data/strava/October_19_2025/activities.csv") |> 
  select(where(~ !all(is.na(.))), -Athlete.Weight) |> # Remove all columns that are all missing values
  filter(Activity.Type == "Run",
         Elapsed.Time < 100000)

labelled::generate_dictionary(activity_dat)


activity_dat2 <- activity_dat |> 
  select(where(~ !all(is.na(.)))) |> # removes any columns that are all NA (only 1)
  mutate(date = mdy_hms(Activity.Date),
         day = day(date),
         month = month(date),
         year = year(date),
         distance_miles = Distance * 0.62137,
         elapsed_minutes = Elapsed.Time / 60,
         moving_minutes = trunc(Moving.Time / 60),
         moving_seconds = Moving.Time %% 60,
         moving_total_c = paste0(moving_minutes, ":", moving_seconds),
         minutes = trunc(elapsed_minutes),
         seconds = Elapsed.Time%%60,
         week = lubridate::week(date),
         # The difference in week here is that this one below starts and ends on Mondays
         # which is important for plotting weekly mileage since i structure my weekly
         # schedule that way and not based on the actual week number of the year
         week_monday = isoweek(date),
         year_monday = isoyear(date),
         # Need to make a speed for MPH, then use that to calculate pace in mph
         # avg_speed_mph = Average.Speed / 1.609344,
         avg_speed_mph = (distance_miles / Moving.Time) * 3600,
         avg_pace_mile = 60 / avg_speed_mph,
         avg_pace_mile_min = trunc(avg_pace_mile),
         avg_pace_mile_sec = trunc(avg_pace_mile %% 1 * 60),
         avg_pace_mile_c = case_when(
           nchar(avg_pace_mile_sec) == 2 ~ paste0(avg_pace_mile_min, ":", avg_pace_mile_sec),
           nchar(avg_pace_mile_sec) == 1 ~ paste0(avg_pace_mile_min, ":0", avg_pace_mile_sec)
         )
         # avg_pace_min = 
         # avg_speed_test = 60/Average.Speed
         )

saveRDS(activity_dat2, "data/activity_dat2.RDS")
