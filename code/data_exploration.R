# Read in the data from strava

pacman::p_load(
  dplyr,
  lubridate,
  ggplot2
)

activity_dat <- read.csv("data/strava/activities.csv") |> 
  select(where(~ !all(is.na(.))), -Athlete.Weight) |> # Remove all columns that are all missing values
  filter(Activity.Type == "Run",
         Elapsed.Time < 100000)

labelled::generate_dictionary(activity_dat)


activity_dat2 <- activity_dat |> 
  mutate(date = mdy_hms(Activity.Date),
         day = day(date),
         month = month(date),
         year = year(date),
         distance_miles = Distance * 0.62137,
         elapsed_minutes = Elapsed.Time / 60,
         minutes = trunc(elapsed_minutes),
         seconds = Elapsed.Time%%60,
         week = lubridate::week(date))

saveRDS(activity_dat2, "data/activity_dat2.RDS")
