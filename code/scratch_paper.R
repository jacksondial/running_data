activity_sel <- activity_dat2 |> 
  select(Activity.Date, distance_miles, Moving.Time, Elapsed.Time, moving_minutes,moving_seconds, moving_total_c, Average.Speed, 
         avg_speed_mph, avg_pace_mile, avg_speed_mph,
         contains("avg_pace"))



##### Lets see what the coros data looks like

remotes::install_github("grimbough/FITfileR")

