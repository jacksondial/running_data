# This script creates a file called 'races.csv', which will be used to help
# identify activities that are races and their associated training block


activity_ids <- c(
  "activities/12216806825.fit.gz", # ogden 2024
  "activities/12774395760.fit.gz", # tracksmith twilight 5000
  "activities/13176936233.fit.gz", # CRM 1
  "activities/13662590277.fit.gz", # cambridge half
  "activities/14976741299.fit.gz", # cambridge classic
  "activities/15264580215.fit.gz", # james joyce
  "activities/15482005847.fit.gz", # ogden 2025
  "activities/16016511436.fit.gz", # Miller mile
  "activities/16805210241.fit.gz", # CRMI 2
  "activities/17491618524.fit.gz" # boston half
)


races <- data.frame(
  name = c(
    # "Run for the Animals",
    "Ogden Marathon 2024",
    "Tracksmith Twilight 5000",
    "Charles River Marathon 2024",
    "Cambridge Half Marathon",
    "Cambridge Spring Classic 5K 2025",
    "James Joyce Ramble 10K 2025",
    "Ogden Marathon 2025",
    "Miller Mile", # should I include this?
    "Charles River Marathon Invitational 2025",
    "Boston Half-Marathon 2025"
    ),
  date_c = c(
    # "May 7, 2023",
    "May 18, 2024",
    "July 25, 2024",
    "September 8, 2024",
    
  ),
  date = c(),
  acitivty_id = c(),
  distance_c = c(),
  distance_n_miles = c(),
  race_priority = c("A", "B"),
  block_length = c(),
  block_start_date = c(),
  block_end_date = c(),
  finish_time_c = c(),
  finish_time_minutes = c(),
  
  
  )