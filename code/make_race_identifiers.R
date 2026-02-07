# This script creates a file called 'races.csv', which will be used to help
# identify activities that are races and their associated training block


activity_filenames <- c(
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

# Start with pulling all the info that i want that is already in the data by 
# using the activity ids
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
  activity_filename = activity_filenames
  
  # date_c = c(),
  # date = c(),
  # distance_c = c(),
  # distance_n_miles = c(),
 
  ) |> 
  left_join(app_dat |> select(Filename, date, distance_miles, moving_hours, moving_minutes, moving_seconds, moving_total_c),
            by = c("activity_filename" = "Filename"))
# then once i have all of the columns i want that exist, use those to derive the
# other columns or just make them myself:
 

# Definitions of race priority:
# A: A targeted race with a specific goal time and planning around it, this 
#   at the moment is only marathons
# B: A tune-up race for an A race
# C: A mostly-for fun race with no larger goal or focused training
race_meta <- tribble(
  ~name,                                   ~race_priority, ~race_type, ~block_length_weeks,
  "Ogden Marathon 2024",                   "A",            "Marathon",  20,
  "Tracksmith Twilight 5000",              "C",            "5K",        NA,
  "Charles River Marathon 2024",           "A",            "Marathon",  16,
  "Cambridge Half Marathon",               "C",            "Half",      NA,
  "Cambridge Spring Classic 5K 2025",      "B",            "5K",        NA,
  "James Joyce Ramble 10K 2025",           "B",            "10K",       NA,
  "Ogden Marathon 2025",                   "A",            "Marathon",  16,
  "Miller Mile",                          "C",            "Mile",       NA,
  "Charles River Marathon Invitational 2025","A",         "Marathon",   12,
  "Boston Half-Marathon 2025",             "C",            "Half",      6
)

races_final <- races |> 
  left_join(race_meta) |> 
  mutate(
    block_start_date = date - (block_length_weeks * 7)
  )





