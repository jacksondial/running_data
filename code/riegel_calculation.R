t1 <- 5
d1 <- "1 Mile"
d2 <- "5k"

distance_to_miles <- function(distance) {
  distance_map <- c(
    "1 Mile" = 1,
    "5K" = 3.1,
    "10K" = 6.2,
    "Half-Marathon" = 13.1,
    "Marathon" = 26.2
  )
  as.numeric(distance_map[distance])
}

riegel_predict_minutes <- function(t1, d1, d2) {
  dist_1 <- distance_to_miles(d1)
  dist_2 <- distance_to_miles(d2)
  t1 * (dist_2 / dist_1)^1.06
}

format_duration_compact <- function(minutes) {
  total_seconds <- round(minutes * 60)
  hrs <- total_seconds %/% 3600
  mins <- (total_seconds %% 3600) %/% 60
  secs <- total_seconds %% 60
  sprintf("%02d:%02d:%02d", hrs, mins, secs)
}

riegel_function <- function(t1, d1, d2){
  output <- format_duration(riegel_predict_minutes(t1, d1, d2))
  output
}



# riegel_calculation(5, 1, 3.1)
# 
# riegel_calculation(85, 13.1, 26.2)
