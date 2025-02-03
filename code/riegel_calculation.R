t1 <- 5
d1 <- "1 Mile"
d2 <- "5k"

riegel_function <- function(t1, d1, d2){
  convert_distance <- function(distance) {
    # Use a named vector to map string distances to numeric values
    distance_map <- c(
      "1 Mile" = 1,
      "5K" = 3.1,
      "10K" = 6.2,  # Corrected value for 10k (6.2 miles)
      "Half-Marathon" = 13.1,
      "Marathon" = 26.2
    )
    return(distance_map[distance])
  }
  d1 <- convert_distance(d1)
  d2 <- convert_distance(d2)
  print("jackson")
  print(d1)
  print(d2)

  output <- format_duration(t1 * (d2/d1)^1.06)
  output
}



# riegel_calculation(5, 1, 3.1)
# 
# riegel_calculation(85, 13.1, 26.2)

