# This function calculates the number of sample points to extract from each plantation
# based on the estimated number of points provided in the dataset
# Needs the input dataset to have an estimated_points column

num_sample_points <- function(dataset) {
  
  # Initialize an empty vector to store the number of sample points for each plantation
  num_points_vector <- c()
  
  # Loop through each row in the dataset
  for (i in 1:nrow(dataset)) {
    
    # Get the estimated number of points for the current plantation
    estimated <- dataset$estimated_points[i]
    
    # Determine the number of sample points based on the estimated value
    if (estimated < 10) {
      # If the estimated number of points is less than 10, set the number of sample points to 1
      num_points <- 1
    } else if (estimated > 200) {
      # If the estimated number of points is greater than 200, set the number of sample points to 20
      num_points <- 20
    } else {
      # Otherwise, set the number of sample points to 10% of the estimated points, with a minimum of 1
      num_points <- max(1, as.integer(estimated * 0.1))
    }
    
    # Append the calculated number of sample points to the vector
    num_points_vector <- c(num_points_vector, num_points)
    
  }
  
  # Return the vector containing the number of sample points for each plantation
  return(num_points_vector)
}

#Brazil
estmiated_hours_to_export <- sum(num_sample_points(top_10_para))/2/60
estmiated_hours_to_export

#Peru
estmiated_hours_to_export <- sum(num_sample_points(top_10_peru))/2/60
estmiated_hours_to_export
