num_sample_points <- function(dataset){ 
  
  num_points_vector <- c()
  
  for (i in 1:nrow(dataset)){
    estimated <- dataset$estimated_points[i]
    
    if (estimated < 10){
      num_points <- 1
    }
    else if (estimated > 200){
      num_points <- 20
    }
    else {
      num_points <- max(1, as.integer(estimated*.1))
    }
  
    num_points_vector <- c(num_points_vector, num_points)
  }
  
  return(num_points_vector)
}

# Brazil
estmiated_hours_to_export <- sum(num_sample_points(top_10_para))/2/60
estmiated_hours_to_export

#Peru
estmiated_hours_to_export <- sum(num_sample_points(top_10_peru))/2/60
estmiated_hours_to_export
