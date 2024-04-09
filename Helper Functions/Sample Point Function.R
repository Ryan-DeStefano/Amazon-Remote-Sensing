num_points_vector <- c()

for (i in 1:nrow(top_10_para)){
  
  estimated <- top_10_para$estimated_points[i]
  
  if (estimated < 10){
    num_points <- 1
  }
  
  else if (estimated > 100){
    num_points <- min(20, as.integer(.01*estimated))
  }
  
  else {
    num_points <- max(1, as.integer(estimated*.01))
  }
  
  num_points_vector <- c(num_points_vector, num_points)
  
}

