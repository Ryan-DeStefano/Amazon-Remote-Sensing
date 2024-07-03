# This function samples points from polygons and stores the sampled points
# in a tibble, which is then converted to an sf object for spatial analysis.

sample_points_from_polygons <- function(df, n_pts_per_polygon) {
  # Create an empty tibble to store the sampled points
  test <- tibble(geometry = vector("list", sum(n_pts_per_polygon)),
                 sample_id = character(sum(n_pts_per_polygon)))
  
  # Initialize the sample ID counter
  sample_counter <- 1
  
  # Loop through each region (polygon) to sample points
  for (i in 1:nrow(df)) {
    # Sample points from the i-th polygon, specifying the number of points to sample
    pts <- st_sample(x = df[i, ], size = n_pts_per_polygon[i])
    
    # Convert the sampled points to their WKT (Well-Known Text) representation
    point_wkt <- st_as_text(pts)
    
    # Determine the indices in the tibble to store the sampled points
    start_index <- sample_counter
    end_index <- sample_counter + n_pts_per_polygon[i] - 1
    
    # Store the WKT representation of the sampled point geometries in the tibble
    test$geometry[start_index:end_index] <- point_wkt
    # Store the sample IDs in the tibble
    test$sample_id[start_index:end_index] <- paste0('S_', start_index:end_index)
    
    # Increment the sample ID counter to the next available index
    sample_counter <- end_index + 1
  }
  
  # Convert the tibble to an sf object, specifying the column containing the WKT geometries and the CRS (Coordinate Reference System)
  test_sf <- st_as_sf(test, wkt = "geometry", crs = 4326)
  
  # Return the sf object containing the sampled points
  return(test_sf)
}