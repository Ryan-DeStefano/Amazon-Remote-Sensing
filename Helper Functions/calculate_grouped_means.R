# This function processes the groups and calculates the mean EVI (Enhanced Vegetation Index) values
# for each combination of day of the year (doy), year, and group ID.

calculate_grouped_means <- function(lsat_dt, n_pts_per_polygon) {
  # Assign the group IDs to the dataset by using the sample IDs and the number of points per polygon
  groups <- lsat_dt %>%
    mutate(group.id = extract_group(sample.id, n_pts_per_polygon))
  
  # Adjust the group ID by subtracting 1 to fix the magnitude
  groups$group.id <- groups$group.id - 1
  
  # Group the dataset by day of the year (doy), year, and group ID, then calculate the mean EVI for each group
  grouped_means <- groups %>%
    group_by(doy, year, group.id) %>%
    summarize(evi = mean(evi, na.rm = TRUE), .groups = 'drop')
  
  # Return the dataframe containing the mean EVI values for each group
  return(grouped_means)
}