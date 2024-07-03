# This function assigns a group (essentially a plantation) to each of the sample points
# based on their sample IDs and the lengths of the groups.

extract_group <- function(sample_id, group_lengths) {
  
  # Extract the numeric part of the sample IDs by removing the "S_" prefix
  group_numbers <- as.numeric(sub("S_", "", sample_id))
  
  # Calculate the cumulative lengths of the groups
  cumulative_lengths <- c(0, cumsum(group_lengths))
  
  # Initialize a vector to store the higher order group for each sample point
  higher_order_group <- rep(NA, length(sample_id))
  
  # Loop through each cumulative length to assign groups
  for (i in 1:length(cumulative_lengths)) {
    if (i == 1) {
      # Assign the first group to sample points with numbers less than or equal to the first cumulative length
      higher_order_group[group_numbers <= cumulative_lengths[i]] <- i
    } else {
      # Assign the group to sample points with numbers within the range of the current and previous cumulative lengths
      higher_order_group[group_numbers > cumulative_lengths[i - 1] & group_numbers <= cumulative_lengths[i]] <- i
    }
  }
  
  # Return the vector containing the group assignments for each sample point
  return(higher_order_group)
}