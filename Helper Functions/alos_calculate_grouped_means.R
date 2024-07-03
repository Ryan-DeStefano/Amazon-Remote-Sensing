# This function calculates the mean ratio values for each combination of year, month, and group ID 
# from the ALOS dataset, while ensuring that only groups with at least 20 observations are included.

alos_calculate_grouped_means <- function(joined_df, ALOS_df, group_ids) {
  # Nested function to assign group IDs based on the sample ID
  extract_group <- function(sample_id, group_ids) {
    # Extract the numeric part of the sample ID
    group_number <- as.numeric(sub("S_", "", sample_id))
    # Calculate the group index using the group number and length of group_ids
    group_index <- ((group_number - 1) %/% 30) %% length(group_ids) + 1
    # Return the corresponding group ID
    return(group_ids[group_index])
  }
  
  # Filter the joined dataset for records from the year 2015 or later
  alos <- joined_df %>% 
    filter(year >= 2015)
  
  # Extract the vector of group IDs from the filtered dataset
  group_id_vector <- alos$group.id
  
  # Assign group IDs to the ALOS dataset and filter out rows with NA values in the ratio column
  ALOS_df <- ALOS_df %>%
    mutate(group.id = extract_group(sample_id, group_id_vector)) %>% 
    filter(!is.na(ratio))
  
  # Calculate the mean ratio value for each combination of year, month, and group ID
  means <- ALOS_df %>%
    group_by(year, month, group.id) %>%
    summarize(ratio = mean(ratio, na.rm = TRUE))
  
  # Count the number of observations (means) for each group ID
  group_counts <- means %>%
    group_by(group.id) %>%
    summarize(count = n())
  
  # Filter out groups with fewer than 20 observations
  filtered_groups <- group_counts %>%
    filter(count >= 20)
  
  # Filter the means dataframe to include only the filtered groups
  means <- means %>%
    filter(group.id %in% unique(filtered_groups$group.id))
  
  # Return the filtered means dataframe
  return(means)
}