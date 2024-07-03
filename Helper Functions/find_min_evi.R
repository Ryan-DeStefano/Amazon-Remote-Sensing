# This function finds the minimum EVI (Enhanced Vegetation Index) spline fit for each plantation.
# It groups the data by plantation, filters for the minimum spline fit, and ensures uniqueness of the results.

find_min_evi <- function(data) {
  data %>%
    # Group the data by group ID (plantation)
    group_by(group.id) %>%
    
    # Filter the data to keep only the rows with the minimum spline fit for each group
    filter(spl.fit == min(spl.fit)) %>%
    
    # Further filter to keep the earliest year if there are ties for the minimum spline fit
    filter(year == min(year)) %>%
    
    # Select relevant columns and rename 'spl.fit' to 'min_evi'
    select(group.id, min_evi = spl.fit, year) %>%
    
    # Ensure distinct group IDs, keeping all other columns
    distinct(group.id, .keep_all = TRUE)
}