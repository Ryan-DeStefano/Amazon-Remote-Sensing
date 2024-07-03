# This function reads a shapefile, makes it valid by removing any invalid geometries,
# transforms the coordinate reference system (CRS), and optionally removes invalid geometries 
# if the 'make_valid' parameter is set to TRUE.

read_shapefile <- function(filepath, crs, make_valid = F) {
  
  # Read in the shapefile from the specified filepath
  file <- st_read(filepath)
  
  # Make the shapefile geometries valid (removes issues like crossing polygon lines)
  file <- st_make_valid(file)
  
  # Transform the coordinate reference system to the specified CRS (e.g., WGS84 with EPSG:4326)
  file <- st_transform(file, crs = crs)
  
  # If make_valid is set to TRUE, filter out any invalid geometries
  if (make_valid == T) {
    file <- file[st_is_valid(file),]
  }
  
  # Return the processed shapefile
  return(file)
}