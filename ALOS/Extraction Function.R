ALOS <- function (pixel_coords_sf, sample_id_from = "sample_id", chunks_from = NULL, 
                  this_chunk_only = NULL, max_chunk_size = 250, drive_export_dir = "alosTS_export", 
                  file_prefix = "alosTS_export", start_date = "1984-01-01", end_date = "today", 
                  buffer_dist = 0, scale = 100, mask_value = 0) 
{
  tryCatch(rgee::ee_user_info(quiet = TRUE), error = function(e) {
    stop("rgee not initialized!\nPlease initialize rgee. See: https://r-spatial.github.io/rgee/index.html")
  })
  
  if (end_date == "today") 
    end_date <- as.character(Sys.Date())
  
  sf::sf_use_s2(FALSE)
  
  if (!("sfc_POINT" %in% class(sf::st_geometry(pixel_coords_sf)))) {
    stop("Invalid argument supplied for pixel_coords_sf!\n", 
         "Please supply an object with 'sfc_POINT' geometries.")
  }
  
  if (length(sf::st_geometry(pixel_coords_sf)) > 1e+05) {
    cat(crayon::red("Warning: Extraction requested for more than 100000 point locations!\n"))
    warning("Extraction requested for more than 100000 point locations!")
    answer <- readline("Would you like to continue nonetheless? (not recommended!) [y/n]: ")
    if (answer == "n") {
      cat("Okay, stopping extraction.\n")
      return(NULL)
    }
    else if (answer == "y") {
      cat("Okay, continuing...\n")
    }
    else {
      cat("Invalid answer, stopping extraction.\n")
      return(NULL)
    }
  }
  
  if ((sample_id_from != "sample_id") & !(sample_id_from %in% 
                                          names(pixel_coords_sf))) {
    stop("Invalid columns specified for 'sample_id_from': ", 
         sample_id_from)
  }
  
  # Define bands and image collection
  bands <- list("HH", "HV")
  ALOS_COLL <- rgee::ee$ImageCollection("JAXA/ALOS/PALSAR/YEARLY/SAR_EPOCH")
  
  # Iterate over each chunk
  task_list <- list()
  chunk_id <- 1
  cat(paste0("Exporting time-series for ", nrow(pixel_coords_sf), 
             " pixels", " in ", length(unique(pixel_coords_sf[[chunks_from]])), " chunks.\n"))
  
  # Iterate over each chunk
  chunk <- pixel_coords_sf
  cat(paste0("Submitting task to EE for chunk_id: ", chunk_id, ".\n"))
  
  # Convert chunk to ee.FeatureCollection
  ee_chunk <- rgee::sf_as_ee(chunk[, c("geometry", sample_id_from, chunks_from)])
  
  # Apply mapped function to the chunk
  ee_chunk_export <- ee_chunk$map(mapped_function)
  
  # Convert to table and export to Drive
  chunk_task <- rgee::ee_table_to_drive(collection = ee_chunk_export, 
                                        description = paste0("alosTS_export_", chunk[[chunks_from]][1]), 
                                        folder = drive_export_dir, 
                                        fileNamePrefix = paste0(file_prefix, "_", chunk[[chunks_from]][1]), 
                                        timePrefix = FALSE, 
                                        fileFormat = "csv")
  
  chunk_task$start()
  
  task_list[[chunk_id]] <- chunk_task
  
  cat(crayon::green("Done!\n"))
  cat("You can monitor the progress of the task(s) using rgee's ee_monitoring() or the GEE WebAPI.\n")
  return(task_list)
}
