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
  
  bands <- list("HH", "HV")
  BAND_LIST <- rgee::ee$List(bands)
  
  ALOS_COLL <- rgee::ee$ImageCollection("JAXA/ALOS/PALSAR/YEARLY/SAR_EPOCH")
  ALL_BANDS <- BAND_LIST
  
  ADDON <- rgee::ee$Image(0)$float()$mask(mask_value)
  
  # Function to apply on each feature within the mapping operation
  mapped_function <- function(feature) {
    tryCatch({
      image <- ALOS_COLL$filterBounds(feature$geometry())$
        map(function(image) {
          image = rgee::ee$Algorithms$If(image$bandNames()$size()$eq(rgee::ee$Number(2)), 
                                         image, image$addBands(ADDON))
          return(image)
        })$
        map(function(image) {
          return(image$float())
        })$
        first() # We take the first image since we're using reduceRegion, which expects a single image
      
      properties <- list("ASCENDING_DESCENDING_FLAG", "CLOUD_COVERAGE_ASSESSMENT", "FREQUENCY_BAND", 
                         "PRODUCT_TYPE", "relOrbitNumber_start")
      result <- rgee::ee$Feature(feature$geometry(), 
                                 image$reduceRegion(reducer = rgee::ee$Reducer$first(), 
                                                    geometry = feature$geometry(), 
                                                    scale = scale))
      
      # Copy properties and set sample_id_from and chunks_from
      result <- result$copyProperties(image, properties)
      result <- result$set(sample_id_from, feature$get(sample_id_from))
      result <- result$set(chunks_from, feature$get(chunks_from))
      
      return(result)
    }, error = function(e) {
      message("Error processing chunk:", conditionMessage(e))
      NULL
    })
  }
  
  if (buffer_dist > 0) {
    # Apply buffer to pixel_coords_sf
  }
  
  if (!is.null(chunks_from)) {
    if (!(chunks_from %in% names(pixel_coords_sf))) {
      stop("Invalid columns specified for 'chunks_from': ", 
           chunks_from)
    }
  }
  else {
    n_chunks <- floor(nrow(pixel_coords_sf) / max_chunk_size) + 1
    pixel_coords_sf$chunk_id <- paste0("chunk_", sort(rep(1:n_chunks, 
                                                          max_chunk_size)))[1:nrow(pixel_coords_sf)]
    chunks_from <- "chunk_id"
  }
  
  if (!is.null(this_chunk_only)) {
    if (!(this_chunk_only %in% unique(pixel_coords_sf[[chunks_from]]))) {
      stop("Could not find chunk specified: ", this_chunk_only)
    }
    pixel_coords_sf <- pixel_coords_sf[pixel_coords_sf[[chunks_from]] == this_chunk_only, ]
  }
  
  cat(paste0("Exporting time-series for ", nrow(pixel_coords_sf), 
             " pixels", " in ", length(unique(pixel_coords_sf[[chunks_from]])), " chunks.\n"))
  
  # Apply the mapped function to each chunk
  task_list <- pixel_coords_sf %>%
    split(., pixel_coords_sf[[chunks_from]]) %>%
    purrr::map(function(chunk) {
      cat(paste0("Submitting task to EE for chunk_id: ", chunk[[chunks_from]][1], ".\n"))
      ee_chunk <- rgee::sf_as_ee(chunk[, c("geometry", sample_id_from, chunks_from)])
      ee_chunk_export <- ee_chunk$map(mapped_function) # Apply mapped_function
      
      # Convert to table and export to Drive
      chunk_task <- tryCatch({
        rgee::ee_table_to_drive(collection = ee_chunk_export, 
                                description = paste0("alosTS_export_", chunk[[chunks_from]][1]), 
                                folder = drive_export_dir, 
                                fileNamePrefix = paste0(file_prefix, "_", chunk[[chunks_from]][1]), 
                                timePrefix = FALSE, 
                                fileFormat = "csv")$start()
      }, error = function(e) {
        message("Error starting task:", conditionMessage(e))
        NULL
      })
      
      return(chunk_task)
    })
  
  cat(crayon::green("Done!\n"))
  cat("You can monitor the progress of the task(s) using rgee's ee_monitoring() or the GEE WebAPI.\n")
  return(task_list)
}
