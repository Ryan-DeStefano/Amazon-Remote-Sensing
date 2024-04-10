function (pixel_coords_sf, sample_id_from = "sample_id", chunks_from = NULL, 
          this_chunk_only = NULL, max_chunk_size = 250, drive_export_dir = "lsatTS_export", 
          file_prefix = "lsatTS_export", start_doy = 152, end_doy = 243, 
          start_date = "1984-01-01", end_date = "today", buffer_dist = 0, 
          scale = 30, mask_value = 0) 
{
  tryCatch(rgee::ee_user_info(quiet = T), error = function(e) {
    stop("rgee not initialized!\nPlease intialize\n                rgee. See: https://r-spatial.github.io/rgee/index.html")
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
    stop("Invalid columns specificed for 'sample_id_from': ", 
         sample_id_from)
  }
  bands <- list("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", 
                "SR_B6", "SR_B7", "QA_PIXEL", "QA_RADSAT")
  BAND_LIST <- rgee::ee$List(bands)
  ADDON <- rgee::ee$Image("JRC/GSW1_0/GlobalSurfaceWater")$float()$unmask(mask_value)
  ADDON_BANDLIST <- rgee::ee$List(list("max_extent"))
  ZERO_IMAGE <- rgee::ee$Image(0)$select(list("constant"), 
                                         list("SR_B6"))$selfMask()
  PROPERTIES <- list("CLOUD_COVER", "COLLECTION_NUMBER", "DATE_ACQUIRED", 
                     "GEOMETRIC_RMSE_MODEL", "LANDSAT_PRODUCT_ID", "LANDSAT_SCENE_ID", 
                     "PROCESSING_LEVEL", "SPACECRAFT_ID", "SUN_ELEVATION")
  ls5_1 <- rgee::ee$ImageCollection("LANDSAT/LT05/C02/T1_L2")
  ls5_2 <- rgee::ee$ImageCollection("LANDSAT/LT05/C02/T2_L2")
  ls7_1 <- rgee::ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")
  ls7_2 <- rgee::ee$ImageCollection("LANDSAT/LE07/C02/T2_L2")
  ls8_1 <- rgee::ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")
  ls8_2 <- rgee::ee$ImageCollection("LANDSAT/LC08/C02/T2_L2")
  ALL_BANDS <- BAND_LIST$cat(ADDON_BANDLIST)
  LS_COLL <- ls5_1$merge(ls7_1$merge(ls8_1$merge(ls5_2$merge(ls7_2$merge(ls8_2)))))$filterDate(start_date, 
                                                                                               end_date)$filter(rgee::ee$Filter$calendarRange(start_doy, 
                                                                                                                                              end_doy, "day_of_year"))$map(function(image) {
                                                                                                                                                image = rgee::ee$Algorithms$If(image$bandNames()$size()$eq(rgee::ee$Number(10)), 
                                                                                                                                                                               image, image$addBands(ZERO_IMAGE))
                                                                                                                                                return(image)
                                                                                                                                              })$map(function(image) {
                                                                                                                                                return(image$addBands(ADDON, ADDON_BANDLIST))
                                                                                                                                              })$select(ALL_BANDS)$map(function(image) {
                                                                                                                                                return(image$float())
                                                                                                                                              })
  if (buffer_dist > 0) {
    pixel_coords_sf_buffered <- pixel_coords_sf %>% split(sf::st_drop_geometry(pixel_coords_sf)[, 
                                                                                                sample_id_from]) %>% purrr::map(lsat_get_pixel_centers, 
                                                                                                                                buffer = buffer_dist + 15, pixel_prefix_from = sample_id_from) %>% 
      dplyr::bind_rows()
    pixel_coords_sf_buffered$sample_id_original <- gsub("(.*)_[0-9]*$", 
                                                        "\\1", sf::st_drop_geometry(pixel_coords_sf_buffered)[, 
                                                                                                              "sample_id"])
    names(pixel_coords_sf_buffered)[names(pixel_coords_sf_buffered) == 
                                      "sample_id_original"] <- paste0(sample_id_from, 
                                                                      "_original")
    names(pixel_coords_sf)[names(pixel_coords_sf) == sample_id_from] <- paste0(sample_id_from, 
                                                                               "_original")
    pixel_coords_sf_buffered <- pixel_coords_sf %>% sf::st_drop_geometry() %>% 
      dplyr::full_join(pixel_coords_sf_buffered, .)
    pixel_coords_sf <- pixel_coords_sf_buffered
  }
  if (!is.null(chunks_from)) {
    if (!(chunks_from %in% colnames(pixel_coords_sf))) {
      stop("Invalid colum name specified for chunks_from: ", 
           chunks_from)
    }
  }
  else {
    n_chunks <- floor(nrow(pixel_coords_sf)/max_chunk_size) + 
      1
    pixel_coords_sf$chunk_id <- paste0("chunk_", sort(rep(1:n_chunks, 
                                                          max_chunk_size)))[1:nrow(pixel_coords_sf)]
    chunks_from <- "chunk_id"
  }
  if (!is.null(this_chunk_only)) {
    if (!(this_chunk_only %in% (pixel_coords_sf %>% sf::st_drop_geometry() %>% 
                                as.data.frame() %>% .[, chunks_from]))) {
      stop("Could not find chunk specified: ", this_chunk_only)
    }
    pixel_coords_sf <- pixel_coords_sf[(sf::st_drop_geometry(pixel_coords_sf) %>% 
                                          as.data.frame() %>% .[, chunks_from]) == this_chunk_only, 
    ]
  }
  cat(paste0("Exporting time-series for ", nrow(pixel_coords_sf), 
             " pixels", " in ", length(unique(sf::st_drop_geometry(pixel_coords_sf) %>% 
                                                as.data.frame() %>% .[, chunks_from])), " chunks.\n"))
  task_list <- pixel_coords_sf %>% split(., sf::st_drop_geometry(.)[, 
                                                                    chunks_from]) %>% purrr::map(function(chunk) {
                                                                      cat(paste0("Submitting task to EE for chunk_id: ", sf::st_drop_geometry(chunk) %>% 
                                                                                   as.data.frame() %>% .[1, chunks_from], ".\n"))
                                                                      ee_chunk <- rgee::sf_as_ee(chunk[, c("geometry", sample_id_from, 
                                                                                                           chunks_from)])
                                                                      ee_chunk_export <- ee_chunk$map(function(feature) {
                                                                        return(rgee::ee$ImageCollection$fromImages(list(rgee::ee$Image(list(0, 
                                                                                                                                            0, 0, 0, 0, 0, 0, 0, 0, 0))$select(list(0, 1, 
                                                                                                                                                                                    2, 3, 4, 5, 6, 7, 8, 9), ALL_BANDS)$copyProperties(ls8_1$first())))$merge(LS_COLL$filterBounds(feature$geometry()))$map(function(image) {
                                                                                                                                                                                      return(rgee::ee$Feature(feature$geometry(), 
                                                                                                                                                                                                              image$reduceRegion(rgee::ee$Reducer$first(), 
                                                                                                                                                                                                                                 feature$geometry(), scale))$copyProperties(image, 
                                                                                                                                                                                                                                                                            PROPERTIES)$set(sample_id_from, feature$get(sample_id_from))$set(chunks_from, 
                                                                                                                                                                                                                                                                                                                                             feature$get(chunks_from)))
                                                                                                                                                                                    }))
                                                                      })$flatten()
                                                                      chunk_task <- rgee::ee_table_to_drive(collection = ee_chunk_export, 
                                                                                                            description = paste0("lsatTS_export_", sf::st_drop_geometry(chunk) %>% 
                                                                                                                                   as.data.frame() %>% .[1, chunks_from]), folder = drive_export_dir, 
                                                                                                            fileNamePrefix = paste0(file_prefix, "_", sf::st_drop_geometry(chunk) %>% 
                                                                                                                                      as.data.frame() %>% .[1, chunks_from]), timePrefix = F, 
                                                                                                            fileFormat = "csv")
                                                                      chunk_task$start()
                                                                      return(chunk_task)
                                                                    })
  cat(crayon::green("Done!\n"))
  cat("You can monitor the progress of the task(s)", "using rgee's ee_monitoring() or the GEE WebAPI.\n")
  return(task_list)
}
