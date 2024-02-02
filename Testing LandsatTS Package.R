# Install LandsatTS Package
install.packages("devtools")
devtools::install_github("logan-berner/LandsatTS", build_vignettes = TRUE)

# Load packages for data handling etc.
library(sf)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(rgee)
library(lwgeom)

# Load LandsatTS package
library(LandsatTS)

# Install python environment (only need to do once ever)
ee_install(py_env = "rgee") 
remotes::install_github("r-spatial/rgee@main", force = TRUE)

# Intialize the Earth Engine with rgee
rgee::ee_install_upgrade()
ee_Initialize()

#----------------- Testing LandsatTS functions on polygon from github ----------

# Specify a polygon
test_poly <- st_polygon(
  list(matrix(c(-138.90125, 69.58413,
                -138.88988, 69.58358,
                -138.89147, 69.58095,
                -138.90298, 69.57986,
                -138.90125, 69.58413),
              ncol = 2, byrow = TRUE)))

# Save as polygon object
test_poly_sf <- st_sfc(test_poly, crs = 4326) %>% st_sf()

# Can select a number of pixels to randomly sample from the polygon
n.pts <- 15
test.pts.sf <- st_sample(x=test_poly_sf, size=n.pts) %>% st_sf()
test.pts.sf$sample_id <- paste0('S_', 1:n.pts)

# Or use lsat_get_pixel_centers to get all pixels within the polygon
pixel_list_test_poly <- lsat_get_pixel_centers(test_poly_sf, plot_map = TRUE)

# Export time-series using lsat_export_ts(), will be saved to a lsat folder in google drive
task_list <- lsat_export_ts(test.pts.sf)

# Read in the exported file
lsat.dt <- do.call("rbind", lapply("lsatTS_export_chunk_1.csv", fread))

# Format the data
lsat.dt <- lsat_format_data(lsat.dt)

# Only keep observations where there was clear sky etc
lsat.dt <- lsat_clean_data(lsat.dt, geom.max = 15, cloud.max = 80, sza.max = 60, filter.cfmask.snow = T, filter.cfmask.water = T, filter.jrc.water = T)

# lsat.dt <- lsat_neighborhood_mean(lsat.dt) Needed if pixel is 3x3

# Returns plot with number of observationms collected from each satellite
data.summary.dt <- lsat_summarize_data(lsat.dt)
data.summary.dt

# Retrieve NDVI values
lsat.dt <- lsat_calc_spectral_index(lsat.dt, si = 'ndvi')

# Cross-calibrate NDVI among sensors using an approach based on Random Forest machine learning
# Note: Need lots of observations for this to work
lsat.dt <- lsat_calibrate_rf(lsat.dt, 
                             band.or.si = 'ndvi', 
                             doy.rng = 151:242, 
                             min.obs = 1, 
                             frac.train = 0.75, 
                             overwrite.col = T, 
                             write.output = F)

# If needed, then manually overwrite the uncalibrated data with the calibrated data, then drop the duplicate column 
# lsat.dt <- lsat.dt[, ndvi := ndvi.xcal]
# lsat.dt <- lsat.dt[, ndvi.xcal := NULL)






