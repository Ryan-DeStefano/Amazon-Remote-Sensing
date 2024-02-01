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

ee_install(py_env = "rgee") # It is just necessary once!

remotes::install_github("r-spatial/rgee@main", force = TRUE)

# Intialize the Earth Engine with rgee
rgee::ee_install_upgrade()
ee_Initialize()

# Specify a region 
test_poly <- st_polygon(
  list(matrix(c(-138.90125, 69.58413,
                -138.88988, 69.58358,
                -138.89147, 69.58095,
                -138.90298, 69.57986,
                -138.90125, 69.58413),
              ncol = 2, byrow = TRUE)))
test_poly_sf <- st_sfc(test_poly, crs = 4326) %>% st_sf()

n.pts <- 50

test.pts.sf <- st_sample(x=test_poly_sf, size=n.pts) %>% st_sf()

# Use lsat_get_pixel_centers to retrieve pixel centers and plot to a file that can be added to this documentation.
# We set plot_map to a file path (or just TRUE) to view 
pixel_list_test_poly <- lsat_get_pixel_centers(test_poly_sf, plot_map = TRUE)

## Ge pixel centers for multiple regions
# Create multi-polygon sf
ellesmere <- st_polygon(list(matrix(c(-75.78526, 78.86973, 
                                      -75.78526, 78.87246,
                                      -75.77116, 78.87246, 
                                      -75.77116, 78.86973, 
                                      -75.78526, 78.86973),
                                    ncol = 2, byrow = TRUE)))
zackenberg <- st_polygon(list(matrix(c(-20.56254, 74.47469, 
                                       -20.56254, 74.47740,
                                       -20.55242, 74.47740, 
                                       -20.55242, 74.47469,
                                       -20.56254, 74.47469), 
                                     ncol = 2, byrow = TRUE)))
toolik <- st_polygon(list(matrix(c(-149.60686, 68.62364, 
                                   -149.60686, 68.62644, 
                                   -149.59918, 68.62644, 
                                   -149.59918, 68.62364, 
                                   -149.60686, 68.62364), 
                                 ncol = 2, byrow = TRUE)))
test_regions_sf <- st_sfc(ellesmere, zackenberg, toolik, crs = 4326) %>% st_sf() %>%
  mutate(region = c("ellesmere", "zackenberg", "toolik"))

# Split and map lsat_get_pixel_centers using dplyr and purrr wihout plotting
pixel_list <- test_regions_sf %>%
  split(.$region) %>%
  map(lsat_get_pixel_centers,
      pixel_prefix_from = "region") %>%
  bind_rows()

# Let's look at the returned sf object:
pixel_list

# Generate test points
test_points_sf <- st_sfc(sf::st_point(c(-149.6026, 68.62574)),
                         sf::st_point(c(-149.6003, 68.62524)),
                         sf::st_point(c(-75.78057, 78.87038)),
                         sf::st_point(c(-75.77098, 78.87256)),
                         sf::st_point(c(-20.56182, 74.47670)),
                         sf::st_point(c(-20.55376, 74.47749)), crs = 4326) %>%
  st_sf() %>%
  mutate(sample_id = c("toolik_1",
                       "toolik_2",
                       "ellesmere_1",
                       "ellesmere_1",
                       "zackenberg_1",
                       "zackenberg_2"),
         region = c("toolik", "toolik",
                    "ellesmere", "ellesmere",
                    "zackenberg", "zackenberg"))
# Export time-series using lsat_export_ts()
task_list <- lsat_export_ts(test_points_sf)

## Further examples:
# Export time-series using with a chunk size of 2
# task_list <- lsat_export_ts(test_points_sf, max_chunk_size = 2)

# Export time-series in chunks by column
# task_list <- lsat_export_ts(test_points_sf, chunks_from = "region")

lsat.dt <- do.call("rbind", lapply("lsatTS_export_chunk_1.csv", fread))

# setnames(lsat.dt, 'sample_id','sample.id') Don't need

lsat.dt <- lsat_format_data(lsat.dt)

lsat.dt <- lsat_clean_data(lsat.dt, geom.max = 15, cloud.max = 80, sza.max = 60, filter.cfmask.snow = T, filter.cfmask.water = T, filter.jrc.water = T)

# lsat.dt <- lsat_neighborhood_mean(lsat.dt) Don't think I need

data.summary.dt <- lsat_summarize_data(lsat.dt)
data.summary.dt

lsat.dt <- lsat_calc_spectral_index(lsat.dt, si = 'ndvi')

# Cross-calibrate NDVI among sensors using an approach based on Random Forest machine learning
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


# ------------------------------------------------------------------------------

kml_data <- st_read(dsn = "para_boundary.kml")






