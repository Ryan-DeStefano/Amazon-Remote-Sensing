library(sf)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(rgee)
library(lwgeom)
library(leaflet)
library(LandsatTS)

rgee::ee_install_upgrade()
ee_Initialize()

# ------------------------ Peru ---------------------------------
set.seed(27)

main <- st_read(
  "04 Dataset_Ucayali_Palm_V2.shp")

# Check the current coordinate reference system
st_crs(main)

# Transform to WGS84 (EPSG:4326)
main <- st_transform(main, crs = 4326)

# Number of sample points
n.pts <- 500

# Sample points
test.pts.sf <- st_sample(x = main, size = n.pts) %>% st_sf()
test.pts.sf$sample_id <- paste0('S_', 1:n.pts)

# Creating the Leaflet map
leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(data = test.pts.sf, 
                   color = 'white',
                   opacity = 0.9,
                   fillColor = 'fuchsia',
                   fillOpacity = 0.75,
                   weight = 1,
                   radius = 5) %>%
  addPolygons(data = main,
              color = 'white',
              weight = 3) %>%
  addScaleBar(options = scaleBarOptions(imperial = FALSE))