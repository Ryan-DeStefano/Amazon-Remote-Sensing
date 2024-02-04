library(sf)
library(leaflet)

# ------------------------ maxar peru aoi ---------------------------------

peru <- st_read("maxar_peru_aoi.shp")

# Check the current coordinate reference system
st_crs(peru)

# Transform to WGS84 (EPSG:4326)
peru <- st_transform(peru, crs = 4326)

# Number of sample points
n.pts <- 1

# Sample points
test.pts.sf <- st_sample(x = peru, size = n.pts) %>% st_sf()
test.pts.sf$sample_id <- paste0('S_', 1:n.pts)

# Create Leaflet map
leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(data = test.pts.sf, 
                   color = 'white',
                   opacity = 0.9,
                   fillColor = 'fuchsia',
                   fillOpacity = 0.75,
                   weight = 1,
                   radius = 5) %>%
  addPolygons(data = peru,
              color = 'white',
              weight = 3) %>%
  addScaleBar(options = scaleBarOptions(imperial = FALSE))

