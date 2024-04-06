library(sf)
library(leaflet)

# ------------------------ ParaCropShapefiles ---------------------------------

paracrop <- st_read(
  "C:/Users/Ryman/OneDrive/Documents/Thesis/ParaCropShapefiles (Master)/ParaCropShapefiles/ParaCropShapefiles.shp")
paracrop <- st_make_valid(paracrop)

# Sample points
n.pts <- 1
test.pts.sf <- st_sample(x = paracrop, size = n.pts) %>% st_sf()
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
  addPolygons(data = paracrop,
              color = 'white',
              weight = 3) %>%
  addScaleBar(options = scaleBarOptions(imperial = FALSE))