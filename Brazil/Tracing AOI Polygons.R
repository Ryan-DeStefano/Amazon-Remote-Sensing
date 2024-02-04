library(sf)
library(leaflet)

# ------------------------ Area Piloto Imagens ---------------------------------

piloto <- st_read(
  "Area_Piloto_Imagens_MAXAR.shp")

# Sample points
n.pts <- 1
test.pts.sf <- st_sample(x = piloto, size = n.pts) %>% st_sf()
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
  addPolygons(data = piloto,
              color = 'white',
              weight = 3) %>%
  addScaleBar(options = scaleBarOptions(imperial = FALSE))

# ------------------------ Municipios magens MAXAR ---------------------------------

municipios <- st_read(
  "Municipios_imagens_MAXAR.shp")

# Sample points
n.pts <- 1
test.pts.sf <- st_sample(x = municipios, size = n.pts) %>% st_sf()
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
  addPolygons(data = municipios,
              color = 'white',
              weight = 3) %>%
  addScaleBar(options = scaleBarOptions(imperial = FALSE))
