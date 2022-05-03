library(tidyRSS)
require(leaflet)
library(dplyr)
require(sp)
require(sf)
require(htmlwidgets)

# Read GML file
file <- 'Data/A.ES.SDGC.BU.09900.building.gml' #Burgos
# file <- 'Data/A.ES.SDGC.BU.28022.building.gml' #Boadilla
# file <- 'Data/A.ES.SDGC.BU.09137.building.gml' #Frias

part <- 'Data/A.ES.SDGC.BU.09900.buildingpart.gml' #Burgos
# part <- 'Data/A.ES.SDGC.BU.28022.buildingpart.gml' #Boadilla
# part <- 'Data/A.ES.SDGC.BU.09137.buildingpart.gml' # Frias



# Clean the buildings df
# ----------------------------------------------
buildings <- sf::st_read(file)
buildings$year_beginning = substr(buildings$beginning,1,4)
buildings$year_beginning = as.integer(buildings$year_beginning)

buildings$use <- rep(NA, nrow(buildings))
buildings[!is.na(buildings$currentUse) & buildings$currentUse == "1_residential", ][, "use"] <- "Residencial"
buildings[!is.na(buildings$currentUse) & buildings$currentUse == "2_agriculture", ][, "use"] <- "Agricultural"
buildings[!is.na(buildings$currentUse) & buildings$currentUse == "3_industrial", ][, "use"] <- "Industrial"
buildings[!is.na(buildings$currentUse) & buildings$currentUse == "4_1_office", ][, "use"] <- "Oficinas"
buildings[!is.na(buildings$currentUse) & buildings$currentUse == "4_2_retail", ][, "use"] <- "Comercial"
buildings[!is.na(buildings$currentUse) & buildings$currentUse == "4_3_publicServices", ][, "use"] <- "Servicios Públicos"

# Get the number of floors from the buildingpart
# ----------------------------------------------
building_part <- sf::st_read(part)
building_part$gml_id <- substr(building_part$gml_id, 0, 25)
building_part_floors <- building_part[,c("gml_id","numberOfFloorsAboveGround")] %>% group_by(gml_id) %>% summarise(Value = max(numberOfFloorsAboveGround))
building_part_floors_simple <- building_part_floors %>% st_drop_geometry()

# Merge with the building
buildings <- merge(x = buildings, y = building_part_floors_simple, by = "gml_id", all.x = TRUE) %>% st_as_sf()
names(buildings)[names(buildings) == 'Value'] <- 'numFloors'

# Create palettes
# ----------------------------------------------
pal_age <- leaflet::colorBin(
  "RdYlBu",
  bins = c(0,1900,1925,1950,1960,1970,1980,1990,1995,2000,2005,2010,2015,2030)
)

pal_use <- colorFactor(palette = "Spectral", buildings$use)
pal_floors <- colorFactor("Oranges", buildings$numFloors)

# Transform data to prepare for the plot
# ----------------------------------------------
geom1 <- st_transform(buildings$geometry, "+init=epsg:4326")
geom2 <- as(geom1, 'Spatial')
geom2$year_beginning <- buildings$year_beginning
geom2$use <- buildings$use
geom2$numFloors <- buildings$numFloors

# Create the plot
# ----------------------------------------------
leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$CartoDB.DarkMatter) %>%
  
  leaflet::addPolygons(
    label=~stringr::str_c(year_beginning),
    color = "#222", weight = 0.001, opacity = 1,
    data = geom2,
    fillColor = ~pal_age(year_beginning), fillOpacity = 1,
    highlight = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
    , group = "Año de construcción"
  ) %>%
  leaflet::addPolygons(
    label=~stringr::str_c(use),
    color = "#222", weight = 0.001, opacity = 1,
    data = geom2,
    fillColor = ~pal_use(use), fillOpacity = 1,
    highlight = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    group = "Uso Principal"
  ) %>%
  leaflet::addPolygons(
    label=~stringr::str_c(numFloors),
    color = "#222", weight = 0.001, opacity = 1,
    data = geom2,
    fillColor = ~pal_floors(numFloors), fillOpacity = 1,
    highlight = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    group = "Numero de alturas"
  ) %>%
  
  leaflet::addLegend(
    pal = pal_age, values = geom2$year_beginning, opacity = 0.7,
    title = "Año de construcción", position = "topleft", group = "Año de construcción", layerId = "Año de construcción"
  ) %>%
  leaflet::addLegend(
    pal = pal_use, values = geom2$use, opacity = 0.7,
    title = "Uso Principal", position = "topleft", group = "Uso Principal", layerId = "Uso Principal"
  ) %>%
  leaflet::addLegend(
    pal = pal_floors, values = geom2$numFloors, opacity = 0.7,
    title = "Numero de alturas", position = "topleft", group = "Numero de alturas", layerId = "Numero de alturas"
  ) %>%
  
  # Add custom layers control (found here: https://stackoverflow.com/questions/66920469/how-to-show-hide-legend-with-control-layer-panel-with-leaflet)
  addLayersControl(
    baseGroups = c("Año de construcción", "Uso Principal","Numero de alturas"),
    options = layersControlOptions(collapsed = FALSE)
  )  %>% 
  htmlwidgets::onRender("
    function() { 
      var map = this;
      var legends = map.controls._controlsById;
      function addActualLegend() {
         var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
         $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
         map.addControl(legends[sel]);
      }
      $('.leaflet-control-layers-base').on('click', addActualLegend);
      addActualLegend();
   }")
