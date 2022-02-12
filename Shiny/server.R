# Trayectorias académicas y laborales de las mujeres en Argentinas
# Por Melina Schamberger y Natasha Siderman
# server.R

library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(plotly)
library(ggthemes)
library(readr)

##################
# DATA WRANGLING #
##################

# preprocessed parks file:
#   3 records were multi states parks, only was was attributed
#     DEVA,Death Valley National Park,CA/NV,4740912,36.24,-116.82  --> CA
#     GRSM,Great Smoky Mountains National Park,TN/NC,521490,35.68,-83.53 --> TN
#     YELL,Yellowstone National Park,WY/MT/ID,2219791,44.6,-110.5 --> WY
#   added (U.S.) suffix to Glacier National Park record for wiki disambigaution

parks <- read.csv("www/parks.csv")
species <- read.csv("www/species.csv")

#Datos
#Sector productivo: valores anuales
df_juntos <- read.csv("https://raw.githubusercontent.com/melinaschamberger/Trayectorias_mujeres/main/Datos/Mujeres_sector_privado/Sector_privado_final.csv", 
                      encoding = "Latin1")


# tidy & enrich dataframes
levels(species$Park.Name)[levels(species$Park.Name)=='Glacier National Park'] <- 'Glacier National Park (U.S.)'
parks$Acres <- as.numeric(parks$Acres)
parks$Latitude <- as.numeric(parks$Latitude)
parks$Longitude <- as.numeric(parks$Longitude)

parks <- parks %>%
  mutate(
    ParkRegion = state.region[match(parks$State,state.abb)]
  )

parks$ParkGroup <- ""
parks$ParkGroup[1:28] <- "First Group"
parks$ParkGroup[29:56] <- "Second Group"

species <- species %>%
  mutate(
    ParkRegion = parks$ParkRegion[match(substr(species$Species.ID,1,4),parks[,c("ParkCode")])]
  )

species <- species %>%
  mutate(
    ParkGroup = parks$ParkGroup[match(substr(species$Species.ID,1,4),parks[,c("ParkCode")])]
  )

species <- species %>%
  mutate(
    ParkState = parks$State[match(species$Park.Name,parks$ParkName)]
  )

# support structures
parksNames <- sort(as.character(unique(species[,c("Park.Name")])))
speciesCategories <- sort(as.character(unique(species[,c("Category")])))
speciesCategoriesByState <- species %>% group_by(Category, ParkState) %>% tally(sort=TRUE)
states <- states(cb=T)
speciesStates <- sort(as.character(unique(speciesCategoriesByState$ParkState[complete.cases(speciesCategoriesByState)]))) 

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
  
  
  # Inserción laboral
  filtrado_uno <- reactive({
    df_juntos %>% filter(clae2_desc %in% input$sector_productivo) %>% 
    group_by(anio) %>% 
    summarise(porc_promedio = round(mean(porc_medio),2))})
  
  output$graf_uno_sp <- renderPlotly({
    graf_anual <- plot_ly(filtrado_uno(), 
                          x = ~anio, y = ~porc_promedio, name = 'Año', type = 'scatter', mode = 'ines+markers',
                          line = list(color = '#F2BBC5', width = 3), 
                          marker = list(color = '#8C0368', size = 8)) %>% 
      layout(title = '',
             xaxis = list(title = "Año"),
             yaxis = list(title = 'Promedio anual (%)')) %>% 
      layout(font = t)
    graf_anual
  })
  
  
  
  # parks map
  output$parksMap <- renderLeaflet({
    leaflet(data=parks) %>% addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      addMarkers(
        ~Longitude,
        ~Latitude,
        icon = makeIcon(
          iconUrl = "32px-US-NationalParkService-Logo.svg.png",
          shadowUrl = "32px-US-NationalParkService-Logo.svg - black.png",
          shadowAnchorX = -1, shadowAnchorY = -2
        ),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLayersControl(
        baseGroups = c("Stamen Watercolor","Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  
  # code to load the park card once the click event on a marker is intercepted 
  observeEvent(input$parksMap_marker_click, { 
    pin <- input$parksMap_marker_click
    #print(Sys.time()) #uncomment to log coords
    #print(pin) #uncomment to log coords
    selectedPoint <- reactive(parks[parks$Latitude == pin$lat & parks$Longitude == pin$lng,])
    leafletProxy("parksMap", data = selectedPoint()) %>% clearPopups() %>% 
      addPopups(~Longitude,
                ~Latitude,
                popup = ~park_card(selectedPoint()$ParkName, selectedPoint()$ParkCode, selectedPoint()$State, selectedPoint()$Acres, selectedPoint()$Latitude, selectedPoint()$Longitude)
      )
  })
  
  
  # DT table
  output$speciesDataTable <- renderDataTable(
    species[,-c(8,12,13,14,15,16,17,18)],
    filter = "top",
    colnames = c('Species ID', 'Park name', 'Category', 'Order', 'Family', 'Scientific name', 'Common name', 'Occurence', 'Nativeness' ,'Abundance')
    
  )
  
  # collapsible tree
  output$parkSelectComboTree <- renderUI({
    selectInput("selectedParkTree","Select a park:", parksNames)
  })
  
  output$categorySelectComboTree <- renderUI({
    selectInput("selectedCategoryTree","Select a category:", sort(as.character(unique(species[species$Park.Name==input$selectedParkTree, c("Category")]))))
  })
  
  speciesTree <- reactive(species[species$Park.Name==input$selectedParkTree & species$Category==input$selectedCategoryTree,
                                  c("Category", "Order", "Family","Scientific.Name")])
  
  output$tree <- renderCollapsibleTree(
    collapsibleTree(
      speciesTree(),
      root = input$selectedCategoryTree,
      attribute = "Scientific.Name",
      hierarchy = c("Order", "Family","Scientific.Name"),
      fill = "Green",
      zoomable = FALSE
    )
  )
  
  # ggplot2 charts
  output$categorySelectComboChart <- renderUI({
    selectInput("selectedCategoryChart","Select a category:", speciesCategories)
  })
  
  speciesGgplot1 <- reactive(species[species$ParkGroup == 'First Group' & species$Category==input$selectedCategoryChart,])
  speciesGgplot2 <- reactive(species[species$ParkGroup == 'Second Group' & species$Category==input$selectedCategoryChart,])
  
  output$ggplot2Group1 <- renderPlot({
    
    g1 <- ggplot(data = speciesGgplot1()) + stat_count(mapping = aes(x=fct_rev(Park.Name)), fill="green3") + labs(title="Species' count per park [A-Hal]", x ="Park name", y = paste0("Total number of ", input$selectedCategoryChart)) + coord_flip() + theme_classic() + geom_text(stat='count', aes(fct_rev(Park.Name), label=..count..), hjust=2, size=4)
    print(g1)
    
  })
  
  output$ggplot2Group2 <- renderPlot({
    
    g2 <- ggplot(data = speciesGgplot2()) + stat_count(mapping = aes(x=fct_rev(Park.Name)), fill="green3") + labs(title="Species' count per park [Haw-Z]", x ="Park name", y = paste0("Total number of ", input$selectedCategoryChart)) + coord_flip() + theme_classic() + geom_text(stat='count', aes(fct_rev(Park.Name), label=..count..), hjust=2, size=4)
    print(g2)
    
  })
  
  # leaflet choropleth
  output$statesSelectCombo <- renderUI({
    selectInput("statesCombo","Select a state:", paste0(state.name[match(speciesStates,state.abb)]," (",speciesStates,")"))
  })
  
  output$categorySelectComboChoro <- renderUI({
    selectInput("selectedCategoryChoro","Select a category:", speciesCategories)
  })
  
  selectedChoroCategory <- reactive(speciesCategoriesByState[speciesCategoriesByState$Category==input$selectedCategoryChoro,])
  selectedChoroCategoryJoinStates <- reactive(geo_join(states, selectedChoroCategory(), "STUSPS", "ParkState"))
  
  output$stateCategoryList <- renderTable({
    speciesCategoriesByState[speciesCategoriesByState$ParkState == substr(input$statesCombo,nchar(input$statesCombo)-2,nchar(input$statesCombo)-1), c("Category","n")]
  },colnames = FALSE) 
  
  
  output$choroplethCategoriesPerState <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addPolygons(data = selectedChoroCategoryJoinStates(),
                  fillColor = colorNumeric("Greens", domain=selectedChoroCategoryJoinStates()$n)(selectedChoroCategoryJoinStates()$n),
                  fillOpacity = 0.7,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = paste0("Total of ", as.character(selectedChoroCategoryJoinStates()$n)," species in ",as.character(selectedChoroCategoryJoinStates()$NAME)," (",as.character(selectedChoroCategoryJoinStates()$STUSPS),").")) %>%
      addLegend(pal = colorNumeric("Greens", domain=selectedChoroCategoryJoinStates()$n),
                values = selectedChoroCategoryJoinStates()$n,
                position = "bottomright",
                title = input$selectedCategoryChoro)
    
  })
  
})