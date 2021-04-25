## server.R
## Author: David Shumway

library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)

library(mapview)
library(sf)
library(tigris)
library(leaflet)
library(tidyr)
options(tigris_use_cache = TRUE)

shinyServer(function(input, output, session) {
  
  # about
  output$textAbout <- renderText({
    '--'
  })
  
  observeEvent(input$nwsReset, {
    
    updateSelectInput(session = session, inputId = 'viewType',
      selected = 'electric')
    updateSelectInput(session = session, inputId = 'month',
      selected = 'all')
    updateCheckboxGroupButtons(session = session, inputId = 'building',
      selected = c('Residential', 'Commercial', 'Industrial')
    )
    
    # This does not reset the button to non-clicked state.
    #updateActionButton(session = session, inputId = 'nwsReset', label = 'Reset Map')
    
    #filters(input, output)
    m <- mapview(nwsTotalKWH, zcol = 'x')
    output$mapplot <- renderLeaflet({
      m@map
    })
  })
  
  #selectedData <- nwsTotalKWH
  m1 <- mapview(nwsTotalKWH, zcol = 'x')
  m2 <- mapview(nwsTotalKWH, zcol = 'x')
  m3 <- mapview(loopTotalKWH, zcol = 'x')
#~   m <- mapview(cooknws, zcol = 'TOTAL.POPULATION')
#~   m <- mapview(x, zcol = 'NAME10')
  #m <- mapview(ycol=sub_data$INTPTLAT10, xcol = sub_data$INTPTLON10, zcol = sub_data$KWH.JANUARY.2010)
  output$mapplot <- renderLeaflet({
    m1@map
  })
  output$mapplot2 <- renderLeaflet({
    m2@map
  })
  output$mapplot3 <- renderLeaflet({
    m3@map
  })

  # Single map inputs
  observeEvent(input$building, {
    filters(input, output)
  })
  observeEvent(input$month, {
    filters(input, output)
  })
  observeEvent(input$viewType, {
    filters(input, output)
  })
  
  # Comparison inputs
  observeEvent(input$building2, {
    filters2(input, output, 2)
  })
  observeEvent(input$building3, {
    filters2(input, output, 3)
  })
  observeEvent(input$month2, {
    filters2(input, output, 2)
  })
  observeEvent(input$month3, {
    filters2(input, output, 3)
  })
  observeEvent(input$viewType2, {
    filters2(input, output, 2)
  })
  observeEvent(input$viewType3, {
    filters2(input, output, 3)
  })
  observeEvent(input$community2, {
    filters2(input, output, 2)
  })
  observeEvent(input$community3, {
    filters2(input, output, 3)
  })
  
# This does not work well.
#~   observeEvent(input$viewType2, {
#~     print(input$viewType2)
#~     print(length(input$viewType2))
#~     if (length(input$viewType2) == 0) {
#~       s <- 'gas'
#~     } else {
#~       s <- input$viewType2[1]
#~     }
#~     updateCheckboxGroupButtons(session = session, inputId = "viewType2", 
#~       choices = c(
#~         'Gas Use' = 'gas',
#~         'Electricity Use' = 'electric',
#~         'Building Age' = 'age',
#~         'Building Type' = 'type',
#~         'Building Height' = 'height',
#~         'Total Population' = 'population'
#~       ), selected = s)
#~   })
  
  
})
