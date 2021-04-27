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
  
  output$textAbout <- renderText({
    ''
  })
  
  output$mapplot <- renderLeaflet({
    #m1@map
  })
  output$mapplot2 <- renderLeaflet({
#~     m2@map
  })
  output$mapplot3 <- renderLeaflet({
#~     m3@map
  })
  
  observeEvent(input$reset2, {
    updateSelectInput(session = session, inputId = 'community2',
      selected = 'Near West Side')
    updateSelectInput(session = session, inputId = 'viewType2',
      selected = 'electric')
    updateSelectInput(session = session, inputId = 'month2',
      selected = 'all')
    updateCheckboxGroupButtons(session = session, inputId = 'building2',
      selected = c('Residential', 'Commercial', 'Industrial')
    )
    m <- mapview(nwsTotalKWH, zcol = 'x')
    output$mapplot2 <- renderLeaflet({
      m@map
    })
  }, ignoreInit = TRUE)
  observeEvent(input$reset3, {
    updateSelectInput(session = session, inputId = 'community3',
      selected = 'Loop')
    updateSelectInput(session = session, inputId = 'viewType3',
      selected = 'electric')
    updateSelectInput(session = session, inputId = 'month3',
      selected = 'all')
    updateCheckboxGroupButtons(session = session, inputId = 'building3',
      selected = c('Residential', 'Commercial', 'Industrial')
    )
    m <- mapview(loopTotalKWH, zcol = 'x')
    output$mapplot3 <- renderLeaflet({
      m@map
    })
  }, ignoreInit = TRUE)
  
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
  }, ignoreInit = TRUE)
  
  

  # Single map inputs
  observeEvent(input$building, {
    filters(input, output)
  }) # let the first one run to populate 
  observeEvent(input$month, {
    filters(input, output)
  }, ignoreInit = TRUE)
  observeEvent(input$viewType, {
    filters(input, output)
  }, ignoreInit = TRUE)
  
  # Comparison inputs
  observeEvent(input$bt2, {
    filters2(input, output, 2)
  }, ignoreInit = TRUE)
  observeEvent(input$bt3, {
    filters2(input, output, 3)
  }, ignoreInit = TRUE)
  observeEvent(input$color2, {
    filters2(input, output, 2)
  }, ignoreInit = TRUE)
  observeEvent(input$color3, {
    filters2(input, output, 3)
  }, ignoreInit = TRUE)
  observeEvent(input$building2, {
    filters2(input, output, 2)
  }, ignoreInit = TRUE)
  observeEvent(input$building3, {
    filters2(input, output, 3)
  }, ignoreInit = TRUE)
  observeEvent(input$month2, {
    filters2(input, output, 2)
  }, ignoreInit = TRUE)
  observeEvent(input$month3, {
    filters2(input, output, 3)
  }, ignoreInit = TRUE)
  observeEvent(input$viewType2, {
    filters2(input, output, 2)
  }, ignoreInit = TRUE)
  observeEvent(input$viewType3, {
    filters2(input, output, 3)
  }, ignoreInit = TRUE)
  observeEvent(input$community2, {
    filters2(input, output, 2)
  })#, ignoreInit = TRUE)
  observeEvent(input$community3, {
    filters2(input, output, 3)
  })#, ignoreInit = TRUE)
  
  # Pretty much useless :(
  # Problem: On init, map3 has census blocks from map2, or vice versa
  # Solution: Use layer.name in mapview
#~   active <- reactiveVal(TRUE)
#~   counter <- reactiveVal(10)
#~   # timer example https://stackoverflow.com/questions/47485981/invalidatelater-shiny
#~   observe({
#~     invalidateLater(1000, session)
#~     isolate({
#~       if (active()) {
#~         counter(counter() - 1)
#~         if (counter() %% 2 == 0) {
#~           updateSelectInput(session = session, inputId = 'community2',
#~             selected = 'Loop')
#~           updateSelectInput(session = session, inputId = 'community3',
#~             selected = 'Near West Side')
#~         } else {
#~           click('reset2')
#~           click('reset3')
#~         }
#~         if (counter() == 0) {
#~           active(FALSE)
#~           click('reset2')
#~           click('reset3')
#~         }
#~       }
#~     })
#~   })
  
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
