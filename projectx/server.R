## server.R
## Author: David Shumway

library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)

shinyServer(function(input, output, session) { #, session
  # about
#~   output$textAbout <- renderText({ 
#~     'Initial template used: (https://shiny.rstudio.com/gallery/superzip-example.html).<br>Author: David Shumway<br>Original data: (https://www.epa.gov/egrid/download-data).'
#~   })
  
  # ill.
#~   West_Bounding_Coordinate: -91.4244
#~   East_Bounding_Coordinate: -87.3840
#~   North_Bounding_Coordinate: 42.4951
#~   South_Bounding_Coordinate: 36.9540
  
#~   output$map <- renderLeaflet({})
    
  # part 2, comparing two states
  
    #map = 'map1', data = data3)
  
  # 1 inputs
  observeEvent(input$m1Range, {
    filter(session, input, 1)
  }, ignoreInit=T)
  observeEvent(input$m1BaseMap, {
    filter(session, input, 1)
  }, ignoreInit=T)
  observeEvent(input$m1Country, {
    redrawMap1()
    filter(session, input, 1)
    ll = firstLL(input$m1Country)
    leafletProxy('map1') %>%
      flyTo(ll[[2]], ll[[1]], zoom = 3)
  }, ignoreInit=T)
  observeEvent(input$m1Link, {
    if (!rv$linked) {
      rv$linked = TRUE
    } else {
      rv$linked = FALSE
    }
    filter(session, input, 1)
  }, ignoreInit=T)
  observeEvent(input$m1Source, {
    # individual cb
    filter(session, input, 1)
  }, ignoreInit=T);
  observeEvent(input$m1All, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filter(session, input, 1)
  }, ignoreInit=T)
  observeEvent(input$m1Ren, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyListRen)
    )
    filter(session, input, 1)
  }, ignoreInit=T)
  observeEvent(input$m1NonRen, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyListNonRen)
    )
    filter(session, input, 1)
  }, ignoreInit=T)
  observeEvent(input$m1Reset, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    updateSelectInput(session, 'm1Country', 'Continent:',
      selected = 'Americas')
    updateSelectInput(session, 'm1BaseMap', 'Map Type:',
      selected = 'OpenStreetMap')
    updateSliderInput(session, 'm1Range', 'GW Range:',
      min = 0, max = 23, step = 1, value = c(0, 23))
    redrawMap1()
  }, ignoreInit=T)
  
  # 2 input
  observeEvent(input$m2Range, {
    filter(session, input, 2)
  }, ignoreInit=T)
  observeEvent(input$m2BaseMap, {
    filter(session, input, 2)
  }, ignoreInit=T)
  observeEvent(input$m2Country, {
    redrawMap2()
    filter(session, input, 2)
    ll = firstLL(input$m2Country)
    leafletProxy('map2') %>%
      flyTo(ll[[2]], ll[[1]], zoom = 3)
  }, ignoreInit=T)
  observeEvent(input$m2Link, {
    if (!rv$linked) {
      rv$linked = TRUE
    } else {
      rv$linked = FALSE
    }
    filter(session, input, 2)
  }, ignoreInit=T)
  observeEvent(input$m2Source, {
    # individual cb
    filter(session, input, 2)
  }, ignoreInit=T)
  observeEvent(input$m2All, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filter(session, input, 2)
  }, ignoreInit=T)
  observeEvent(input$m2Ren, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyListRen)
    )
    filter(session, input, 2)
  }, ignoreInit=T)
  observeEvent(input$m2NonRen, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyListNonRen)
    )
    filter(session, input, 2)
  }, ignoreInit=T)
  observeEvent(input$m2Reset, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    updateSelectInput(session, 'm2Country', 'Continent:',
      selected = 'Asia')
    updateSelectInput(session, 'm2BaseMap', 'Map Type:',
      selected = 'OpenStreetMap')
    updateSliderInput(session, 'm2Range', 'GW Range:',
      min = 0, max = 23, step = 1, value = c(0, 23))
    redrawMap2()
  }, ignoreInit=T)
  
  # p2, maps 1/2 
  # maps require redraw altering dataset year
  # Apparently there's a magical "map" parameter that's getting passed
  # here. Without it, leaflet throws a fit.
  addLegendCustom <- function(map, opacity = 0.5) {
    labels = c('<500MW', '<2000MW', '<4000MW', '<6000MW', '>6000MW')# "0.5 mil, 6 mil, 12 mil"
    sizes = c(4, 6, 12, 18, 22)
    # https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
    colors <- c('blue')
    colorAdditions <- paste0(colors, '; width:', sizes, 'px; height:',
      sizes, 'px')
    labelAdditions <- paste0('<div style="display: inline-block;height: ',
      sizes, 'px;margin-top: 4px;line-height: ', sizes, 'px;">', labels,
      '</div>')
    addLegend(map, colors = colorAdditions, labels = labelAdditions,
      opacity = opacity) #map, 
  }
  output$map1 <- renderLeaflet({
    leaflet(subset(data, country_long == 'Americas')) %>%
      addProviderTiles(input$m1BaseMap) %>%
      addCircleMarkers(
        radius = ~frad(capacity_mw),
        fillColor = ~fcol(primary_fuel),
        color = 'black',
        stroke = TRUE, fillOpacity = 0.5, weight = 1,
        popup = ~as.character(Popup),
      ) %>%
      addLegend(position = 'topright', colors = xc, labels = xl, opacity = 1) %>%
      addLegendCustom(opacity = 0.6)
  })
  output$map2 <- renderLeaflet({
    leaflet(subset(data, country_long == 'Asia')) %>%
      addProviderTiles(input$m2BaseMap) %>%
      addCircleMarkers(
        radius = ~frad(capacity_mw),
        fillColor = ~fcol(primary_fuel),
        color = 'black',
        stroke = TRUE, fillOpacity = 0.5, weight = 1,
        popup = ~as.character(Popup),
      ) %>%
      addLegend(colors = xc, labels = xl, opacity = 1) %>%
      addLegendCustom(opacity = 0.6)
  })
  redrawMap1 <- reactive({
    output$map1 <- renderLeaflet({
      leaflet(rv$d1) %>%
        addProviderTiles(input$m1BaseMap) %>%
        addCircleMarkers(
          radius = ~frad(capacity_mw),
          fillColor = ~fcol(primary_fuel),
          color = 'black',
          stroke = TRUE, fillOpacity = 0.5, weight = 1,
          popup = ~as.character(Popup),
        ) %>%
        addLegend(colors = xc, labels = xl, opacity = 1) %>%
        addLegendCustom(opacity = 0.6)
    })
  })
  redrawMap2 <- reactive({
    output$map2 <- renderLeaflet({
      leaflet(rv$d2) %>%
        addProviderTiles(input$m2BaseMap) %>%
        addCircleMarkers(
          radius = ~frad(capacity_mw),
          fillColor = ~fcol(primary_fuel),
          color = 'black',
          stroke = TRUE, fillOpacity = 0.5, weight = 1,
          popup = ~as.character(Popup),
        ) %>%
        addLegend(colors = xc, labels = xl, opacity = 1) %>%
        addLegendCustom(opacity = 0.6)
    })
  })
  
  output$datatbl <- DT::renderDataTable({
    df <- data %>%
      mutate(Action = paste(
        '<a class="go-map" href="" data-lat="', latitude,
        '" data-long="', longitude,
        '"><i class="fa fa-crosshairs"></i></a>', sep = ''))
    action <- DT::dataTableAjax(session, df, outputId = 'datatbl')
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
})
