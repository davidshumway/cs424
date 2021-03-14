## server.R
## Author: David Shumway

library(shiny)
library(DT)
#~ library(data.table) # for calculating percentages
#~ library(ggplot2)
#~ library(usmap)
#~ library(stringr)

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

energyListRen <- list(
  'Geothermal' = 'Geothermal',
  'Hydro' = 'Hydro',
  'Solar' = 'Solar',
  'Wind' = 'Wind',
  'Biomass' = 'Biomass' # ren?
)
energyListNonRen <- list(
  'Coal' = 'Coal',
  'Gas' = 'Gas',
  'Nuclear' = 'Nuclear',
  'Oil' = 'Oil'
)

shinyServer(function(input, output, session) { #, session
  # about
  output$textAbout <- renderText({ 
    'This is the app.'
  })
  
  
  
  # ill.
#~   West_Bounding_Coordinate: -91.4244
#~   East_Bounding_Coordinate: -87.3840
#~   North_Bounding_Coordinate: 42.4951
#~   South_Bounding_Coordinate: 36.9540
  filteredData <- reactive({
    c <- data3Illinois
    print(input$illSource)
    if (!'Coal' %in% input$illSource) c <- subset(c, Type != 'Coal')
    if (!'Geothermal' %in% input$illSource) c <- subset(c, Type != 'Geothermal')
    if (!'Gas' %in% input$illSource) c <- subset(c, Type != 'Gas')
    if (!'Nuclear' %in% input$illSource) c <- subset(c, Type != 'Nuclear')
    if (!'Oil' %in% input$illSource) c <- subset(c, Type != 'Oil')
    if (!'Solar' %in% input$illSource) c <- subset(c, Type != 'Solar')
    if (!'Wind' %in% input$illSource) c <- subset(c, Type != 'Wind')
    if (!'Biomass' %in% input$illSource) c <- subset(c, Type != 'Biomass')
    if (!'Other' %in% input$illSource) c <- subset(c, Type != 'Other')
    if (!'Hydro' %in% input$illSource) c <- subset(c, Type != 'Hydro')
    leafletProxy('map', data = c) %>%
      clearShapes() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
        lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
        group = 'Type', # Appears to be ineffective
        clusterOptions = markerClusterOptions() # without cluster crashing
                                                # b/c too many markers
      )
  })
  observeEvent(input$illSource, {
    # click on an individual cb
    filteredData()
  });
  observeEvent(input$illAll, {
    updateCheckboxGroupInput(session, 'illSource', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filteredData()
  })
  observeEvent(input$illRen, {
    updateCheckboxGroupInput(session, 'illSource', 'Source:',
      choices = energyList, selected = unlist(energyListRen)
    )
    filteredData()
  })
  observeEvent(input$illNonRen, {
    updateCheckboxGroupInput(session, 'illSource', 'Source:',
      choices = energyList, selected = unlist(energyListNonRen)
    )
    filteredData()
  })
  observeEvent(input$illReset, {
    updateCheckboxGroupInput(session, 'illSource', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filteredData()
  })
  output$map <- renderLeaflet({
    # Apparently lightred = ff8e7f and darkpurple = 5a386a
    xc <- c('red','#ff8e7f','beige','darkgreen','blue','lightblue',
      '#5a386a','cadetblue','gray','black')
    xl <- c('Coal','Oil','Gas','Nuclear','Hydro','Biomass','Wind',
      'Solar','Geothermal','Other')
    leaflet(data3Illinois) %>%
      addTiles() %>%
      addAwesomeMarkers(
        icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
        lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
        clusterOptions = markerClusterOptions() # without cluster crashing
                                                # b/c too many markers
      ) %>%
      addLegend(colors = xc, labels = xl, opacity = 1)
  })
  
  output$data1tbl <- DT::renderDataTable({
    a <- paste(
#~       '<a class="go-map" href="" data-lat="', Lat,
#~       '" data-long="', Long,
#~       '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>',
#~       sep = ''
      '<a class="go-map" href=""><i class="fa fa-crosshairs"></i></a>',
      sep = ''
    )
    df <- data3 %>%
#~       filter(
#~         Score >= input$minScore,
#~         Score <= input$maxScore,
#~         is.null(input$states) | State %in% input$states,
#~         is.null(input$cities) | City %in% input$cities,
#~         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#~       ) %>%
      mutate(Action = a)
    action <- DT::dataTableAjax(session, df, outputId = 'data1tbl')
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  output$data2tbl <- DT::renderDataTable({
    a <- paste(
      '<a class="go-map" href=""><i class="fa fa-crosshairs"></i></a>',
      sep = ''
    )
    df <- data3 %>%
      mutate(Action = a)
    action <- DT::dataTableAjax(session, df, outputId = 'data2tbl')
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  output$data3tbl <- DT::renderDataTable({
    a <- paste(
      '<a class="go-map" href=""><i class="fa fa-crosshairs"></i></a>',
      sep = ''
    )
    df <- data3 %>%
      mutate(Action = a)
    action <- DT::dataTableAjax(session, df, outputId = 'data3tbl')
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
})
