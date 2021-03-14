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
#~ library(dplyr)

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
    'Initial template used is here: (https://shiny.rstudio.com/gallery/superzip-example.html).'
  })
  
  # ill.
#~   West_Bounding_Coordinate: -91.4244
#~   East_Bounding_Coordinate: -87.3840
#~   North_Bounding_Coordinate: 42.4951
#~   South_Bounding_Coordinate: 36.9540
  filteredData <- reactive({
    c <- data3Illinois
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
  
  # part 2, comparing two states
  rv <- reactiveValues(linked = FALSE, d1 = data1Illinois, d2 = data3Illinois)
  #map = 'map1', data = data3)

  # 1 input
  filteredData1 <- reactive({
    if (rv$linked) {
      updateCheckboxGroupInput(session, 'm2Source', 'Source:',
        choices = energyList, selected = unlist(input$m1Source)
      )
    }
    if (input$m1Year == 2000) c <- data1
    else if (input$m1Year == 2010) c <- data2
    else c <- data3
    if (input$m1State != 'ALL') c <- subset(c, State == input$m1State)
    cbSource <- input$m1Source
    if (!'Coal' %in% cbSource) c <- subset(c, Type != 'Coal')
    if (!'Geothermal' %in% cbSource) c <- subset(c, Type != 'Geothermal')
    if (!'Gas' %in% cbSource) c <- subset(c, Type != 'Gas')
    if (!'Nuclear' %in% cbSource) c <- subset(c, Type != 'Nuclear')
    if (!'Oil' %in% cbSource) c <- subset(c, Type != 'Oil')
    if (!'Solar' %in% cbSource) c <- subset(c, Type != 'Solar')
    if (!'Wind' %in% cbSource) c <- subset(c, Type != 'Wind')
    if (!'Biomass' %in% cbSource) c <- subset(c, Type != 'Biomass')
    if (!'Other' %in% cbSource) c <- subset(c, Type != 'Other')
    if (!'Hydro' %in% cbSource) c <- subset(c, Type != 'Hydro')
    leafletProxy('map1', data = c) %>%
      clearShapes() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
        lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
        clusterOptions = markerClusterOptions()
      )
  })
#~   ex <- eventReactive(input$m1State, {
#~ #     if (input$y == 'h'){j <- 4}
#~ #     return(c(i, j))
#~     ## ignoreNULL: run at startup
#~     redrawMap1()
#~     filteredData1()
#~   }, ignoreNULL = TRUE)
  
  observeEvent(input$m1State, {
    redrawMap1()
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1Year, {
    # redraw leaflet
    if (input$m1Year == 2000) rv$d1 <- data1
    else if (input$m1Year == 2010) rv$d1 <- data2
    else rv$d1 <- data3
    redrawMap1()
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1Link, {
    if (!rv$linked) {
      rv$linked = TRUE
    } else {
      rv$linked = FALSE
    }
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1Source, {
    # individual cb
    print('y')
    filteredData1()
  }, ignoreInit=TRUE);
  observeEvent(input$m1All, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1Ren, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyListRen)
    )
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1NonRen, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyListNonRen)
    )
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1Reset, {
    updateCheckboxGroupInput(session, 'm1Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filteredData1()
  }, ignoreInit=TRUE)
  # 2 input
  filteredData2 <- reactive({
    if (rv$linked) {
      updateCheckboxGroupInput(session, 'm1Source', 'Source:',
        choices = energyList, selected = unlist(input$m2Source)
      )
    }
    if (input$m2Year == 2000) c <- data1
    else if (input$m2Year == 2010) c <- data2
    else c <- data3
    if (input$m2State != 'ALL') c <- subset(c, State == input$m2State)
    cbSource <- input$m2Source
    if (!'Coal' %in% cbSource) c <- subset(c, Type != 'Coal')
    if (!'Geothermal' %in% cbSource) c <- subset(c, Type != 'Geothermal')
    if (!'Gas' %in% cbSource) c <- subset(c, Type != 'Gas')
    if (!'Nuclear' %in% cbSource) c <- subset(c, Type != 'Nuclear')
    if (!'Oil' %in% cbSource) c <- subset(c, Type != 'Oil')
    if (!'Solar' %in% cbSource) c <- subset(c, Type != 'Solar')
    if (!'Wind' %in% cbSource) c <- subset(c, Type != 'Wind')
    if (!'Biomass' %in% cbSource) c <- subset(c, Type != 'Biomass')
    if (!'Other' %in% cbSource) c <- subset(c, Type != 'Other')
    if (!'Hydro' %in% cbSource) c <- subset(c, Type != 'Hydro')
    leafletProxy('map2', data = c) %>%
      clearShapes() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
        lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
        clusterOptions = markerClusterOptions()
      )
  })
  observeEvent(input$m2State, {
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2Year, {
    # redraw leaflet
    if (input$m2Year == 2000) rv$d2 <- data1
    else if (input$m2Year == 2010) rv$d2 <- data2
    else rv$d2 <- data3
    redrawMap2()
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2Link, {
    if (!rv$linked) {
      rv$linked = TRUE
    } else {
      rv$linked = FALSE
    }
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2Source, {
    # individual cb
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2All, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2Ren, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyListRen)
    )
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2NonRen, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyListNonRen)
    )
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2Reset, {
    updateCheckboxGroupInput(session, 'm2Source', 'Source:',
      choices = energyList, selected = unlist(energyList)
    )
    filteredData2()
  }, ignoreInit=TRUE)
  
  # p2, maps 1/2 
  # maps require redraw altering dataset year
  output$map1 <- renderLeaflet({
    xc <- c('red','#ff8e7f','beige','darkgreen','blue','lightblue',
      '#5a386a','cadetblue','gray','black')
    xl <- c('Coal','Oil','Gas','Nuclear','Hydro','Biomass','Wind',
      'Solar','Geothermal','Other')
    leaflet(data1Illinois) %>%
      addTiles() %>%
      addAwesomeMarkers(
        icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
        lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(colors = xc, labels = xl, opacity = 1)
  })
  output$map2 <- renderLeaflet({
    xc <- c('red','#ff8e7f','beige','darkgreen','blue','lightblue',
      '#5a386a','cadetblue','gray','black')
    xl <- c('Coal','Oil','Gas','Nuclear','Hydro','Biomass','Wind',
      'Solar','Geothermal','Other')
    leaflet(data3Illinois) %>%
      addTiles() %>%
      addAwesomeMarkers(
        icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
        lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(colors = xc, labels = xl, opacity = 1)
  })
  redrawMap1 <- reactive({
    output$map1 <- renderLeaflet({
      xc <- c('red','#ff8e7f','beige','darkgreen','blue','lightblue',
        '#5a386a','cadetblue','gray','black')
      xl <- c('Coal','Oil','Gas','Nuclear','Hydro','Biomass','Wind',
        'Solar','Geothermal','Other')
      leaflet(rv$d1) %>%
        addTiles() %>%
        addAwesomeMarkers(
          icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
          lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
          clusterOptions = markerClusterOptions() # without cluster crashing
                                                  # b/c too many markers
        ) %>%
        addLegend(colors = xc, labels = xl, opacity = 1)
    })
  })
  redrawMap2 <- reactive({
    output$map2 <- renderLeaflet({
      xc <- c('red','#ff8e7f','beige','darkgreen','blue','lightblue',
        '#5a386a','cadetblue','gray','black')
      xl <- c('Coal','Oil','Gas','Nuclear','Hydro','Biomass','Wind',
        'Solar','Geothermal','Other')
      leaflet(rv$d2) %>%
        addTiles() %>%
        addAwesomeMarkers(
          icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = ~Color),
          lng = ~Lng, lat = ~Lat, popup = ~as.character(Popup),
          clusterOptions = markerClusterOptions() # without cluster crashing
                                                  # b/c too many markers
        ) %>%
        addLegend(colors = xc, labels = xl, opacity = 1)
    })
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
