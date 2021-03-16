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
    'Initial template used: (https://shiny.rstudio.com/gallery/superzip-example.html).<br>Author: David Shumway<br>Original data: (https://www.epa.gov/egrid/download-data).'
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
  frad <- function(ag) {
    # faster to check here than global?
    # AnnualGen
    m = 1000000
    ifelse(
      ag < 500000, '4', ifelse(
        ag < 8*m, '6', ifelse(
          ag < 16*m, '12', ifelse(
            ag < 24*m, '18', '22'
          )
        )
    ))
  }
  fcol <- function(t) {
    # t: Type
    ifelse(
      t == 'Coal', '#8dd3c7', ifelse(
      t == 'Oil', '#ffffb3', ifelse(
      t == 'Gas', '#bebada', ifelse(
      t == 'Nuclear', '#fb8072', ifelse(
      t == 'Hydro', '#80b1d3', ifelse(
      t == 'Biomass', '#fdb462', ifelse(
      t == 'Wind', '#b3de69', ifelse(
      t == 'Solar', '#fccde5', ifelse(
      t == 'Geothermal', '#d9d9d9', '#bc80bd' # last is Other
    )))))))))
  }
  flp <- function(mapId, data, tiles) {
    leafletProxy(mapId, data = data) %>%
      clearShapes() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearTiles() %>%
      addProviderTiles(tiles) %>%
      addCircleMarkers(
        radius = ~frad(AnnualGen),
        fillColor = ~fcol(Type),
        color = 'black',
        stroke = TRUE, fillOpacity = 1, weight = 1,
        popup = ~as.character(Popup),
      )
  }
  xc <- c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462',
    '#b3de69', '#fccde5', '#d9d9d9', '#bc80bd')
  xl <- c('Coal', 'Oil', 'Gas', 'Nuclear', 'Hydro', 'Biomass', 'Wind',
    'Solar', 'Geothermal', 'Other')
  firstLL <- function(state) {
    c <- subset(data1, State == state)
    list(c[0:1,]$Lat, c[0:1,]$Lng)
  }
  `%notin%` <- Negate(`%in%`)
  
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
    if (input$m1Idled) {
      if (input$m1Year == 2000) c <- subset(c, Lat == 0) # empty df
      else if (input$m1Year == 2010) c <- data1[data1$Oris %notin% data2$Oris,] # data1 not in data2
      else c <- data2[data2$Oris %notin% data3$Oris,] # data2 not in data3
    } else if (input$m1New) {
      if (input$m1Year == 2000) c <- subset(c, Lat == 0) # empty df
      else if (input$m1Year == 2010) c <- data2[data2$Oris %notin% data1$Oris,] # data2 not in data1
      else c <- data3[data3$Oris %notin% data2$Oris,] # data3 not in data2
    }
    if (input$m1State != 'ALL') c <- subset(c, State == input$m1State)
    c <- subset(c, # size
      AnnualGen > as.numeric(input$m1Range[[1]])*1000000)
    c <- subset(c, # size
      AnnualGen < as.numeric(input$m1Range[[2]])*1000000)
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
    flp('map1', c, input$m1BaseMap)
  })
  observeEvent(input$m1Idled, {
    if (input$m1Idled) {
      updateSwitchInput(session, 'm1New', 'Highlight new',
        value = FALSE)
    }
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1New, {
    if (input$m1New) {
      updateSwitchInput(session, 'm1Idled', 'Highlight idled',
        value = FALSE)
    }
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1Range, {
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1BaseMap, {
    filteredData1()
  }, ignoreInit=TRUE)
  observeEvent(input$m1State, {
    redrawMap1()
    filteredData1()
    if (input$m1State != 'ALL') {
      ll = firstLL(input$m1State)
      leafletProxy('map1') %>%
        flyTo(ll[[2]], ll[[1]], zoom = 6)
    }
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
    updateSelectInput(session, 'm1Year', 'Year:',
      selected = '2000')
    updateSelectInput(session, 'm1State', 'State:',
      selected = 'IL')
    updateSelectInput(session, 'm1BaseMap', 'Map Type:',
      selected = 'OpenStreetMap')
    updateSliderInput(session, 'm1Range', 'mWh Range:',
      min = 0, max = 32, step = 8, value = c(0, 32))
    redrawMap1()
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
    if (input$m2Idled) {
      if (input$m2Year == 2000) c <- subset(c, Lat == 0) # empty df
      else if (input$m2Year == 2010) c <- data1[data1$Oris %notin% data2$Oris,] # data1 not in data2
      else c <- data2[data2$Oris %notin% data3$Oris,] # data2 not in data3
    } else if (input$m2New) {
      if (input$m2Year == 2000) c <- subset(c, Lat == 0) # empty df
      else if (input$m2Year == 2010) c <- data2[data2$Oris %notin% data1$Oris,] # data2 not in data1
      else c <- data3[data3$Oris %notin% data2$Oris,] # data3 not in data2
    }
    if (input$m2State != 'ALL') c <- subset(c, State == input$m2State)
    c <- subset(c, # size
      AnnualGen > as.numeric(input$m2Range[[1]])*1000000,
      AnnualGen < as.numeric(input$m2Range[[2]])*1000000)
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
    flp('map2', c, input$m2BaseMap)
  })
  observeEvent(input$m2Idled, {
    if (input$m2Idled) { # only allows new or idled, not both
      updateSwitchInput(session, 'm2New', 'Highlight new',
        value = FALSE)
    }
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2New, {
    if (input$m2New) {
      updateSwitchInput(session, 'm2Idled', 'Highlight idled',
        value = FALSE)
    }
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2Range, {
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2BaseMap, {
    filteredData2()
  }, ignoreInit=TRUE)
  observeEvent(input$m2State, {
    redrawMap2
    filteredData2()
    if (input$m2State != 'ALL') {
      ll = firstLL(input$m2State)
      leafletProxy('map2') %>%
        flyTo(ll[[2]], ll[[1]], zoom = 6)
    }
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
    updateSelectInput(session, 'm2Year', 'Year:',
      selected = '2018')
    updateSelectInput(session, 'm2State', 'State:',
      selected = 'IL')
    updateSelectInput(session, 'm2BaseMap', 'Map Type:',
      selected = 'OpenStreetMap')
    updateSliderInput(session, 'm2Range', 'mWh Range:',
      min = 0, max = 32, step = 8, value = c(0, 32))
    redrawMap2()
  }, ignoreInit=TRUE)
  
  # p2, maps 1/2 
  # maps require redraw altering dataset year
  addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5) {
    # https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
    colorAdditions <- paste0(colors, '; width:', sizes, 'px; height:',
      sizes, 'px')
    labelAdditions <- paste0('<div style="display: inline-block;height: ',
      sizes, 'px;margin-top: 4px;line-height: ', sizes, 'px;">', labels,
      '</div>')  
    addLegend(map, colors = colorAdditions, labels = labelAdditions,
      opacity = opacity)
  }
  output$map1 <- renderLeaflet({
    leaflet(data1Illinois) %>%
      addProviderTiles(input$m1BaseMap) %>%
      addCircleMarkers(
        radius = ~frad(AnnualGen),
        fillColor = ~fcol(Type),
        color = 'black',
        stroke = TRUE, fillOpacity = 1, weight = 1,
        popup = ~as.character(Popup),
      ) %>%
      addLegend(position = 'topright', colors = xc, labels = xl, opacity = 1) %>%
      addLegendCustom(colors = c('blue', 'blue', 'blue'),
        labels = c('<0.5 mil mWh', '<6 mil mWh', '<12 mil mWh', '<18 mil mWh', '>22 mil mWh'),
        sizes = c(4, 6, 12, 18, 22))
  })
  output$map2 <- renderLeaflet({
    leaflet(data3Illinois) %>%
      addProviderTiles(input$m2BaseMap) %>%
      addCircleMarkers(
        radius = ~frad(AnnualGen),
        fillColor = ~fcol(Type),
        color = 'black',
        stroke = TRUE, fillOpacity = 1, weight = 1,
        popup = ~as.character(Popup),
      ) %>%
      addLegend(colors = xc, labels = xl, opacity = 1) %>%
      addLegendCustom(colors = c('blue', 'blue', 'blue'),
        labels = c('<0.5 mil mWh', '<6 mil mWh', '<12 mil mWh', '<18 mil mWh', '>22 mil mWh'),
        sizes = c(4, 6, 12, 18, 22))
  })
  redrawMap1 <- reactive({
    output$map1 <- renderLeaflet({
      leaflet(rv$d1) %>%
        addProviderTiles(input$m1BaseMap) %>%
        addCircleMarkers(
          radius = ~frad(AnnualGen),
          fillColor = ~fcol(Type),
          color = 'black',
          stroke = TRUE, fillOpacity = 1, weight = 1,
          popup = ~as.character(Popup),
        ) %>%
        addLegend(colors = xc, labels = xl, opacity = 1) %>%
        addLegendCustom(colors = c('blue', 'blue', 'blue'),
          labels = c('<0.5 mil mWh', '<6 mil mWh', '<12 mil mWh', '<18 mil mWh', '>22 mil mWh'),
          sizes = c(4, 6, 12, 18, 22))
    })
  })
  redrawMap2 <- reactive({
    output$map2 <- renderLeaflet({
      leaflet(rv$d2) %>%
        addProviderTiles(input$m2BaseMap) %>%
        addCircleMarkers(
          radius = ~frad(AnnualGen),
          fillColor = ~fcol(Type),
          color = 'black',
          stroke = TRUE, fillOpacity = 1, weight = 1,
          popup = ~as.character(Popup),
        ) %>%
        addLegend(colors = xc, labels = xl, opacity = 1) %>%
        addLegendCustom(colors = c('blue', 'blue', 'blue'),
          labels = c('<0.5 mil mWh', '<6 mil mWh', '<12 mil mWh', '<18 mil mWh', '>22 mil mWh'),
          sizes = c(4, 6, 12, 18, 22))
    })
  })
  
  output$data1tbl <- DT::renderDataTable({
#~     a <- 
#~       sep = ''
#~       '<a class="go-map" href=""><i class="fa fa-crosshairs"></i></a>',
#~       sep = ''
#~     )
    df <- data1 %>%
#~       filter(
#~         Score >= input$minScore,
#~         Score <= input$maxScore,
#~         is.null(input$states) | State %in% input$states,
#~         is.null(input$cities) | City %in% input$cities,
#~         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#~       ) %>%
      mutate(Action = paste(
        '<a class="go-map" href="" data-lat="', Lat,
        '" data-long="', Lng,
  #~       '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>',
        '"><i class="fa fa-crosshairs"></i></a>', sep = ''))
    action <- DT::dataTableAjax(session, df, outputId = 'data1tbl')
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  output$data2tbl <- DT::renderDataTable({
    a <- paste(
      '<a class="go-map" href=""><i class="fa fa-crosshairs"></i></a>',
      sep = ''
    )
    df <- data2 %>%
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
