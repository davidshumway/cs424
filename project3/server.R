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

#~ data1 <- read.csv('data/Energy_Usage_2010.csv')
data1 <- read.csv('data/test.csv')

# always room for broken data
names(data1)[names(data1) == 'TERM.APRIL.2010'] <- 'THERM.APRIL.2010'

kwhMonths <- c('KWH.JANUARY.2010', 'KWH.FEBRUARY.2010', 'KWH.MARCH.2010',
  'KWH.APRIL.2010', 'KWH.MAY.2010', 'KWH.JUNE.2010', 'KWH.JULY.2010',
  'KWH.AUGUST.2010', 'KWH.SEPTEMBER.2010', 'KWH.OCTOBER.2010',
  'KWH.NOVEMBER.2010','KWH.DECEMBER.2010'
)
thmMonths <- c('THERM.JANUARY.2010', 'THERM.FEBRUARY.2010', 'THERM.MARCH.2010',
  'THERM.APRIL.2010', 'THERM.MAY.2010', 'THERM.JUNE.2010', 'THERM.JULY.2010',
  'THERM.AUGUST.2010', 'THERM.SEPTEMBER.2010', 'THERM.OCTOBER.2010',
  'THERM.NOVEMBER.2010','THERM.DECEMBER.2010'
)

data1$TOTAL.KWH <- as.double(data1$TOTAL.KWH)
data1$TOTAL.THERMS <- as.double(data1$TOTAL.THERMS)
data1$AVERAGE.STORIES <- as.double(data1$AVERAGE.STORIES)
data1$TOTAL.POPULATION <- as.double(data1$TOTAL.POPULATION)
data1$AVERAGE.BUILDING.AGE <- as.double(data1$AVERAGE.BUILDING.AGE)
data1$BUILDING.TYPE <- as.factor(data1$BUILDING.TYPE)

data1$THERM.JANUARY.2010 <- as.double(data1$THERM.JANUARY.2010)
data1$THERM.FEBRUARY.2010 <- as.double(data1$THERM.FEBRUARY.2010)
data1$THERM.MARCH.2010 <- as.double(data1$THERM.MARCH.2010)
data1$THERM.APRIL.2010 <- as.double(data1$THERM.APRIL.2010)
data1$THERM.MAY.2010 <- as.double(data1$THERM.MAY.2010)
data1$THERM.JUNE.2010 <- as.double(data1$THERM.JUNE.2010)
data1$THERM.JULY.2010 <- as.double(data1$THERM.JULY.2010)
data1$THERM.AUGUST.2010 <- as.double(data1$THERM.AUGUST.2010)
data1$THERM.SEPTEMBER.2010 <- as.double(data1$THERM.SEPTEMBER.2010)
data1$THERM.OCTOBER.2010 <- as.double(data1$THERM.OCTOBER.2010)
data1$THERM.NOVEMBER.2010 <- as.double(data1$THERM.NOVEMBER.2010)
data1$THERM.DECEMBER.2010 <- as.double(data1$THERM.DECEMBER.2010)

data1$KWH.JANUARY.2010 <- as.double(data1$KWH.JANUARY.2010)
data1$KWH.FEBRUARY.2010 <- as.double(data1$KWH.FEBRUARY.2010)
data1$KWH.MARCH.2010 <- as.double(data1$KWH.MARCH.2010)
data1$KWH.APRIL.2010 <- as.double(data1$KWH.APRIL.2010)
data1$KWH.MAY.2010 <- as.double(data1$KWH.MAY.2010)
data1$KWH.JUNE.2010 <- as.double(data1$KWH.JUNE.2010)
data1$KWH.JULY.2010 <- as.double(data1$KWH.JULY.2010)
data1$KWH.AUGUST.2010 <- as.double(data1$KWH.AUGUST.2010)
data1$KWH.SEPTEMBER.2010 <- as.double(data1$KWH.SEPTEMBER.2010)
data1$KWH.OCTOBER.2010 <- as.double(data1$KWH.OCTOBER.2010)
data1$KWH.NOVEMBER.2010 <- as.double(data1$KWH.NOVEMBER.2010)
data1$KWH.DECEMBER.2010 <- as.double(data1$KWH.DECEMBER.2010)

data1$TOTAL.KWH[is.na(data1$TOTAL.KWH)] <- 0
data1$TOTAL.THERMS[is.na(data1$TOTAL.THERMS)] <- 0
data1$AVERAGE.STORIES[is.na(data1$AVERAGE.STORIES)] <- 0
data1$TOTAL.POPULATION[is.na(data1$TOTAL.POPULATION)] <- 0
data1$AVERAGE.BUILDING.AGE[is.na(data1$AVERAGE.BUILDING.AGE)] <- 0
#data1$BUILDING.TYPE[is.na(data1$BUILDING.TYPE)] <- 0

data1$THERM.JANUARY.2010[is.na(data1$THERM.JANUARY.2010)] <- 0
data1$THERM.FEBRUARY.2010[is.na(data1$THERM.FEBRUARY.2010)] <- 0
data1$THERM.MARCH.2010[is.na(data1$THERM.MARCH.2010)] <- 0
data1$THERM.APRIL.2010[is.na(data1$THERM.APRIL.2010)] <- 0
data1$THERM.MAY.2010[is.na(data1$THERM.MAY.2010)] <- 0
data1$THERM.JUNE.2010[is.na(data1$THERM.JUNE.2010)] <- 0
data1$THERM.JULY.2010[is.na(data1$THERM.JULY.2010)] <- 0
data1$THERM.AUGUST.2010[is.na(data1$THERM.AUGUST.2010)] <- 0
data1$THERM.SEPTEMBER.2010[is.na(data1$THERM.SEPTEMBER.2010)] <- 0
data1$THERM.OCTOBER.2010[is.na(data1$THERM.OCTOBER.2010)] <- 0
data1$THERM.NOVEMBER.2010[is.na(data1$THERM.NOVEMBER.2010)] <- 0
data1$THERM.DECEMBER.2010[is.na(data1$THERM.DECEMBER.2010)] <- 0

data1$KWH.JANUARY.2010[is.na(data1$KWH.JANUARY.2010)] <- 0
data1$KWH.FEBRUARY.2010[is.na(data1$KWH.FEBRUARY.2010)] <- 0
data1$KWH.MARCH.2010[is.na(data1$KWH.MARCH.2010)] <- 0
data1$KWH.APRIL.2010[is.na(data1$KWH.APRIL.2010)] <- 0
data1$KWH.MAY.2010[is.na(data1$KWH.MAY.2010)] <- 0
data1$KWH.JUNE.2010[is.na(data1$KWH.JUNE.2010)] <- 0
data1$KWH.JULY.2010[is.na(data1$KWH.JULY.2010)] <- 0
data1$KWH.AUGUST.2010[is.na(data1$KWH.AUGUST.2010)] <- 0
data1$KWH.SEPTEMBER.2010[is.na(data1$KWH.SEPTEMBER.2010)] <- 0
data1$KWH.OCTOBER.2010[is.na(data1$KWH.OCTOBER.2010)] <- 0
data1$KWH.NOVEMBER.2010[is.na(data1$KWH.NOVEMBER.2010)] <- 0
data1$KWH.DECEMBER.2010[is.na(data1$KWH.DECEMBER.2010)] <- 0


cook <- blocks(state = 'IL', county = 'Cook', year = 2010)
nwsBlocks <- subset(data1, COMMUNITY.AREA.NAME == 'Near West Side')
nwsBlocks <- unique(nwsBlocks[c('CENSUS.BLOCK')])
names(data1)[names(data1) == 'CENSUS.BLOCK'] <- 'GEOID10'

# drop nws' total row
nwsBlocks <- nwsBlocks %>% drop_na()

cookdata1 <- merge(cook, data1, by = 'GEOID10')

cooknws <- subset(cookdata1, cookdata1$GEOID10 %in% nwsBlocks$CENSUS.BLOCK)

totalKWH <- aggregate(
  cookdata1$TOTAL.KWH, by=list(GEOID10=cookdata1$GEOID10), FUN=sum)

# calculate monthly totals for thm, kwh
#~ thmByMonth = list(
#~   'jan' = aggregate(
#~     cookdata1$THERM.JANUARY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'feb' = aggregate(
#~     cookdata1$THERM.FEBRUARY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'mar' = aggregate(
#~     cookdata1$THERM.MARCH.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'apr' = aggregate(
#~     cookdata1$THERM.APRIL.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'may' = aggregate(
#~     cookdata1$THERM.MAY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'jun' = aggregate(
#~     cookdata1$THERM.JUNE.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'jul' = aggregate(
#~     cookdata1$THERM.JULY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'aug' = aggregate(
#~     cookdata1$THERM.AUGUST.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'sep' = aggregate(
#~     cookdata1$THERM.SEPTEMBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'oct' = aggregate(
#~     cookdata1$THERM.OCTOBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'nov' = aggregate(
#~     cookdata1$THERM.NOVEMBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'dec' = aggregate(
#~     cookdata1$THERM.DECEMBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum)
#~ )
#~ for (i in 1:length(thmByMonth)) {
#~   thmByMonth[[i]][thmByMonth[[i]] == 0] <- NA
#~   thmByMonth[[i]] <- merge(cooknws, thmByMonth[[i]], by = 'GEOID10')
#~ }
#~ kwhByMonth = list(
#~   'jan' = aggregate(
#~     cookdata1$KWH.JANUARY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'feb' = aggregate(
#~     cookdata1$KWH.FEBRUARY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'mar' = aggregate(
#~     cookdata1$KWH.MARCH.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'apr' = aggregate(
#~     cookdata1$KWH.APRIL.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'may' = aggregate(
#~     cookdata1$KWH.MAY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'jun' = aggregate(
#~     cookdata1$KWH.JUNE.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'jul' = aggregate(
#~     cookdata1$KWH.JULY.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'aug' = aggregate(
#~     cookdata1$KWH.AUGUST.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'sep' = aggregate(
#~     cookdata1$KWH.SEPTEMBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'oct' = aggregate(
#~     cookdata1$KWH.OCTOBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'nov' = aggregate(
#~     cookdata1$KWH.NOVEMBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum),
#~   'dec' = aggregate(
#~     cookdata1$KWH.DECEMBER.2010, by=list(GEOID10=cookdata1$GEOID10), FUN=sum)
#~ )
#~ for (i in 1:length(kwhByMonth)) {
#~   kwhByMonth[[i]][kwhByMonth[[i]] == 0] <- NA
#~   kwhByMonth[[i]] <- merge(cooknws, kwhByMonth[[i]], by = 'GEOID10')
#~ }

# if still zero, then change to NA
totalKWH[totalKWH == 0] <- NA

nwsTotalKWH <- merge(cooknws, totalKWH, by = 'GEOID10')

filters <- function(input, output) {
  selectedData <- cooknws
  
  if (length(input$building) != 3) {
    selectedData <- subset(selectedData, BUILDING.TYPE %in% input$building)
  }
  #print(head(selectedData, 10))
  
  if ((input$viewType == 'gas' || input$viewType == 'electric') &&
    input$month != 'all') {
    selectedData <- subset(selectedData, BUILDING.TYPE %in% input$building)
  }
  
  # month
  if (input$viewType == 'gas') {             # + month
    # consider month and building type, if selected
    if (input$month == 'all') {
      t <- aggregate(
        selectedData$TOTAL.THERMS, by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalThm <- merge(cooknws, t, by = 'GEOID10')
      m <- mapview(nwsTotalThm, zcol = 'x')
      output$mapplot <- renderLeaflet({
        m@map
      })
    } else {
      i <- paste0('THERM', input$month)
      t <- aggregate(
        selectedData[[i]], by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalThm <- merge(cooknws, t, by = 'GEOID10')
      m <- mapview(nwsTotalThm, zcol = 'x')
      output$mapplot <- renderLeaflet({
        m@map
      })
    }
  } else if (input$viewType == 'electric') { # + month
    if (input$month == 'all') {
      t <- aggregate(
        selectedData$TOTAL.KWH, by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalKWH <- merge(cooknws, t, by = 'GEOID10')
      m <- mapview(nwsTotalKWH, zcol = 'x')
      output$mapplot <- renderLeaflet({
        m@map
      })
    } else {
      i <- paste0('KWH', input$month)
      t <- aggregate(
        selectedData[[i]], by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalKwh <- merge(cooknws, t, by = 'GEOID10')
      m <- mapview(nwsTotalKwh, zcol = 'x')
      output$mapplot <- renderLeaflet({
        m@map
      })
    }
  } else if (input$viewType == 'age') {
    # consider building type, if selected
    t <- aggregate(
      selectedData$AVERAGE.BUILDING.AGE, by=list(GEOID10=selectedData$GEOID10), FUN=mean)
    t$x <- round(t$x, 1)
    t[t == 0] <- NA
    nwsAvgAge <- merge(cooknws, t, by = 'GEOID10')
    m <- mapview(nwsAvgAge, zcol = 'x')
    output$mapplot <- renderLeaflet({
      m@map
    })
  } else if (input$viewType == 'type') {
    options(warn = -1)
    t <- aggregate(
      selectedData$BUILDING.TYPE, by=list(GEOID10=selectedData$GEOID10),
      FUN = function(x) {
        if (length(unique(x)) == 1) {
          if (x == 'Residential')
            'Residential'
          else if (x == 'Industrial')
            'Industrial'
          else if (x == 'Commercial')
            'Commercial'
          else
            'test'
        } else {
          'Mixed'
        }
      }
    )
    options(warn = 0) # reset
    t <- merge(cooknws, t, by = 'GEOID10')
    m <- mapview(t, zcol = 'x')
    output$mapplot <- renderLeaflet({
      m@map
    })
  } else if (input$viewType == 'height') {
    # consider building type, if selected
    t <- aggregate(
      selectedData$AVERAGE.STORIES, by=list(GEOID10=selectedData$GEOID10), FUN=mean)
    t[t == 0] <- NA
    nwsAvgStories <- merge(cooknws, t, by = 'GEOID10')
    m <- mapview(nwsAvgStories, zcol = 'x')
    output$mapplot <- renderLeaflet({
      m@map
    })
  } else if (input$viewType == 'population') {
    # consider building type, if selected
    t <- aggregate( # mean unnecessary but fits workflow
      selectedData$TOTAL.POPULATION, by=list(GEOID10=selectedData$GEOID10), FUN=mean)
    t[t == 0] <- NA
    nwsTotalPop <- merge(cooknws, t, by = 'GEOID10')
    m <- mapview(nwsTotalPop, zcol = 'x')
    output$mapplot <- renderLeaflet({
      m@map
    })
  }
  
  # Remove geometry, coerce to data.frame
  # https://www.rdocumentation.org/packages/sf/versions/0.7-3/topics/st_geometry
  # selectedData refers to all census areas currently showing
  
  # electric
  output$bar1 <- renderPlot({
    t <- st_set_geometry(selectedData, NULL) 
    mos <- c(1:12)
    t <- data.frame(
      mWh = colSums(
        t[, kwhMonths],
        na.rm = TRUE),
      month = mos
    )
    t$mWh <- round(t$mWh / 1000, 1)
    t$month <- factor(t$month, levels = mos)
    ggplot(t, aes(x = month, y = mWh), show.legend = FALSE) + 
      geom_col(fill = 'blue', width = 0.75, show.legend = FALSE) +
      labs(x = 'Month', y = 'Energy (mWh)') +
      scale_y_continuous(label = comma)
  })
  
  # gas
  output$bar2 <- renderPlot({
    t <- st_set_geometry(selectedData, NULL) 
    mos <- c(1:12)
    t <- data.frame(
      mWh = colSums(
        t[, thmMonths],
        na.rm = TRUE),
      month = mos
    )
    t$mWh <- round(t$mWh / 1000, 1)
    t$month <- factor(t$month, levels = mos)
    ggplot(t, aes(x = month, y = mWh), show.legend = FALSE) + 
      geom_col(fill = 'blue', width = 0.75, show.legend = FALSE) +
      labs(x = 'Month', y = 'Energy (mWh)') +
      scale_y_continuous(label = comma)
  })
  
  # dt
  output$data1tbl <- DT::renderDataTable({
    t <- st_set_geometry(selectedData, NULL) 
    mos <- c(1:12)
    t <- data.frame(
      mWh = colSums(
        t[, kwhMonths],
        na.rm = TRUE),
      month = mos
    )
    t$mWh <- round(t$mWh / 1000, 1)
    t$month <- factor(t$month, levels = mos)
    DT::datatable(t, escape = FALSE, options = list(pageLength = 3, dom = 'tp'))
  })
  output$data2tbl <- DT::renderDataTable({
    t <- st_set_geometry(selectedData, NULL) 
    mos <- c(1:12)
    t <- data.frame(
      mWh = colSums(
        t[, thmMonths],
        na.rm = TRUE),
      month = mos
    )
    t$mWh <- round(t$mWh / 1000, 1)
    t$month <- factor(t$month, levels = mos)
    DT::datatable(t, escape = FALSE, options = list(pageLength = 3, dom = 'tp'))
  })
  
  
#~   output$bar1 <- renderPlot({
#~     a = doFilters(input, FALSE, input$STATE1, input$YEAR1)
#~     ggplot(a, aes(x = YEAR, y = GEN)) + 
#~       geom_col(aes(fill = ENERGY.SOURCE), width = 0.7) +
#~       labs(x = 'Year', y = 'Energy produced') +
#~       scale_y_continuous(label = comma)
#~   })
}

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
  m3 <- mapview(nwsTotalKWH, zcol = 'x')
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

  observeEvent(input$building, {
    filters(input, output)
  })
  
  observeEvent(input$month, {
    filters(input, output)
  })
  
  observeEvent(input$viewType, {
    filters(input, output)
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
  
#~   output$data1tbl <- DT::renderDataTable({
#~     df <- data1 %>%
#~       mutate(Action = paste(
#~         '<a class="go-map" href="" data-lat="', Lat,
#~         '" data-long="', Lng,
#~         '"><i class="fa fa-crosshairs"></i></a>', sep = ''))
#~     action <- DT::dataTableAjax(session, df, outputId = 'data1tbl')
#~     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#~   })
#~   output$data2tbl <- DT::renderDataTable({
#~     a <- paste(
#~       '<a class="go-map" href=""><i class="fa fa-crosshairs"></i></a>',
#~       sep = ''
#~     )
#~     df <- data2 %>%
#~       mutate(Action = a)
#~     action <- DT::dataTableAjax(session, df, outputId = 'data2tbl')
#~     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#~   })
#~   output$data3tbl <- DT::renderDataTable({
#~     a <- paste(
#~       '<a class="go-map" href=""><i class="fa fa-crosshairs"></i></a>',
#~       sep = ''
#~     )
#~     df <- data3 %>%
#~       mutate(Action = a)
#~     action <- DT::dataTableAjax(session, df, outputId = 'data3tbl')
#~     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#~   })
  
})
