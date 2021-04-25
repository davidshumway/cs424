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

data1 <- read.csv('data/Energy_Usage_2010.csv')
#~ data1 <- read.csv('data/test.csv')

# always room for broken data
names(data1)[names(data1) == 'TERM.APRIL.2010'] <- 'THERM.APRIL.2010'

communities <- unique(data1[c('COMMUNITY.AREA.NAME')])
print(paste0('#c:', nrow(communities)))

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

for (i in 1:length(kwhMonths)) {
  n <- kwhMonths[i]
#~   data1[[n]] <- as.double(data1[[n]])
  data1[[n]][is.na(data1[[n]])] <- 0
}
for (i in 1:length(thmMonths)) {
  n <- kwhMonths[i]
#~   data1[[n]] <- as.double(data1[[n]])
  data1[[n]][is.na(data1[[n]])] <- 0
}

data1$TOTAL.KWH[is.na(data1$TOTAL.KWH)] <- 0
data1$TOTAL.THERMS[is.na(data1$TOTAL.THERMS)] <- 0
data1$AVERAGE.STORIES[is.na(data1$AVERAGE.STORIES)] <- 0
data1$TOTAL.POPULATION[is.na(data1$TOTAL.POPULATION)] <- 0
data1$AVERAGE.BUILDING.AGE[is.na(data1$AVERAGE.BUILDING.AGE)] <- 0

names(data1)[names(data1) == 'CENSUS.BLOCK'] <- 'GEOID10'
cook <- blocks(state = 'IL', county = 'Cook', year = 2010)
cookdata1 <- merge(cook, data1, by = 'GEOID10')

# Data for initial load
nwsBlocks <- subset(data1, COMMUNITY.AREA.NAME == 'Near West Side')
nwsBlocks <- unique(nwsBlocks[c('GEOID10')])
loopBlocks <- subset(data1, COMMUNITY.AREA.NAME == 'Loop')
loopBlocks <- unique(loopBlocks[c('GEOID10')])
nwsBlocks <- nwsBlocks %>% drop_na()   # drop total row
loopBlocks <- loopBlocks %>% drop_na() # drop total row
cooknws <- subset(cookdata1, cookdata1$GEOID10 %in% nwsBlocks$GEOID10)
cookloop <- subset(cookdata1, cookdata1$GEOID10 %in% loopBlocks$GEOID10)
# kwh initial load
totalKWH <- aggregate(
  cookdata1$TOTAL.KWH, by=list(GEOID10=cookdata1$GEOID10), FUN=sum)
# if zero, change to NA
totalKWH[totalKWH == 0] <- NA
nwsTotalKWH <- merge(cooknws, totalKWH, by = 'GEOID10')
loopTotalKWH <- merge(cookloop, totalKWH, by = 'GEOID10')
options(shiny.fullstacktrace = TRUE)

render <- function(outputElem, reactiveElem, side, output) {
  s <- side
  if (outputElem == 'mapplot') {
    output[[paste0('mapplot', s)]] <- reactiveElem
  } else if (outputElem == 'bar1') {
    output[[paste0('bar', s, '1')]] <- reactiveElem
  } else if (outputElem == 'bar2') {
    output[[paste0('bar', s, '2')]] <- reactiveElem
  } else if (outputElem == 'data1tbl') {
    output[[paste0('data', s, '1tbl')]] <- reactiveElem
  } else if (outputElem == 'data2tbl') {
    output[[paste0('data', s, '2tbl')]] <- reactiveElem
  }
}

filters2 <- function(input, output, side) {
  s <- side

  # doesn't work :(
  e = list (
    #in
#~     community = input[[paste0('community', s)]],
#~     building = input[[paste0('building', s)]],
#~     viewType = input[[paste0('viewType', s)]],
#~     month = input[[paste0('month', s)]]#,
    #out
#~     mapplot = output[[paste0('mapplot', s)]],
#~     bar1 = output[[paste0('bar', s, '1')]],
#~     bar2 = output[[paste0('bar', s, '2')]],
#~     data1tbl = output[[paste0('data', s, '1tbl')]],
#~     data2tbl = output[[paste0('data', s, '2tbl')]]
  )
  n <- subset(data1, COMMUNITY.AREA.NAME == input[[paste0('community', s)]])
  n <- unique(n[c('GEOID10')])
  n <- n %>% drop_na()   # drop total row
  selectedData <- subset(cookdata1, cookdata1$GEOID10 %in% n$GEOID10)
  
  if (length(input[[paste0('building', s)]]) != 3) {
    selectedData <- subset(selectedData, BUILDING.TYPE %in% input[[paste0('building', s)]])
  }
  
  if ((input[[paste0('viewType', s)]] == 'gas' || input[[paste0('viewType', s)]] == 'electric') &&
    input[[paste0('month', s)]] != 'all') {
    selectedData <- subset(selectedData, BUILDING.TYPE %in% input[[paste0('building', s)]])
  }
  
  # No data
  if (nrow(selectedData) == 0) {
    print('it is zerooooooooooo')
    m <- mapview()
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
    
    render('bar1', renderPlot({ggplot(data.frame(), show.legend = FALSE)}), s, output)
    render('bar2', renderPlot({ggplot(data.frame(), show.legend = FALSE)}), s, output)
    
    a <- DT::renderDataTable({
      DT::datatable(data.frame())
    })
    render('data1tbl', a, s, output)
    a <- DT::renderDataTable({
      DT::datatable(data.frame())
    })
    render('data2tbl', a, s, output)
    
    return()
  }
  
  # month
  if (input[[paste0('viewType', s)]] == 'gas') {             # + month
    # consider month and building type, if selected
    if (input[[paste0('month', s)]] == 'all') {
      t <- aggregate(
        selectedData$TOTAL.THERMS, by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalThm <- merge(selectedData, t, by = 'GEOID10')
      m <- mapview(nwsTotalThm, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else {
      i <- paste0('THERM', input[[paste0('month', s)]])
      t <- aggregate(
        selectedData[[i]], by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalThm <- merge(selectedData, t, by = 'GEOID10')
      m <- mapview(nwsTotalThm, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    }
  } else if (input[[paste0('viewType', s)]] == 'electric') { # + month
    if (input[[paste0('month', s)]] == 'all') {
      t <- aggregate(
        selectedData$TOTAL.KWH, by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalKWH <- merge(selectedData, t, by = 'GEOID10')
      m <- mapview(nwsTotalKWH, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else {
      i <- paste0('KWH', input[[paste0('month', s)]])
      t <- aggregate(
        selectedData[[i]], by=list(GEOID10=selectedData$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalKwh <- merge(selectedData, t, by = 'GEOID10')
      m <- mapview(nwsTotalKwh, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    }
  } else if (input[[paste0('viewType', s)]] == 'age') {
    # consider building type, if selected
    t <- aggregate(
      selectedData$AVERAGE.BUILDING.AGE, by=list(GEOID10=selectedData$GEOID10), FUN=mean)
    t$x <- round(t$x, 1)
    t[t == 0] <- NA
    nwsAvgAge <- merge(selectedData, t, by = 'GEOID10')
    m <- mapview(nwsAvgAge, zcol = 'x')
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'type') {
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
    t <- merge(selectedData, t, by = 'GEOID10')
    m <- mapview(t, zcol = 'x')
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'height') {
    # consider building type, if selected
    t <- aggregate(
      selectedData$AVERAGE.STORIES, by=list(GEOID10=selectedData$GEOID10), FUN=mean)
    t[t == 0] <- NA
    nwsAvgStories <- merge(selectedData, t, by = 'GEOID10')
    m <- mapview(nwsAvgStories, zcol = 'x')
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'population') {
    # consider building type, if selected
    t <- aggregate( # mean unnecessary but fits workflow
      selectedData$TOTAL.POPULATION, by=list(GEOID10=selectedData$GEOID10), FUN=mean)
    t[t == 0] <- NA
    nwsTotalPop <- merge(selectedData, t, by = 'GEOID10')
    m <- mapview(nwsTotalPop, zcol = 'x')
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  }
  
  # Remove geometry, coerce to data.frame
  # https://www.rdocumentation.org/packages/sf/versions/0.7-3/topics/st_geometry
  # selectedData refers to all census areas currently showing
  
  # electric
  a <- renderPlot({
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
  render('bar1', a, s, output)
  
  # gas
  a <- renderPlot({
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
  render('bar2', a, s, output)
  
  # dt
  a <- DT::renderDataTable({
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
  render('data1tbl', a, s, output)
  a <- DT::renderDataTable({
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
  render('data2tbl', a, s, output)
}

filters <- function(input, output) {
  selectedData <- cooknws
  
  if (length(input$building) != 3) {
    selectedData <- subset(selectedData, BUILDING.TYPE %in% input$building)
  }
  
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
}
