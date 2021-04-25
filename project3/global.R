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
data1$OCCUPIED.UNITS.PERCENTAGE <- as.double(data1$OCCUPIED.UNITS.PERCENTAGE)
data1$RENTER.OCCUPIED.HOUSING.PERCENTAGE <- as.double(data1$RENTER.OCCUPIED.HOUSING.PERCENTAGE)

data1$BUILDING.TYPE <- as.factor(data1$BUILDING.TYPE)

#~ data1$TOTAL.KWH[is.na(data1$TOTAL.KWH)] <- 0
#~ data1$TOTAL.THERMS[is.na(data1$TOTAL.THERMS)] <- 0
#~ data1$AVERAGE.STORIES[is.na(data1$AVERAGE.STORIES)] <- 0
#~ data1$TOTAL.POPULATION[is.na(data1$TOTAL.POPULATION)] <- 0
#~ data1$AVERAGE.BUILDING.AGE[is.na(data1$AVERAGE.BUILDING.AGE)] <- 0
#data1$OCCUPIED.UNITS.PERCENTAGE[is.na(data1$OCCUPIED.UNITS.PERCENTAGE)] <- 0
#data1$RENTER.OCCUPIED.HOUSING.PERCENTAGE[is.na(data1$RENTER.OCCUPIED.HOUSING.PERCENTAGE)] <- 0

# Representing as decimal causes issue later with mapview!
# (Warning in min(x) : no non-missing arguments to min; returning Inf)
data1$OCCUPIED.UNITS.PERCENTAGE <- data1$OCCUPIED.UNITS.PERCENTAGE * 100
data1$RENTER.OCCUPIED.HOUSING.PERCENTAGE <-
  data1$RENTER.OCCUPIED.HOUSING.PERCENTAGE * 100

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

names(data1)[names(data1) == 'CENSUS.BLOCK'] <- 'GEOID10'

# blocks
cook <- blocks(state = 'IL', county = 'Cook', year = 2010)
cookdata1 <- merge(cook, data1, by = 'GEOID10')

# tracks
# cb = FALSE
cookTracts <- tracts(state = 'IL', county = 'Cook', year = 2010)
# add a col to data1 for tracts, then merge with tracts
# 17031840300 vs.
# 170318403001021
tmp <- data1
names(tmp)[names(tmp) == 'GEOID10'] <- 'GEOID17____'
tmp$GEOID10 <- substr(tmp$GEOID17____, 0, 11)
cookTracts <- merge(cookTracts, tmp, by = 'GEOID10')
cookTracts$GEOID10 <- as.character(cookTracts$GEOID10)

#~ # cb = TRUE generalized file ??
#~ # but must change GEO_ID/GEOID10 and trim
#~ # 1400000US17031440300 vs.
#~ # 17031440300
#~ cookTracts <- tracts(state = 'IL', county = 'Cook', year = 2010, cb = TRUE)
#~ names(cookTracts)[names(cookTracts) == 'GEO_ID'] <- 'GEOID10'
#~ cookTracts$GEOID10 <- substr(cookTracts$GEOID10, 10, 20)
#~ # add a col to data1 for tracts, then merge with tracts
#~ # 17031840300 vs.
#~ # 170318403001021
#~ tmp <- data1
#~ names(tmp)[names(tmp) == 'GEOID10'] <- 'GEOID17____'
#~ tmp$GEOID10 <- substr(tmp$GEOID17____, 0, 11)
#~ print(head(tmp, 1))
#~ print(head(cookTracts, 1))
#~ cookTracts <- merge(cookTracts, tmp, by = 'GEOID10')
#~ print(head(cookTracts, 1))

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

#
# selectedData
# col: eg 'AVERAGE.STORIES'
# aggType: eg mean or sum
#  -- Note: Using an external vector in selections is ambiguous.
#  -- ℹ Use `all_of(col)` instead of `col` to silence this message.
agg <- function(selectedData, col, aggType) {
  selectedData <- selectedData %>% drop_na(all_of(col))
  t <- aggregate(
    selectedData[[col]], by=list(GEOID10=selectedData$GEOID10), FUN=aggType)
  t$x <- round(t$x, 1)
  #t[t == 0] <- NA
  t <- merge(selectedData, t, by = 'GEOID10')
  t <- t[!duplicated(t[,c('GEOID10')]),] # remove duplicates
#~   t <- t[order(t[[col]]),] # order eg t$AVERAGE.STORIES
  # don't order by eg t$AVERAGE.STORIES, order by the aggregate (ie x)!
  t <- t[order(t$x),] 
}

filters2 <- function(input, output, side) {
  s <- side

  # Doesn't work :(
  e = list (
    #in
#~     community = input[[paste0('community', s)]],
#~     ...
    #out
#~     mapplot = output[[paste0('mapplot', s)]],
#~     ...
  )
  
  if (input[[paste0('bt', s)]] == 'blocks') {
    n <- subset(data1, COMMUNITY.AREA.NAME == input[[paste0('community', s)]])
    n <- unique(n[c('GEOID10')])
    n <- n %>% drop_na(GEOID10) # drop where GEOID10 is empty, i.e., the total rows
    selectedData <- subset(cookdata1, cookdata1$GEOID10 %in% n$GEOID10)
  } else {
    selectedData <- cookTracts %>% drop_na(GEOID10) # drop where GEOID10 is empty, i.e., the total rows
    #selectedData <- cookTracts
  }
  
  if (length(input[[paste0('building', s)]]) != 3) {
    selectedData <- subset(selectedData, BUILDING.TYPE %in% input[[paste0('building', s)]])
  }
  
  if ((input[[paste0('viewType', s)]] == 'gas' || input[[paste0('viewType', s)]] == 'electric') &&
    input[[paste0('month', s)]] != 'all') {
    selectedData <- subset(selectedData, BUILDING.TYPE %in% input[[paste0('building', s)]])
  }
  
  # No data
  if (nrow(selectedData) == 0) {
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
  
  # sequential colors via colorbrewer
  if (input[[paste0('color', s)]] == 1) { # default
    mapviewOptions(vector.palette = colorRampPalette(c("#fcc5c0", "#dd3497", "#49006a")))
  } else if (input[[paste0('color', s)]] == 2) {
    mapviewOptions(vector.palette = colorRampPalette(c("#d0d1e6", "#74a9cf", "#023858")))
  } else {
    # Kind of bug: Basemap changes on its own with certain color
    # palettes, including this one, especially when data is mostly
    # low-valued ad has a few high-valued data points.
    mapviewOptions(vector.palette = colorRampPalette(c("#fee8c8", "#ef6548", "#7f0000")))
  }
  
  # month
  if (input[[paste0('viewType', s)]] == 'gas') {
    selectedData <- selectedData %>% drop_na(TOTAL.THERMS)
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
  } else if (input[[paste0('viewType', s)]] == 'electric') {
    selectedData <- selectedData %>% drop_na(TOTAL.KWH)
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
    selectedData <- selectedData %>% drop_na(AVERAGE.BUILDING.AGE)
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
    selectedData <- selectedData %>% drop_na(AVERAGE.STORIES)
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
    selectedData <- selectedData %>% drop_na(TOTAL.POPULATION)
    t <- aggregate( # mean unnecessary but fits workflow
      selectedData$TOTAL.POPULATION, by=list(GEOID10=selectedData$GEOID10), FUN=mean)
    t[t == 0] <- NA
    nwsTotalPop <- merge(selectedData, t, by = 'GEOID10')
    m <- mapview(nwsTotalPop, zcol = 'x')
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } 
  # View types for top 10% / full city, only when tracts is selected
  # Use drop_na to remove empty/NA data (note: keeps 0s)
  else if (input[[paste0('bt', s)]] == 'tracts') {
    if (input[[paste0('viewType', s)]] == '10oldest') {
      t <- agg(selectedData, 'AVERAGE.BUILDING.AGE', mean)
      t <- tail(t, round(length(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10newest') {
      t <- agg(selectedData, 'AVERAGE.BUILDING.AGE', mean)
      t <- head(t, round(length(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10tallest') {
      t <- agg(selectedData, 'AVERAGE.STORIES', mean)
      t <- tail(t, round(length(t) * 0.1)) # trim
      print(head(t, 2))
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10shortest') {
      t <- agg(selectedData, 'AVERAGE.STORIES', mean)
      t <- head(t, round(length(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10electric') {
      t <- agg(selectedData, 'TOTAL.KWH', sum)
      t <- tail(t, round(length(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10gas') {
      t <- agg(selectedData, 'TOTAL.THERMS', sum)
      t <- tail(t, round(length(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10population') {
      t <- agg(selectedData, 'TOTAL.POPULATION', sum)
      t <- tail(t, round(length(t) * 0.1)) # trim
      print(head(t, 4))
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10poplowest') {
      t <- agg(selectedData, 'TOTAL.POPULATION', sum)
      t <- head(t, round(length(t) * 0.1)) # trim
      print(head(t, 4))
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10occupied') {
      t <- agg(selectedData, 'OCCUPIED.UNITS.PERCENTAGE', mean)
      t <- tail(t, round(length(t) * 0.1)) # trim
      print(head(t, 2))
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10renters') {
      t <- agg(selectedData, 'RENTER.OCCUPIED.HOUSING.PERCENTAGE', mean)
      t <- tail(t, round(length(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x')
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    }
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
  
  mapviewOptions(vector.palette = colorRampPalette(c("#fcc5c0", "#dd3497", "#49006a")))
  mapviewOptions(basemaps = 
    c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap",
      "Esri.WorldImagery", "OpenTopoMap"))
  
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
      m <- mapview(nwsTotalThm, zcol = 'x', basemap = 'CartoDB.Positron')
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
