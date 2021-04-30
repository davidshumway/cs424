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

########################################################################
# Code used to download additional census data
########################################################################

# Quick lookup for codes:
#  https://cran.r-project.org/web/packages/UScensus2000tract/UScensus2000tract.pdf
# 700-page guide: https://www.census.gov/prod/cen2010/doc/sf1.pdf
# Double check it's right: https://api.census.gov/data/2010/dec/sf1/groups/H5.html
# Sys.setenv(CENSUS_KEY=...)

# What is pop? Hint: Matches chicago file.
#~ a <- getCensus(
#~   name = "dec/sf1",
#~   vintage = 2010,
#~   vars = "P001001", 
#~   region = "block:*",
#~   regionin = "state:17+county:031+tract:*")

# P13. MEDIAN AGE BY SEX [3] Both sexes (1 expressed decimal)
#~ a <- getCensus(
#~   name = "dec/sf1",
#~   vintage = 2010,
#~   vars = "P013001", 
#~   region = "block:*",
#~   regionin = "state:17+county:031+tract:*")
#~ write.csv(a, 'ageTract.csv')

#(H003003) hh.vacant vacant housing units
#~ a <- getCensus(
#~   name = "dec/sf1",
#~   vintage = 2010,
#~   vars = "H003003", 
#~   region = "block:*",
#~   regionin = "state:17+county:031+tract:*")
#~ write.csv(a, 'vacant.csv')

#(H005007) hh.vacant vacant housing units
#~ a <- getCensus(
#~   name = "dec/sf1",
#~   vintage = 2010,
#~   vars = "H005007", 
#~   region = "block:*",
#~   regionin = "state:17+county:031+tract:*")
#~ write.csv(a, 'migrantTract.csv')

########################################################################
# End census download code
########################################################################

data1 <- read.csv('data/Energy_Usage_2010.csv')
dataAge <- read.csv('data/age.csv')
dataMigrant <- read.csv('data/migrant.csv')

dataAge$P013001 <- as.double(dataAge$P013001)
dataMigrant$H005007 <- as.double(dataMigrant$H005007)

dataAge$GEOID10 <- paste0(
  dataAge$state, '0',
  dataAge$county,
  dataAge$tract,
  dataAge$block)
dataMigrant$GEOID10 <- paste0(
  dataMigrant$state, '0',
  dataMigrant$county,
  dataMigrant$tract,
  dataMigrant$block)

# generate tract data: aggregate, remove dups, rename, round
# warning for mean: contains 0s
dataAge$GEOIDtmp <- paste0(
  dataAge$state, '0',
  dataAge$county,
  dataAge$tract)
dataMigrant$GEOIDtmp <- paste0(
  dataMigrant$state, '0',
  dataMigrant$county,
  dataMigrant$tract)
y <- dataAge
y <- y[!(y$P013001 == 0),]
dataAgeTract <- aggregate(y$P013001,
  by=list(GEOIDtmp=y$GEOIDtmp), FUN=mean)
dataMigrantTract <- aggregate(dataMigrant$H005007,
  by=list(GEOIDtmp=dataMigrant$GEOIDtmp), FUN=sum)

dataAgeTract <- dataAgeTract[!duplicated(dataAgeTract[,c('GEOIDtmp')]),]
dataMigrantTract <-
  dataMigrantTract[!duplicated(dataMigrantTract[,c('GEOIDtmp')]),]

names(dataAgeTract)[names(dataAgeTract) == 'GEOIDtmp'] <- 'GEOID10'
names(dataMigrantTract)[names(dataMigrantTract) == 'GEOIDtmp'] <- 'GEOID10'

dataAgeTract$x <- round(dataAgeTract$x, 1)

# NA for block data
dataAge[dataAge == 0] <- NA
dataMigrant[dataMigrant == 0] <- NA

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
  data1[[n]][is.na(data1[[n]])] <- 0
}
for (i in 1:length(thmMonths)) {
  n <- kwhMonths[i]
  data1[[n]][is.na(data1[[n]])] <- 0
}

names(data1)[names(data1) == 'CENSUS.BLOCK'] <- 'GEOID10'

# blocks
cook <- blocks(state = 'IL', county = 'Cook', year = 2010)

# block data
cookdata1 <- merge(cook, data1, by = 'GEOID10')

# tracts
# cb = FALSE
tracts <- tracts(state = 'IL', county = 'Cook', year = 2010)
print(head(tracts))
tracts <- subset(tracts, select = c(GEOID10, geometry))
# add a col to data1 for tracts, then merge with tracts
# 17031840300 vs.
# 170318403001021
tmp <- data1
names(tmp)[names(tmp) == 'GEOID10'] <- 'GEOID17____'
tmp$GEOID10 <- substr(tmp$GEOID17____, 0, 11)
cookTracts <- merge(tracts, tmp, by = 'GEOID10')
cookTracts$GEOID10 <- as.character(cookTracts$GEOID10)

# Caution: total pop. is 305 for four blocks with #170317608012014.
#~ print(subset(cookTracts, GEOID17____ == '170317608012014'))

# worker / age
cookAge <- merge(cook, dataAge, by = 'GEOID10') #block
cookAgeTract <- merge(tracts, dataAgeTract, by = 'GEOID10')
cookMigrant <- merge(cook, dataMigrant, by = 'GEOID10') #block
cookMigrantTract <- merge(tracts, dataMigrantTract, by = 'GEOID10')

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
#~ cookTracts <- merge(cookTracts, tmp, by = 'GEOID10')

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

# agg
# selection
# col: eg 'AVERAGE.STORIES'
# aggType: eg mean or sum
#  -- Note: Using an external vector in selections is ambiguous.
#  -- ℹ Use `all_of(col)` instead of `col` to silence this message.
agg <- function(selection, col, aggType) {
  selection <- selection %>% drop_na(all_of(col))
  t <- aggregate(
    selection[[col]], by=list(GEOID10=selection$GEOID10), FUN=aggType)
  t$x <- round(t$x, 1)
  #t[t == 0] <- NA
  t <- merge(selection, t, by = 'GEOID10')
  t <- t[!duplicated(t[,c('GEOID10')]),] # remove duplicates
  # don't order by eg t$AVERAGE.STORIES, order by the aggregate (ie x)!
  t <- t[order(t$x),] 
}

emptyPlots <- function(a, s, output) {
  a <- renderPlot({
  })
  render('bar1', a, s, output)
  
  # gas
  a <- renderPlot({
  })
  render('bar2', a, s, output)
  
  # dt
  a <- DT::renderDataTable({
  })
  render('data1tbl', a, s, output)
  a <- DT::renderDataTable({
  })
  render('data2tbl', a, s, output)
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
    communityGeos <- list()
    n <- subset(data1, COMMUNITY.AREA.NAME == input[[paste0('community', s)]])
    n <- unique(n[c('GEOID10')])
    n <- n %>% drop_na(GEOID10) # drop where GEOID10 is empty, i.e., the total rows
    communityGeos <- n
    selection <- subset(cookdata1, cookdata1$GEOID10 %in% communityGeos$GEOID10)
#~     if (input[[paste0('viewType', s)]] != 'resAge') {
#~       selection <- subset(cookdata1, cookdata1$GEOID10 %in% communityGeos$GEOID10)
#~     } else if (input[[paste0('viewType', s)]] == 'resAge') {
#~       selection <- subset(cookAge, cookAge$GEOID10 %in% communityGeos$GEOID10)
#~     }
  } else if (input[[paste0('bt', s)]] == 'tracts') {
    selection <- cookTracts %>% drop_na(GEOID10) # drop where GEOID10 is empty, i.e., the total rows
    #selection <- cookTracts
    
#~     if (input[[paste0('viewType', s)]] == 'resAge') {
#~       selection <- cookAgeTract %>% drop_na(GEOID10)
#~     }
  }
  
  
    
  if (length(input[[paste0('building', s)]]) != 3) {
    selection <- subset(selection, BUILDING.TYPE %in% input[[paste0('building', s)]])
  }
  
#~   # filter month
#~   if ((input[[paste0('viewType', s)]] == 'gas' || input[[paste0('viewType', s)]] == 'electric') &&
#~     input[[paste0('month', s)]] != 'all') {
#~     if (input[[paste0('viewType', s)]] == 'gas')
#~       i <- paste0('THERM', input[[paste0('month', s)]])
#~     else
#~       i <- paste0('KWH', input[[paste0('month', s)]])
#~     selection <- subset(selection, )
#~   }
  
  # No data
  if (nrow(selection) == 0) {
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
  
  ###selection <- selection %>% drop_na(TOTAL.THERMS)#??
  ###selection <- selection %>% drop_na(TOTAL.KWH)
  
  # view type
  if (input[[paste0('viewType', s)]] == 'gas') {
    selection <- selection %>% drop_na(TOTAL.THERMS)
    # consider month and building type, if selected
    if (input[[paste0('month', s)]] == 'all') {
      t <- aggregate(
        selection$TOTAL.THERMS, by=list(GEOID10=selection$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalThm <- merge(selection, t, by = 'GEOID10')
      m <- mapview(nwsTotalThm, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else {
      i <- paste0('THERM', input[[paste0('month', s)]])
      t <- aggregate(
        selection[[i]], by=list(GEOID10=selection$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalThm <- merge(selection, t, by = 'GEOID10')
      m <- mapview(nwsTotalThm, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    }
  } else if (input[[paste0('viewType', s)]] == 'electric') {
    selection <- selection %>% drop_na(TOTAL.KWH)
    if (input[[paste0('month', s)]] == 'all') {
      t <- aggregate(
        selection$TOTAL.KWH, by=list(GEOID10=selection$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalKWH <- merge(selection, t, by = 'GEOID10')
      m <- mapview(nwsTotalKWH, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else {
      i <- paste0('KWH', input[[paste0('month', s)]])
      t <- aggregate(
        selection[[i]], by=list(GEOID10=selection$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalKwh <- merge(selection, t, by = 'GEOID10')
      m <- mapview(nwsTotalKwh, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    }
  } else if (input[[paste0('viewType', s)]] == 'age') {
    # consider building type, if selected
    selection <- selection %>% drop_na(AVERAGE.BUILDING.AGE)
    t <- aggregate(
      selection$AVERAGE.BUILDING.AGE, by=list(GEOID10=selection$GEOID10), FUN=mean)
    t$x <- round(t$x, 1)
    t[t == 0] <- NA
    nwsAvgAge <- merge(selection, t, by = 'GEOID10')
    m <- mapview(nwsAvgAge, zcol = 'x', layer.name = paste0('layer', s))
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'type') {
    options(warn = -1)
    t <- aggregate(
      selection$BUILDING.TYPE, by=list(GEOID10=selection$GEOID10),
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
    t <- merge(selection, t, by = 'GEOID10')
    m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'height') {
    # consider building type, if selected
    selection <- selection %>% drop_na(AVERAGE.STORIES)
    t <- aggregate(
      selection$AVERAGE.STORIES, by=list(GEOID10=selection$GEOID10), FUN=mean)
#~     t[t == 0] <- NA
    nwsAvgStories <- merge(selection, t, by = 'GEOID10')
    m <- mapview(nwsAvgStories, zcol = 'x', layer.name = paste0('layer', s))
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'population') {
    # consider building type, if selected
    selection <- selection %>% drop_na(TOTAL.POPULATION)
    t <- aggregate( # mean unnecessary but fits workflow
      selection$TOTAL.POPULATION, by=list(GEOID10=selection$GEOID10), FUN=mean)
    nwsTotalPop <- merge(selection, t, by = 'GEOID10')
    nwsTotalPop$TOTAL.POPULATION[nwsTotalPop$TOTAL.POPULATION == 0] <- NA
    m <- mapview(nwsTotalPop, zcol = 'TOTAL.POPULATION', layer.name = paste0('layer', s)) #??? if x then wrong #
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'resAge') {
    if (input[[paste0('bt', s)]] == 'blocks') {
      t <- subset(cookAge, cookAge$GEOID10 %in% communityGeos$GEOID10)
      m <- mapview(t, zcol = 'P013001', layer.name = paste0('layer', s))
    } else {
      t <- cookAgeTract
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
    }
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
  } else if (input[[paste0('viewType', s)]] == 'migrant') { 
    if (input[[paste0('bt', s)]] == 'blocks') {
      t <- subset(cookMigrant, cookMigrant$GEOID10 %in% communityGeos$GEOID10)
      m <- mapview(t, zcol = 'H005007', layer.name = paste0('layer', s))
    } else {
      t <- cookMigrantTract
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
    }
    render('mapplot', renderLeaflet({
      m@map
    }), s, output)
#~     t <- subset(cookMigrant, cookMigrant$GEOID10 %in% communityGeos$GEOID10)
#~     m <- mapview(t, zcol = 'H005007', layer.name = paste0('layer', s))
#~     render('mapplot', renderLeaflet({
#~       m@map
#~     }), s, output)
#~     g <- unique(t[c('GEOID10')])
#~     selection <- subset(selection, selection$GEOID10 %in% g$GEOID10)
  }
#~   }
  # View types for top 10% / full city, only when tracts is selected
  # Use drop_na to remove empty/NA data (note: keeps 0s)
  else if (input[[paste0('bt', s)]] == 'tracts') {
    if (input[[paste0('viewType', s)]] == '10oldest') {
      t <- agg(selection, 'AVERAGE.BUILDING.AGE', mean)
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      #selection <- t #update this
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
    } else if (input[[paste0('viewType', s)]] == '10newest') {
      t <- agg(selection, 'AVERAGE.BUILDING.AGE', mean)
      t <- head(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      #selection <- t #update this
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
    } else if (input[[paste0('viewType', s)]] == '10tallest') {
      t <- agg(selection, 'AVERAGE.STORIES', mean)
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
#~       print(paste0('tall',length(selection)))
    } else if (input[[paste0('viewType', s)]] == '10shortest') {
      t <- agg(selection, 'AVERAGE.STORIES', mean)
      t <- head(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
#~       print(paste0('short',length(selection)))
    } else if (input[[paste0('viewType', s)]] == '10electric') {
      if (input[[paste0('month', s)]] == 'all')
        i <- 'TOTAL.KWH'
      else
        i <- paste0('KWH', input[[paste0('month', s)]])
      t <- agg(selection, i, sum)
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
    } else if (input[[paste0('viewType', s)]] == '10gas') {
      if (input[[paste0('month', s)]] == 'all')
        i <- 'TOTAL.THERMS'
      else
        i <- paste0('THERM', input[[paste0('month', s)]])
      t <- agg(selection, i, sum)
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
    } else if (input[[paste0('viewType', s)]] == '10population') {
      # Issue: in block it's mean, here it's sum! (Eg 03+03+04+04!!)
      # so start by dropping (subset)
      # Subsetting by 17 (block) or 10 (tract) is essentially the same
      # resutl as rows in cookTracts are identified by both. 
      g <- unique(selection[c('GEOID17____')])
      tmp <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
      t <- agg(tmp, 'TOTAL.POPULATION', sum)
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      print(head(t, 4))
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
    } else if (input[[paste0('viewType', s)]] == '10poplowest') {
      # Issue: same as above
      g <- unique(selection[c('GEOID17____')])
      tmp <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
      t <- agg(tmp, 'TOTAL.POPULATION', sum)
      t <- head(t, round(nrow(t) * 0.1)) # trim
      print(head(t, 4))
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
    } else if (input[[paste0('viewType', s)]] == '10occupied') {
      t <- agg(selection, 'OCCUPIED.UNITS.PERCENTAGE', mean)
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      print(head(t, 2))
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
    } else if (input[[paste0('viewType', s)]] == '10renters') {
      t <- agg(selection, 'RENTER.OCCUPIED.HOUSING.PERCENTAGE', mean)
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID17____')])
      selection <- subset(selection, selection$GEOID17____ %in% g$GEOID17____)
    } else if (input[[paste0('viewType', s)]] == '10age') {
      #selection <- cookAgeTract
      t <- cookAgeTract
      t <- t[order(t$x),]
      t <- head(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID10')])
      selection <- subset(selection, selection$GEOID10 %in% g$GEOID10)
    } else if (input[[paste0('viewType', s)]] == '10ageoldest') {
      #selection <- cookAgeTract
      t <- cookAgeTract
      t <- t[order(t$x),]
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID10')])
      selection <- subset(selection, selection$GEOID10 %in% g$GEOID10)
    } else if (input[[paste0('viewType', s)]] == '10worker') {
      # already in tract level
      #selection <- cookMigrantTract
      t <- cookMigrantTract
      t <- t[order(t$x),]
      t <- tail(t, round(nrow(t) * 0.1)) # trim
      t <- subset(t, t$x > 0) # less than 10% of tracts contain housing unit
      m <- mapview(t, zcol = 'x', layer.name = paste0('layer', s))
      render('mapplot', renderLeaflet({
        m@map
      }), s, output)
      g <- unique(t[c('GEOID10')])
      selection <- subset(selection, selection$GEOID10 %in% g$GEOID10)
    }
  }
  
  # Remove geometry, coerce to data.frame
  # https://www.rdocumentation.org/packages/sf/versions/0.7-3/topics/st_geometry
  # selection refers to all census areas currently showing
  
  # electric
  a <- renderPlot({
    t <- st_set_geometry(selection, NULL) 
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
    t <- st_set_geometry(selection, NULL) 
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
    t <- st_set_geometry(selection, NULL) 
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
    t <- st_set_geometry(selection, NULL) 
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
  selection <- cooknws
  
  mapviewOptions(vector.palette = colorRampPalette(c("#fcc5c0", "#dd3497", "#49006a")))
  mapviewOptions(basemaps = 
    c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap",
      "Esri.WorldImagery", "OpenTopoMap"))
  
  if (length(input$building) != 3) {
    selection <- subset(selection, BUILDING.TYPE %in% input$building)
  }
  
  if ((input$viewType == 'gas' || input$viewType == 'electric') &&
    input$month != 'all') {
    selection <- subset(selection, BUILDING.TYPE %in% input$building)
  }
  
  # month
  if (input$viewType == 'gas') {             # + month
    # consider month and building type, if selected
    if (input$month == 'all') {
      t <- aggregate(
        selection$TOTAL.THERMS, by=list(GEOID10=selection$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalThm <- merge(cooknws, t, by = 'GEOID10')
      m <- mapview(nwsTotalThm, zcol = 'x', basemap = 'CartoDB.Positron')
      output$mapplot <- renderLeaflet({
        m@map
      })
    } else {
      i <- paste0('THERM', input$month)
      t <- aggregate(
        selection[[i]], by=list(GEOID10=selection$GEOID10), FUN=sum)
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
        selection$TOTAL.KWH, by=list(GEOID10=selection$GEOID10), FUN=sum)
      t[t == 0] <- NA
      nwsTotalKWH <- merge(cooknws, t, by = 'GEOID10')
      m <- mapview(nwsTotalKWH, zcol = 'x')
      output$mapplot <- renderLeaflet({
        m@map
      })
    } else {
      i <- paste0('KWH', input$month)
      t <- aggregate(
        selection[[i]], by=list(GEOID10=selection$GEOID10), FUN=sum)
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
      selection$AVERAGE.BUILDING.AGE, by=list(GEOID10=selection$GEOID10), FUN=mean)
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
      selection$BUILDING.TYPE, by=list(GEOID10=selection$GEOID10),
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
      selection$AVERAGE.STORIES, by=list(GEOID10=selection$GEOID10), FUN=mean)
    t[t == 0] <- NA
    nwsAvgStories <- merge(cooknws, t, by = 'GEOID10')
    m <- mapview(nwsAvgStories, zcol = 'x')
    output$mapplot <- renderLeaflet({
      m@map
    })
  } else if (input$viewType == 'population') {
    # consider building type, if selected
    t <- aggregate( # mean unnecessary but fits workflow
      selection$TOTAL.POPULATION, by=list(GEOID10=selection$GEOID10), FUN=mean)
    t[t == 0] <- NA
    nwsTotalPop <- merge(cooknws, t, by = 'GEOID10')
    m <- mapview(nwsTotalPop, zcol = 'x')
    output$mapplot <- renderLeaflet({
      m@map
    })
  }
  
  # Remove geometry, coerce to data.frame
  # https://www.rdocumentation.org/packages/sf/versions/0.7-3/topics/st_geometry
  # selection refers to all census areas currently showing
  
  # electric
  output$bar1 <- renderPlot({
    t <- st_set_geometry(selection, NULL) 
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
    t <- st_set_geometry(selection, NULL) 
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
    t <- st_set_geometry(selection, NULL) 
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
    t <- st_set_geometry(selection, NULL) 
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
