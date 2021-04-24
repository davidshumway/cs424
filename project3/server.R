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

data1 <- read.csv('data/Energy_Usage_2010.csv')#, check.names = FALSE)
data1$TOTAL.KWH <- as.double(data1$TOTAL.KWH)
data1$TOTAL.THERMS <- as.double(data1$TOTAL.THERMS)
data1$AVERAGE.STORIES <- as.double(data1$AVERAGE.STORIES)
data1$TOTAL.POPULATION <- as.double(data1$TOTAL.POPULATION)
data1$AVERAGE.BUILDING.AGE <- as.double(data1$AVERAGE.BUILDING.AGE)
data1$BUILDING.TYPE <- as.factor(data1$BUILDING.TYPE)

data1$TOTAL.KWH[is.na(data1$TOTAL.KWH)] <- 0
data1$TOTAL.THERMS[is.na(data1$TOTAL.THERMS)] <- 0
data1$AVERAGE.STORIES[is.na(data1$AVERAGE.STORIES)] <- 0
data1$TOTAL.POPULATION[is.na(data1$TOTAL.POPULATION)] <- 0
data1$AVERAGE.BUILDING.AGE[is.na(data1$AVERAGE.BUILDING.AGE)] <- 0
#data1$BUILDING.TYPE[is.na(data1$BUILDING.TYPE)] <- 0

cook <- blocks(state = 'IL', county = 'Cook', year = 2010)
nwsBlocks <- subset(data1, COMMUNITY.AREA.NAME == 'Near West Side')
nwsBlocks <- unique(nwsBlocks[c('CENSUS.BLOCK')])
names(data1)[names(data1) == 'CENSUS.BLOCK'] <- 'GEOID10'

#nrow(y)
# drop nws' total row
nwsBlocks <- nwsBlocks %>% drop_na()

#print(head(cook, 1))

cookdata1 <- merge(cook, data1, by = 'GEOID10')

#print(head(combined, 1))
#x <- subset(cook, TRACTCE10 == '833000')
cooknws <- subset(cookdata1, cookdata1$GEOID10 %in% nwsBlocks$CENSUS.BLOCK)
#x <- head(x, 1)
#y <- subset(cookdata1, cookdata1$GEOID10 == '170312831002002')
#print(y)

totalThm <- aggregate(
  cookdata1$TOTAL.THERMS, by=list(GEOID10=cookdata1$GEOID10), FUN=sum)
totalKWH <- aggregate(
  cookdata1$TOTAL.KWH, by=list(GEOID10=cookdata1$GEOID10), FUN=sum)
avgStories <- aggregate(
  cookdata1$AVERAGE.STORIES, by=list(GEOID10=cookdata1$GEOID10), FUN=mean)
totalPop <- aggregate( # mean unnecessary but fits workflow
  cookdata1$TOTAL.POPULATION, by=list(GEOID10=cookdata1$GEOID10), FUN=mean)
avgAge <- aggregate(
  cookdata1$AVERAGE.BUILDING.AGE, by=list(GEOID10=cookdata1$GEOID10), FUN=mean)
# Causes a lot of warnings, suppress for now
# Warning in
# if (x == "Industrial") "Industrial"
# else if (x == "Commercial") "Commercial"
# else "test"
# : the condition has length > 1 and only the first element will be used
options(warn=-1)
buildingType <- aggregate(
  cookdata1$BUILDING.TYPE, by=list(GEOID10=cookdata1$GEOID10),
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
options(warn=-1)
print(head(buildingType, 10))

# if still zero, then change to NA
totalThm[totalThm == 0] <- NA
totalKWH[totalKWH == 0] <- NA
avgStories[avgStories == 0] <- NA
totalPop[totalPop == 0] <- NA
avgAge[avgAge == 0] <- NA
#buildingType[buildingType == 0] <- NA

nwsTotalThm <- merge(cooknws, totalThm, by = 'GEOID10')
nwsTotalKWH <- merge(cooknws, totalKWH, by = 'GEOID10')
nwsAvgStories <- merge(cooknws, avgStories, by = 'GEOID10')
nwsTotalPop <- merge(cooknws, totalPop, by = 'GEOID10')
nwsAvgAge <- merge(cooknws, avgAge, by = 'GEOID10')
nwsBuildingType <- merge(cooknws, buildingType, by = 'GEOID10')
#x <- subset(combined, combined$GEOID10 %in% nws$CENSUS.BLOCK)

#~ print(head(nwsBuildingType, 10))

# commercial

shinyServer(function(input, output, session) {
  
  # about
  output$textAbout <- renderText({
    '--'
  })
  
  m <- mapview(nwsBuildingType, zcol = 'x')
#~   m <- mapview(cooknws, zcol = 'TOTAL.POPULATION')
#~   m <- mapview(x, zcol = 'NAME10')
  #m <- mapview(ycol=sub_data$INTPTLAT10, xcol = sub_data$INTPTLON10, zcol = sub_data$KWH.JANUARY.2010)

  output$mapplot <- renderLeaflet({
    m@map
  })
  
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
