library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)

library(mapview)
library(leaflet)
library(tidyr)

library(dplyr)

options(shiny.fullstacktrace = TRUE)

data <- read.csv('data/global_power_plant_database.csv')
#~ data <- read.csv('data/test.csv')

library(countrycode)
data$country_long = countrycode(data$country_long, origin = 'country.name', 'continent')

# Russia in Europe ala cc package? Hmmmmmm.
data$country_long[data$country_long == 'Russia'] = 'Asia'

# drop some unused coluns
data = subset(data, select = -c(generation_gwh_2013,generation_gwh_2014,generation_gwh_2015,generation_gwh_2016,generation_gwh_2017, wepp_id, owner, source, url, geolocation_source, other_fuel1, other_fuel2, other_fuel3, gppd_idnr, country))

uniqueCountries <- unique(data[c('country_long')])

print(head(data, 1))
print(head(uniqueCountries, 2))

# Petcoke to coal
data$primary_fuel[data$primary_fuel == 'Petcoke'] = 'Coal'

#capacity vs. estimated?

energyListRen <- list(
  'Hydro' = 'Hydro',
  'Wave and Tidal' = 'Wave and Tidal',
  'Biomass' = 'Biomass',
  'Geothermal' = 'Geothermal',
  'Wind' = 'Wind',
  'Solar' = 'Solar',
  'Geothermal' = 'Geothermal',
  'Cogeneration' = 'Cogeneration',
  'Storage' = 'Storage'  
)
energyListNonRen <- list(
  'Coal' = 'Coal',
  'Oil' = 'Oil',
  'Gas' = 'Gas',
  'Nuclear' = 'Nuclear',
  'Waste' = 'Waste'
)

energyTypes = c(
  'Coal',
  'Oil',
  'Gas',
  'Nuclear',
  'Waste',
  'Hydro',
  'Wave and Tidal',
  'Biomass',
  'Wind',
  'Solar',
  'Geothermal',
  'Cogeneration',
  'Storage')
energyList <- list(
  'Coal' = 'Coal',
  'Oil' = 'Oil',
  'Gas' = 'Gas',
  'Nuclear' = 'Nuclear',
  'Waste' = 'Waste',
  'Hydro' = 'Hydro',
  'Wave and Tidal' = 'Wave and Tidal',
  'Biomass' = 'Biomass',
  'Wind' = 'Wind',
  'Solar' = 'Solar',
  'Geothermal' = 'Geothermal',
  'Cogeneration' = 'Cogeneration',
  'Storage' = 'Storage'
)
# awesome markers source:
#' @param markerColor Possible values are \code{"red"}, \code{"darkred"}, \code{"lightred"}, \code{"orange"},
#' \code{"beige"}, \code{"green"}, \code{"darkgreen"}, \code{"lightgreen"}, \code{"blue"},
#' \code{"darkblue"}, \code{"lightblue"}, \code{"purple"}, \code{"darkpurple"}, \code{"pink"},
#' \code{"cadetblue"}, \code{"white"}, \code{"gray"}, \code{"lightgray"}, \code{"black"}
colors = list(
  'Coal' = 'red',
  'Oil' = 'lightred',
  'Gas' = 'orange',
  'Nuclear' = 'darkred',
  'Waste' = 'purple',
  'Hydro' = 'blue',
  'Wave and Tidal' = 'blue', # sameish
  'Biomass' = 'lightblue',
  'Wind' = 'cadetblue',
  'Solar' = 'green',
  'Geothermal' = 'lightgreen',
  'Cogeneration' = 'gray',
  'Storage' = 'black'
)

xc <- c('#FF0000', '#ffcccb', '#FFA500', '#8B0000', '#800080', '#0000FF',
  '#0000FF', '#ADD8E6', '#5F9EA0', '#008000','#90EE90','#808080','#000000')
xl <- c('Coal', 'Oil', 'Gas', 'Nuclear', 'Waste', 'Hydro', 'Wave and Tidal',
  'Biomass', 'Wind', 'Solar', 'Geothermal', 'Cogeneration', 'Storage')
    
# fill color for addCircleMarkers
fcol <- function(t) {
  # t: Type
  ifelse(
    t == 'Coal', '#FF0000', ifelse(
    t == 'Oil', '#ffcccb', ifelse(
    t == 'Gas', '#FFA500', ifelse(
    t == 'Nuclear', '#8B0000', ifelse(
    t == 'Waste', '#800080', ifelse(
    t == 'Hydro', '#0000FF', ifelse(
    t == 'Wave and Tidal', '#0000FF', ifelse(
    t == 'Biomass', '#ADD8E6', ifelse(
    t == 'Wind', '#5F9EA0', ifelse(
    t == 'Solar', '#008000', ifelse(
    t == 'Geothermal', '#90EE90', ifelse(
    t == 'Cogeneration', '#808080', '#000000' # last is Storage
  ))))))))))))
}

#~ maplbl <- list(
#~   'labels' = c('<500 mWh', '<6000 mWh', '<12000 mWh', '<18000 mWh', '>22000 mil mWh'),# "0.5 mil, 6 mil, 12 mil"
#~   'sizes' = c(4, 6, 12, 18, 22)
#~ )
  
# Calculate other, pctOther max, and subsequently type
library(matrixStats)
data$Max = max(data$capacity_mw)

# Set color col
data$Color = colors[data$primary_fuel]

# >>>
# clicking on a marker shows the plant name, and its generation capacity
# (for the different types it has), the percent of the total capacity
# that is renewable, and the percent of the total capacity that is
# non-renewable
# Assumption: Capacity here refers to generation----
# https://stackoverflow.com/questions/66620441/conditional-concatenate-columns/66621957#66621957
fmt <- function(x) {
  format(round(x, 1), nsmall = 1, big.mark = ',')
}
f <- function(df) {
  paste0(
    '<b>Name: </b>', df$name, '<br>',
    '<b>Country: </b>', df$country_long, '<br>',
    '<b>Capacity (MW): </b>', fmt(df$capacity_mw), '<br>',
    '<b>Primary fuel: </b>', df$primary_fuel, '<br>',
    '<b>Estimated generation 2017 (gWh): </b>', fmt(df$estimated_generation_gwh)
  )
}
data$Popup <- f(data)

# Jitter LL
library(mapview)
library(sp)
data$latitude <- jitter(data$latitude, factor = 0.04)
data$longitude <- jitter(data$longitude, factor = 0.01)

# leaflet proxy
flp <- function(mapId, data, tiles) {
  leafletProxy(mapId, data = data) %>%
    clearShapes() %>%
    clearMarkerClusters() %>%
    clearMarkers() %>%
    clearTiles() %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(
      radius = ~frad(capacity_mw),
      fillColor = ~fcol(primary_fuel),
      color = 'black',
      stroke = TRUE, fillOpacity = 0.5, weight = 1,
      popup = ~as.character(Popup),
    )
}

# zoom level
# IconZoomClass
# 22k mw largest
#~ izc <- function(x) {
#~   m <- 100
#~   ifelse(
#~     x$capacity_mw < 6*m, 'izoom25', ifelse(
#~       x$capacity_mw < 12*m, 'izoom50', ifelse(
#~         x$capacity_mw < 18*m, 'izoom75', ''
#~       )
#~     )
#~   )
#~ }
#~ data$IconZoomClass <- izc(data)
frad <- function(ag) {
  # primary_fuel
  m = 1000
  ifelse(
    ag < 0.5*m, '3', ifelse(
    ag < 2*m, '6', ifelse(
    ag < 4*m, '12', ifelse(
    ag < 6*m, '18', '24'
  ))))
}
firstLL <- function(country) {
  #print(country == 'Asia')
  if (country == 'Asia')
    c <- subset(data, country_long == 'Asia')
  else if (country == 'Americas')
    c <- subset(data, country_long == 'Americas')
  else if (country == 'Europe')
    c <- subset(data, country_long == 'Europe')
  else if (country == 'NA')
    c <- subset(data, country_long == 'NA')
  else if (country == 'Oceana')
    c <- subset(data, country_long == 'Oceana')
  else #if (country == 'Africa')
    c <- subset(data, country_long == 'Africa')

  ### ??? not working ???
  ### c <- subset(data, country_long == country)
  
  x <- list(c[0:1,]$latitude, c[0:1,]$longitude)
}
`%notin%` <- Negate(`%in%`)

rv <- reactiveValues(linked = FALSE,
  d1 = data, #subset(data, country_long == 'Americas'),
  d2 = data, #subset(data, country_long == 'Asia'),
  data = data)

# 1 inputs
filter <- function(session, input, side) {
  s <- side
  if (rv$linked) {
    if (s == 1) {
      updateCheckboxGroupInput(session, 'm2Source', 'Source:',
        choices = energyList, selected = unlist(input$m1Source)
      )
    } else {
      updateCheckboxGroupInput(session, 'm1Source', 'Source:',
        choices = energyList, selected = unlist(input$m2Source)
      )
    }
  }
  c <- data
  if (input[[paste0('m', s, 'Country')]] != 'ALL')
    c <- subset(c, country_long == input[[paste0('m', s, 'Country')]])
  c <- subset(c, # size
    capacity_mw > as.numeric(input[[paste0('m', s, 'Range')]][[1]]*1000))
  c <- subset(c, # size
    capacity_mw < as.numeric(input[[paste0('m', s, 'Range')]][[2]]*1000))
  c <- subset(c, primary_fuel %in% input[[paste0('m', s, 'Source')]])
#~   src <- input[[paste0('m', s, 'Source')]]
#~   print(src)
#~   for (i in length(energyList)) {
#~     if (!energyList[i] %in% src) {
#~       print(energyList[i])
#~       c <- subset(c, primary_fuel != energyList[i])
#~     }
#~   }


#~   if (!'Coal' %in% src) c <- subset(c, Type != 'Coal')
#~   if (!'Geothermal' %in% src) c <- subset(c, Type != 'Geothermal')
#~   if (!'Gas' %in% src) c <- subset(c, Type != 'Gas')
#~   if (!'Nuclear' %in% src) c <- subset(c, Type != 'Nuclear')
#~   if (!'Oil' %in% src) c <- subset(c, Type != 'Oil')
#~   if (!'Solar' %in% src) c <- subset(c, Type != 'Solar')
#~   if (!'Wind' %in% src) c <- subset(c, Type != 'Wind')
#~   if (!'Biomass' %in% src) c <- subset(c, Type != 'Biomass')
#~   if (!'Other' %in% src) c <- subset(c, Type != 'Other')
#~   if (!'Hydro' %in% src) c <- subset(c, Type != 'Hydro')
  flp(paste0('map', s), c, input[[paste0('m', s, 'BaseMap')]])
}





