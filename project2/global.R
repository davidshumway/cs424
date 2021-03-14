library(dplyr)

energyTypes = c('Coal', 'Oil', 'Gas', 'Nuclear', 'Hydro', 'Biomass',
  'Wind', 'Solar', 'Geothermal', 'Other')
energyList <- list(
  'Coal' = 'Coal',
  'Geothermal' = 'Geothermal',
  'Hydro' = 'Hydro',
  'Gas' = 'Gas',
  'Nuclear' = 'Nuclear',
  'Oil' = 'Oil',
  'Solar' = 'Solar',
  'Wind' = 'Wind',
  'Biomass' = 'Biomass',
  'Other' = 'Other'
)
# awesome markers source:
#' @param markerColor Possible values are \code{"red"}, \code{"darkred"}, \code{"lightred"}, \code{"orange"},
#' \code{"beige"}, \code{"green"}, \code{"darkgreen"}, \code{"lightgreen"}, \code{"blue"},
#' \code{"darkblue"}, \code{"lightblue"}, \code{"purple"}, \code{"darkpurple"}, \code{"pink"},
#' \code{"cadetblue"}, \code{"white"}, \code{"gray"}, \code{"lightgray"}, \code{"black"}
colors = list(
  'Coal' = 'red',
  'Oil' = 'lightred',
  'Gas' = 'beige',
  'Nuclear' = 'darkgreen',
  'Hydro' = 'blue',
  'Biomass' = 'lightblue',
  'Wind' = 'darkpurple',
  'Solar' = 'cadetblue',
  'Geothermal' = 'gray',
  'Other' = 'black'
)

data1 <- read.csv('data/plnt00.csv', check.names = FALSE)
data2 <- read.csv('data/plnt10.csv', check.names = FALSE)
data3 <- read.csv('data/plnt18.csv', check.names = FALSE)

data3 <- data3 %>%
  select(
    Name = 'Plant name',
    State = 'Plant state abbreviation',
    Lat = 'Plant latitude',
    Lng = 'Plant longitude',
    
    Coal = 'Plant annual coal net generation (MWh)',
    Oil = 'Plant annual oil net generation (MWh)',
    Gas = 'Plant annual gas net generation (MWh)',
    Nuclear = 'Plant annual nuclear net generation (MWh)',
    Hydro = 'Plant annual hydro net generation (MWh)',
    Biomass = 'Plant annual biomass net generation (MWh)',
    Wind = 'Plant annual wind net generation (MWh)',
    Solar = 'Plant annual solar net generation (MWh)',
    Geothermal = 'Plant annual geothermal net generation (MWh)',
    
    # combine
    Other1 = 'Plant annual other fossil net generation (MWh)',
    Other2 = 'Plant annual other unknown/ purchased fuel net generation (MWh)',
    # Other ...
    
    AnnualGen = 'Plant annual net generation (MWh)',

    PctCoal = 'Plant coal generation percent (resource mix)',
    PctOil = 'Plant oil generation percent (resource mix)',
    PctGas = 'Plant gas generation percent (resource mix)',
    PctNuclear = 'Plant nuclear generation percent (resource mix)',
    PctHydro = 'Plant hydro generation percent (resource mix)',
    PctBiomass = 'Plant biomass generation percent (resource mix)',
    PctWind = 'Plant wind generation percent (resource mix)',
    PctSolar = 'Plant solar generation percent (resource mix)',
    PctGeothermal = 'Plant geothermal generation percent (resource mix)',
    
    # combine
    PctOther1 = 'Plant other fossil generation percent (resource mix)',
    PctOther2 = 'Plant other unknown / purchased fuel generation percent (resource mix)',
    # PctOther ...
    
    PctRenewables = 'Plant total nonrenewables generation percent (resource mix)',
    PctNonRenewables = 'Plant total renewables generation percent (resource mix)'
  )

# As numeric and replace NA with 0
nonnumCols <- as.vector(
  c('Name', 'State', 'Lat', 'Lng')
)
for (i in names(data3)) {
  if (i %in% nonnumCols) next
  data3[[i]] <- as.numeric(gsub(',', '', data3[[i]]))
  data3[[i]] <- ifelse(is.na(data3[[i]]), 0, data3[[i]])
}

# Some plants have zero or less output.
# Let's remove those.
#AnnualGen
data3 <- data3[!(data3$AnnualGen <= 0),]

# Calculate other, pctOther max, and subsequently type
library(matrixStats)
data3$Other = rowSums(as.matrix(data3[,c('Other1','Other2')]))
data3$PctOther = rowSums(as.matrix(data3[,c('PctOther1','PctOther2')]))
data3$Max = rowMaxs(as.matrix(data3[,energyTypes]))

# https://statisticsglobe.com/return-column-name-of-largest-value
# -for-each-row-in-r
data3$Type = colnames(data3[,energyTypes])[
  max.col(data3[,energyTypes], ties.method = "first")
]

data3$Color = colors[data3$Type]
data3$Color = unlist(data3$Color[data3$Type])

# >>>
# clicking on a marker shows the plant name, and its generation capacity
# (for the different types it has), the percent of the total capacity
# that is renewable, and the percent of the total capacity that is
# non-renewable
# Assumption: Capacity here refers to generation----
#~ data3$Popup = paste(data3$Name, '<br>', 'Assumed type: ', data3$Type, sep = '')
data3$Popup <- paste(data3$Name, '<br>', apply(data3[energyTypes], 1, function(x) {
  inds <- x > 0
  paste(energyTypes[inds], x[inds], sep = ': ', collapse = '<br>')
}))

#~ head(data3$Popup)

# Illinois 2018
data3Illinois <- subset(data3, State == 'IL')
#head(data3Illinois)


