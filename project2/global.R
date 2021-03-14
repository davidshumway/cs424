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

names(data1) = sub('[^\n]+\n', '', names(data1))
names(data1) = sub('2000 ', '', names(data1))
names(data1) = sub('biomass\\/ wood', 'biomass', names(data1)) 
names(data1)[names(data1) == 'State abbreviation'] <- 'Plant state abbreviation'
#mwh
x <- names(data1) == 'Plant annual other fossil (tires, batteries, chemicals, etc.) net generation (MWh)'
names(data1)[x] <- 'Plant annual other fossil net generation (MWh)'
x <- names(data1) == 'Plant annual solid waste net generation (MWh)'
names(data1)[x] <- 'Plant annual other unknown/ purchased fuel net generation (MWh)'
#pct
x <- names(data1) == 'Plant other fossil (tires, batteries, chemicals, etc.) generation percent (resource mix)'
names(data1)[x] <- 'Plant other fossil generation percent (resource mix)'
x <- names(data1) == 'Plant solid waste generation percent (resource mix)'
names(data1)[x] <- 'Plant other unknown / purchased fuel generation percent (resource mix)'


#head(data1)

f <- function(x) {
  # Select a portion of columns and additionally rename them.
  x %>%
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
}
data1 <- f(data1)
data2 <- f(data2)
data3 <- f(data3)

# As numeric and replace NA with 0
nonnumCols <- as.vector(
  c('Name', 'State', 'Lat', 'Lng')
)
for (i in names(data3)) {
  if (i %in% nonnumCols) next
  
  data1[[i]] <- as.numeric(gsub(',', '', data1[[i]]))
  data1[[i]] <- ifelse(is.na(data1[[i]]), 0, data1[[i]])
  data2[[i]] <- as.numeric(gsub(',', '', data2[[i]]))
  data2[[i]] <- ifelse(is.na(data2[[i]]), 0, data2[[i]])
  data3[[i]] <- as.numeric(gsub(',', '', data3[[i]]))
  data3[[i]] <- ifelse(is.na(data3[[i]]), 0, data3[[i]])
}

# Some plants have zero or less output.
# Let's remove those.
#AnnualGen
data1 <- data1[!(data1$AnnualGen <= 0),]
data2 <- data2[!(data2$AnnualGen <= 0),]
data3 <- data3[!(data3$AnnualGen <= 0),]

# Calculate other, pctOther max, and subsequently type
library(matrixStats)
data1$Other = rowSums(as.matrix(data1[,c('Other1','Other2')]))
data1$PctOther = rowSums(as.matrix(data1[,c('PctOther1','PctOther2')]))
data1$Max = rowMaxs(as.matrix(data1[,energyTypes]))
data2$Other = rowSums(as.matrix(data2[,c('Other1','Other2')]))
data2$PctOther = rowSums(as.matrix(data2[,c('PctOther1','PctOther2')]))
data2$Max = rowMaxs(as.matrix(data2[,energyTypes]))
data3$Other = rowSums(as.matrix(data3[,c('Other1','Other2')]))
data3$PctOther = rowSums(as.matrix(data3[,c('PctOther1','PctOther2')]))
data3$Max = rowMaxs(as.matrix(data3[,energyTypes]))

# Determine main/assumed type
# https://statisticsglobe.com/return-column-name-of-largest-value
# -for-each-row-in-r
data1$Type = colnames(data1[,energyTypes])[
  max.col(data1[,energyTypes], ties.method = "first")
]
data2$Type = colnames(data2[,energyTypes])[
  max.col(data2[,energyTypes], ties.method = "first")
]
data3$Type = colnames(data3[,energyTypes])[
  max.col(data3[,energyTypes], ties.method = "first")
]

# Set color col
data1$Color = colors[data1$Type]
data1$Color = unlist(data1$Color[data1$Type])
data2$Color = colors[data2$Type]
data2$Color = unlist(data2$Color[data2$Type])
data3$Color = colors[data3$Type]
data3$Color = unlist(data3$Color[data3$Type])

# >>>
# clicking on a marker shows the plant name, and its generation capacity
# (for the different types it has), the percent of the total capacity
# that is renewable, and the percent of the total capacity that is
# non-renewable
# Assumption: Capacity here refers to generation----
#data3$Popup = paste(data3$Name, '<br>', 'Assumed type: ', data3$Type, sep = '')
#~ for (i in 1:nrow(data3)) {
#~     row <- data3[i,]
#~     for (j in 1:length(data3)) {
#~         type <- energyList[[j]][1]
#~         if (row[[type]] > 0) {
#~             data3[i,][['Popup']] <- paste(row[['Popup']], '<br>', type, ': ', row[[type]], sep = '')
#~         }
#~     }
#~ }
data1$Popup <- paste(data1$Name, '<br>', apply(data1[energyTypes], 1, function(x) {
  inds <- x > 0
  paste(energyTypes[inds], x[inds], sep = ': ', collapse = '<br>')
}))
data2$Popup <- paste(data2$Name, '<br>', apply(data2[energyTypes], 1, function(x) {
  inds <- x > 0
  paste(energyTypes[inds], x[inds], sep = ': ', collapse = '<br>')
}))
data3$Popup <- paste(data3$Name, '<br>', apply(data3[energyTypes], 1, function(x) {
  inds <- x > 0
  paste(energyTypes[inds], x[inds], sep = ': ', collapse = '<br>')
}))

#head(data3$Popup)

# Illinois 2018
data3Illinois <- subset(data3, State == 'IL')
#data3Illinois <- subset(data3, State == 'IL')
#data3Illinois <- subset(data3, State == 'IL')
#head(data3Illinois)


