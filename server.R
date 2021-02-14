## server.R
## Author: David Shumway

library(shiny)
library(DT)
library(data.table) # for calculating percentages

data <- read.csv('annual_generation_state.csv')#, sep = ',', header = TRUE)
data$GENERATION..Megawatthours. <- as.numeric(gsub(',', '', data$GENERATION..Megawatthours.))
data$STATE <- toupper(data$STATE)
data <- data[!(data$ENERGY.SOURCE == 'Other'),]
data <- data[!(data$ENERGY.SOURCE == 'Other Gases'),]
data <- data[!(data$ENERGY.SOURCE == 'Other Biomass'),]
data <- data[!(data$ENERGY.SOURCE == 'Pumped Storage'),]
data$ENERGY.SOURCE[data$ENERGY.SOURCE == 'Hydroelectric Conventional'] <- 'Hydro'
data$ENERGY.SOURCE[data$ENERGY.SOURCE == 'Wood and Wood Derived Fuels'] <- 'Wood'
data$ENERGY.SOURCE[data$ENERGY.SOURCE == 'Solar Thermal and Photovoltaic'] <- 'Solar'
data$STATE <- as.factor(data$STATE)
data$TYPE.OF.PRODUCER <- as.factor(data$TYPE.OF.PRODUCER)

data <- data[!(data$STATE == '  '),]
data <- data[!(data$GENERATION..Megawatthours. < 0),]
names(data)[names(data) == 'GENERATION..Megawatthours.'] <- 'GEN'

data$ENERGY.SOURCE <- as.factor(data$ENERGY.SOURCE)

data <- subset(data, TYPE.OF.PRODUCER == 'Total Electric Power Industry')

library(tidyr)
#~ data <- complete(data, expand(data, YEAR, ENERGY.SOURCE), fill = list(ENERGY.SOURCE = 0))
#~ data <- complete(data, expand(data,YEAR,ENERGY.SOURCE,STATE,TYPE.OF.PRODUCER,GEN), fill = list(GEN = 0))
#~ data <- complete(data, YEAR, ENERGY.SOURCE,STATE,TYPE.OF.PRODUCER,GEN, fill = list(GEN = 0))
#~ data <- complete(data, YEAR, ENERGY.SOURCE,STATE,GEN, fill = list(GEN = 0))
data <- complete(data, YEAR, STATE, ENERGY.SOURCE, fill = list(GEN = 0, TYPE.OF.PRODUCER = 'Total Electric Power Industry'))
print(head(subset(data, STATE == 'IL'), 20))
#~ print(head(data, 20))
#~ library(tidyr)
#~ data <- complete(data, YEAR, ENERGY.SOURCE, fill = list(ENERGY.SOURCE = 0.0))
#~ complete(data, YEAR, ENERGY.SOURCE, fill = list(ENERGY.SOURCE = 0.0))

#~ library(dplyr, warn.conflicts = FALSE)
#~ df %>% 
#~ data <- complete(data, nesting(YEAR, ENERGY.SOURCE), fill = list(ENERGY.SOURCE = 0))
#~ print(head(df))

# filters b1 and b2 data based on inputs: checkboxes, states, and years
doFilters <- function(input, percentize, cstate, cyear) {
  # stacked bar total
  # stacked bar percent
  # line chart
  # table 1
  # previously, only us: b1 <- subset(data, STATE == 'US-TOTAL')
  c <- data
  c <- subset(c, ENERGY.SOURCE != 'Total') # remove 'Total'
  c <- subset(c, TYPE.OF.PRODUCER == 'Total Electric Power Industry')
  
#~   c = b1
  if (!input$filter1) {
    if (!input$filter2) c <- subset(c, ENERGY.SOURCE != 'Coal')
    if (!input$filter3) c <- subset(c, ENERGY.SOURCE != 'Geothermal')
    if (!input$filter4) c <- subset(c, ENERGY.SOURCE != 'Hydro')
    if (!input$filter5) c <- subset(c, ENERGY.SOURCE != 'Natural Gas')
    if (!input$filter6) c <- subset(c, ENERGY.SOURCE != 'Nuclear')
    if (!input$filter7) c <- subset(c, ENERGY.SOURCE != 'Petroleum')
    if (!input$filter8) c <- subset(c, ENERGY.SOURCE != 'Solar')
    if (!input$filter9) c <- subset(c, ENERGY.SOURCE != 'Wind')
    if (!input$filter10)c <- subset(c, ENERGY.SOURCE != 'Wood')
  }
  c <- subset(c, STATE == cstate)
  if (cyear != 'ALL')
    c <- subset(c, YEAR == cyear)
  
  if (percentize == TRUE) {
    # line chart percent
    # table 2 pct
    dt <- data.table(c)
    setkey(dt, 'YEAR') 
    X <- dt[, list(SUM=sum(GEN)), by=key(dt)] 
    c <- dt[X,
      list(ENERGY.SOURCE, GEN, STATE, YEAR, PCT=round(100*GEN/SUM, digits = 1))
    ]
  }
  
#~   library(tidyr)
#~   print(c)
#~   c <- complete(c, expand(c, YEAR, STATE, ENERGY.SOURCE), fill = list(ENERGY.SOURCE = 0))
#~   c <- complete(c, YEAR, STATE, c('Solar', 'Wood'), fill = list(ENERGY.SOURCE = 0))
#~   complete(data, YEAR, ENERGY.SOURCE, fill = list(ENERGY.SOURCE = 0.0))

#~   print(c)
#~   print(head(c, 10))
#~   print(head(c, 10))
  c
}

shinyServer(function(input, output) {

  

#~     newNoons <- newNoonsReactive()
#~     temperatures <- as.data.frame(table(newNoons[,input$Room]))
#~     temperatures$Var1 <- as.numeric(as.character(temperatures$Var1))

  # stacked bar chart showing amount of each energy source per year
  # from 1990 - 2019 (assumption: for entire US?)
  output$bar1 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = GEN)) + 
      geom_col(aes(fill = ENERGY.SOURCE), width = 0.7) +
      labs(x = 'Year', y = 'Count')
  })
  output$bar3 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE2, input$YEAR2)
    ggplot(a, aes(x = YEAR, y = GEN)) + 
      geom_col(aes(fill = ENERGY.SOURCE), width = 0.7) +
      labs(x = 'Year', y = 'Count')
  })
  
  # stacked bar chart showing percent of the total production for each
  # energy source per year from 1990 - 2019
  output$bar2 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE)) + 
      geom_bar(position = 'fill', stat = 'identity') +
      labs(x = 'Year', y = 'Count')
  })
  output$bar4 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE2, input$YEAR2)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE)) + 
      geom_bar(position = 'fill', stat = 'identity') +
      labs(x = 'Year', y = 'Count')
  })
  
  # line chart showing the amount of each energy source per year from
  # 1990 - 2019
  output$line1 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Energy produced')
  })
  output$line3 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE2, input$YEAR2)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Energy produced')
  })
  
  # line chart showing the percent of the total production for each
  # energy source per year from 1990 - 2019
  output$line2 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = PCT, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Percentage of total energy produced')
  })
  output$line4 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE2, input$YEAR2)
    ggplot(a, aes(x = YEAR, y = PCT, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Percentage of total energy produced')
  })
  
  # table of raw numbers for the amount of each energy source per year
  # from 1990 - 2019
  # similar sorting as options.order is arrange(.., ..)
  output$tab1 <- DT::renderDataTable(
    DT::datatable(
      doFilters(input, FALSE, input$STATE1, input$YEAR1)[, c('YEAR','GEN','ENERGY.SOURCE')],
      options = list(searching = FALSE, pageLength = 9,
        lengthChange = FALSE, order = list(list(0, 'asc'))
      ),
      rownames = FALSE 
    )
  )
  output$tab3 <- DT::renderDataTable(
    DT::datatable(
      doFilters(input, FALSE, input$STATE2, input$YEAR2)[, c('YEAR','GEN','ENERGY.SOURCE')],
      options = list(searching = FALSE, pageLength = 9,
        lengthChange = FALSE, order = list(list(0, 'asc'))
      ),
      rownames = FALSE 
    )
  )
  
  # table of raw numbers for the percent of the total production for
  # each energy source per year from 1990 - 2019
  output$tab2 <- DT::renderDataTable(
    DT::datatable(
      doFilters(input, TRUE, input$STATE1, input$YEAR1)[, c('YEAR','PCT','ENERGY.SOURCE')],
      options = list(searching = FALSE, pageLength = 9,
        lengthChange = FALSE, order = list(list(0, 'asc'))
      ),
      rownames = FALSE 
    )
  )
  output$tab4 <- DT::renderDataTable(
    DT::datatable(
      doFilters(input, TRUE, input$STATE2, input$YEAR2)[, c('YEAR','PCT','ENERGY.SOURCE')],
      options = list(searching = FALSE, pageLength = 9,
        lengthChange = FALSE, order = list(list(0, 'asc'))
      ),
      rownames = FALSE 
    )
  )
  
})
