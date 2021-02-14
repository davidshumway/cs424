## server.R
## Author: David Shumway

library(shiny)
library(DT)
#~ library(scales)
#~ library(tidyverse)
#library(rsconnect)


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
data$ENERGY.SOURCE <- as.factor(data$ENERGY.SOURCE)
data <- data[!(data$STATE == '  '),]
data <- data[!(data$GENERATION..Megawatthours. < 0),]
names(data)[names(data) == 'GENERATION..Megawatthours.'] <- 'GEN'

# stacked bar total
# stacked bar percent
# line chart
# table 1
b1 <- subset(data, STATE == 'US-TOTAL')
b1 <- subset(b1, ENERGY.SOURCE != 'Total') # remove 'Total'
b1 <- subset(b1, TYPE.OF.PRODUCER == 'Total Electric Power Industry')
# line chart percent
# table 2 pct
library(data.table)
dt <- data.table(b1)
setkey(dt, "YEAR") 
X <- dt[, list(SUM=sum(GEN)), by=key(dt)] 
b2 <- dt[X,
  list(ENERGY.SOURCE, GEN, YEAR, PCT=round(100*GEN/SUM, digits = 1))
]

# filters b1 and b2 data based on checkboxes
doFilters <- function(input, data) {
  c = data
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
  c
}

shinyServer(function(input, output) {
#~   print(input)
#~          checkboxInput('filter1', 'All', TRUE),
#~          checkboxInput('filter2', 'Coal', FALSE),
#~          checkboxInput('filter3', 'Geothermal', FALSE),
#~          checkboxInput('filter4', 'Hyrdo', FALSE),
#~          checkboxInput('filter5', 'Nat. Gas', FALSE),
#~          checkboxInput('filter6', 'Nuclear', FALSE),
#~          checkboxInput('filter7', 'Petroleum', FALSE),
#~          checkboxInput('filter8', 'Solar', FALSE),
#~          checkboxInput('filter9', 'Wind', FALSE),
#~          checkboxInput('filter10','Wood', FALSE)
#~   b1 <- reactive({
#~     if (!input$filter1) {
#~       b1
#~     } else {
#~       b1
#~     }
#~     subset(allData, year(allData$newDate) == input$Year)
#~   })
#~   a <- reactive(input$filter1)
#~   print(a)
#~   b1 <- reactive({
#~     subset(b1, year(allData$newDate) == input$Year)
#~   })
  
#~   
#~     newNoons <- newNoonsReactive()
#~     temperatures <- as.data.frame(table(newNoons[,input$Room]))
#~     temperatures$Var1 <- as.numeric(as.character(temperatures$Var1))

  # stacked bar chart showing amount of each energy source per year
  # from 1990 - 2019 (assumption: for entire US?)
  output$bar1 <- renderPlot({
    a = doFilters(input, b1)
    ggplot(a, aes(x = YEAR, y = GEN)) + 
      geom_col(aes(fill = ENERGY.SOURCE), width = 0.7) +
      labs(x = 'Year', y = 'Count')
  })
  
  # stacked bar chart showing percent of the total production for each
  # energy source per year from 1990 - 2019
#~   n1 <- as.numeric(as.character(b1$GEN))
#~   n2 <- as.numeric(as.character(b2$GEN))
  output$bar2 <- renderPlot({
    a = doFilters(input, b1)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE)) + 
      geom_bar(position = 'fill', stat = 'identity') +
      labs(x = 'Year', y = 'Count')
  })
  
  # line chart showing the amount of each energy source per year from
  # 1990 - 2019
  output$line1 <- renderPlot({
    a = doFilters(input, b1)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Energy produced')
  })
  
  # line chart showing the percent of the total production for each
  # energy source per year from 1990 - 2019
  output$line2 <- renderPlot({
    a = doFilters(input, b2)
    ggplot(b2, aes(x = YEAR, y = PCT, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Percentage of total energy produced')
  })
  
  # table of raw numbers for the amount of each energy source per year from 1990 - 2019
  # use DT to help out with the tables - https://datatables.net/reference/option/
  #newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
  output$tab1 <- DT::renderDataTable(
    DT::datatable(
      doFilters(input, b1)[, c('YEAR','GEN','ENERGY.SOURCE')],
      # similar option is arrange(.., ..)
#~       b1[, c('YEAR','GEN','ENERGY.SOURCE')], 
#~       {
#~         newOne <- reactive({b1})
#~           b1
#~         xx <- as.data.frame(table(newOne[,input$GEN], dnn = list("YEAR")), responseName = "YEAR")
#~           reactive(b1)
          #subset(b2, year(allData$newDate) == input$Year & Hour == 12)
#~         newNoons <- newNoonsReactive()
#~         temperatures <- as.data.frame(table(newNoons[,input$Room], dnn = list("Year")), responseName = "Year")
#~       },
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
      doFilters(input, b2)[, c('YEAR','GEN','ENERGY.SOURCE', 'PCT')],
#~       {
#~         newOne <- reactive({b1})
#~           b1
#~         xx <- as.data.frame(table(newOne[,input$GEN], dnn = list("YEAR")), responseName = "YEAR")
#~           reactive(b1)
          #subset(b2, year(allData$newDate) == input$Year & Hour == 12)
#~         newNoons <- newNoonsReactive()
#~         temperatures <- as.data.frame(table(newNoons[,input$Room], dnn = list("Year")), responseName = "Year")
#~       },
      options = list(searching = FALSE, pageLength = 9,
        lengthChange = FALSE, order = list(list(0, 'asc'))
      ),
      rownames = FALSE 
    )
  )
  
  # using normalize from
  # https://stackoverflow.com/questions/27240655/transform-data-by-group
#~   normalize = function (x) (x - mean(x))/sd(x)
#~   normalize = function (x) x / sum(x)
#~   output$line2 <- renderPlot({
#~     ggplot(b1, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
#~       geom_line(aes(y = 100*normalize(GEN))) +
#~       labs(x = 'Year', y = 'Percentage of total energy produced (annual)')
#~   })
#~   output$line2 <- renderPlot({
#~     ggplot(b1, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
#~       geom_line(stat = 'identity', aes(y = (GEN)/sum(GEN))) +
#~       labs(x = 'Year', y = 'c')
#~ #       scale_y_continuous(labels = scales::percent)
#~   })
#~   irisNew <- b1 %>% group_by(YEAR, ENERGY.SOURCE) %>% 
#~   summarize(count = n()) %>%  # count records by species
#~   mutate(pct = count/sum(count))  # find percent of total
#~   output$line2 <- renderPlot({
#~     ggplot(irisNew, aes(YEAR, pct, fill = ENERGY.SOURCE)) +  #, fill = ENERGY.SOURCE
#~       geom_line(stat = 'identity') + 
#~       geom_text(aes(label = scales::percent(pct)), position = position_stack(vjust = .5)) +
#~       scale_y_continuous(labels = scales::percent)
#~   })
  
#~   output$distPlot <- renderPlot({

#~     # generate bins based on input$bins from ui.R
#~     x    <- faithful[, 2]
#~     bins <- seq(min(x), max(x), length.out = input$bins + 1)

#~     # draw the histogram with the specified number of bins
#~     hist(x, breaks = bins, col = 'darkgray', border = 'white')

#~   })

})
