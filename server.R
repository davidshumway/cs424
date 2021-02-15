## server.R
## Author: David Shumway

library(shiny)
library(DT)
library(data.table) # for calculating percentages
library(ggplot2)
library(usmap)
library(scales)
library(stringr)

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
data$TYPE.OF.PRODUCER <- as.factor(data$TYPE.OF.PRODUCER)
data <- subset(data, STATE != '  ', STATE != ' ')
data$STATE <- as.factor(data$STATE)

#~ data <- data[!(data$STATE == ''),]
#~ data <- data[!(data$STATE == ' '),]
#~ data <- data[!(data$STATE == '  '),]
#~ data <- data[!(data$STATE == '   '),]
#~ data <- data[!(data$STATE == '    '),]
#~ data <- data[!(data$STATE == '     '),]
data <- data[!(data$GENERATION..Megawatthours. < 0),]
names(data)[names(data) == 'GENERATION..Megawatthours.'] <- 'GEN'

data$ENERGY.SOURCE <- as.factor(data$ENERGY.SOURCE)

data <- subset(data, TYPE.OF.PRODUCER == 'Total Electric Power Industry')

library(tidyr)
data <- complete(data, YEAR, STATE, ENERGY.SOURCE,
  fill = list(GEN = 0, TYPE.OF.PRODUCER = 'Total Electric Power Industry'))



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
  if (cstate != FALSE)
    c <- subset(c, STATE == cstate)
  if (cyear != 'ALL')
    c <- subset(c, YEAR == cyear)
  
  if (percentize == TRUE) {
    # line chart percent
    # table 2 pct
    dt <- data.table(c)
    setkey(dt, 'YEAR', 'STATE')  # add state ??
    X <- dt[, list(SUM=sum(GEN)), by=key(dt)] 
    c <- dt[X,
      list(ENERGY.SOURCE, GEN, STATE, YEAR, PCT=round(100*GEN/SUM, digits = 1))
    ]
  }
  c
}
# copy of filters function but does not filter on top part of form
doFilters2 <- function(input, percentize, cyear) {
  c <- data
  c <- subset(c, ENERGY.SOURCE != 'Total') # remove 'Total'
  c <- subset(c, TYPE.OF.PRODUCER == 'Total Electric Power Industry')
  c <- subset(c, YEAR == cyear)
  
  if (percentize == TRUE) {
    # line chart percent
    # table 2 pct
    dt <- data.table(c)
    setkey(dt, 'YEAR', 'STATE')  # add state ??
    X <- dt[, list(SUM=sum(GEN)), by=key(dt)] 
    c <- dt[X,
      list(ENERGY.SOURCE, GEN, STATE, YEAR, PCT=round(100*GEN/SUM, digits = 1))
    ]
  }
  c$state <- c$STATE
  c
}

compareState = ''

shinyServer(function(input, output) { #, session

  observe({
#~     print(input$compare)
#tail(x, 4)
    if (input$compare != compareState) {
      compareState = input$compare
      # wind vs solar, wind heatmap
      if (input$compare == 'wind/solar/hydro, wind heatmap, 90/19, TX') {
        updateTabItems(getDefaultReactiveDomain(), 'tab', selected = 'line')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource1", selected = 'Wind')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource2", selected = 'Wind')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR1", selected = '1990')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR2", selected = '2019')
        updateSelectInput(getDefaultReactiveDomain(), "STATE1", selected = 'US-TOTAL')
        updateSelectInput(getDefaultReactiveDomain(), "STATE2", selected = 'TX')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR1", selected = 'ALL')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR2", selected = 'ALL')
        for (i in 1:10)
          updateCheckboxInput(getDefaultReactiveDomain(), paste0("filter",i), value = FALSE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter4", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter8", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter9", value = TRUE)
      }
      else if (input$compare == 'wind/solar/hydro, solar heatmap, 90/19, CA') {
        updateTabItems(getDefaultReactiveDomain(), 'tab', selected = 'line')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource1", selected = 'Solar')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource2", selected = 'Solar')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR1", selected = '1990')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR2", selected = '2019')
        updateSelectInput(getDefaultReactiveDomain(), "STATE1", selected = 'US-TOTAL')
        updateSelectInput(getDefaultReactiveDomain(), "STATE2", selected = 'CA')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR1", selected = 'ALL')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR2", selected = 'ALL')
        for (i in 1:10)
          updateCheckboxInput(getDefaultReactiveDomain(), paste0("filter",i), value = FALSE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter4", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter8", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter9", value = TRUE)
      }
      else if (input$compare == 'wind/solar/hydro, hydro heatmap, 90/19, WA') {
        updateTabItems(getDefaultReactiveDomain(), 'tab', selected = 'line')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource1", selected = 'Hydro')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource2", selected = 'Hydro')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR1", selected = '1990')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR2", selected = '2019')
        updateSelectInput(getDefaultReactiveDomain(), "STATE1", selected = 'US-TOTAL')
        updateSelectInput(getDefaultReactiveDomain(), "STATE2", selected = 'WA')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR1", selected = 'ALL')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR2", selected = 'ALL')
        for (i in 1:10)
          updateCheckboxInput(getDefaultReactiveDomain(), paste0("filter",i), value = FALSE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter4", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter8", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter9", value = TRUE)
      }
      else if (input$compare == 'coal/ng/nuclear, coal heatmap, 90/19, WV') {
        updateTabItems(getDefaultReactiveDomain(), 'tab', selected = 'stacked')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource1", selected = 'Coal')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource2", selected = 'Coal')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR1", selected = '1990')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR2", selected = '2019')
        updateSelectInput(getDefaultReactiveDomain(), "STATE1", selected = 'US-TOTAL')
        updateSelectInput(getDefaultReactiveDomain(), "STATE2", selected = 'WV')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR1", selected = 'ALL')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR2", selected = 'ALL')
        for (i in 1:10)
          updateCheckboxInput(getDefaultReactiveDomain(), paste0("filter",i), value = FALSE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter2", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter5", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter6", value = TRUE)
      }
      else if (input$compare == 'coal/ng/nuclear, ng heatmap, 90/19, TX') {
        updateTabItems(getDefaultReactiveDomain(), 'tab', selected = 'stacked')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource1", selected = 'Natural Gas')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource2", selected = 'Natural Gas')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR1", selected = '1990')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR2", selected = '2019')
        updateSelectInput(getDefaultReactiveDomain(), "STATE1", selected = 'US-TOTAL')
        updateSelectInput(getDefaultReactiveDomain(), "STATE2", selected = 'TX')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR1", selected = 'ALL')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR2", selected = 'ALL')
        for (i in 1:10)
          updateCheckboxInput(getDefaultReactiveDomain(), paste0("filter",i), value = FALSE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter2", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter5", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter6", value = TRUE)
      }
      else if (input$compare == 'coal/ng/nuclear, nuclear heatmap, 90/19, IL') {
        updateTabItems(getDefaultReactiveDomain(), 'tab', selected = 'stacked')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource1", selected = 'Nuclear')
        updateSelectInput(getDefaultReactiveDomain(), "heatmapSource2", selected = 'Nuclear')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR1", selected = '1990')
        updateSelectInput(getDefaultReactiveDomain(), "HMYEAR2", selected = '2019')
        updateSelectInput(getDefaultReactiveDomain(), "STATE1", selected = 'US-TOTAL')
        updateSelectInput(getDefaultReactiveDomain(), "STATE2", selected = 'IL')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR1", selected = 'ALL')
        updateSelectInput(getDefaultReactiveDomain(), "YEAR2", selected = 'ALL')
        for (i in 1:10)
          updateCheckboxInput(getDefaultReactiveDomain(), paste0("filter",i), value = FALSE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter2", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter5", value = TRUE)
        updateCheckboxInput(getDefaultReactiveDomain(), "filter6", value = TRUE)
      }
    }
  })
  
  # You need to add how geography affects this mix
  # geo,solar,wind,wood empty legend?
  # solved by adding theme() and/or breaks()? 
  output$maptt1 <- renderText({ 
    paste0(
      'Percent ',
      tolower(input$heatmapSource1),' use by state (',input$HMYEAR1,')')
  })
  output$maptt2 <- renderText({ 
    paste0(
      'Amount ',
      tolower(input$heatmapSource1),' use by state (',input$HMYEAR1,')')
  })
  output$maptt3 <- renderText({ 
    paste0(
      'Percent ',
      tolower(input$heatmapSource2),' use by state (',input$HMYEAR2,')')
  })
  output$maptt4 <- renderText({ 
    paste0(
      'Amount ',
      tolower(input$heatmapSource2),' use by state (',input$HMYEAR2,')')
  })
  output$stackedtt1 <- renderText({ 
    y <- ifelse(input$YEAR1 == 'ALL', 'All years', input$YEAR1)
    paste0('Energy by source ',
      '(', input$STATE1, ', ', y, ')')
  })
  output$stackedtt2 <- renderText({ 
    y <- ifelse(input$YEAR1 == 'ALL', 'All years', input$YEAR1)
    paste0('Percent energy by source ',
      '(', input$STATE1, ', ', y, ')')
  })
  output$stackedtt3 <- renderText({ 
    y <- ifelse(input$YEAR2 == 'ALL', 'All years', input$YEAR2)
    paste0('Energy by source ',
      '(', input$STATE2, ', ', y, ')')
  })
  output$stackedtt4 <- renderText({ 
    y <- ifelse(input$YEAR2 == 'ALL', 'All years', input$YEAR2)
    paste0('Percent energy by source ',
      '(', input$STATE2, ', ', y, ')')
  })
  output$linett1 <- renderText({ 
    y <- ifelse(input$YEAR1 == 'ALL', 'All years', input$YEAR1)
    paste0('Energy by source ',
      '(', input$STATE1, ', ', y, ')')
  })
  output$linett2 <- renderText({ 
    y <- ifelse(input$YEAR1 == 'ALL', 'All years', input$YEAR1)
    paste0('Percent energy by source ',
      '(', input$STATE1, ', ', y, ')')
  })
  output$linett3 <- renderText({ 
    y <- ifelse(input$YEAR2 == 'ALL', 'All years', input$YEAR2)
    paste0('Energy by source ',
      '(', input$STATE2, ', ', y, ')')
  })
  output$linett4 <- renderText({ 
    y <- ifelse(input$YEAR2 == 'ALL', 'All years', input$YEAR2)
    paste0('Percent energy by source ',
      '(', input$STATE2, ', ', y, ')')
  })
  output$tablett1 <- renderText({ 
    y <- ifelse(input$YEAR1 == 'ALL', 'All years', input$YEAR1)
    paste0('Energy by source ',
      '(', input$STATE1, ', ', y, ')')
  })
  output$tablett2 <- renderText({ 
    y <- ifelse(input$YEAR1 == 'ALL', 'All years', input$YEAR1)
    paste0('Percent energy by source ',
      '(', input$STATE1, ', ', y, ')')
  })
  output$tablett3 <- renderText({ 
    y <- ifelse(input$YEAR2 == 'ALL', 'All years', input$YEAR2)
    paste0('Energy by source ',
      '(', input$STATE2, ', ', y, ')')
  })
  output$tablett4 <- renderText({ 
    y <- ifelse(input$YEAR2 == 'ALL', 'All years', input$YEAR2)
    paste0('Percent energy by source ',
      '(', input$STATE2, ', ', y, ')')
  })
  
  
  
  output$map1 <- renderPlot({
    a = doFilters2(input, TRUE, input$HMYEAR1)
    a = subset(a, ENERGY.SOURCE == input$heatmapSource1)
    plot_usmap(
      data = a,
      values = 'PCT'
    ) +
    scale_fill_continuous(low = 'white', high = 'blue',
      name = paste('Percent', tolower(input$heatmapSource1)),
      breaks = c(25,50,75,100), limits = c(0, 100)) +
    theme(legend.position = "right")
  })
  output$map2 <- renderPlot({
    a = doFilters2(input, FALSE, input$HMYEAR1)
    a = subset(a, ENERGY.SOURCE == input$heatmapSource1)
    plot_usmap(
      data = a,
      values = 'GEN'
    ) +
    scale_fill_continuous(low = 'white', high = 'blue',
      name = paste('Total', tolower(input$heatmapSource1)),
      label = comma#,
      #breaks = c(25,50,75,100), limits = c(0, 100)
      ) +
    theme(legend.position = "right")
  })
  output$map3 <- renderPlot({
    a = doFilters2(input, TRUE, input$HMYEAR2)
    a = subset(a, ENERGY.SOURCE == input$heatmapSource2)
    plot_usmap(
      data = a,
      values = 'PCT'
    ) +
    scale_fill_continuous(low = 'white', high = 'blue',
      name = paste('Percent', tolower(input$heatmapSource2)),
      breaks = c(25,50,75,100), limits = c(0, 100)) +
    theme(legend.position = "right")
  })
  output$map4 <- renderPlot({
    a = doFilters2(input, FALSE, input$HMYEAR2)
    a = subset(a, ENERGY.SOURCE == input$heatmapSource2)
    plot_usmap(
      data = a,
      values = 'GEN'
    ) +
    scale_fill_continuous(low = 'white', high = 'blue',
      name = paste('Total', tolower(input$heatmapSource2)),
      label = comma#,
      #breaks = c(25,50,75,100), limits = c(0, 100)
      ) +
    theme(legend.position = "right")
  })
  
  # stacked bar chart showing amount of each energy source per year
  # from 1990 - 2019 (assumption: for entire US?)
  output$bar1 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = GEN)) + 
      geom_col(aes(fill = ENERGY.SOURCE), width = 0.7) +
      labs(x = 'Year', y = 'Energy produced') +
      scale_y_continuous(label = comma)
  })
  output$bar3 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE2, input$YEAR2)
    ggplot(a, aes(x = YEAR, y = GEN)) + 
      geom_col(aes(fill = ENERGY.SOURCE), width = 0.7) +
      labs(x = 'Year', y = 'Energy produced') +
      scale_y_continuous(label = comma)
  })
  
  # stacked bar chart showing percent of the total production for each
  # energy source per year from 1990 - 2019
  output$bar2 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE)) + 
      geom_bar(position = 'fill', stat = 'identity') +
      labs(x = 'Year', y = '% energy produced') +
      scale_y_continuous()
  })
  output$bar4 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE2, input$YEAR2)
#~     print(input$YEAR2)
#~     b <- ifelse(!is.numeric(input$YEAR2), 1990:2019, c(as.numeric(input$YEAR2)))
#~     b <- 1990:2019
#~     print(b)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE)) + 
      geom_bar(position = 'fill', stat = 'identity') +
      labs(x = 'Year', y = '% energy produced') +
      scale_y_continuous()
#~       scale_x_continuous()#breaks = b 
  })
  
  # line chart showing the amount of each energy source per year from
  # 1990 - 2019
  output$line1 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Energy produced') +
      scale_y_continuous(label=comma)
  })
  output$line3 <- renderPlot({
    a = doFilters(input, FALSE, input$STATE2, input$YEAR2)
    ggplot(a, aes(x = YEAR, y = GEN, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = 'Energy produced') +
      scale_y_continuous(label=comma)
  })
  
  # line chart showing the percent of the total production for each
  # energy source per year from 1990 - 2019
  output$line2 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE1, input$YEAR1)
    ggplot(a, aes(x = YEAR, y = PCT, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = '% energy produced') +
      scale_y_continuous(label=comma)
  })
  output$line4 <- renderPlot({
    a = doFilters(input, TRUE, input$STATE2, input$YEAR2)
    ggplot(a, aes(x = YEAR, y = PCT, fill = ENERGY.SOURCE, color = ENERGY.SOURCE)) +
      geom_line(stat = 'identity') +
      labs(x = 'Year', y = '% energy produced') +
      scale_y_continuous(label=comma)
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
