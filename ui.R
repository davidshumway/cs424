## ui.R
## Author: David Shumway

library(shiny)
library(shinydashboard)
library(ggplot2)
library(usmap)

options(shiny.fullstacktrace = TRUE)

#~               var x = $('#filter1').parent();#.get(0);
#~               console.log(x);
#~               var cb = $('input');
#~               console.log(cb);
#~               cb.prop('checked', true); //!checkBoxes.prop('checked')
#~               x.prop('checked', true);


## todo: load data function?
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
data$ENERGY.SOURCE <- as.factor(data$ENERGY.SOURCE)
data <- data[!(data$STATE == '  '),]
data$STATE <- as.factor(data$STATE)
data <- data[!(data$GENERATION..Megawatthours. < 0),]
names(data)[names(data) == 'GENERATION..Megawatthours.'] <- 'GEN'



# Data fix: some states have no entry for some types of energy,
#  such as solar, especially early in the data, e.g., 1990.
#  This causes colors to be different when comparing across different
#  years, same state. E.g., 2019 IL and 1990 IL have a different
#  color scheme!
#~ print(data$ENERGY.SOURCE)
#~ for(i in 1990:2019) {       # for-loop over rows
#~   for (j in data$STATE) {
#~     for (k in data$ENERGY.SOURCE) {
#~       if (!data[i][j][k])
#~         data[i][j][k] = 0
#~     }
#~   }
#~ }
#~ for(i in 1:nrow(data)) {       # for-loop over rows
#~   print(data[i,]$ENERGY.SOURCE)
#~   break
#~   #if (data[i,]$
#~   #data[i, ] <- data[i, ] - 100
#~ }

states = unique(data$STATE)
years2= unique(data$YEAR)
years = unique(data$YEAR)
years = rbind('ALL', years) # prepend ALL to years list

dashboardPage(
  dashboardHeader(title = 'CS 424 Spring 2020: Project 1'),
  dashboardSidebar(
#~       disable = FALSE,
#~       collapsed = FALSE,
    sidebarMenu(
      menuItem('Charts', icon = NULL,
        menuItem('Chart type', icon = NULL,
          menuItem('Stacked', tabName = 'stacked', icon = NULL),
          menuItem('Line', tabName = 'line', icon = NULL),
          menuItem('Table', tabName = 'table', icon = NULL)
        ),
        menuItem('Energy Source', icon = NULL,
          checkboxInput('filter1', 'All', TRUE),
          checkboxInput('filter2', 'Coal', FALSE),
          checkboxInput('filter3', 'Geothermal', FALSE),
          checkboxInput('filter4', 'Hyrdo', FALSE),
          checkboxInput('filter5', 'Nat. Gas', FALSE),
          checkboxInput('filter6', 'Nuclear', FALSE),
          checkboxInput('filter7', 'Petroleum', FALSE),
          checkboxInput('filter8', 'Solar', FALSE),
          checkboxInput('filter9', 'Wind', FALSE),
          checkboxInput('filter10','Wood', FALSE)
        ),
        menuItem('State', icon = NULL,
          selectInput('STATE1', 'Select first state to compare (left)', states, selected = 'US-TOTAL'),
          selectInput('STATE2', 'Select second state to compare (right)', states, selected = 'IL')
        ),
        menuItem('Year', icon = NULL,
          # selected = 'ALL' does not work, but selected = 0 works??
          # Also, on first load, it seems to be empty
          # but then after reloading its set to ALL??
          selectInput('YEAR1', 'Select first year to compare (left)', years, selected = 0),
          selectInput('YEAR2', 'Select second year to compare (right)', years, selected = 0)
        )
      ),
      menuItem('Heatmap', icon = NULL,
        selectInput('heatmapSource1', 'Energy to map (left)',
          choices = c('Coal', 'Geothermal', 'Hydro', 'Natural Gas', 'Nuclear',
            'Petroleum', 'Solar', 'Wind', 'Wood'),
          selected = 0),
        selectInput('heatmapSource2', 'Energy to map (right)',
          choices = c('Coal', 'Geothermal', 'Hydro', 'Natural Gas', 'Nuclear',
            'Petroleum', 'Solar', 'Wind', 'Wood'),
          selected = 0),
        selectInput('HMYEAR1', 'Select first year to compare', years2, selected = 0),
        selectInput('HMYEAR2', 'Select second year to compare', years2, selected = 2019)
      )
    )
  ),
  dashboardBody(
    fluidRow(
      column(3,
        fluidRow(
          box(title = textOutput('maptt2'),
            solidHeader = TRUE, status = 'primary', width = 12,
            plotOutput('map2', height = 300)
          )
        )
      ),
      column(3,
        fluidRow(
          box(title = textOutput('maptt1'),
            solidHeader = TRUE, status = 'primary', width = 12,
            plotOutput('map1', height = 300)
          )
        )
      ),
      column(3,
        fluidRow(
          box(title = textOutput('maptt4'),
            solidHeader = TRUE, status = 'primary', width = 12,
            plotOutput('map4', height = 300)
          )
        )
      ),
      column(3,
        fluidRow(
          box(title = textOutput('maptt3'),
            solidHeader = TRUE, status = 'primary', width = 12,
            plotOutput('map3', height = 300)
          )
        )
      )
    ),
    tabItems(
      tabItem(
        tabName = 'stacked',
#~         fluidRow(
#~           column(6,
#~             fluidRow(
#~ #               selectInput('STATE1', 'Select a state to compare', states, selected = 'US-TOTAL')
#~             )
#~           ),
#~           column(6,
#~             fluidRow(
#~ #               selectInput('STATE2', 'Select a state to compare', states, selected = 'IL')
#~             )
#~           )
#~         ),
        fluidRow(
          column(3,
            fluidRow(
              box(title = textOutput('stackedtt1'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar1', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('stackedtt2'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar2', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('stackedtt3'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar3', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('stackedtt4'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar4', height = 300)
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'line',
        fluidRow(
          column(3,
            fluidRow(
              box(title = textOutput('linett1'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('line1', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('linett2'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('line2', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('linett3'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('line3', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('linett4'),
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('line4', height = 300)
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'table',
        fluidRow(
          column(3,
            fluidRow(
              box(title = textOutput('tablett1'),
                solidHeader = TRUE, status = 'primary', width = 12,
                DT::dataTableOutput('tab1', height='auto')
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('tablett2'),
                solidHeader = TRUE, status = 'primary', width = 12,
                DT::dataTableOutput('tab2', height='auto')
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('tablett3'),
                solidHeader = TRUE, status = 'primary', width = 12,
                DT::dataTableOutput('tab3', height='auto')
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = textOutput('tablett4'),
                solidHeader = TRUE, status = 'primary', width = 12,
                DT::dataTableOutput('tab4', height='auto')
              )
            )
          )
        )
      )   
    )
  )
)

