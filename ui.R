## ui.R
## Author: David Shumway

library(shiny)
library(shinydashboard)
library(ggplot2)

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
data$STATE <- as.factor(data$STATE)
data$TYPE.OF.PRODUCER <- as.factor(data$TYPE.OF.PRODUCER)
data$ENERGY.SOURCE <- as.factor(data$ENERGY.SOURCE)
data <- data[!(data$STATE == '  '),]
data <- data[!(data$GENERATION..Megawatthours. < 0),]
names(data)[names(data) == 'GENERATION..Megawatthours.'] <- 'GEN'


states = unique(data$STATE)
years = unique(data$YEAR)
print(years)
#~ years[nrow(years) + 1,] = 'ALL'

dashboardPage(
  dashboardHeader(title = 'CS 424 Spring 2020: Project 1'),
  dashboardSidebar(
     tags$head(    # checkbox JS, adapted from
      tags$script( # https://stackoverflow.com/questions/32847743/
        HTML(      # switching-between-menusubitems-in-shinydashboard
          "
#~           $(document).ready(function() {
#~             $('#filter1').on('click', function() {
#~               console.log('x');
#~               console.log($(this).parent());
#~               console.log($(this).parent('input'));
#~               var x = $(this).parent().parent().get(0).getElementsByTagName('input');
#~               console.log(x);
#~               for (var i=0; i<x.length; i++) {
#~                 x[i].checked = true; //$(this).attr('checked');
#~               }
#~             })
#~           })
          "
        )
      )
    ),
    
#~       disable = FALSE,
#~       collapsed = FALSE,
    sidebarMenu(
      menuItem('Stacked', tabName = 'stacked', icon = NULL),
      menuItem('Line',    tabName = 'line', icon = NULL),
      menuItem('Table',   tabName = 'table', icon = NULL),
      
      # menu: select energy sources <all, 1,2,3,4,5>
      menuItem('Sources', icon = NULL,
#~      collapsible =  ??????
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
      
#~       menuItem('States', icon = NULL,
        selectInput('STATE1', 'Select first state to compare', states, selected = 'US-TOTAL'),
        selectInput('STATE2', 'Select second state to compare', states, selected = 'IL'),
        selectInput('YEAR1', 'Select first year to compare', years, selected = 'ALL'),
        selectInput('YEAR2', 'Select second year to compare', years, selected = 'ALL')
#~       )
    )
  ),
  dashboardBody(
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
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar1', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar2', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar3', height = 300)
              )
            )
          ),
          column(3,
            fluidRow(
              box(title = 'Annual energy by source %',
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
          column(6,
            fluidRow(
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('line1', height = 300)
              )
            )
          ),
          column(6,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('line2', height = 300)
              )
            )
          )
        )
#~       )
      ),
      
      tabItem(
        tabName = 'table',
        fluidRow(
          column(6,
            fluidRow(
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = 'primary', width = 12,
                dataTableOutput('tab1')
              )
            )
          ),
          column(6,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = 'primary', width = 12, #height = 300,
                dataTableOutput('tab2') # height = 'auto' ??????? causing error when first running R. Or always on shinyapps.io.
                                        # Warning: Error in dataTableOutput: unused argument (height = "auto")
              )
            )
          )
        )
      ),
      
      tabItem(tabName = 'TESTING-------------------')
      
    )
  )
)


#~ shinyUI(fluidPage(

#~   # Application title
#~   titlePanel('Old Faithful Geyser Data'),

#~   # Sidebar with a slider input for number of bins
#~   sidebarLayout(
#~     sidebarPanel(
#~       sliderInput('bins',
#~                   'Number of bins:',
#~                   min = 1,
#~                   max = 50,
#~                   value = 30)
#~     ),

#~     # Show a plot of the generated distribution
#~     mainPanel(
#~       plotOutput('distPlot')
#~     )
#~   )
#~ ))
