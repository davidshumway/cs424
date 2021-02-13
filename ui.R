## ui.R
## Author: David Shumway

library(shiny)
library(shinydashboard)
library(ggplot2)

options(shiny.fullstacktrace = FALSE)


dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2020 Example Dashboard"),
  dashboardSidebar(
#~       disable = FALSE,
#~       collapsed = FALSE,
    sidebarMenu(
      menuItem('Stacked', tabName = 'stacked', icon = NULL),
      menuItem('Line',    tabName = 'line', icon = NULL),
      menuItem('Table',   tabName = 'table', icon = NULL)
      
      # menu: select energy sources <all, 1,2,3,4,5>
      
#~         selectInput("Year", "Select the year to visualize"),
#~         selectInput("Room", "Select the room to visualize", listNamesGood, selected = "Meeting Room")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'stacked',
        fluidRow(
          column(6,
            fluidRow(
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = "primary", width = 12,
                plotOutput('bar1', height = 300)
              )
            )
          ),
          column(6,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = "primary", width = 12,
                plotOutput('bar2', height = 300)
              )
            )
          ),
        )
      ),
      
      tabItem(
        tabName = 'line',
        fluidRow(
          column(6,
            fluidRow(
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = "primary", width = 12,
                plotOutput('line1', height = 300)
              )
            )
          ),
          column(6,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = "primary", width = 12,
                plotOutput('line2', height = 300)
              )
            )
          ),
        )
      ),
      
      tabItem(
        tabName = 'table',
        fluidRow(
          column(6,
            fluidRow(
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = "primary", width = 12,
                dataTableOutput('tab1', height = 300)
              )
            )
          ),
          column(6,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = "primary", width = 12,
                dataTableOutput('tab2', height = 300)
              )
            )
          ),
        )
      )
    )
    
  )
)


#~ shinyUI(fluidPage(

#~   # Application title
#~   titlePanel("Old Faithful Geyser Data"),

#~   # Sidebar with a slider input for number of bins
#~   sidebarLayout(
#~     sidebarPanel(
#~       sliderInput("bins",
#~                   "Number of bins:",
#~                   min = 1,
#~                   max = 50,
#~                   value = 30)
#~     ),

#~     # Show a plot of the generated distribution
#~     mainPanel(
#~       plotOutput("distPlot")
#~     )
#~   )
#~ ))
