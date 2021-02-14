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
      )
              
#~             menuSubItem(
#~         menuSubItem('All', icon = NULL,
#~           checkboxInput('somevalue', 'All', FALSE)
#~         ),
#~         menuSubItem("Sub-item 2")
#~       box(
#~         title = 'Energy source(s)', solidHeader = TRUE,
#~ #         'Box content here', br(), 'More box content',
#~ #         sliderInput('slider', 'Slider input:', 1, 100, 50),
#~         checkboxInput('somevalue', 'All', FALSE),
#~         checkboxInput('somevalue', 'Coal', FALSE)
#~       )
      
#~         selectInput('Year', 'Select the year to visualize'),
#~         selectInput('Room', 'Select the room to visualize', listNamesGood, selected = 'Meeting Room')
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
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar1', height = 300)
              )
            )
          ),
          column(6,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = 'primary', width = 12,
                plotOutput('bar2', height = 300)
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
      ),
      
      tabItem(
        tabName = 'table',
        fluidRow(
          column(6,
            fluidRow(
              box(title = 'Annual energy by source',
                solidHeader = TRUE, status = 'primary', width = 12,
                dataTableOutput('tab1', height = 300)
              )
            )
          ),
          column(6,
            fluidRow(
              box(title = 'Annual energy by source %',
                solidHeader = TRUE, status = 'primary', width = 12,
                dataTableOutput('tab2', height = 300)
              )
            )
          )
        )
      )
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
