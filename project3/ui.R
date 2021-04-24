## ui.R
## Author: David Shumway
## Initial template is from here:
##   https://shiny.rstudio.com/gallery/superzip-example.html

library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(shinyWidgets)

library(mapview)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

navbarPage('Raw Power', id='nav',

  tabPanel('Illinois map',
    div(class = 'outer',

      tags$head(
        #includeCSS('style.css'),
        #includeScript('gomap.js')
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      #leafletOutput('map', width='100%', height='100%'),
      leafletOutput('mapplot', height='800px'),# width = '100%', height = '100%'),
      #mapview:::plainViewOutput('test'),
      

      # Shiny versions prior to 0.11 should use class = 'modal' instead.
      absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',

        h4('Map explorer'),
        
        actionButton('illAll', 'Electric'),
        actionButton('illRen', 'Renewables'),
        actionButton('illNonRen', 'Non-Renewables'),
        
        selectInput('viewType', 'View Type:', c(
            'Gas Use' = 'gas',
            'Electricity Use' = 'electric',
            'Building Age' = 'age',
            'Building Type' = 'type',
            'Building Height' = 'height',
            'Total Population' = 'population'
          ), multiple = FALSE, size = 6, selectize = FALSE
        ),
        checkboxGroupButtons(inputId = 'viewType', label = 'View Type:', 
          choices = c(
            'Gas Use' = 'gas',
            'Electricity Use' = 'electric',
            'Building Age' = 'age',
            'Building Type' = 'type',
            'Building Height' = 'height',
            'Total Population' = 'population'
          )#, multiple = FALSE, size = 6, selectize = FALSE
        ),
        
#~         checkboxGroupInput('illSource', 'Source:',
#~           choices = energyList, selected = unlist(energyList)
        ),
        
        br(),
        actionButton('illReset', 'Reset Map')
    )
  ),

#~   tabPanel('Data explorer (2000)',
#~     fluidRow(
#~       column(3,
#~       ),
#~       column(3,
#~       ),
#~       column(3,
#~       )
#~     ),
#~     fluidRow(
#~       column(1,
#~       ),
#~       column(1,
#~       )
#~     ),
#~     hr()#,
    #DT::dataTableOutput('data1tbl')
#~   ),
#~   tabPanel('Data explorer (2010)'#,
#~     #DT::dataTableOutput('data2tbl')
#~   ),
#~   tabPanel('Data explorer (2018)'#,
#~     #DT::dataTableOutput('data3tbl')
#~   ),
  
  tabPanel('About',
      tags$div(
        HTML('--')
      )
  )
)



