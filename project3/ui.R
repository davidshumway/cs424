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

navbarPage('We\'ve Got the Power', id='nav',

  tabPanel('Near West Side',
    div(class = 'outer',

      tags$head(
        #includeCSS('style.css'),
        #includeScript('gomap.js')
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      #leafletOutput('map', width='100%', height='100%'),
      leafletOutput('mapplot', height='1000px'),# width = '100%', height = '100%'),

      # Shiny versions prior to 0.11 should use class = 'modal' instead.
      absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 360, height = 'auto',

        h4('Map explorer'),
        
#~         actionButton('illAll', 'Electric'),
#~         actionButton('illRen', 'Renewables'),
#~         actionButton('illNonRen', 'Non-Renewables'),
        
        selectInput('viewType', 'View Type:', c(
            'Gas Use' = 'gas',
            'Electricity Use' = 'electric',
            'Building Age' = 'age',
            'Building Type' = 'type',
            'Building Height' = 'height',
            'Total Population' = 'population'
          ), selected = 'electric'
          #, multiple = FALSE, size = 6, selectize = FALSE
        ),
        
        selectInput('month', 'Month:', c(
            'Full Year' = 'all',
            'January' = '.JANUARY.2010',
            'February' = '.FEBRUARY.2010',
            'March' = '.MARCH.2010',
            'April' = '.APRIL.2010',
            'May' = '.MAY.2010',
            'June' = '.JUNE.2010',
            'July' = '.JULY.2010',
            'August' = '.AUGUST.2010',
            'September' = '.SEPTEMBER.2010',
            'October' = '.OCTOBER.2010',
            'November' = '.NOVEMBER.2010',
            'December' = '.DECEMBER.2010'
          ), selected = 'all'
        ),
        
        checkboxGroupButtons(inputId = 'building', label = 'Building Type:',
          choices = c(
            'Residential' = 'Residential',
            'Commercial' = 'Commercial',
            'Industrial' = 'Industrial'#,
            #'Mixed Use' = 'mix'
          ), selected = c('Residential', 'Commercial', 'Industrial')#, 'mix')
        ),
        
        # This does not work well.
#~         checkboxGroupButtons(inputId = 'viewType2', label = 'View Type:', 
#~           choices = c(
#~             'Gas Use' = 'gas',
#~             'Electricity Use' = 'electric',
#~             'Building Age' = 'age',
#~             'Building Type' = 'type',
#~             'Building Height' = 'height',
#~             'Total Population' = 'population'
#~           )
#~         ),
        
#~         checkboxGroupInput('illSource', 'Source:',
#~           choices = energyList, selected = unlist(energyList)
#~         ),
        
        h5('Electricity per month'),
        plotOutput('bar1', height = 120),
        h5('Gas per month'),
        plotOutput('bar2', height = 120),
        h5('Raw data'),
        DT::dataTableOutput('data1tbl'),
        DT::dataTableOutput('data2tbl'),
        actionButton('nwsReset', 'Reset Map'),
      )
    )
  ),
  
  tabPanel('Comparison',
    div(class = 'outer',

      tags$head(
        #includeCSS('style.css'),
        #includeScript('gomap.js')
      ),

      fluidRow(style = 'height:calc(100vh - 80px);',
        box(style = 'height:calc(100vh - 80px);',
          div(leafletOutput('mapplot2', height = '100%'), style = 'height:calc(100vh - 80px);')
        ),
        box(style = 'height:calc(100vh - 80px);',
          div(leafletOutput('mapplot3', height = '100%'), style = 'height:calc(100vh - 80px);')
        )
      ),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      #leafletOutput('map', width='100%', height='100%'),
#~       leafletOutput('mapplot', width = '50%', height='1000px'),# width = '100%', height = '100%'),
#~       leafletOutput('mapplot', width = '50%', height='1000px'),# width = '100%', height = '100%'),

      # left
      absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 20, right = 'auto', bottom = 'auto',
        width = 360, height = 'auto',

        h4('Map explorer'),
        
        selectInput('viewType', 'View Type:', c(
            'Gas Use' = 'gas',
            'Electricity Use' = 'electric',
            'Building Age' = 'age',
            'Building Type' = 'type',
            'Building Height' = 'height',
            'Total Population' = 'population'
          ), selected = 'electric'
        ),
        
        selectInput('month', 'Month:', c(
            'Full Year' = 'all',
            'January' = '.JANUARY.2010',
            'February' = '.FEBRUARY.2010',
            'March' = '.MARCH.2010',
            'April' = '.APRIL.2010',
            'May' = '.MAY.2010',
            'June' = '.JUNE.2010',
            'July' = '.JULY.2010',
            'August' = '.AUGUST.2010',
            'September' = '.SEPTEMBER.2010',
            'October' = '.OCTOBER.2010',
            'November' = '.NOVEMBER.2010',
            'December' = '.DECEMBER.2010'
          ), selected = 'all'
        ),
        
        checkboxGroupButtons(inputId = 'building', label = 'Building Type:',
          choices = c(
            'Residential' = 'Residential',
            'Commercial' = 'Commercial',
            'Industrial' = 'Industrial'
          ), selected = c('Residential', 'Commercial', 'Industrial')#, 'mix')
        ),
        
        h5('Electricity per month'),
        plotOutput('bar10', height = 120),
        h5('Gas per month'),
        plotOutput('bar20', height = 120),
        h5('Raw data'),
        DT::dataTableOutput('data10tbl'),
        DT::dataTableOutput('data20tbl'),
        actionButton('nwsReset', 'Reset Map'),
      ),
      # right
      absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 360, height = 'auto',

        h4('Map explorer'),
        
        selectInput('viewType', 'View Type:', c(
            'Gas Use' = 'gas',
            'Electricity Use' = 'electric',
            'Building Age' = 'age',
            'Building Type' = 'type',
            'Building Height' = 'height',
            'Total Population' = 'population'
          ), selected = 'electric'
        ),
        
        selectInput('month', 'Month:', c(
            'Full Year' = 'all',
            'January' = '.JANUARY.2010',
            'February' = '.FEBRUARY.2010',
            'March' = '.MARCH.2010',
            'April' = '.APRIL.2010',
            'May' = '.MAY.2010',
            'June' = '.JUNE.2010',
            'July' = '.JULY.2010',
            'August' = '.AUGUST.2010',
            'September' = '.SEPTEMBER.2010',
            'October' = '.OCTOBER.2010',
            'November' = '.NOVEMBER.2010',
            'December' = '.DECEMBER.2010'
          ), selected = 'all'
        ),
        
        checkboxGroupButtons(inputId = 'building', label = 'Building Type:',
          choices = c(
            'Residential' = 'Residential',
            'Commercial' = 'Commercial',
            'Industrial' = 'Industrial'
          ), selected = c('Residential', 'Commercial', 'Industrial')#, 'mix')
        ),
        
        h5('Electricity per month'),
        plotOutput('bar30', height = 120),
        h5('Gas per month'),
        plotOutput('bar40', height = 120),
        h5('Raw data'),
        DT::dataTableOutput('data30tbl'),
        DT::dataTableOutput('data40tbl'),
        actionButton('nwsReset', 'Reset Map'),
      )
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
      HTML('Author: David Shumway<br>Data source: https://www.kaggle.com/chicago/chicago-energy-usage-2010')
    )
  )
)



