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
        includeCSS('css/main.css')
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
        
        fluidRow(
          column(6, 
            selectInput('viewType', 'View Type:', c(
                'Gas Use' = 'gas',
                'Electricity Use' = 'electric',
                'Building Age' = 'age',
                'Building Type' = 'type',
                'Building Height' = 'height',
                'Total Population' = 'population'
              ), selected = 'electric'
              #, multiple = FALSE, size = 6, selectize = FALSE
            )
          ),
          column(6,
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
            )
          )
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
        
        fluidRow(
          column(6, 
            h5('Electricity per month'),
            plotOutput('bar1', height = 120)
          ),
          column(6, 
            h5('Gas per month'),
            plotOutput('bar2', height = 120)
          )
        ),
        
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
        
        selectInput('bt2', 'Map type:',
          c('Blocks' = 'blocks','Tracts' = 'tracts'),
          selected = 'blocks'),
          
        fluidRow(
          column(6, 
            selectInput('community2', 'Community:', communities, selected = 'Near West Side')
          ),
          column(6, 
            selectInput('color2', 'Color:', c(1,2,3), selected = 1)
          )
        ),
        
        fluidRow(
          column(6, 
            selectInput('viewType2', 'View Type:', 
              list(
                'Blocks / Tracts' = c(
                  'Gas Use' = 'gas',
                  'Electricity Use' = 'electric',
                  'Building Age' = 'age',
                  'Building Type' = 'type',
                  'Building Height' = 'height',
                  'Total Population' = 'population'
                ),
                'Tract-view only' = c(
                  '10% oldest' = '10oldest',
                  '10% newest' = '10newest',
                  '10% tallest' = '10tallest',
                  '10% shortest' = '10shortest',
                  '10% most electric' = '10electric',
                  '10% most gas' = '10gas',
                  '10% high. population' = '10population',
                  '10% low. population' = '10poplowest',
                  '10% most occupied' = '10occupied',
                  '10% highest rental %' = '10renters'
                )
              ), selected = 'electric'
            )
          ),
          column(6,
            selectInput('month2', 'Month:', c(
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
            )
          )
        ),
        
        checkboxGroupButtons(inputId = 'building2', label = 'Building Type:',
          choices = c(
            'Residential' = 'Residential',
            'Commercial' = 'Commercial',
            'Industrial' = 'Industrial'
          ), selected = c('Residential', 'Commercial', 'Industrial')#, 'mix')
        ),
        
        fluidRow(
          column(6, 
            h5('Electricity per month'),
            plotOutput('bar21', height = 120)
          ),
          column(6, 
            h5('Gas per month'),
            plotOutput('bar22', height = 120)
          )
        ),
        
        DT::dataTableOutput('data21tbl'),
        DT::dataTableOutput('data22tbl'),
        
        actionButton('reset2', 'Reset Map'),
      ),
      # right
      absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 360, height = 'auto',

        h4('Map explorer'),
        
        selectInput('bt3', 'Map type:',
          c('Blocks' = 'blocks','Tracts' = 'tracts'),
          selected = 'blocks'),
          
        fluidRow(
          column(6, 
            selectInput('community3', 'Community:', communities, selected = 'Loop')
          ),
          column(6, 
            selectInput('color3', 'Color:', c(1,2,3), selected = 1)
          )
        ),
        
        fluidRow(
          column(6, 
            selectInput('viewType3', 'View Type:', 
              list(
                'Blocks / Tracts' = c(
                  'Gas Use' = 'gas',
                  'Electricity Use' = 'electric',
                  'Building Age' = 'age',
                  'Building Type' = 'type',
                  'Building Height' = 'height',
                  'Total Population' = 'population'
                ),
                'Tract-view only' = c(
                  '10% oldest' = '10oldest',
                  '10% newest' = '10newest',
                  '10% tallest' = '10tallest',
                  '10% shortest' = '10shortest',
                  '10% most electric' = '10electric',
                  '10% most gas' = '10gas',
                  '10% high. population' = '10population',
                  '10% low. population' = '10poplowest',
                  '10% most occupied' = '10occupied',
                  '10% highest rental %' = '10renters'
                )
              ), selected = 'electric'
            )
          ),
          column(6,
            selectInput('month3', 'Month:', c(
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
            )
          )
        ),
        
        checkboxGroupButtons(inputId = 'building3', label = 'Building Type:',
          choices = c(
            'Residential' = 'Residential',
            'Commercial' = 'Commercial',
            'Industrial' = 'Industrial'
          ), selected = c('Residential', 'Commercial', 'Industrial')#, 'mix')
        ),
        
        fluidRow(
          column(6, 
            h5('Electricity per month'),
            plotOutput('bar31', height = 120)
          ),
          column(6, 
            h5('Gas per month'),
            plotOutput('bar32', height = 120)
          )
        ),
        
        DT::dataTableOutput('data31tbl'),
        DT::dataTableOutput('data32tbl'),
        
        actionButton('reset3', 'Reset Map'),
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



