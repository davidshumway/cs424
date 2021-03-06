## ui.R
## Author: David Shumway
## Initial template is from here:
##   https://shiny.rstudio.com/gallery/superzip-example.html

library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(shinyWidgets)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

navbarPage('Raw Power', id='nav',

  tabPanel('Illinois map',
    div(class = 'outer',

      tags$head(
        includeCSS('style.css'),
        includeScript('gomap.js')
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput('map', width='100%', height='100%'),

      # Shiny versions prior to 0.11 should use class = 'modal' instead.
      absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',

        h4('Map explorer'),
        
        actionButton('illAll', 'All'),
        actionButton('illRen', 'Renewables'),
        actionButton('illNonRen', 'Non-Renewables'),
        
        checkboxGroupInput('illSource', 'Source:',
          choices = energyList, selected = unlist(energyList)
        ),
        
        br(),
        actionButton('illReset', 'Reset Map')

    )
  ),
  
  tabPanel('Two-state comparison', style = 'height:calc(100vh - 51px); margin-top: -20px;', # 100vh is full, minus 50+1border nav, minus 20 nav margin
    
      tags$head(
        includeCSS('style.css'),
        includeScript('gomap.js')
      ),
      fluidRow(style = 'height:calc(100vh - 51px);',
        box(style = 'height:calc(100vh - 51px);',
          div(leafletOutput('map1', height = '100%'), style = 'height:calc(100vh - 51px);')
        ),
        box(style = 'height:calc(100vh - 51px);',
          div(leafletOutput('map2', height = '100%'), style = 'height:calc(100vh - 51px);')
        )
      ),
      absolutePanel(id = 'controls1', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 20, right = 'auto', bottom = 'auto',
        width = 330, height = 'auto',
        h4('Map explorer'),
        actionButton('m1All', 'All'),
        actionButton('m1Ren', 'Renewables'),
        actionButton('m1NonRen', 'Non-Renewables'),
        actionButton('m1Link', '', icon = icon('link')),
        checkboxGroupInput('m1Source', 'Source:',
          choices = energyList, selected = unlist(energyList)
        ),
        selectInput('m1Year', 'Year:',
          c('2000', '2010', '2018'),
          multiple = FALSE, selected = 2000
        ),
        selectInput('m1State', 'State:',
          c('All states' = 'ALL', structure(state.abb, names = state.name), 'Washington, DC' = 'DC'),
          multiple = FALSE, selected = 'IL'
        ),
        selectInput('m1BaseMap', 'Map Type:',
          c('OpenStreetMap', 'Stamen.Toner', 'CartoDB.Positron', 'Esri.NatGeoWorldMap'),
          multiple = FALSE, selected = 'OpenStreetMap'
        ),
        sliderInput('m1Range', 'mWh Range:',
          min = 0, max = 32, step = 8, value = c(0, 32)
        ),
        # idled plants: present in any previous year but not present in
        #   selected year
        switchInput('m1Idled', 'Highlight idled', value = FALSE),
        # new plant: not present in any previous year but present in
        #   selected year
        switchInput('m1New', 'Highlight new', value = FALSE),
        actionButton('m1Reset', 'Reset Map')
      ),
      absolutePanel(id = 'controls2', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',
        h4('Map explorer'),
        actionButton('m2All', 'All'),
        actionButton('m2Ren', 'Renewables'),
        actionButton('m2NonRen', 'Non-Renewables'),
        actionButton('m2Link', '', icon = icon('link')),
        checkboxGroupInput('m2Source', 'Source:',
          choices = energyList, selected = unlist(energyList)
        ),
        selectInput('m2Year', 'Year:',
          c('2000', '2010', '2018'),
          multiple = FALSE, selected = 2018
        ),
        selectInput('m2State', 'State:',
          c('All states' = 'ALL', structure(state.abb, names = state.name), 'Washington, DC' = 'DC'),
          multiple = FALSE, selected = 'IL'
        ),
        selectInput('m2BaseMap', 'Map Type:',
          c('OpenStreetMap', 'Stamen.Toner', 'CartoDB.Positron', 'Esri.NatGeoWorldMap'),
          multiple = FALSE, selected = 'OpenStreetMap'
        ),
        # really buggy in Chrome ??
        sliderInput('m2Range', 'mWh Range:',
          min = 0, max = 32, step = 8, value = c(0, 32)
        ),
        switchInput('m2Idled', 'Highlight idled', value = FALSE),
        switchInput('m2New', 'Highlight new', value = FALSE),
        actionButton('m2Reset', 'Reset Map')
      )
  ),

  tabPanel('Data explorer (2000)',
    fluidRow(
      column(3,
      ),
      column(3,
      ),
      column(3,
      )
    ),
    fluidRow(
      column(1,
      ),
      column(1,
      )
    ),
    hr(),
    DT::dataTableOutput('data1tbl')
  ),
  tabPanel('Data explorer (2010)',
    DT::dataTableOutput('data2tbl')
  ),
  tabPanel('Data explorer (2018)',
    DT::dataTableOutput('data3tbl')
  ),
  
  tabPanel('About',
      tags$div(
        HTML('Initial template used: (https://shiny.rstudio.com/gallery/superzip-example.html).<br>Author: David Shumway<br>Original data: (https://www.epa.gov/egrid/download-data).')
      )
  )
)



