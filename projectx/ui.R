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

navbarPage('Project X - Power to the People', id='nav',

   # 100vh is full, minus 50+1border nav, minus 20 nav margin
  tabPanel('World-map comparison', style = 'height:calc(100vh - 51px); margin-top: -20px;',
    
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
        selectInput('m1Country', 'Continent:',
          c('All countries' = 'ALL', uniqueCountries), #
          multiple = FALSE, selected = 'Americas'
        ),
        selectInput('m1BaseMap', 'Map Type:',
          c('OpenStreetMap', 'Stamen.Toner', 'CartoDB.Positron', 'Esri.NatGeoWorldMap'),
          multiple = FALSE, selected = 'OpenStreetMap'
        ),
        sliderInput('m1Range', 'GW Range:',
          min = 0, max = 23, step = 1, value = c(0, 23), dragRange=FALSE
        ),
#~         switchInput('m1Idled', 'Highlight idled', value = FALSE),
#~         switchInput('m1New', 'Highlight new', value = FALSE),
        actionButton('m1Reset', 'Reset Map')
      ),
      
      absolutePanel(id = 'controls3', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 160, height = 'auto',
      
        sliderInput('m2Range', 'GW Range:',
         min = 0, max = 23, step = 1, value = c(0, 23), dragRange=FALSE
        )),
        
      absolutePanel(id = 'controls2', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',
        h4('Map explorer'),
#~         sliderInput('m2Range', 'GW Range:',
#~          min = 0, max = 23, step = 1, value = c(0, 23), dragRange=FALSE
#~         ),
        actionButton('m2All', 'All'),
        actionButton('m2Ren', 'Renewables'),
        actionButton('m2NonRen', 'Non-Renewables'),
        actionButton('m2Link', '', icon = icon('link')),
        checkboxGroupInput('m2Source', 'Source:',
          choices = energyList, selected = unlist(energyList)
        ),
        selectInput('m2Country', 'Continent:',
          c('All countries' = 'ALL', uniqueCountries), #
          multiple = FALSE, selected = 'Asia'
        ),
        selectInput('m2BaseMap', 'Map Type:',
          c('OpenStreetMap', 'Stamen.Toner', 'CartoDB.Positron', 'Esri.NatGeoWorldMap'),
          multiple = FALSE, selected = 'OpenStreetMap'
        ),
#~         sliderInput('m2Range', 'GW Range:',
#~          min = 0, max = 23, step = 1, value = c(0, 23), dragRange=FALSE
#~         ),
#~         sliderTextInput(
#~           inputId = 'm2Range',
#~           label = 'GW Range:',
#~           grid = TRUE,
#~           force_edges = TRUE,
#~           choices = c(0:23)
#~         ),

#~         switchInput('m2Idled', 'Highlight idled', value = FALSE),
#~         switchInput('m2New', 'Highlight new', value = FALSE),
        actionButton('m2Reset', 'Reset Map')
      )
#~       sliderInput('m2Range', 'GW Range:',
#~        min = 0, max = 23, step = 1, value = c(0, 23), dragRange=FALSE
#~       )
  ),

  tabPanel('Data explorer',
    DT::dataTableOutput('datatbl')
  ),
  
  tabPanel('About',
    tags$div(
      HTML('Initial template used is online here: (https://shiny.rstudio.com/gallery/superzip-example.html).<br>Author: David Shumway<br>Original data is online here: (https://datasets.wri.org/dataset/globalpowerplantdatabase).')
    )
  )
)



