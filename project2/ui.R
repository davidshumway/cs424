## ui.R
## Author: David Shumway
## Initial template is from here:
## https://shiny.rstudio.com/gallery/superzip-example.html

#~ library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
#~ library(shinyWidgets)
#~ library(usmap)
#~ library(stringr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)



navbarPage('Superz', id='nav',

  tabPanel('Illinois map',
    div(class = 'outer',

      tags$head(
        # Include our custom CSS
        includeCSS('style.css'),
#~         includeCSS('bootstrap.min.css'),
        includeScript('gomap.js')
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput('map', width='100%', height='100%'),

      # Shiny versions prior to 0.11 should use class = 'modal' instead.
      absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',

        h2('Z explorer'),
        
        actionButton('illAll', 'All'),
        actionButton('illRen', 'Renewables'),
        actionButton('illNonRen', 'Non-Renewables'),
        
        checkboxGroupInput('illSource', 'Source:',
          choices = energyList, selected = unlist(energyList)
        ),
        
        br(),
        actionButton('illReset', 'Reset Map')

        #selectInput('color', 'Color', vars),
        
        #selectInput('size', 'Size', vars, selected = 'adultpop'),
        
#~         conditionalPanel('input.color == "superzip" || input.size == "superzip"',
#~           # Only prompt for threshold when coloring or sizing by superzip
#~           numericInput('threshold', 'SuperZIP threshold (top n percentile)', 5)
#~         ),

#~         plotOutput('histCentile', height = 200),
#~         plotOutput('scatterCollegeIncome', height = 250)
      )#,

#~       tags$div(id='cite',
#~         'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
#~       )
    )
  ),
  
  tabPanel('Two-state comparison', style = 'height:calc(100vh - 51px); margin-top: -20px;', # 100vh is full, minus 50+1border nav, minus 20 nav margin
    
#~     div(class = 'outer',
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
        h2('Z explorer'),
        actionButton('m1All', 'All'),
        actionButton('m1Ren', 'Renewables'),
        actionButton('m1NonRen', 'Non-Renewables'),
        actionButton('m1Link', '', icon = icon('link')),
        checkboxGroupInput('m1Source', 'Source:',
          choices = energyList, selected = unlist(energyList)
        ),
#~         br(),
        selectInput('m1Year', 'Year:',
          c('2000', '2010', '2018'),
          selected = 2000
        ),
        selectInput('m1State', 'State:',
          c('All states' = 'ALL', structure(state.abb, names = state.name), 'Washington, DC' = 'DC'),
          multiple = FALSE, selected = 'IL'
        ),
        actionButton('m1Reset', 'Reset Map')
      ),
      absolutePanel(id = 'controls2', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',
        h2('Z explorer'),
        actionButton('m2All', 'All'),
        actionButton('m2Ren', 'Renewables'),
        actionButton('m2NonRen', 'Non-Renewables'),
        actionButton('m2Link', '', icon = icon('link')),
        checkboxGroupInput('m2Source', 'Source:',
          choices = energyList, selected = unlist(energyList)
        ),
        selectInput('m2Year', 'Year:',
          c('2000', '2010', '2018'),
          selected = 2018
        ),
        selectInput('m2State', 'State:',
          c('All states' = 'ALL', structure(state.abb, names = state.name), 'Washington, DC' = 'DC'),
          multiple = FALSE, selected = 'IL'
        ),
        actionButton('m2Reset', 'Reset Map')
      )
#~     )
  ),

  tabPanel('Data explorer (2000)',
    fluidRow(
      column(3,
      
        #selectInput('states', 'States', c('All states'='', structure(state.abb, names=state.name), 'Washington, DC'='DC'), multiple=TRUE)
      ),
      column(3,
        #conditionalPanel('input.states',
        #  selectInput('cities', 'Cities', c('All cities'=''), multiple=TRUE)
        #)
      ),
      column(3,
        #conditionalPanel('input.states',
        #  selectInput('zipcodes', 'Zipcodes', c('All zipcodes'=''), multiple=TRUE)
        #)
      )
    ),
    fluidRow(
      column(1,
        #numericInput('minScore', 'Min score', min=0, max=100, value=0)
      ),
      column(1,
        #numericInput('maxScore', 'Max score', min=0, max=100, value=100)
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
    fluidRow(
      column(8, class="card",
        tags$div(class="card",
          #card Start
          tags$div(
            class = "card-body",
            tags$div(
              class = "card-title",
              tags$span("")
            ),
            tags$div(
              class = "card-text", style = "height: 750px",
              textOutput("textAbout")
            )
          )
        )
      )
    )
  )
      
#~   conditionalPanel('false', icon('crosshair'))
)



