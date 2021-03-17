# CS424 Project 2

## Installation
Install the following packages in R:
```
library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
```

Clone the package:
```bash
$ git clone https://github.com/davidshumway/cs424 && cd ./cs424/project2/
```

## Running the program
The program may then be run in an R console:
```bash
shiny::runApp()
```
This will open a web browser pointed to the running program.

## RStudio
Alternatively, the program may be run via RStudio by first downloading and install RStudio here: (https://rstudio.com/products/rstudio/download/).

## Future work
One future task is to alter the marker size based on zoom level. At present, there appears to be no default method available in Leaflet to achieve this. However, implementing the feature should be fairly straightforward by simply creating an event listener for zoom changes within Leaflet, and programming the markers to update based on this size.
