# # load libraries
# packages <- c("shiny","dplyr","ggplot2","leaflet",
# 							"raster","ggvis","maptools", "maps","sp", 
# 							"gplots", "htmltools", "readr", "DT", "pool",
# 							"shinyjs")
# 
# for(i in packages) {
# 	require(i, character.only=TRUE)
# }

library("shiny")
library("dplyr")
library("ggplot2")
library("leaflet")
library("raster")
library("ggvis")
library("maptools")
library("maps")
library("sp")
library("gplots")
library("htmltools")
library("readr")
library("DT")
library("pool")
library("shinyjs")
library("shinyWidgets")
library("leaflet.extras")

## DEVelopment switch
DEV <- TRUE
options(shiny.sanitize.errors = !DEV)

#Define the size of the pixels in degrees. 0.05 is for NZ VCSN
pixelSizeDeg <- 0.05
halfPixelDeg <- pixelSizeDeg/2

poolConnection <- NULL

# load credentials
path <- './creds.R'

#function for validating path and read credential file
readCred <- function(path) {
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  source(path)
}
readCred(path)

project_list <- ls()[grep("^P[0-9]{3}",ls())]

#palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

vcsnStations <- read.csv("vcsnStations.csv")

vcsnCoordinates <- SpatialPointsDataFrame(vcsnStations[,c('LONGT', 'LAT')] , vcsnStations)

#install.packages('raster', repos = 'http://r-forge.r-project.org/', type = 'source') # using new raster lib

inputname_list_alt <<- list()
inputname_list_ref <<- list()


# trying to sr=et this to avoid ggplot crash
#options(expressions=5000)
