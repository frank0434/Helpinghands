
# Author: Jian (AKA Frank) Liu
# Aim: Loading essentials for the shiny app
# Date: 2021-02-09

library(influxdbr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(data.table)
library(tinytex)
library(plotly)
library(knitr)
library(rmarkdown)

source("secrets.R")
con <- influx_connection(scheme = "http",
                         host=influxdbcreds$host,
                         port=influxdbcreds$port,
                         user=influxdbcreds$user,
                         pass=influxdbcreds$pass
                         # dbname="Ablock"
                         )
avalDBs <- show_databases(con)$name



# Test db connection ------------------------------------------------------


# db_tab <- influx_query(con = con, 
#              db = "indoorUnit",
#              query = "SELECT * FROM file LIMIT 1", 
#              return_xts = FALSE, simplifyList = TRUE) %>% 
#   data.table::as.data.table()
# 
# colnames(db_tab)
conventions <- c("statement_id", "series_names", "series_tags","time", "host", 
                 "series_partial")
