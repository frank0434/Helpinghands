#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)

# Define UI for application that draws a histogram


# Define the sidebar ------------------------------------------------------

sidebar <- dashboardSidebar(
    selectInput(inputId = "input_db",
                label = "Database Name",
                choices = avalDBs),
    selectInput(inputId = "input_variables",
                label = "Variable Name",
                choices = "", multiple = TRUE))
dashboardPage(skin = "purple",
              # Application title
              dashboardHeader(title = 'IoT for Decision Making - Demo', 
                              titleWidth = 350),
              sidebar,
              dashboardBody(
                  fluidPage(
                      windowTitle = "Blue Sky IoT Project"),
                  br(),
                  tabsetPanel(
                    tabPanel(h3('Overview'),
                             br(),
                             fillRow(
                               column(width = 12,
                                      plotlyOutput("scatterPlot", height = "600px")))
                             ),
                    tabPanel(h3('Accumulation')
                             ),
                    tabPanel(h3('Water Balance')
                             ),
                    tabPanel(h3('Report'),
                             verticalLayout(
                             column(5,
                                    br(),
                                    # radioButtons('format_data', 'File format', c('csv', 'Excel'), inline = TRUE),
                                    downloadButton("csv", "Download Data")),
                             column(5,
                                    br(),
                                    # radioButtons('format', 'Document format', c('PDF', 'Word'), inline = TRUE),
                                    downloadButton("report", "Download Report")
                             )
                         )
                ),
                tabPanel(h3("debugging tab"),
                         # textOutput("df_AMN"),
                         # textOutput("df_days"),
                         # textOutput("crop_period"),
                         # textOutput("AMN.supply.rate"),
                         DT::dataTableOutput("db_selected"),
                         # DT::dataTableOutput("df_graph2")
                )
            ) # The end of the main tabset   
        )# The end of fluid page  
    )