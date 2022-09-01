library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(plotly)
library(zip)

# plotting functions ------------------------------------------------------

base_plot <- function(df){
  p <- ggplot(df, aes(x = Name, y = Size)) +
    geom_col(fill = "blue",
             colour = "black") +
    theme_bw()
  p
}

plotly_p <- function(p){
  plotly_p <- ggplotly(p, 
                       tooltip = c("Name", "Size"),
                       dynamicTicks = "y")
  plotly_p
}

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    # upload file box
    fileInput(
      inputId = "file",
      label = "Select one or more .xlsx spreadsheet(s)",
      multiple = TRUE,
      accept = c(
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        ".xlsx")), # only allows .xlsx format files to be uploaded
    
    # radio buttons (default value of not visible)
    radioButtons("yaxisGrp2", "", c(" " = "1")),
    
    #download button
    downloadButton("report", "Create report(s)"),
    
    radioButtons("report_type", "Number of Files:",
                 c("single" = "single", "multiple" = "multiple"),
                 selected = "single"
    )
  )
  ),
  dashboardBody(
    plotlyOutput(
      "myplot", inline = TRUE)
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) { 
  
  cb_list <- reactiveValues(samples = NULL)
  
  #### Collect data details ####
  # List cases which can be ticked to be replaced with files (.xlsx)
  # selected by the user
  observe({
    data <- input$file
    dsnames2 <- data[, 1]
    cb_options <- list()
    cb_options[dsnames2] <- dsnames2
    updateRadioButtons(session,
                       "yaxisGrp2", # update radio buttons with values
                       label = " ",
                       choices = cb_options,
                       selected = cb_options[1]
    )
    
    cb_list$samples <- cb_options
  })
  
  
  # record file name into a variable
  input_data <- reactive({
    validate(
      need(input$file != "", "Choose one or more files (.xlsx)")
    )
    
    infile <- input$file
    
    if (is.null(infile)) {
      return(NULL)
    }
    
    infile
  })
  
  # monitor for a change in radio button selection
  chosen_file <- eventReactive(input$yaxisGrp2, {
    input_file <- input_data()
    reactiveValues(
      path = input_file$datapath[input_file$name == input$yaxisGrp2],
      name = input_file$name
    )
  })
  
  # load the selected files (.xlsx) based on selection
  mydata <- reactive({
    
    # check that the uploaded data is in the expected format and stop if it isnt
    temp <- read_excel(chosen_file()$path, sheet = 1)
    
    validate(
      need(
        names(temp)[1] == "Name",
        "I am an error"
      )
    )
    # Read-in each datatable as a different element of a list
    # data_list <- list(
    #   read_excel(chosen_file()$path)
    # )  
    data_list <- lapply(chosen_file()$path, function(x){
      df <- read_excel(x)
      df
    })
    return(data_list)
  })



  # make the base plot
  # do_the_plot <- reactive({
  #   ggplot(mydata()[[1]], aes(x = Name, y = Size)) +
  #     geom_col(fill = "blue",
  #              colour = "black")
  # })
  
  # use the plotting function to deal with different conditions
  do_the_plot <- reactive({
    base_plot(mydata()[[1]])
  })


  #change plot to plotly for interactive env
  # plot_for_shiny <- reactive({
  #   ggplotly(do_the_plot(),
  #            tooltip = c("Name", "Size"),
  #            dynamicTicks = "y")
  # 
  # })
  plot_for_shiny <- reactive({
    plotly_p(do_the_plot())
  })
  # 
  # #change theme elements for pdf only
  # # plot_for_report <- reactive({
  # #   do_the_plot() +
  # #     theme_bw()
  # # })
  
  output$myplot  <- renderPlotly({ 
    plot_for_shiny()
  })
  
  
  #output report
  output$report <-
    downloadHandler(
      filename = function() { # choose filename based on combination of radio buttons chosen
        #select substring of name for file naming
        sample_name <- substr(input$yaxisGrp2, 1, regexpr("([ ])", input$yaxisGrp2) - 1)
        
        if (input$report_type == "single") {
          filename <- paste(sample_name, "Report.pdf", sep = "_")
        } else {
          filename <- "output.zip"
        }
        
        return(filename)
      },
      content = function(file) { # choose content based on combination of radio buttons chosen
        filedir <- dirname(chosen_file()$path[1])
        tempReport <- normalizePath("example_report.Rmd")
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        # Copy the report file to a known directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        # render a single report
        if (input$report_type == "single") {
          
          sample_name <- substr(input$yaxisGrp2, 1, regexpr("([ ])", input$yaxisGrp2) - 1)
      
          file.copy("example_report.Rmd", tempReport, overwrite = TRUE)
          
          
          parameters <- list(
            filename = sample_name,
            myplot = plot_for_report()
          )
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport,
                            output_format = "pdf_document",
                            output_file = file,
                            params = parameters,
                            envir = new.env(parent = globalenv())
          )
          
          
          
        } else {  # if downloading multiple reports
          # fs <- c()
          # count how many files have been uploaded
          no.of_files <- nrow(input$file)
          # Allocate spaces for a character vector same length as the number of 
          # files uploaded. 
          fs <- vector(mode = "character", length = no.of_files)
          
          # Iterate through each file 
          for( i in seq_len(no.of_files)){
            # Extract output file name from input excel file name
            sample_name <- substr(input$file$name[i], 1, regexpr("([ ])", input$file$name[i]) - 1)
            # Assemble the pdf name
            fname <- paste0(sample_name, "_Report.pdf")
            # Assemble the rmd name for rendering 
            templateRmd <- paste0(sample_name, "_Report.Rmd")
            # Copy the example.Rmd to a new name
            file.copy(tempReport, templateRmd, overwrite = TRUE)
            
            # Read data from cache
            df <- read_excel(input$file$datapath[i])
   
            parameters <- list(
              filename = sample_name,
              myplot = base_plot(df)
            )
            
            rmarkdown::render(templateRmd,
                              output_format = "pdf_document",
                              output_file = fname,
                              params = parameters,
                              envir = new.env(parent = globalenv()))
            # Store the filename into the vector 
            
            fs[i] <-  fname
          }
          print(fs)
          zip(file, fs)
          
        }
        
      })
}


shiny::shinyApp(ui = ui, server = server)