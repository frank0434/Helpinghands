# Author: Jian (AKA Frank) Liu
# Aim: Backend of the app
# Date: 2021-02-09



shinyServer(function(input, output) {
    
    # Create an reactive object for all the available options for the db
    input_dbcols <- reactive({
        db <- influx_query(con = con, 
                           db = input$input_db,
                           query = "SELECT * FROM file LIMIT 1", 
                           return_xts = FALSE, simplifyList = TRUE) %>% 
            data.table::as.data.table(., check.names = FALSE )
        cols <- colnames(db)[!colnames(db) %in% conventions]
        cols
    })
    # Create an reactive object for user selected input variables
    inputvars <- reactive({input$input_variables})
    # Retrieve data from user defined variables with time
    input_db <- reactive({
        cols <- paste0(inputvars(), collapse = "\",\"")
        fields <- paste0('time, \"' , cols, '\", host')
        db <- influx_select(con = con, 
                            db = input$input_db,
                            field_keys = fields,
                            measurement = "file",
                            return_xts = FALSE) %>% 
            data.table::as.data.table(.)
        db
    })

# Update the user selection for variable name input  ----------------------
    observe({
        updateSelectInput(inputId = "input_variables",
                          choices = input_dbcols())
    })
    
    

# graphing ----------------------------------------------------------------
    
    overview <- reactive({
        validate(
            need(!is.null(inputvars()), "Please select variable name(s)!!!")
        )
        # Construct the required cols
        cols <- c("time", gsub("\\s",".",inputvars()))
        
        # Melt the table to long format for plotting 
        dt <- input_db()[, ..cols]
        dt_long <- data.table::melt.data.table(dt, id.vars = "time")
        p <- dt_long %>% 
            ggplot(aes(x = time, y = value)) +
            geom_point()+
            geom_line()+
            theme_light() +
            facet_wrap(~variable, ncol = 1, scales = "free_y")
       p
    })




# output tables -----------------------------------------------------------
    output$db_selected <- DT::renderDataTable({input_db()})
    

# output graphs -----------------------------------------------------------

    output$scatterPlot <- renderPlotly({
        
        fig <- ggplotly(overview())
        fig
    })

# download stuff ----------------------------------------------------------
##### CSV for raw
    output$csv <- downloadHandler(
        
        
        filename = function() {
            "dataYouWant.csv"
        },
        
        content = function(file) {
            # the content
            cols <- c("time", gsub("\\s",".",inputvars()))
            
            # Melt the table to long format for plotting 
            dt <- input_db()[, ..cols]
            

            f <- file(file, "w")
            cat("Some metadata!\r\n", file = f)
            
            # cat(format(unitLine), "\r",file = f)
            write.table(dt, file = f, append = TRUE, quote = FALSE, sep = ",", 
                        eol = "\r", row.names=F)
            close(f)
            
            # }
        }
        
        
    )
##### Report for graphs
    output$report <- downloadHandler(
        
        # For PDF output, change this to "report.pdf"
        filename = function() { # https://shiny.rstudio.com/gallery/download-knitr-reports.html
            paste('your-report',  "pdf",sep = '.')
        },
        
        content = function(file) {
            params <- list(overall = overview())
            src <- normalizePath('report.Rmd')
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd',
                                     output_format = pdf_document(),
                                     params = params,
                                     envir = new.env(parent = globalenv())
                                     )
            file.rename(out, file)
            }
        )
    
})
