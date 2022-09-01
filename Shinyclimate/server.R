# CCII - MBIE prototype App (RA2)

# for debbuging
DEBUG <- F
if(DEBUG) source("debugVar.R")

server <- function(input, output, session) {
  hide(id = "loading-page")
  values <- reactiveValues()
  
  #######################
  # Database Connection #
  #######################
  
  # get DB cred
  dbcred <- reactive({
    validate(need(input$project != "","no project selected"))
    get(input$project)
  })
 
  # set up connection pool
  pool <- reactive({
    if(!is.null(poolConnection)){
      poolClose(poolConnection)
    }
    poolConnection <<- dbPool(
      RPostgreSQL::PostgreSQL(), 
      dbname = dbcred()$dbname,
      host = dbcred()$host,
      user = dbcred()$username,
      password = dbcred()$password)
    validate(need(try(dbIsValid(poolConnection)), "Database connection is not vaild"))
    return(poolConnection)
  })
  
  ##############################
  # Cache all the small tables #
  ##############################
  
  ### project table
  proj_df <- reactive({
    pj_df <- pool() %>% tbl("Project") %>% collect()
    if("lat" %in% names(pj_df)){
	    pj_df <- rename(pj_df,
	    								Lat = lat,
	    								Lon = lon)
    }
    return(pj_df)
		  	
  })
  project_name_check <- reactive({
    proj_df() %>% dplyr::select(name)
  })
  
  ### variable table
  var_df <- reactive({
    df <- pool() %>% tbl("Variable") %>% collect()
    df <- df[df$dbTable == "Simulation",]
    return(df)
  })
  var_list <- reactive({
    lst <- var_df()$variable
    names(lst) <- var_df()$fullName
    lst
  })
  
  ### scenario table
  rcp_df <- reactive({
    pool() %>% tbl("RCP") %>% collect()
  })
  rcp_list <- reactive({
    lst <- rcp_df()$rcp_id
    names(lst) <- rcp_df()$name
    lst
  })
  
  ### model table
  gcm_df <- reactive({
    pool() %>% tbl("GCM") %>% collect()
  })
  gcm_list <- reactive({
    lst <- gcm_df()$gcm_id
    names(lst) <- gcm_df()$name
    lst
  })
  
  ### time slice table
  timeslice_df <- reactive({
    pool() %>% tbl("TimeSlice") %>% collect()
  })
  timeslice_list <- reactive({
    lst <- timeslice_df()$time_slice_id
    names(lst) <- timeslice_df()$name
    lst
  })
  
  ## pixel table
  pixel_df <- reactive({
		pool() %>% tbl("Pixel") %>% collect()
  })
  
  ## factor table
  factor_df <- reactive({
    pool() %>% tbl("Factor") %>% collect()
  })
  
  ## combination table
  combination_df <- reactive({
  	pool() %>% tbl("Combination") %>% collect()
  })
  
  #
  regions <- reactive({
  	pool() %>% getRegionsAsSPDF()
  })
  
  ############################
  # update drop down choices #
  ############################
  
  observe({
    updateSelectInput(session, "mainvar", choices = c(var_list()), selected = var_list()[1])
  	settingInputNames <- build_factor_dropdowns(factor_df(), "ref", "1", pool())
    updateSelectInput(session, "xcol", choices = c(names(unlist(inputname_list_ref)),var_list()), selected = var_list()[1])
  })
  
  
  ##################################################
  # query simulation table and update reactive dfs #
  ##################################################
  
  ## list of factor inputs for reference dataset
  factor_inputs_ref <- function() {
    lst <- unlist(inputname_list_ref)
    for (i in seq(lst)) {
      lst[[i]] <- paste(input[[lst[[i]][1]]],collapse="','")
    }
    return(lst)
  }
  
  ## list of factor inputs for alternative dataset
  factor_inputs_alt <- function() {
    lst <- unlist(inputname_list_alt)
    for (i in seq(lst)) {
      lst[[i]] <- paste(input[[lst[[i]][1]]],collapse="','")
    }
    return(lst)
  }
  
  reference_df <- NULL
  alternative_df <- NULL
  rasterDF_Base <- NULL
  rasterDF_Alt <- NULL
  rasterDF_Diff <- NULL
  diff_rasterLayer <- NULL
  
  taGeoJson <- NULL
  catchmentGeoJson <- NULL
  
  selectedParameterMarkers <- list()
  
  observeEvent(input$project,{
  	taGeoJson <<- NULL
  	catchmentGeoJson <<- NULL
  	taGeoJson <<- getRegionsAsSPDF(pool(), "regionboundaries")
  	catchmentGeoJson <<- getRegionsAsSPDF(pool(), "catchmentboundaries")
  	
  })
  
  # Calculate dfs/rasters for RA panel
  observeEvent(input$updateScenarios,{
  	

  	if(input$analysisTabs %in% c("Regional analysis",
  															 "Grid-cell analysis")){
	  	reference_df <<- simulation_query(pool(),
	  																		input$mainvar,
	  																		pixel_df(),
	  																		factor_inputs_ref())
	  	if(anyNA(as.numeric(reference_df[[input$mainvar]]))){
	  		reference_df <<- NULL
	  	}
	  	if(!is.null(reference_df)){
	  		reference_df$ver <<- "Reference"
	  	}
	  	rasterDF_Base <<- rasterDF(reference_df,input$mainvar,statSelection(),
	  														 factor_inputs_ref())
	  	
	  	alternative_df <<- simulation_query(pool(),
	  																			input$mainvar,
	  																			pixel_df(),
	  																			factor_inputs_alt())
	  	if(anyNA(as.numeric(alternative_df[[input$mainvar]]))){
	  		alternative_df <<- NULL
	  	}
	  	if(!is.null(alternative_df)){
	  		alternative_df$ver <<- "Alternative"
	  	}
	  	rasterDF_Alt <<- rasterDF(alternative_df,input$mainvar,statSelection(),
	  														factor_inputs_alt())
	  	
	  	plot_ref_map()
	  	plot_alt_map()
	  	
	  	if(!is.null(rasterDF_Base) && !is.null(rasterDF_Alt)){ 
	  		rasterDF_Diff <<- rasterDF_Diffcalc(rasterDF_Base,rasterDF_Alt,
	  																				input$comp)
	  		
	  		diff_rasterLayer <<- createRaster(rasterDF_Diff[complete.cases(rasterDF_Diff),])
	  		plot_diff_map()
	  	}

  	}
  })
  
  #if the stats method is changed then the rasters need to be recalculated
  #and replotted
  observeEvent(input$stats,{
  
	  rasterDF_Base <<- rasterDF(reference_df,input$mainvar,statSelection(),
	  													 factor_inputs_ref())
  	rasterDF_Alt <<- rasterDF(alternative_df,input$mainvar,statSelection(),
  														factor_inputs_alt())
  	
  	plot_ref_map()
  	plot_alt_map()
  	
  	if(!is.null(rasterDF_Base) && !is.null(rasterDF_Alt)){ 
  		rasterDF_Diff <<- rasterDF_Diffcalc(rasterDF_Base,rasterDF_Alt,
  																				input$comp)
  		diff_rasterLayer <<- createRaster(rasterDF_Diff)
  		plot_diff_map()
  	}
  	
  })

  factor_analy_df <- NULL

  #calculate dfs/rasters for the FA panel
  observeEvent({
  	input$xcol
  	input$updateScenarios
  	input$analysisTabs
  	input$mainvar
  },
  {
  	if(input$analysisTabs == "Graphic analysis"){
  		reference_df <<- simulation_query(pool(),
  																			input$mainvar,
  																			pixel_df(),
  																			factor_inputs_ref())
  		if(!is.null(reference_df)){
  			reference_df$ver <<- "Reference"
  		}

  		alternative_df <<- simulation_query(pool(),
  																				input$mainvar,
  																				pixel_df(),
  																				factor_inputs_alt())
  		if(!is.null(alternative_df)){
  			alternative_df$ver <<- "Alternative"
  		}
  		
  		
  		print(input$xcol)
	  	if(!is.null(reference_df) && input$xcol != ""){
	  		ref_df <<- simulation_query(pool(),
	  															 input$xcol,
	  															 pixel_df(),
	  															 factor_inputs_ref(),
	  															 T) %>%
	  			
	  			left_join(reference_df)
	  	}
	  	
	  	if(!is.null(alternative_df)){
	  		alt_df <<- simulation_query(pool(),
	  																input$xcol,
	  																pixel_df(),
	  																factor_inputs_alt(),
	  																T) %>%
	  			left_join(alternative_df)
	  	}
	  	
	  	if(exists("ref_df") && exists("alt_df") && !is.null(ref_df) && !is.null(alt_df)){
	  		factor_analy_df <<- rbind(ref_df,alt_df)
	  		# factor_analy_df$ver <<- factor(factor_analy_df$ver,
	  		# 											 levels = c('Reference','Alternative'),ordered = TRUE)
  
	  	}
  	}
  })
  
  # -------------- Reactive expressions to filter data of BASE raster ------------------
  
  # flags to show which factors are being compared
  flagFact <- reactive({ # aims to substiture most varNames

  return(c(NULL,NULL,NULL))
  
  })
  
  ## Units of selected variable
  varUnits2 <- reactive({
  	var_df()[var_df()$variable==input$xcol,]$unit
  })
  
  ## Units of selected variable
  varUnits <- reactive({
  	switch(input$stats,
  				 av = var_df()[var_df()$variable==input$mainvar,]$unit,
  				 cv = "CV%",
  				 mean = var_df()[var_df()$variable==input$mainvar,]$unit)
  	
  #ifelse(input$stats == "av",var_df()[var_df()$variable==input$mainvar,]$unit, "CV%")
  })
  
  varFullName2 <- reactive({
  	var_df()[var_df()$variable==input$xcol,"fullName"]
  })
  
  varFullName <- reactive({
  	var_df()[var_df()$variable==input$mainvar,"fullName"]
  })
  
  # select stats
  statSelection <- reactive({
  	switch(input$stats,
  				 av = 3,
  				 cv = 4,
  				 mean = 5)
		#ifelse(input$stats == "av", 3, 4) # FIXME: this is selected by hand now, make it smarter later
  })

  # Cluster analysis --------------------------------------------------
  
  # clusters <- reactive({
  #   kmeans(selectedData_Base(), input$clusters)
  # })
  # 
  # cluster_Alt <- reactive({
  #   kmeans(selectedData_Alt(), input$clusters)
  # })
  
  ############################
  # Spatial Pixel Dataframes #
  ############################

  

  
  ##################################
  # Create and Update Leaflet Maps #
  ##################################
  
  output$basemap_ra_ref <- renderLeaflet({
  	createMainMap(pixel_df())
  }) 
  output$basemap_ra_alt <- renderLeaflet({
  	createMainMap(pixel_df())
  })
  output$basemap_ra_diff <- renderLeaflet({
  	createMainMap(pixel_df())
  })
  output$basemap_ga <- renderLeaflet({
  	createMainMap(pixel_df())
  })
  output$parameter_map <- renderLeaflet({
  	createParameterMap(vcsnStations)
  }) 
  
  #update maps if the display inputs change
  observeEvent({
  	input$ra_match_scales
  	input$ra_colours
  	input$opacity
  	input$boundaries
  	input$mainvar
  },{
  	plot_ref_map()
  	plot_alt_map()
  	plot_diff_map()
  })
  
  #update diff map if the comp mode changes
  observeEvent(input$comp,{
  	if(!is.null(rasterDF_Base) && !is.null(rasterDF_Alt)){ 
  		rasterDF_Diff <<- rasterDF_Diffcalc(rasterDF_Base,rasterDF_Alt,
  																				input$comp)
  		diff_rasterLayer <<- createRaster(rasterDF_Diff)
  		plot_diff_map()
  	}
  })
  
  # raster base
	plot_ref_map <- function(){
		
		
  	if(!is.null(rasterDF_Base)){
  		pal <-  map_colour_palette(rasterDF_Base, rasterDF_Alt, input$ra_match_scales, input$ra_colours)
  		valRasters <- rasterDF_Base$thisVar
  		if(input$ra_match_scales)
  			valRasters <- c(rasterDF_Base$thisVar, rasterDF_Alt$thisVar)
	    plot_map("basemap_ra_ref", rasterDF_Base, valRasters, input$opacity, input$boundaries, varUnits(),pal = pal)
  	}
    
  }
  
	# raster alternative
	plot_alt_map <- function(){
		
		if(!is.null(rasterDF_Alt)){
			pal <-  map_colour_palette(rasterDF_Alt, rasterDF_Base, input$ra_match_scales, input$ra_colours)
			valRasters <- rasterDF_Alt$thisVar
			if(input$ra_match_scales)
				valRasters <- c(rasterDF_Base$thisVar, rasterDF_Alt$thisVar)
			plot_map("basemap_ra_alt", rasterDF_Alt, valRasters, input$opacity, input$boundaries, varUnits(),pal = pal)
		}
		
	}
  
  # add raster difference
  plot_diff_map <- function(){

  	if(!is.null(diff_rasterLayer)){
  		pal <- colorNumeric(getGraphColours(input$ra_colours),
  												values(isolate(diff_rasterLayer)), na.color = "transparent")
  		
  		thisTitle <- ifelse((input$comp == "rel"| statSelection() == 4), "(%)", varUnits()) # FIXME: the use of int for statSel is not intuitive
  		
  		plot_map("basemap_ra_diff", rasterDF_Diff, values(diff_rasterLayer), input$opacity, input$boundaries, thisTitle,pal = pal)
  	}
  }
  
  
  #adds markers for selected locations. updates selectedParameterMarkers global variable
  observeEvent(input$parameter_map_draw_all_features,{
  	selectedParameterMarkers <- list()
  	input$parameter_map_draw_all_features
  	
  	for(feature in input$parameter_map_draw_all_features$features){
  	
	  	found_in_bounds <- findLocations(shape = feature
	  																	 , location_coordinates = vcsnCoordinates
	  																	 , location_id_colname = "AGENT_NO")
	  	
	  	found_in_bounds <- found_in_bounds[!is.na(found_in_bounds)]
	  	
	  	for(id in found_in_bounds){
	  		if(id %in% selectedParameterMarkers$clickedMarker){
	  			# don't add id
	  		} else {
	  			# add id
	  			selectedParameterMarkers$clickedMarker <-append(selectedParameterMarkers$clickedMarker, id, 0)
	  		}
	  	}
  	}

  	selected <- vcsnCoordinates[vcsnCoordinates$AGENT_NO %in% selectedParameterMarkers$clickedMarker,]
  	selectedParameterMarkers <<- selectedParameterMarkers
  	proxy <- leafletProxy("parameter_map")
  	proxy %>% clearMarkers()
  	proxy %>% addMarkers(lat = selected$LAT,
  											 lng = selected$LONGT)
  	
  })

  output$downloadSelectedPRM <- downloadHandler("PRM_GridCellsMet.txt", 
  																							content = function(file){
  																								if(!is.null(selectedParameterMarkers$clickedMarker)){
	  																								write.table(cbind(selectedParameterMarkers$clickedMarker,"Yes"),
	  																														file,col.names = c("agent_no", "include"), 
	  																														row.names = F, quote = F)
  																								}else{
  																									write.table(c("agent_no", "include"),file,
  																															row.names=F,col.names = F,
  																															quote = F)
  																								}
  																							})
  # add raster to base 4 (FIXME: temporary test)
  observe({
  	#  pal <- colorNumeric(c("#ffffe5", "#fff7bc", "#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"),
  	#                      values(diff_rasterLayer), na.color = "transparent")
  	
  	input$updateScenarios
  	input$analysisTabs
  	
  	if(!is.null(diff_rasterLayer) && !is.null(rasterDF_Base)){
  		
  		pal <- colorNumeric(getGraphColours(input$ga_colours),
  												values(diff_rasterLayer), na.color = "transparent")
  		thisTitle <- ifelse((input$comp == "rel"| statSelection() == 4), "(%)", varUnits()) # FIXME: the use of int for statSel is not intuitive
  		
  		plot_map("basemap_ga", rasterDF_Diff, values(diff_rasterLayer), input$opacity, input$boundaries, thisTitle,pal = pal)

  	
  		if(!is.null(input$basemap_ga_click$lng) && !is.null(input$basemap_ga_click$lat)){
	  		leafletProxy("basemap_ga") %>%
	  			addMarkers(input$basemap_ga_click$lng,input$basemap_ga_click$lat) 
  		}
  	}
  })
  
  observe({
  	
	
    leafletProxy("basemap_ra_ref") %>% clearPopups()
    event <- input$basemap_ra_ref_click
    if (is.null(event))
      return()
    
    print(event)
    
    pixel <- getPixelFromClick(input$basemap_ra_ref_click, proj_df(), pixel_df())
    if(is.na(pixel))
    	return()
    if(!is.null(reference_df)){
	    r <- reference_df
	    r <- r[,c("Lat","Lon",isolate(input$mainvar))]
	    r <- r[r$Lat==pixel[1] & r$Lon == pixel[2],3]
	    if(nrow(r) > 0){
	    	r <- collect(r)[[1]]
	    	nYears <- length(r)
	    }else{
	    	r <- NA
	    	nYears <- NA
	    }
    }else{
    	r <- NA
    	nYears <- NA
    }
	    
	    
    firstText <- ""
    if(statSelection() == 3){
      averageVal <- round(mean(r),2)
      firstText <- paste0("Average: ", averageVal, "<br>")
    }else if(statSelection() == 4){
    	cvVal <- cvFunc(r)
    	firstText <- paste0("CV: ", cvVal, "<br>")
    }else if(statSelection() == 5){
    	medianVal <- median(r)
    	firstText <- paste0("Median: ", medianVal, "<br>")
    }
    
    
    p10 <- quantile(r,.1,na.rm=T)
    p90 <- quantile(r,.9,na.rm=T)
    
    
    
    isolate({
      showPopup(event$lat, event$lng, paste0(firstText,
                                             #"Median: ", medianVal, "<br>",
                                             "No. Years: ", nYears, "<br>",
                                             "P10: ", p10, "<br>",
                                             "P90: ", p90, "<br>"),
                "basemap_ra_ref")
    })

  })
  
  observe({
  	
    leafletProxy("basemap_ra_alt") %>% clearPopups()
    event <- input$basemap_ra_alt_click
    if (is.null(event))
      return()
    
    print(event)
    
    pixel <- getPixelFromClick(input$basemap_ra_alt_click, proj_df(), pixel_df())
    if(is.na(pixel))
    	return()
    
    if(!is.null(alternative_df)){
	    r <- alternative_df
	    r <- r[,c("Lat","Lon",isolate(input$mainvar))]
	    r <- r[r$Lat==pixel[1] & r$Lon == pixel[2],3]
	    if(nrow(r) > 0){
		    r <- collect(r)[[1]]
		    nYears <- length(r)
	    }else{
	    	r <- NA
	    	nYears <- NA
	    }
    }else{
    	r <- NA
    	nYears <- NA
    }
    
    
    
    firstText <- ""
    if(statSelection() == 3){
    	averageVal <- round(mean(r),2)
    	firstText <- paste0("Average: ", averageVal, "<br>")
    }else if(statSelection() == 4){
    	cvVal <- cvFunc(r)
    	firstText <- paste0("CV: ", cvVal, "<br>")
    }else if(statSelection() == 5){
    	medianVal <- median(r)
    	firstText <- paste0("Median: ", medianVal, "<br>")
    }
    
    p10 <- quantile(r,.1,na.rm=T)
    p90 <- quantile(r,.9,na.rm=T)
    
    
    
    isolate({
      showPopup(event$lat, event$lng, paste0(firstText,
                                             #"Median: ", medianVal, "<br>",
                                             "No. Years: ", nYears, "<br>",
                                             "P10: ", p10, "<br>",
                                             "P90: ", p90, "<br>"),
                "basemap_ra_alt")
    })
  })
  
  observe({
  	
  	leafletProxy("basemap_ra_diff") %>% clearPopups()
  	event <- input$basemap_ra_diff_click
  	if (is.null(event))
  		return()
  	
  	print(event)
  	
  	pixel <- getPixelFromClick(input$basemap_ra_diff_click, proj_df(), pixel_df())
  	if(is.na(pixel))
  		return()
  	
  	if(!is.null(alternative_df) & !is.null(reference_df)){
  		
  		
  		
  		if("year" %in% colnames(alternative_df)){
  			yearCol <- "year"
  		}else{
  			yearCol <- "Year"
  		}
  		
  		r_ref <- reference_df[reference_df$Lat==pixel[1] & reference_df$Lon == pixel[2],
  													c(yearCol,"Lat","Lon",isolate(input$mainvar))]
  		
  		r_alt <- alternative_df[alternative_df$Lat==pixel[1] & alternative_df$Lon == pixel[2],
  													c(yearCol,"Lat","Lon",isolate(input$mainvar))]
  		
  		r <- merge(r_ref,
  							 r_alt,
  							 by = c("Lat","Lon", yearCol), all=F, suffixes=c("_ref","_alt"))
  		r$diff <- r[[paste0(input$mainvar,"_alt")]] - r[[paste0(input$mainvar,"_ref")]]
  		r <- r[,c("Lat","Lon", "diff")]

  		if(nrow(r) > 0){
  			r <- collect(r)$diff
  			nYears <- length(r)
  		}else{
  			r <- NA
  			nYears <- NA
  		}
  	}else{
  		r <- NA
  		nYears <- NA
  	}
  	
  	
  	
  	if(statSelection() == 3){
  		averageVal <- round(mean(r),1)
  		firstText <- paste0("Average: ", averageVal, "<br>")
  	}else{
  		cvVal <- cvFunc(r)
  		firstText <- paste0("CV: ", cvVal, "<br>")
  	}
  	medianVal <- round(median(r),1)
  	p10 <- round(quantile(r,.1,na.rm=T),1)
  	p90 <- round(quantile(r,.9,na.rm=T),1)
  	
  	
  	
  	isolate({
  		showPopup(event$lat, event$lng, paste0(firstText,
  																					 "Median: ", medianVal, "<br>",
  																					 "No. Years: ", nYears, "<br>",
  																					 "P10: ", p10, "<br>",
  																					 "P90: ", p90, "<br>"),
  							"basemap_ra_diff")
  	})
  })
  

  output$debug1 <- renderPrint({
    validate(
      need(input$project != "", "Please select project"),
      need(try(dbIsValid(pool())), "Database connection is not vaild"),
      need(try(project_name_check()[[1]] == input$project), "Database connection error")
    )
    project_name_check()[[1]]
    # dbGetInfo(pool)$valid
    ###FIXME: add query to check name of DB is what it seems...as in we are actually in the correct db (create table of projname in db????)
  })

  ##############
  # Output txt #
  ##############
  
  output$debug2 <- renderPrint({
  	input$updateScenarios
  	dim(reference_df)
  })
  output$debug3 <- renderPrint({
  	
  	input$updateScenarios
  	dim(alternative_df)
  })

  # observeEvent(input$updateScenarios,{
  # 	output$debug2 <- html(id = "output$debug2", html = dim(reference_df))
  # 	output$debug3 <- html(id = "output$debug3", html = dim(alternative_df))
  # })



  # Show unit of main var
  output$txt_unit <- renderText({ 
    paste0("Variable unit is: ",varUnits())
  })  
  
  
  # Show selected coordinates on grid analysis
  output$txt_gcCoord <- renderText({ 
  	if(is.null(input$basemap_ga_click))
  		return(paste0("No grid-cell selected. Please click on a grid-cell below"))
  	coord <- getPixelFromClick(input$basemap_ga_click, proj_df(), pixel_df())
  	if(is.na(coord))
  		return()
    paste0("Selected grid-cell has Latitude: ", coord[1]," Longitude: " ,coord[2])
    
  }) 
  
  mainVarEvent <- function(){
  	return(isolate(input$mainvar))
  }
  
  #################
  # Output Graphs #
  #################

  # diff of rasters
  output$plt_ra_freq_dist <- renderPlot({

  	event <- input$updateScenarios
  	print("inside plt_ra_freq_dist")

  	if(!is.null(reference_df) && !is.null(alternative_df)
  		 && !anyNA(as.numeric(reference_df[[input$mainvar]]))
  		 					&& !anyNA(as.numeric(alternative_df[[input$mainvar]]))){
  		
	    df_merge <- rbind(reference_df,alternative_df)
	    stat <- statSelection()
	    
	    df_merge <- df_merge[,c("Lat","Lon", input$mainvar, "ver")]

	    if(stat == 4){
	    	df_merge <- df_merge %>%
	    		dplyr::select(Lat,Lon,input$mainvar,ver) %>%
	    		group_by(Lat, Lon, ver) %>%
	    		summarise_all(funs(cvFunc)) %>%
	    		dplyr::select(Lat, Lon, input$mainvar,ver)
	    	axisLabel = "Inter-annual variability"
	    }
	    	
    	df_merge %>%
    		ggplot(aes_string(input$mainvar), aes(..count..)) +
    		geom_density(aes(colour = as.factor(ver),fill = as.factor(ver)), size = 2, alpha = 0.5) +
    		theme(legend.position = c(.1, .8),text = element_text(size=20)) +
    		theme(legend.title=element_blank()) +
    		scale_x_continuous(name=paste0(varFullName()," (",as.character(varUnits()),")"))
    	
  	}else{
  		ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100)
  	}


  })

  # within selected single pixel data
  output$plt_ga_varibility <- renderPlot({

  	if(is.null(input$basemap_ga_click)){
  		ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100)
  	}else{
	  	event <- input$updateScenarios
	  	
	  	if(!is.null(reference_df) && !is.null(alternative_df)){
	  	
	    gaPixel <- getPixelFromClick(input$basemap_ga_click, proj_df(), pixel_df())
	    if(is.na(gaPixel))
	    	return()
	
	    altData <- alternative_df %>%
	      filter(Lat == gaPixel[1] & Lon == gaPixel[2]) %>%
	      dplyr::select(one_of(isolate(input$mainvar))) %>%
	      mutate(ver = "Alternative")
	
	    refData <- reference_df %>%
	      filter(Lat == gaPixel[1] & Lon == gaPixel[2]) %>%
	      dplyr::select(one_of(isolate(input$mainvar))) %>%
	      mutate(ver = "Reference")
	
	    df_merge <- rbind(refData,altData)
	
	    df_merge %>%
	      ggplot(aes_string(isolate(input$mainvar)), aes(..count..)) +
	      geom_density(aes(colour = as.factor(ver),fill = as.factor(ver)), size = 2, alpha = 0.5) +
	   #   geom_boxplot(aes(colour = as.factor(ver),fill = as.factor(ver)), size = 2, alpha = 0.5) +
	      theme(legend.position = c(.1, .8),text = element_text(size=20)) +
	      theme(legend.title=element_blank()) +
	      # scale_colour_brewer(name = "Scenario", ) +
	      # ggtitle(as.character(input$mainvar)) +
	      scale_x_continuous(name=paste0(as.character(varFullName())," (",as.character(varUnits()),")"))
	    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
	  	}
  	}
  })

  # X and Y comparison of factors
  output$plt_fa_varRelationship <- renderPlot({
  	
  	event <- input$updateScenarios
  	
  	xcolName <- input$xcol
  	if(!is.null(factor_analy_df)){
	  	if(input$xcol %in% names(unlist(inputname_list_ref))){
	  		xLabel <- ""
	  		if(input$xcol == "RCP"){
	  			xcolName <- "rcp_id"
	  			factor_analy_df[[xcolName]] <- names(rcp_list())[factor_analy_df[[xcolName]]]
	  			xLabel <- "RCP"
	  		}
	  		if(input$xcol == "GCM"){
	  			xcolName <- "gcm_id"
	  			factor_analy_df[[xcolName]] <- names(gcm_list())[factor_analy_df[[xcolName]]]
	  			xLabel <- "GCM"
	  		}
	  		if(input$xcol == "TimeSlice"){
	  			xcolName <- "time_slice_id"
	  			factor_analy_df[[xcolName]] <- names(timeslice_list())[factor_analy_df[[xcolName]]]
	  			xLabel <- "Time Slice"
	  		}
	  		factor_analy_df$ver <- factor(factor_analy_df$ver,
	  					 													levels = c('Reference','Alternative'),ordered = TRUE)
	  		factor_analy_df %>%
	  			ggplot(aes(x=factor_analy_df[[xcolName]], y=factor_analy_df[[input$mainvar]], fill=factor_analy_df$ver)) +
	  			geom_boxplot(alpha= 0.5) +
	  			theme(legend.title=element_blank()) +
	  			theme(plot.margin=unit(c(1,1,1,0.5),"cm"))+
	  			theme(legend.position = c(0, -.18),text = element_text(size=20))  +
	  			theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
	  			scale_x_discrete(name=paste0(as.character(xLabel))) +
	  			scale_y_continuous(name=paste0(as.character(varFullName()))) +
	  			scale_fill_discrete(breaks=c("Alternative","Reference"), h = c(195,15))
	  		
	  	}else{
  	
  	
		    factor_analy_df %>%
		      ggplot(aes_string(x=xcolName, y=input$mainvar)) +
		      geom_point(aes(colour = as.factor(ver)), size = 3) +
		      geom_smooth(aes(colour = as.factor(ver)))+
		      theme(legend.title=element_blank()) +
		      theme(legend.position = c(.1, .8),text = element_text(size=20))  +
		      scale_x_continuous(name=paste0(as.character(varFullName2())," (",as.character(varUnits2()),")")) +
		      scale_y_continuous(name=paste0(as.character(varFullName())," (",as.character(varUnits()),")"))
	  	}
  	}
  })

  # distribution of x-axes variable
  output$plt_fa_distX<- renderPlot({
  	event <- input$updateScenarios
  	
  	xcolName <- input$xcol
  	
  	if(!is.null(factor_analy_df)){
	  	if(input$xcol %in% names(unlist(inputname_list_ref))){
	  		xLabel <- input$xcol
	  		if(input$xcol == "RCP"){
	  			xcolName <- "rcp_id"
	  			xLabel <- "RCP"
	  		}
	  		if(input$xcol == "GCM"){
	  			xcolName <- "gcm_id"
	  			xLabel <- "GCM"
	  		}
	  		if(input$xcol == "TimeSlice"){
	  			xcolName <- "time_slice_id"
	  			xLabel <- "Time Slice"
	  		}
	  		factor_analy_df$ver <- factor(factor_analy_df$ver,
	  																	levels = c('Reference','Alternative'),ordered = TRUE)
	  		factor_analy_df %>%
	  			ggplot(aes_string(x="ver", y=input$mainvar)) +
	  			geom_boxplot(aes(fill = as.factor(ver)), alpha= 0.5) +
	  			theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
	  			theme(legend.title=element_blank()) +
	  			theme(legend.position = c(.1, .8),text = element_text(size=20))  +
	  			scale_x_discrete(name=paste0(as.character(xLabel))) +
	  			scale_y_continuous(name=paste0(as.character(varFullName()))) +
	  			scale_fill_discrete(breaks=c("Alternative","Reference"), h = c(195,15))
	  	}else{
	  	
	  	
		    factor_analy_df %>%
		      ggplot(aes_string(xcolName)) +
		      geom_density(aes( fill = as.factor(ver), colour = as.factor(ver)), alpha= 0.5) + # , fill = as.factor(ver),  alpha = 0.5
		      theme(legend.position = c(.1, .8),text = element_text(size=20)) +
		      theme(legend.title=element_blank()) +
		      #  ggtitle(as.character(input$xcol)) +
		      scale_x_continuous(name=paste0(as.character(varFullName2())," (",as.character(varUnits2()),")"))
	    	# theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
	  	}
  	}

  })

  # distribution of y-axes values
  output$plt_fa_distY <- renderPlot({
  	event <- input$updateScenarios
  	if(!is.null(factor_analy_df)){
	    factor_analy_df %>%
	      ggplot(aes_string(isolate(input$mainvar))) +
	      geom_density(aes(fill = as.factor(ver), colour = as.factor(ver)), alpha= 0.5) + # order of factors matter
	      theme(legend.title=element_blank()) +
	      theme(legend.position = c(.1, .8),text = element_text(size=20)) +
	      scale_x_continuous(name=paste0(as.character(isolate(varFullName()))," (",as.character(varUnits()),")"))
  	}
  })
  
  ######################
  # Output Dataset Tab #
  ######################
  
  output$outputTable <- renderDataTable({
    dataset <-
      switch(input$cmbOutputTable,
             "dl_ref" = rasterDF_Base,
             "dl_alt" = rasterDF_Alt,
             "dl_dif" = rasterDF_Diff)
    # data.frame(lat = dataset$Lat, 
    #            lon = dataset$Lon, 
    #            PixelValue = dataset$thisVar)
    dataset
  },
  server = TRUE,
  options = list(
    dom = 'frtip'
  ))
  
  ########################################
  # populate factor dropdown boxes in UI #
  ########################################
  
  #Output the intial UI controls
  output$ui_factor_dropdowns_ref <- renderUI({
    validate(need(try(nrow(factor_df())>0),"no project selected"))
  	factors <- build_factor_dropdowns(factor_df(), "ref", "1", pool())
  	div(id = "factor_combo_ref", factors)
  })
  
  #Keeps track of all the reference dynamic inputs. Triggers if any of
  #them change
  refInputChanged <- reactive({
  	factors <- ""
  	for(factor in inputname_list_ref){
  		paste0(factors,input[[factor]])
  	}
  	return(factors)
  })
  
  #Runs if any inputs change (including when this function changes its
  #own inputs)
  # observeEvent(refInputChanged(),{
  # 	limit_factor_dropdowns(input, inputname_list_ref, rcp_df(), session)
  # })
  
  #Output the intial UI controls
  output$ui_factor_dropdowns_alt <- renderUI({
    validate(need(try(nrow(factor_df())>0),"no project selected"))
  	factors <- build_factor_dropdowns(factor_df(), "alt", "2", pool())
  	div(id = "factor_combo_alt", factors)
  })
  
  output$ui_factors_same <- renderUI({
  	diffs <- list()
  	diffs[[1]] <- HTML("<h3>&nbsp</h3>")
  	for(i in 1:length(inputname_list_ref)){
  		htmlText <- "<h2>&nbsp</h2><h5>%s</h5>"
  		if(identical(input[[inputname_list_alt[[i]]]],
  								 input[[inputname_list_ref[[i]]]])){
  			htmlText <- sprintf(htmlText, "=")
  		}else{
  			htmlText <- sprintf(htmlText, "diff")
  		}
  		diffs[[i]] <- HTML(htmlText)
  	}
  	div(id = "factors_same", diffs)
  })
  
  #Keeps track of all the reference dynamic inputs. Triggers if any of
  #them change
  altInputChanged <- reactive({
  	factors <- ""
  	for(factor in inputname_list_alt){
  		paste0(factors,input[[factor]])
  	}
  	return(factors)
  })
  
  #Runs if any inputs change (including when this function changes its
  #own inputs)
  # observeEvent(altInputChanged(),{
  # 	limit_factor_dropdowns(input, inputname_list_alt, rcp_df(), session)
  # })
 
  #####################################
  # DOWNLOAD Feature on sidebar panel # FIXME: need file naming
  #####################################
  
  # select the output type (FIXME: why not do all in same txt file or zipped 3 raster for GeoTiff?)
  datasetInput <- reactive({
    switch(input$fileToDownload,
           "dl_ref" = rasterDF_Base,
           "dl_alt" = rasterDF_Alt,
           "dl_dif" = rasterDF_Diff)
  })
  
  
  # raster selection
  rasterInput <- reactive({
    switch(input$fileToDownload,
           "dl_ref" = createRaster(rasterDF_Base),
           "dl_alt" = createRaster(rasterDF_Alt),
           "dl_dif" = createRaster(rasterDF_Diff))
  })
  
  output$downloadData <- downloadHandler(
    
    # ext <- ifelse(input$fileType == "txt",".txt",".tif"),
    
    filename = function() { paste0(input$mainvar,"_(", 
                                   # input$rcp,"_", 
                                   # input$rcp2,")_(", 
                                   # input$gcm,"_", 
                                   # input$gcm2,")_(", 
                                   # input$scn,"_", 
                                   # input$scn2,")_",
                                   input$fileToDownload,"_(", 
                                   input$stats,")_(", 
                                   input$comp,")_",
                                   input$crop,".", 
                                   input$fileType) },
    
    content = function(file) {
      
      df <- data.frame(lat = datasetInput()$Lat, lon = datasetInput()$Lon, PixelValue = datasetInput()$thisVar)
      
      if(input$fileType == "txt") {
        
        thisHeader <- paste0("#",input$mainvar," ",varUnits()," ", as.character(statSelection()))
        # FIME: Add header with meta-data
        write.table(df, file, row.names=F)
        #  write.csv(df, file ,row.names=F)
        
      } else {
        
        # save as raster
        r <- rasterInput()
        proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        
        res <- writeRaster(r, file=tempfile(), format="GTiff", overwrite=TRUE)
        
        # Show the corresponding output filename
        print(res@file@name)
        
        # Rename it to the correct filename
        file.rename(res@file@name, file)
        
      }
    }
  )
}
