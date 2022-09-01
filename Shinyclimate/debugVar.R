if(DEBUG == T){
	input <- list()
	input$mainvar <- "TotalBiomass"
	input$gcm <- "1"
	input$gcm2 <- "1"
	input$rcp <- "1"
	input$scn <- "1"
	input$ra_match_scales <- T
	input$ra_colours <- "sequential"
	dbcred <- P009_LucerneNZ
	pool <- dbPool(
		RPostgreSQL::PostgreSQL(), 
		dbname = dbcred$dbname,
		host = dbcred$host,
		user = dbcred$username,
		password = dbcred$password
	)
	factor_df <- pool %>% 
		tbl("Factor") %>% 
		collect()
	factors <- unique(factor_df$variable)
	pixel_df <- pool %>% 
		tbl("Pixel") %>% 
		collect()
	ref_df <- pool %>%
		tbl("Simulation") %>%
		filter(gcm_id %in% !!input$gcm) %>%
		filter(rcp_id %in% !!input$rcp) %>%
		filter(time_slice_id %in% !!input$scn) %>%
		filter(SoilStamp %in% "SandLoam") %>%
		filter(DefoliationCycle %in% "Long") %>%
		filter(DormancyRating %in% "FD5") %>%
		filter(IrrigationTreatment %in% "Rainfed") %>%
		# filter(Species %in% "Maize silage") %>%
		dplyr::select(simulation_id,pixel_id,one_of(!!input$mainvar)) %>%
		# dplyr::select(simulation_id,pixel_id,one_of(!!input$mainvar,!!factors)) %>%
		collect() %>%
		left_join(pixel_df)
	
	alt_df <- pool %>%
		tbl("Simulation") %>%
		filter(gcm_id %in% !!input$gcm2) %>%
		filter(rcp_id %in% !!input$rcp) %>%
		filter(time_slice_id %in% !!input$scn) %>%
		dplyr::select(simulation_id,pixel_id,one_of(!!input$mainvar)) %>%
		# dplyr::select(simulation_id,pixel_id,one_of(!!input$mainvar,!!factors)) %>%
		collect() %>%
		left_join(pixel_df)
}
