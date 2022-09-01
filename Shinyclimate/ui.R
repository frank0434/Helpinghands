source("globals.R")
source("functions.R")

ui <- fluidPage(
	theme = "style.css",
	useShinyjs(),
	
	# loading indicator
	div(
		id="loading-page",
		span(id="loading-box",
				 h2("Loading..."),
				 img(src = "ajax-loader-bar.gif", class = "spinner")
		)
	),
	
#	headerPanel('Discovery Science | Spatial Analysis | Crops'),
	headerPanel('The ATLAS framework | Assessment Tool for Landscape-scale Agricultural Systems'),
	p(),
	
	# Side panel details
	sidebarPanel(
		width = 3,
		selectInput('project',label = "Project", choices = c(project_list)),
		tags$hr(),
		
		selectInput('mainvar', 'Select the output variable:', choices= c(""), selected = 1),
		textOutput("txt_unit"), 
		tags$hr(),
		actionButton("updateScenarios", "Update scenarios", style="align: center;background-color: orange;position:-webkit-sticky;position:sticky;top:0px;  width:inherit; width: 100%; z-index: 999; height: 50px; font-size: 150%"),
		h4(tags$b("Construct scenarios:")),
		fluidRow(
			column(5,
						 h4(tags$b("Reference")),
						 uiOutput("ui_factor_dropdowns_ref")
			),
			column(
				width = 5,
				h4(tags$b("Alternative")),
				uiOutput("ui_factor_dropdowns_alt")
			),
			column(
				width = 2,
				uiOutput("ui_factors_same")
			)
		),
		# download controls
		tags$hr(),
		h4(tags$b("Download selected data")),
		radioButtons("fileToDownload", "Select output:", inline = TRUE,
								 c("Reference" = "dl_ref",
								 	"Alternative" = "dl_alt",
								 	"Difference" = "dl_dif")
		),
		p(),
		radioButtons("fileType", "Select output format:", inline = TRUE,
								 c("Text" = "txt","GeoTiff" = "tif")
		),
		downloadButton("downloadData", "Download data"),
		
		tags$hr(),
		verbatimTextOutput("debug1"), # indicate db connection
		verbatimTextOutput("debug2", placeholder = T), # dim(reference_df)
		verbatimTextOutput("debug3", placeholder = T) # dim(alternative_df)
	),
	
	# Main panel details
	mainPanel(
		
		# First tab
		tabsetPanel(id = "analysisTabs",
								
								# tab - Spatial analysis
								tabPanel("Regional analysis",
												 br(),
												 
												 # map controls
												 fluidRow(
												 	column(
												 		width = 2,
												 		radioButtons("stats", "Choose statistics:", c("Average" = "av", "Inter-annual variability (CV%)" = "cv",
												 																									"Median" = "mean"))
												 	),
												 	column(
												 		width = 2,
												 		radioButtons("comp", label = "Comparison method", c("Absolute" = "abs",
												 																												"Relative (%)" = "rel"))
												 	),
												 	column(
												 		width = 2,
												 		radioButtons("boundaries", label = "Map Boundaries", c("None" = "none",
												 																													 "Catchment" = "catch",
												 																													 "Region" = "ta"))
												 	),
												 	column(
												 		width = 3,
												 		sliderInput("opacity", "Raster transparency", min = 0, max = 1, value = 0.5)
												 	),
												 	column(
												 		width = 3,
												 		selectInput("ra_colours", "Colour Palette", list("Sequential"="sequential",
												 																										 "Diverging"="diverging"),
												 								selected = 1),
												 		checkboxInput("ra_match_scales", "Match Colour Scales:", T)
												 	)
												 ),
												 
												 # maps and plot
												 div(
												 div(fluidRow(
												 	column(
												 		width = 6,
												 		h4(tags$b("Reference scenario")),
												 		leafletOutput("basemap_ra_ref", width = "100%")
												 	),
												 	column(
												 		width = 6,
												 		h4(tags$b("Alternative scenario")),
												 		leafletOutput("basemap_ra_alt", width = "100%")
												 	)
												 ),
												 fluidRow(
												 	column(
												 		width = 6,
												 		h4(tags$b("Differences between selected scenarios")),
												 		leafletOutput("basemap_ra_diff")
												 	),
												 	column(
												 		width = 6,
												 		h4(tags$b("Frequency distribution of values across all grid-cells")),
												 		plotOutput("plt_ra_freq_dist")
												 	)
												 ),style="position:-webkit-sticky;position:sticky;top:0px"),
												 style="height:1080px")
								),
								
								# tab - Grid cell analysis FIXME: to be moved to Difference map analysis
								tabPanel("Grid-cell analysis", 
												 selectInput("ga_colours", "Colour Palette", list("Sequential"="sequential",
												 																								 "Diverging"="diverging"),
												 						selected = 1),
												 br(),
												 
												 h4(tags$b(textOutput("txt_gcCoord")),align = "center"),
												 tags$hr(),
												 h4(tags$b("Click in the location of interest")),
												 leafletOutput("basemap_ga"),
												 tags$hr(),
												 h4(tags$b("Inter-annual variability in selected pixel")),
												 
												 plotOutput("plt_ga_varibility")
								),
								
								
								# tab - Factor analysis
								tabPanel("Graphic analysis",
												 br(),
												 
												 selectInput('xcol', 'Select driving variable (X axes)', "", selected =  ""),
												 h4(tags$b("Relationship between selected variables")),
												 plotOutput("plt_fa_varRelationship"), 
												 tags$hr(),
												 fluidRow(
												 	column(
												 		width = 6,
												 		h4(tags$b("Distribution of X-axes values")),
												 		plotOutput("plt_fa_distX")
												 	),
												 	column(
												 		width = 6,
												 		h4(tags$b("Distribution of Y-axes values")),
												 		plotOutput("plt_fa_distY")
												 	)
												 )
								),
								
								# tab - data table tab
								tabPanel("Tabular Analysis",
												 br(),
												 selectInput("cmbOutputTable",
												 						"Select result dataset",
												 						choices =
												 							list(Reference="dl_ref",
												 									 Alternative="dl_alt",
												 									 Difference="dl_dif")
												 ),
												 dataTableOutput("outputTable")
								),
								tabPanel("Parameters",
												 downloadButton("downloadSelectedPRM", "Download Selected"),
												 leafletOutput("parameter_map"),
												 tags$style(type = "text/css", "#parameter_map {height: calc(100vh - 160px) !important;}")
												 
												 
												 
								)
		)
	)
)