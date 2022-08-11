
# Model 1 ----------------------------------------------------------------


#' model_richness
#' @description use the fitted coeficients from Jochum et al 2017 paper.
#' 
#' @param data the input data frame
#' @param fig1.coefs the coef data frame. three columns, first one is the group,
#'  the second column is the predictor,
#'  the third column is the coefs.
#' @param name_predictor name of the column in coef
#' @param name_group name of the column in data 
#'
#' @return a richness data frame with site name and integer value
#' @import 
#'  tidyr 
#'  dplyr
#'  magrittr
#'
#' @export
#'
#' @examples
model_richness <- function(data, Spp.Rich.coeff, site_name, 
                           name_predictor = "predictor",
                           name_group = "extrap_richness_response_group"){
  
 richness <- data %>% 
    # select the correct predictor
    # select(Spp.Rich.coeff[[name_predictor]]) %>% 
    # give each combination an unique id
    mutate(Site_Code = site_name) %>% 
    # transform to long format for calculation
    pivot_longer(!Site_Code, names_to = "predictor") %>% 
    # join the coefs
    right_join(Spp.Rich.coeff) %>% 
    # get the product of value and its coef
    mutate(prod = value * coef) %>% 
    # group by the combination of predicators
    group_by(Site_Code)  %>% 
    # get the product for each combination and round to integer
    summarise(richness = round(sum(prod), digits = 0)) %>%
    # remove negative values
    mutate(richness = if_else(richness < 0, 0, richness))
  return(richness)
}

#' model_richness_fun
#' @description simple version model 1 for run sensitivity 
#'
#' @param dt the matrix yielded from sobol_matrices
#' @param dat the data with litter mass and stoichiometry.
#'
#' @return
#' @export
#'
#' @examples
model_richness_fun <- function(dt, dat){
  apply(dt, 1, function(x){
    dt <- as.data.table(x, keep.rownames = TRUE)
    dt <- merge.data.table(dat, dt, by.x = "params", by.y = "rn")
    dt[, prod:=value*x]
    richness <- sum(dt$prod)
    ifelse(richness < 0, 0, richness)
    
  }, simplify = TRUE)
}

#' fill_mat
#' @description fill the matrix with random values based on the true distribution
#' 
#' @param mat 
#'
#' @return
#' @export
#'
#' @examples
fill_mat <- function(mat, distribution){
  
  for( i in 1:ncol(mat)){
    mat[, i] <- rnorm(mat[, i], 
                      distribution$mean[i],
                      abs(distribution$sd[i]))
  }
  return(mat)
  
}


# Model 2 -----------------------------------------------------------------

#' decomposition
#' @description the equation from Wall et al 2008 paper. 
#'  This one eats richness prediction from the Jochum et al 2017 richness 
#'  predictions and climate pattern index to predict litter decomposition rate.
#'
#' @param AllCDIs data.frame. must share a column with the taxonomic.richness
#' data.frame in order to perform joining.
#' @param taxonomic.richness data.frame. must share a column with the AllCDIs
#' data.frame in order to perform joining.
#' @param name_site column name for the site
#'
#' @return
#' @export
#'
#' @examples
decomposition <- function(taxonomic.richness = richness,
                          AllCDIs = data_CDI,
                          name_site = "Site_Code"){
  
  # taxonomic.richness <- 5 # List of values from each site comes from Model 1
  # AllCDIs <- read.csv(here::here("Data/LIDET-CDI.csv")) #Weather data, this is poor practice!
  
  
  ##################
  #Model
  richness_CDI <- merge(taxonomic.richness, AllCDIs, by = name_site)
  for(i in 1:nrow(richness_CDI))
  {
    log.k.value <- 0.127 * (richness_CDI[["richness"]][i]) + 0.6299 * log(richness_CDI[["CDI"]][i]) - 5.755 #calculation
    k.value <- exp(log.k.value) #back transform
    # Store the value
    richness_CDI$decompose_rate[i] <- k.value
    print(paste0(richness_CDI[[name_site]][i]," k-value is ",k.value)) #Do some sort of output, just for testing
  }
  return(richness_CDI)
  
  
}





# Model 3 -----------------------------------------------------------------

#' surfaceOM
#' 
#' @description surface organic matter model uses litter characteristics and the
#'   output from model 2 to predict the mass of litter through time
#'   
#' @param Weather data.frame. daily or monthly weather data. must have 
#' "maxt","mint", "radn","rain" and time in day count. 
#' @param decomposition data.frame. 
#' @param name_rate character. the column name has the decomposition rate
#' @param duration integer or null. the duration of experiments in days
#' @param name_site character. the column name has the site names 
#'
#' @return
#' @export
#'
#' @examples
#' 
surfaceOM <- function(Weather, decomposition, 
                      duration = 365L,
                      name_rate = "decompose_rate",
                      name_site = "Site_Code"){
  
  #############################
  # Model inputs

  Mass <- 100 # Mass of surface OM dry weight (kg/ha)
  CNRatio <- 40 # Overall C:N ratio of surface OM (unitless)
  Duration <- ifelse(is.null(duration), nrow(Weather), duration) # Duration of experiment (days)
  PotDecomp <- decomposition[[name_rate]] # Potential decomposition rate (/day)
  sites <- decomposition[[name_site]]
  # Weather <- read.csv("//lin-file/HOME$/cfljms/My Documents/Growing Futures/DHS/DataArchitecture/Model/SurfaceOM/Weather.csv") #Weather data, this is poor practice!
  
  #############################
  # Using weather data
  stopifnot(c("maxt","mint", "radn","rain") %in% colnames(Weather))
  MaxT <- Weather$maxt # Maximum temperature on a daily basis
  MinT <- Weather$mint # Minimum temperature on a daily basis
  Radn <- Weather$radn # Radiation on a daily basis
  Rain <- Weather$rain # Rain on a daily basis
  
 
  # create a list for storing the prediction for each site
  allsites <- vector("list", length = length(sites))
  names(allsites) <- sites
  #############################
  # Daily calculations
  for(site in sites){
    print(paste0("Estimate litter mass for Site ", site))
    # slice out the potential decomposition rate
    PotDecomp_site <- subset(decomposition, 
                             get(name_site) == site, 
                             select = get(name_rate),
                             drop = TRUE)
    #############################
    # Other model parameters DO NOT CHANGE
    Mass <- 100
    StandingFraction <- 0 # Scalar (0-1) as to whether surface OM is standing (1) and therefore inert, or lying (0) and in contact with soil surface
    SpecificArea <- 0.005 # Specific area of residue (ha/kg)
    ContactFactor <- 1 # To capture haystack effect when Surface OM piles up and not in contact with ground. See documentation for calculation of value
    surfaceom_cover <- 1.0 - exp(- SpecificArea * Mass) # Fraction of ground covered by all surface OM's
    
    #############################
    # Model constants DO NOT CHANGE
    
    OpTemp <- 20  # Temperature at which decomp reaches optimum rate (oC)
    max_crit_temp <- 35 # Max critical temperature
    min_crit_temp <- 5 # Min critical temperature
    
    CumEOSMax <- 20 #  Cumulative EOS at which decomposition rate becomes zero (mm H2O)
    A_to_evap_fact <- 0.44 # Used for residue cover calculation
    Albedo <- 0.23 # Soil roughness, used for PET calculations
    OpCN <- 25 # Optimum C:N for decomposition
    CNFactorCoeffecient <- 0.277 # For calculating CNFactor
    
    #############################
    # Set first row of table with initial values
    
    Time <- 0
    CumEOS <- 0
    
    Results <- data.frame(Site = site, Time=Time, Mass=Mass,
                          Loss=0, Temp=0, Moist=0, Decomp=0)
    
    for (Time in 1:Duration) {
      # Calculation of moisture factor
      wt_ave_temp <- (0.60 * MaxT[Time]) + (0.40 * MinT[Time])
      eeq <- Radn[Time] * 23.8846 * (0.000204 - 0.000183 * Albedo) * (wt_ave_temp + 29.0)
      soilwat2_eeq_fac <- ifelse(MaxT[Time] > max_crit_temp,((MaxT[Time] - max_crit_temp) * 0.05 + 1.1),
                                 ifelse(MaxT[Time] < min_crit_temp,(0.01 * exp(0.18 * (MaxT[Time] + 20.0))),1.1))
      eo <- eeq * soilwat2_eeq_fac
      eos <- eo * ((1.0 - surfaceom_cover)^A_to_evap_fact)
      CumEOS <- ifelse(Rain[Time]>4,(eos-Rain[Time]),CumEOS+eos-Rain[Time])
      CumEOS <- ifelse(CumEOS<0,0,CumEOS)
      MoistureFactor <- 1 - (CumEOS/CumEOSMax)
      MoistureFactor <- ifelse(MoistureFactor<0,0,MoistureFactor)
      
      # Calculation of temperature factor
      AverageDailyAirT <- (MaxT[Time]+MinT[Time])/2
      TemperatureFactor <- (AverageDailyAirT/OpTemp)^2
      
      CNFactor <- exp((-CNFactorCoeffecient*(CNRatio-OpCN))/OpCN) # Calculation of CN factor
      
      FDecomp <- PotDecomp * MoistureFactor * TemperatureFactor * CNFactor * ContactFactor # Daily decomp rate
      
      MassLoss <- Mass*FDecomp # Daily mass loss
      
      Mass <- Mass - MassLoss # Update mass
      
      # Do output
      temp <- c(site, Time, Mass, MassLoss, 
                TemperatureFactor, MoistureFactor, FDecomp)
      Results <- rbind(Results,temp)
      allsites[[site]] <- Results
    }
    
  }
  allsites <- do.call(rbind, allsites)
  return(allsites)
  }

#############################



# plot  -------------------------------------------------------------------

output_plots <- function(data, site){
  for(i in site){
     p <- data %>% 
    mutate(Mass = round(as.numeric(Mass)),
           Time = round(as.numeric(Time))) %>% 
    filter(Site == i) %>% 
    ggplot(aes(Time, Mass)) +
    geom_line() +
    theme_classic()
  ggsave(filename = paste0("Reports/", i, ".png"), plot = p, dpi = 300)
  }
 
}
