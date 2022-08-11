#############################
# Model inputs 

Mass <- 100 # Mass of surface OM dry weight (kg/ha)
CNRatio <- 40 # Overall C:N ratio of surface OM (unitless)
Duration <- 365 # Duration of experiment (days)
PotDecomp <- 0.1 # Potential decomposition rate (/day)
Weather <- read.csv("Data/Weather.csv") #Weather data, this is poor practice!

#############################
# Using weather data 

MaxT <- Weather$maxt # Maximum temperature on a daily basis
MinT <- Weather$mint # Minimum temperature on a daily basis
Radn <- Weather$radn # Radiation on a daily basis
Rain <- Weather$rain # Rain on a daily basis

#############################
# Other model parameters DO NOT CHANGE

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
Results <- data.frame(Time=Time, Mass=Mass, Loss=0, Temp=0, Moist=0, Decomp=0)

#############################
# Daily calculations

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
  temp <- c(Time,Mass,MassLoss,TemperatureFactor,MoistureFactor,FDecomp) 
  Results <- rbind(Results,temp)

}

#############################
# Plot

plot(Results$Time, Results$Mass, type = "l", lty = 1)
