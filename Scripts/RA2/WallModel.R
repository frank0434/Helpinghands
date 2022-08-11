# Relationship between macro-invertebrate richness, climate and decomposition rate (Wall et al. 2008) 

##################
#Inputs

taxonomic.richness <- 5 # List of values from each site comes from Model 1
AllCDIs <- read.csv(here::here("Data/LIDET-CDI.csv")) #Weather data, this is poor practice!


##################
#Model

for(i in 1:nrow(AllCDIs))
{
  log.k.value <- 0.127 * taxonomic.richness + 0.6299 * log(AllCDIs$CDI[i]) - 5.755 #calculation
  k.value <- exp(log.k.value) #back transform
  print(paste0(AllCDIs$Site[i]," k-value is ",k.value)) #Do some sort of output, just for testing
}






















# 
# 
# 
# 
# log(k.value) + 5.755 = 0.127 * taxonomic.richness + 0.6299 * log(CDI)
# 
# log(k.value) + 5.755 - 0.127 * taxonomic.richness = 0.6299 * log(CDI)
# 
# (log(k.value) + 5.755 - 0.127 * taxonomic.richness )/ 0.6299 = log(CDI)
# 
# log(CDI) = 1.588* log(k.value) + 9.136 - 0.2016 * taxomic.richness



