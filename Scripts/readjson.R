library(jsonlite)
library(magrittr)
library(data.table)

summary <- read_json("Data/20220404Junqi/summary.json")
summary %>% 
  as.data.table()
# Read json in 
data0401 <- read_json("Data/20220404Junqi/2022-04-01T14_41_49.065328.jpg.json")
# Make it into data.table
dt <- data0401 %>% 
  as.data.table()

# Observation ID to make each row unique
DT <- dt[, ID := 1:.N]
# Unnested with unlist by the unique ID
unnested_DT <- DT[, unlist(grapes,recursive = FALSE,
                           use.names = TRUE), by = .(timestamp, total_area,ID)] 


