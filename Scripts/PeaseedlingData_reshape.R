library(data.table)
library(readxl)
library(magrittr)
HB <- read_excel("Data/Peaseedling.xlsx", skip = 9, .name_repair = "universal") %>% 
  as.data.table()

LN <- read_excel("Data/Peaseedling.xlsx", sheet = 2, .name_repair = "universal") %>% 
  as.data.table()
HB
LN

colnames(HB)[1:12]
colnames(LN)[1:12]

# NOTES -------------------------------------------------------------------

# 1. Block is 1 all the way down in HB  
# 2. Rep # is not in LN 
# 3. Looks like Block in LN is the Rep # in HB???
# 4. No of stem is in HB but not in LN 
# 5. Identify the key variables. LN highlight the key columns in blue
# column A:I
# 6. Plot.No in HB BUT Plot in LN
# 7. DAP in HB BUT Days.After.Planting in LN

# solutions 
# 1. create a rep column in LN
# 2. create a no of stem in LN

keys <- colnames(HB)[1:11]

LN[, Rep.. := Block
   ][, Block := 1
     ][, No.of.stem := .N, by = .(Plot, Block, Rep.., Plant)]
colnames(LN)[1:9]
data.table::setnames(LN, old = c("Days.After.Planting", "Plot"), 
                     new = c("DAP", "Plot.No"))
colnames(LN)[1:9]
# Are all HB key
keys %in% colnames(LN)

LN %>% 
  melt.data.table(id.vars = keys, variable.factor = FALSE)
# Eliminate the NA cols
nrow(LN)
coltype <- sapply(LN, function(x) is.logical(x), simplify = TRUE) 
which(coltype == TRUE)
