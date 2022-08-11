library(xml2)
library(magrittr)
library(data.table)
# The xml metadata file downloaded from 
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-and.4041.11

# Read xml in
tagsxml <- read_xml("Data/knb-lter-and.4041.11.xml")
# Extract the attribute
nodes <- xml_find_all(tagsxml,'//attribute')
attr_id <- sapply(nodes, xml_attrs)

# Number of elements in the nodesets
lens <- unique(sapply(nodes, xml_length))
stopifnot(length(lens) == 1)
xml_text(nodes[[1]], trim = TRUE)

# Go through each node and extract the text from all the children
info_list <- lapply(nodes, function(x){
  list <- vector("list", length = lens)
  for(i in seq_len(lens)){
    col <- xml_child(x, i) %>% 
      xml_text()
    list[[i]] <- col
    
  }
  DT <- data.table::as.data.table(list)
  
})
# Convert the list into a data.table

info_DT <- data.table::rbindlist(info_list) %>% 
  unique()

# Add a new column - may be not necessary 
info_DT[, Domain := "Soil"]
# Output 
fwrite(info_DT, file = "Data/metadata.csv",col.names = TRUE)



# species richness --------------------------------------------------------

files <- list.files(path = "Data/", pattern = "Jochum", full.names = TRUE)
list_jochum <- sapply(files, fread, USE.NAMES = TRUE)
names(list_jochum) <- gsub("Data/Jochum_JAE2017_","",files)
list_jochum$consumer_biomass.csv %>% colnames()
sapply(list_jochum, str)

LONG_DT <- rbindlist(lapply(list_jochum, melt.data.table, variable.factor = FALSE), idcol =  "ID", fill = TRUE, use.names = TRUE)
LONG_DT[, land_use_system:= ifelse(is.na(land_use_system), `land-use_system`, land_use_system) 
        ][,land_use_system:= ifelse(is.na(land_use_system), trans_system, land_use_system) ]
tidied <- LONG_DT[variable!="V1", .(ID, site,landscape, land_use_system,variable, value,
                                    extrap_richness_response_group,predictor, biomass_response_group)]
keys <- unique(tidied[,.(site, landscape,land_use_system, variable)])
keys[, replicates := gsub("\\D", "", site)
     ][, site := gsub("\\d", "", site)]
sapply(keys, unique)


# wide format -------------------------------------------------------------
wide_DT <- rbindlist(list_jochum, idcol =  "ID", fill = TRUE, use.names = TRUE)
wide_DT[, land_use_system:= ifelse(is.na(land_use_system), `land-use_system`, land_use_system)
        ][,land_use_system:= ifelse(is.na(land_use_system), trans_system, land_use_system) ]


str(wide_DT)

list_jochum$consumer_biomass.csv
list_jochum$observed_consumer_richness.csv
