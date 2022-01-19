library(data.table)
library(readxl)
library(RColorBrewer)
library(ggplot2)
data = read_excel("Data/Nitrate Heat Map.xlsx", skip = 1,
                  .name_repair = "universal")
data
# ?heatmap
# dim(data)
# m <- as.matrix(data[,2:8])
# 
# dimnames(m) <- list(data$Depth, colnames(m))
# 
# color_pal <- colorRampPalette(brewer.pal(8, "Blues"))(4)
# heatmap(m, Colv = NA, Rowv = NA, revC = TRUE, scale="column", col = color_pal,
#         margins = c(10,10))


