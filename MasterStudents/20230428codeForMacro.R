
library(ggplot2)
library(magrittr)
meta
final
df <- merge(meta, final, by.x = "PlotID", by.y = "Plot_n")
df %>% 
  ggplot(aes(as.character(Amount), TotalYield, 
             color = as.character(ReplicateID))) +
  geom_point(size = 3) 
