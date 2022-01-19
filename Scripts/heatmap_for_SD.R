library(data.table)
library(readxl)
library(magrittr)
library(ggplot2)
data = read_excel("Data/Nitrate Heat Map.xlsx", skip = 1) %>% 
  as.data.table()
long_dt <- data %>% 
  melt.data.table(id.vars = "Depth", variable.name = "Site") 
p <- long_dt %>% 
  ggplot(aes(x = Site, y = Depth, fill = value)) +
  geom_tile()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete(position = "top",name = "", label = gsub("(Old Coach)\\s", "\\1\n", unique(long_dt$Site)))+
  scale_fill_distiller(palette = "Blues", direction = 1 )+
  theme_minimal() +
  theme(axis.text = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        title = element_text(size = 20), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  labs(title = "Nitrate-N (kg N/ha)", y = "", caption = "Mean values from a sample size of 39 in each size and depth." )
ggsave("Data/heatmap.png", plot = p, dpi = 500, height = 7, width = 10.8)
# ?heatmap
# dim(data)
# m <- as.matrix(data[,2:8])
# 
# dimnames(m) <- list(data$Depth, colnames(m))
# 
color_pal <- colorRampPalette(brewer.pal(8, "Blues"))(4)
# heatmap(m, Colv = NA, Rowv = NA, revC = TRUE, scale="column", col = color_pal,
#         margins = c(10,10))


x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)
