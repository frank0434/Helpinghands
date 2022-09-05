
draw_senti <- function(DT){
  
  senti_cols <- c("syuzhet", "bing", "afinn", "nrc")
  doc_senti <- DT[, (lapply(.SD, mean)),
                  by = .(ID, Country), .SDcols = senti_cols]
  p <- doc_senti %>% 
    melt.data.table(id.vars = c("ID","Country")) %>% 
    ggplot(aes(ID, value, color = Country))+
    geom_point(size = 3) +
    facet_wrap(~variable) +
    theme_minimal() +
    theme(strip.background = element_rect(fill = pfr_colors[1]),
          strip.text = element_text(size = 16, color = "white"),
          title = element_text(size = 16, face = "bold"))+
    scale_color_manual(values = pfr_colors[seq(2, 15, by = 3)]) +
    labs(title = "Dictionary-based sentiment analysis on transcript data",
         subtitle = "The topic is Controlled Environment Agricutural",
         caption = "syuzhet, bing, afinn and nrc are four sentiment dictionaries. 
       Transcript data were preprocessed before the sentiment scoring.",
         x = "Document ID",
         y = "Sentiment Scores")+
    geom_hline(yintercept = 0, color = "red")
  return(p)
  ggsave(here::here("Reports/doc_senti.png"), plot = p,
         height = 8, width = 10, dpi = 500)
}

neo_palette <- c("#fe0000",#(254,0,0) red
                 "#32b179",#(253,254,2) yellow 
                 "#38761d",#(11,255,1) g
                 "#011efe",#(1,30,254) b 
                 "#fe00f6", #(254,0,246))purple
                 "#001933", # B
                 "#008000"
                 )
