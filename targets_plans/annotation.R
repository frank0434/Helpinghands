
annotion <- function(DT){
  DT[, annoted := lapply(text, function(x){
    t <- udpipe_annotate(udpipe_load_model("english-gum-ud-2.5-191206.udpipe"),
                         x,
                         trace = 20)
    as.data.frame(t)
  })]
}



# test --------------------------------------------------------------------

# library(targets)
# tar_load("sub_data")
# tar_load("ud_model")
# 
# 
# library(udpipe)
# t <- udpipe_annotate(udpipe_load_model("english-gum-ud-2.5-191206.udpipe"),
#                      unlist(sub_data$text[1]),trace = 10)
# 
# annoted <- as.data.frame(t)
