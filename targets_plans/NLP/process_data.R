# library(targets)


aggregate_labels <- function(DT){
  labels <- DT[, .(ID, Stakeholder.category, Country, Overall.Sentiment, 
                         overall.comment..jian., Interviewee.area.of.expertise)]
  lab_keys <- c("ID", "Stakeholder.category", "Country", 
                "Interviewee.area.of.expertise")
  
  
  # split labels 
  labels[, c("Overall.Sentiment_1", "Overall.Sentiment_2") := tstrsplit(Overall.Sentiment, split = "/", fill = NA)]
  cols <- colnames(labels)
  senti_cols <- cols[!cols %in% lab_keys]
  labels_long <- labels %>% 
    melt.data.table(id.vars = lab_keys, variable.factor = FALSE)
  labels_long <- labels_long[, value:= tolower(value)
                             ][, value := as.integer(fcase(value == "0", as.integer(NA),
                                                           value == "positive", 1L, 
                                                           value == "neutral", 0L,
                                                           value == "negative", -1L))] 
  polarised <- labels_long[,.( senti = sign(mean(value, na.rm = TRUE))), 
                           by = lab_keys
                           ][, Stakeholder.category := gsub("scientists","scientist", Stakeholder.category)]
  
  
}

tf_idf <- function(DT){
  tokens_collapse <- DT[, .(ID, Country, Stakeholder.category, 
                            Interviewee.area.of.expertise, lemma)
                        ][, .(strings = toString(lemma)), 
                          by = .(ID, Country, Stakeholder.category, 
                                 Interviewee.area.of.expertise)]
  corpus <- quanteda::corpus(tokens_collapse, text_field = "strings",
                   docid_field = "ID",
                   meta = list(source = "From a data.frame called corpus."))
  dfm <- quanteda::tokens(corpus, remove_punct = TRUE) %>% 
    quanteda::dfm()
  
  tf_idf <- quanteda::dfm_tfidf(dfm)
  
  m <- tf_idf %>% 
    as.matrix()

  tf_idf_DT <- as.data.table(m, keep.rownames = T)
  return(tf_idf_DT)
}



#' multi_download
#' @description download multiple files from iplant and output the file paths
#' @param DT 
#'
#' @return
#' @export
#'
#' @examples
multi_download <- function(DT, iplant_string){
  # Remove the extract strings 
  datafiles <- DT[, For.Jian..NLP.:=gsub(iplant_string, "", For.Jian..NLP.)
                  ][, temp_path := sapply(For.Jian..NLP., function(x){
                    tf <- download_file(x,userName,password, filetype = ".docx") # download the file
                    if(file.exists(tf)){
                      cat("Downloading", 
                          gsub('https://iplant.plantandfood.co.nz/project2/I200122/HgURA1/', '', x), "\n")
                      # doc <- textreadr::read_docx(tf)
                      } else {
                        print("Check the url or password!")
                        }
                    return(tf)
                    }, simplify = TRUE)]
  return(datafiles)
  }
process_data <- function(DT, skip_pattern){
  # DT <- file_paths# for debugging purpose
  df <- as.data.frame(DT)
  df$starts <- NA
  df$text <- NA
  
  for( i in seq_len(nrow(df))){
    # i = 1 # for debugging purpose
    cat("Docx", i, "\n")
    test_str <- c(df$Country[i], df$Stakeholder.category[i])
    # test <- grep("China|M.+ori", test_str)
    pattern <-  gsub("\\s", "\\\\s*",  df$background.information[i])
    temp_path <- df$temp_path[i]
    if(!i %in% c(25, 29, 30)){
      txt <- textreadr::read_docx(temp_path)
      start <- grep(pattern, txt)
      df$starts[i] <- start
    } else if (i == 25){
      df$starts[i] <- as.integer(pattern)
    } else {
      cat("Docx", i, "\n")
      docx <- docxtractr::read_docx(temp_path)
      tbls <- docxtractr::docx_extract_all_tbls(docx, guess_header = FALSE)
      if (i == 30){
        txt <- tbls[[1]]$V3
        # txt <- txt[as.integer(pattern) :length(txt)]
        start <- as.integer(pattern)
        df$starts[i] <- start
        txt <- txt[start:length(txt)]
        df$text[i] <- list(txt)
        
        next
      } else {
        cat("Docx", i, "\n")
        txt <- tbls[[1]]$V2
        start <- grep(pattern, txt)
        df$starts[i] <- start
        txt <- txt[start:length(txt)]
        txt <- gsub("Prof.|[A-Z][a-z]{1,}(\\s|\\.\\s)(([A-Z][a-z]{1,}\\s)|(\\(.+\\))):", "", txt)
        df$text[i] <- list(txt)
        next
      }
    }
    # some docs may not have the patterns, only process the right one
      txt <- txt[start:length(txt)]
      txt <- txt[grep(skip_pattern, txt, invert = TRUE)]
      df$text[i] <- list(txt)

    
  }
  return(data.table::as.data.table(df))
  
  
}

unnest_annoted <- function(DT, customised_stpw = NULL){
  stopifnot("annoted" %in% colnames(DT))
  DT[, annoted:=lapply(annoted, function(x){
    tokens <- data.table::as.data.table(x)[,1:8]
    # Filter out unwanted words
    stopwds <- c(stopwords::data_stopwords_snowball, customised_stpw)
    tokens <- tokens[(!lemma %in% stopwds) & 
                       (upos %in% c("NOUN", "ADJ", "VERB"))]
    tokens
  } )]
  
  DT[, unlist(annoted, recursive = FALSE), by = .(ID,
                                                  Stakeholder.category,
                                                  Country, 
                                                  Interviewee.area.of.expertise, 
                                                  overall.comment..jian.)]
  
}
# test  -------------------------------------------------------------------

# tar_load("meta_processed")
# dt <- meta_processed[(!is.na(background.information)) & (Country != 0)] 
# dt[,rawtext:=sapply(For.Jian..NLP., function(x){
#   tf <- download_file(x, filetype = ".docx")
#   if(file.exists(tf)){
#     cat("Reading", 
#         gsub('https://iplant.plantandfood.co.nz/project2/I200122/HgURA1/', '', x), "\n")
#                    doc <- textreadr::read_docx(tf)
#                  } else {
#                      print("Check the url or password!")
#                    }
#                  }, simplify = TRUE)
#   ]
# tar_load("skip_pattern")
# dt[, background.information:=gsub("\\s", "\\\\s*", background.information)
#    ][, starts := mapply(function(x, y){
#      txt <- (unlist(x))
#      grep(y, txt)
#      }, rawtext, background.information)
#      ][, text := mapply(function(x, y, z){
#        txt <- (unlist(x))
#        txt <- txt[y:length(txt)]
#        txt[grep(z, txt, invert = TRUE)]
#        }, rawtext, starts, skip_pattern)]
# 


# tar_load("metadata")
# # meta
# 
# meta[, For.Jian..NLP. := gsub("(\\/\\_layouts\\/15\\/WopiFrame.aspx\\?sourcedoc\\=\\/project2\\/I200122)|(&action=default)|\\s", 
#                               "", For.Jian..NLP.)]
# tf_doc <- download_file(meta$For.Jian..NLP.[1], filetype = ".docx")
# 
# library(textreadr)
# txt <- read_docx(tf_doc)
# starts <- grep(meta$background.information[1], txt)
# txt <- txt[starts:length(txt)]
# skip_pattern <- '([A-Z][a-z]{3,}\\s*){1,}(\\d\\s){0,1}(\\d{1,2}:){1,}\\d{2}|\\[End of interview\\]|Katie|Amy'
# txt[grep(skip_pattern,txt, invert = TRUE)]
