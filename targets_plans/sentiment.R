



#' dict_senti
#' @description 
#'  four dictionaries sentiment scoring. 
#' @param DT 
#'
#' @return data.table with new columns content 
#' 
#' @export 
#' @import syuzhet
#'
#' @examples

dict_senti <- function(DT){

  stopifnot("lemma" %in% colnames(DT))
  DT[, ':='(syuzhet = sign(get_sentiment(lemma)),
            bing = sign(get_sentiment(lemma, method = "bing")),
            afinn = sign(get_sentiment(lemma, method = "afinn")),
            nrc = sign(get_sentiment(lemma, method = "nrc", lang = "english"))
            # syuzhet = get_sentiment(lemma, method="stanford", tagger_path)
  )]
  return(DT)
  
}

sentimentanalyzer <- function(DT){
  DT <- DT[, analyzesenti := list(lapply(sentence, function(x){
    # cat(substr(x, 1, 10))
    sign(analyzeSentiment(x))
    }))]
  return(DT)
  
}

#' sentimentr_score
#' @description using sentimentr package to do senti scoring. 
#' average_mean applied on the document level
#' 
#' @param DT 
#' @param polarised 
#'
#' @return
#' @export
#'
#' @examples
sentimentr_score <- function(DT, polarised){
  lexicon_dicts <- c("lexicon::hash_sentiment_jockers_rinker"
                     ,"lexicon::hash_sentiment_jockers"
                     ,"lexicon::hash_sentiment_emojis"
                     ,"lexicon::hash_sentiment_huliu"
                     ,"lexicon::hash_sentiment_loughran_mcdonald"
                     ,"lexicon::hash_sentiment_nrc"
                     ,"lexicon::hash_sentiment_senticnet"
                     ,"lexicon::hash_sentiment_sentiword"
                     ,"lexicon::hash_sentiment_slangsd"
                     ,"lexicon::hash_sentiment_socal_google")
  lexicon_overall <- data.frame(dicts = gsub("lexicon::","",lexicon_dicts),
                                accuracy = NA)
  cnt <- 1L
  for (i in lexicon_dicts) {
    cat(cnt, i, "\n")
    senti_jocker <- DT[, sentiment_by(get_sentences(sentence), list(ID), 
                                      averaging.function = average_mean,
                                      polarity_dt = eval(parse(text = i)))]
    senti_jocker[, ave_sentiment:=sign(ave_sentiment)]
    conf_mat <- confusion_matrix(targets = polarised$senti, 
                                 predictions = senti_jocker$ave_sentiment)
    lexicon_overall$accuracy[cnt] <- conf_mat$`Overall Accuracy`
    cnt = cnt + 1
  }
  return(lexicon_overall)
}


# test --------------------------------------------------------------------


# library(syuzhet)
# # get_sentiment(tokens$lemma)
# 
# groupkeys <- c("ID", "Stakeholder.category", "Country,",
#                "Interviewee.area.of.expertise", "overall.comment",
#                
#                "doc_id", "paragraph_id", "sentence_id", "sentence")
# # tagger_path <- 
# tokens[, ':='(syuzhet = sign(get_sentiment(lemma)),
#               bing = sign(get_sentiment(lemma, method = "bing")),
#               afinn = sign(get_sentiment(lemma, method = "afinn")),
#               nrc = sign(get_sentiment(lemma, method = "nrc", lang = "english"))
#               # syuzhet = get_sentiment(lemma, method="stanford", tagger_path)
# )]