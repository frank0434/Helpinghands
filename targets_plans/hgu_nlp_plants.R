# _targets.R file
library(targets)
sapply(list.files('Scripts/functions/', full.names = T), source)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("httr", "readxl", "data.table", "magrittr", 
                            "udpipe", "stopwords", "syuzhet", "ggplot2",
                            "here", "sentimentr", "cvms", "SentimentAnalysis"))
stopwords_specific <- c("inaudible", "Illegible", "thing", "yeah", "think",
                        "know","ineligible", "illegible")

list(
  tar_target(metadata,
             read_excel(download_file(url,userName, password), sheet = "TranscriptMeta",
                        skip = 1,.name_repair = "universal") %>% 
               as.data.table()),
  tar_target(iplant_string,
             "(\\/\\_layouts\\/15\\/WopiFrame.aspx\\?sourcedoc\\=\\/project2\\/I200122)|(&action=default)|\\s|\\?Web.+$"
             ),
  tar_target(skip_pattern, 
             '([A-Z][a-z]{3,}\\s*){1,}(\\d)(\\s){0,2}(\\d{1,2}:){1,}\\d{2}|\\[End of interview\\]|Katie|Amy|Rene|Wendy'
             ),
  tar_target(file_paths, multi_download(metadata, iplant_string)),
  tar_target(meta_processed, process_data(file_paths, skip_pattern)),
  tar_target(polarised, aggregate_labels(metadata)),
  tar_target(sub_data, meta_processed[,.(ID, text)]),
  tar_target(ud_model, udpipe_load_model(file = "english-gum-ud-2.5-191206.udpipe")),
  tar_target(sub_data_annoted, annotion(sub_data)),
  tar_target(data, merge.data.table(sub_data_annoted, 
                                    meta_processed[,.(ID, 
                                                      Stakeholder.category,
                                                      Country, 
                                                      Interviewee.area.of.expertise, 
                                                      overall.comment..jian.)],
                                    by = "ID")),
  tar_target(tokens, unnest_annoted(data,
                                    customised_stpw = stopwords_specific)),
  # dictionary based sentiment scores 
  tar_target(tokens_senti, dict_senti(tokens)), 
  tar_target(lexicon_senti, sentimentr_score(
    unique(tokens[,.(ID, paragraph_id, sentence_id, sentence)]),
    polarised)),
  tar_target(analysisenti, sentimentanalyzer(unique(
    tokens[,.(ID, paragraph_id, sentence_id, sentence)]))
    ),
  # Visualisation 
  tar_target(plot_doc_senti, draw_senti(tokens_senti)),
  tar_target(tf_idf_DT, tf_idf(tokens))
)