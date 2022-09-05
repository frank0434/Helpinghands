# Aim download the data from iplant 

#' download_excel
#' @description This function downloads excel file from iplant to a temp file. 
#'
#' @param url a string of url. The url needs to be like:
#' "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/SVS_PotatoOnion_SoilWater.xlsx"
#'
#' @details Iplant authenticate type is `ntlm`. Need environmental variables to 
#' do authentication 
#' 
#' @return
#' @export
#' @import httr
#'
#' @examples
#' 
#' url = "https://iplant.plantandfood.co.nz/project2/I200122/HgURA1/HGU%20Stakeholder%20Interview%20Schedule%20Final%20Nov%202021.xlsx"
#' 
download_file <- function(url, 
                           username = userName, 
                           pass =  password,
                           filetype = ".xlsx"){
  name <- gsub("https://iplant.plantandfood.co.nz/project2/I200122/HgURA1/",
               "", url)
  httr::GET(url, httr::authenticate(user = username, password = pass,
                              type = "ntlm"), 
            httr::write_disk(tf <- file.path(here::here(paste0(name, filetype))),
                             overwrite = TRUE))
  return(tf)
  
}
#  The url for download the metadata
url = "https://iplant.plantandfood.co.nz/project2/I200122/HgURA1/HGU%20Stakeholder%20Interview%20Schedule%20Final%20Nov%202021.xlsx"



# test  -------------------------------------------------------------------

# 
# tf <- download_excel(url) 
# ## Check if the file exisits
# file.exists(tf)
# ## Read data in 
# df <-  data.table::as.data.table(readxl::read_excel(tf,sheet = "TranscriptMeta", skip = 1,
#                                                     .name_repair = "universal")) 
# library(targets)
# tar_load("meta")
# meta
# 
# meta[, For.Jian..NLP. := gsub("(\\/\\_layouts\\/15\\/WopiFrame.aspx\\?sourcedoc\\=\\/project2\\/I200122)|(&action=default)|\\s", 
#                               "", For.Jian..NLP.)]
# tf_doc <- download_file(meta$For.Jian..NLP.[1], filetype = ".docx")

