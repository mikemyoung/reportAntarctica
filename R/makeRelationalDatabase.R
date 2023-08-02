#' makeRelationalDatabase
#'
#' Function used to convert data from makePublicationTable.R into a relational database
#'
#' @param theData Data frame returned by makePublicationTable.R
#'
#' @return List with 3 elements
#'
#' @examples
#' makeRelationalDatabase(theData = theData)
#'
#' @import tidyverse
#' @import httr
#' @import stringi
#'
#' @export


makeRelationalDatabase <- function(theData){
  
  message("Building database")

  theData$author_name <- stri_trans_general(theData$author_name, "Latin-ASCII")
  
  theData <- theData %>%
    filter(!str_detect(author_name, "[a-z], "))
  
  removedData <- nzAntDatabase %>%
    filter(str_detect(author_name, "[a-z], "))
  
  if(nrow(removedData)){
    message("Detected errors from Scopus. Affected publications have been remove from database. See: removed_pubs_table")
  }
  
  
  full_table <- theData %>%
    mutate(author_names = strsplit(as.character(author_name), "\\.,"),
           author_ids = strsplit(as.character(author_id), ";"),
           affiliation_name = strsplit(as.character(affiliation_name), "--"),
           affiliation_id = strsplit(as.character(affiliation_id), ";")) %>%
    unnest(c("author_names", "author_ids", "affiliation_name", "affiliation_id")) %>%
    mutate(author_name = str_trim(author_name, "both"),
           author_names = str_trim(author_names, "both")) %>%
    rowwise() %>%
    mutate(author_names = trimFirstName(author_names),
           author_names = addComma(author_names),
           author_names = removeLeadInitial(author_names)) %>%
    ungroup() %>%
    mutate(author_names = str_trim(author_names, "both"))
  

  message("Cleaning data")
  
  for(i in 1:nrow(full_table)){
    if(length(str_split(full_table$affiliation_name[i], "\\.,")[[1]]) != length(str_split(full_table$affiliation_id[i], "\\.,")[[1]])){

      id_length <- length(str_split(full_table$affiliation_id[i], "\\.,")[[1]])
      
      #message(paste0("Detected different affiliation id and name lengths: ", full_table$affiliation_name[i], ". Affiliation_id: ", full_table$affiliation_id[i], " retaining ", str_split(full_table$affiliation_name[i], "\\.,")[[1]][1:id_length], ". For scopus_id: ", full_table$scopus_id[i]))
      
      message("Detected effor from Scopus: unequal affiliation id and affiliation name strings are of different length")
      message(paste0("Affiliation Names: ", full_table$affiliation_name[i]))
      message(paste0("Affiliation Ids: ", full_table$affiliation_id[i]))
      message(paste0("Retaining: ", paste(str_split(full_table$affiliation_name[i], "\\.,")[[1]][1:id_length], collapse = ", ")))
      message(paste0("Scopus Ids affected: ", full_table$scopus_id[i]))
      
      full_table$affiliation_name[i] <- paste(str_split(full_table$affiliation_name[i], "\\.,")[[1]][1:id_length], collapse = "., ")
    }
  }

  full_table <- full_table %>%
    mutate(affiliation_names = strsplit(as.character(affiliation_name), "\\.,"),
           affiliation_ids = strsplit(as.character(affiliation_id), "\\.,")) %>%
    unnest(c("affiliation_names", "affiliation_ids"))


  author_table <- full_table %>%
    select(author_ids, author_names, doi) %>%
    distinct() %>%
    group_by(author_ids) %>%
    mutate(doi = paste(doi, collapse = ";")) %>%
    ungroup() %>%
    distinct(author_ids, .keep_all = TRUE) %>%
    arrange(author_names) %>%
    mutate(author_names = toupper(author_names))


  publication_table <- full_table %>%
    select(cover_date, title, total_citations, publication_name, author_name, doi, eid) %>%
    distinct(title, .keep_all = TRUE)


  affiliation_table <- full_table %>%
    select(affiliation_ids, affiliation_names, author_ids, doi) %>%
    distinct() %>%
    group_by(affiliation_ids) %>%
    mutate(doi = paste(doi, collapse = ";"),
           author_ids = paste(author_ids, collapse = ";")) %>%
    ungroup() %>%
    distinct(affiliation_ids, .keep_all = TRUE) %>%
    rowwise() %>%
    mutate(doi = paste(unique(str_split(doi, ";")[[1]]), collapse = ";")) %>%
    ungroup() %>%
    arrange(affiliation_ids)

  if(nrow(removedData)){
    return(list(author_table = author_table, publication_table = publication_table, affiliation_table = affiliation_table, removed_pubs_table = removedData))
  } else{
    return(list(author_table = author_table, publication_table = publication_table, affiliation_table = affiliation_table))
  }

}
