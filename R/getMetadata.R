#' getMetadata
#'
#' Function used to get author and affiliation metadata for a publication record
#'
#' @param theScopusID Character string with the Scopus ID
#' @param theApiKey Character string with your Scopus API key
#'
#' @return Data frame of the metadata and the Scopus ID
#'
#' @examples
#' getMetadata(theScopusID = "85103423955",
#'             theApiKey = your_api_key)
#'
#'
#' @import tidyverse
#' @import httr
#'
#' @export





getMetadata <- function(theScopusID, theApiKey, subjectMetadata){

  hdrs = c("Accept" = "application/json",
           "X-ELS-APIKey" = theApiKey)

  thePubs <- NULL

  while(rlang::is_empty(thePubs)){

    thePubs <- GET(paste0("https://api.elsevier.com/content/abstract/scopus_id/", theScopusID),
                   add_headers(hdrs)) %>%
      content()

  }


  # Get each author's name and their affiliation ID
  theAuthorTable <- lapply(thePubs$`abstracts-retrieval-response`$authors$author, function(x){

    author = data.frame(author_ids = ifelse(!is.null(x$`@auid`), x$`@auid`, NA),
                        author_names = ifelse(!is.null(x$`preferred-name`$`ce:indexed-name`), x$`preferred-name`$`ce:indexed-name`, NA))


    # Check that the author has given an affiliation
    if(!is.null(x$affiliation)){


      # multiple affiliations - held in a list
      if(is.null(names(x$affiliation))){

        affil_table <- lapply(x$affiliation, function(y){

          affil_ids <- data.frame(affil_id = ifelse(!is.null(y$`@id`), y$`@id`, NA))

        }) %>% bind_rows()


        # single affiliation - not held in a list
      } else {

        affil_table <- data.frame(affil_id = ifelse(!is.null(x$affiliation$`@id`), x$affiliation$`@id`, NA))

      }

      # Make affil_id for author without a given affiliation
    } else {

      affil_table <- data.frame(affil_id = NA)

    }

    # Bind the author with their affiliation info
    # Working across one affiliation at a time so isn't necessary to join
    authorAffilTable <- bind_cols(author, affil_table)


  }) %>% bind_rows()


  # Check that the publication has the affiliation element - if not they're all NA
  if(!is.null(thePubs$`abstracts-retrieval-response`$affiliation)){

    # multiple affiliations - held in a list
    if(is.null(names(thePubs$`abstracts-retrieval-response`$affiliation))){

      theAffilTable <- lapply(thePubs$`abstracts-retrieval-response`$affiliation, function(x){

        affilTable <- data.frame(affil_id = ifelse(!is.null(x$`@id`), x$`@id`, NA),
                                 affiliation_names = ifelse(!is.null(x$affilname), x$affilname, NA),
                                 affiliation_countries = ifelse(!is.null(x$`affiliation-country`), x$`affiliation-country`, NA))

      }) %>% bind_rows()


      # single affiliation - not held in a list
    } else {

      theAffilTable <- data.frame(affil_id = ifelse(!is.null(thePubs$`abstracts-retrieval-response`$affiliation$`@id`), thePubs$`abstracts-retrieval-response`$affiliation$`@id`, NA),
                                  affiliation_names = ifelse(!is.null(thePubs$`abstracts-retrieval-response`$affiliation$affilname), thePubs$`abstracts-retrieval-response`$affiliation$affilname, NA),
                                  affiliation_countries = ifelse(!is.null(thePubs$`abstracts-retrieval-response`$affiliation$`affiliation-country`), thePubs$`abstracts-retrieval-response`$affiliation$`affiliation-country`, NA))

    }



  } else {

    theAffilTable <- data.frame(affil_id = NA,
                                affiliation_names = NA,
                                affiliation_countries = NA)

  }



  author_affiliation <- left_join(theAuthorTable, theAffilTable, by = "affil_id")



  authorIdList <- author_affiliation %>%
    distinct(author_ids) %>%
    split(f = .$author_ids)


  theOutput <- lapply(authorIdList, function(x){

    theId <- x %>% pull()

    theAffiliations_id <- author_affiliation %>%
      filter(author_ids == theId) %>%
      pull(affil_id)

    theAffiliations_name <- author_affiliation %>%
      filter(author_ids == theId) %>%
      pull(affiliation_names)
    
    theAffiliations_country <- author_affiliation %>%
      filter(author_ids == theId) %>%
      pull(affiliation_countries)

    authorId_affilId <- data.frame(author_ids = theId,
                                   affil_id_concat = paste(theAffiliations_id, collapse = ".,"),
                                   affil_name_concat = paste(theAffiliations_name, collapse = ".,"),
                                   affil_country_concat = paste(theAffiliations_country, collapse = ".,"))

  }) %>% bind_rows()



  author_affiliation <- left_join(author_affiliation, theOutput, by = "author_ids") %>%
    select(-affil_id, -affiliation_names, -affiliation_countries) %>%
    distinct() %>%
    rowwise() %>%
    mutate(author_names = addStop(author_names)) %>%
    ungroup() %>%
    mutate(author_id = paste(author_ids, collapse = ";"),
           author_name = paste(author_names, collapse = ", "),
           affiliation_id = paste(affil_id_concat, collapse = ";"),
           affiliation_name = paste(affil_name_concat, collapse = "--"),
           affiliation_country = paste(affil_country_concat, collapse = "--")) %>%
    select(author_name, author_id, affiliation_name, affiliation_id, affiliation_country) %>%
    distinct() %>%
    mutate(scopus_id = theScopusID)

  
  
  
  # Get references
  # referenceList <- lapply(thePubs$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference, function(x){
  #   
  #   theResult <- data.frame(references = ifelse(!is.null(x$`ref-fulltext`), x$`ref-fulltext`, NA)) 
  #   
  # }) %>%
  #   bind_rows()
  # 
  # theReferences <- data.frame(references = paste(referenceList$references, collapse = "; ")) %>% 
  #   mutate(references = str_replace_all(references, "\\.;", "\\;"))
  # 
  # 
  # author_affiliation <- bind_cols(author_affiliation, theReferences)
  
  
  
  # Get keywords
  # keywordList <- lapply(thePubs$`abstracts-retrieval-response`$authkeywords$`author-keyword`, function(x){
  #   
  #   theResult <- data.frame(keywords = ifelse(!is.null(x$`$`), x$`$`, NA))
  #   
  # }) %>%
  #   bind_rows()
  # 
  # 
  # 
  # if(is_empty(keywordList)){
  #   
  #   theKeywords <- data.frame(author_keywords = NA)
  # 
  # } else {
  #   
  #   theKeywords <- data.frame(author_keywords = paste(keywordList$keywords, collapse = "; "))
  #   
  # }
  # 
  # author_affiliation <- bind_cols(author_affiliation, theKeywords)
  

  if(subjectMetadata){
    
  subjectMetadata <- getSubject(abstractData = thePubs, theScopusID = theScopusID, theApiKey = theApiKey)

  theMetadata <- left_join(author_affiliation, subjectMetadata, by = "scopus_id")
  
  } else {
    
    theMetadata <- author_affiliation
    
  }

  return(theMetadata)


}
