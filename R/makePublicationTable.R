#' makePublicationTable
#'
#' High level wrapper function which aggregates metadata from each Scopus API
#'
#' @param theQuery Character string with the Scopus query
#' @param apiKey Character string with your Scopus API key
#' @param acknowledgement_source List which includes keywords used to search publication acknowledgement section
#' @param citations Logical to attach citations to search results
#' @param articleMetadata Logical to attach author, affiliation and subject metadata
#' @param onlineDate Logical to get first available online date for publications in Science Direct
#'
#' @return Data frame of publications returned by search query
#'
#' @examples
#' query = '(TITLE-ABS-KEY(antarct* OR "southern ocean" OR "ross sea" OR "amundsen sea" OR "weddell sea") AND NOT TITLE-ABS-KEY(candida OR "except antarctica" OR "not antarctica")) AND PUBYEAR > 2021 AND AFFILCOUNTRY("New Zealand")'
#'
#' acknowledgement_search <- list("ANTNZ" = 'FUND-ALL("Antarctica New Zealand" OR "Antarctica NZ") AND PUBYEAR > 2021',
#'                                "ASP" = 'FUND-ALL(Antarctic AND Science AND Platform OR ASP OR "ANTA 1801") AND PUBYEAR > 2021',
#'                                "NZARI" = 'FUND-ALL("NZARI" OR "New Zealand Antarctic Research Institute") AND PUBYEAR > 2021')
#'
#' makePublicationTable(theQuery = query,
#'                      apiKey = your_api_key,
#'                      citations = TRUE,
#'                      articleMetadata = TRUE,
#'                      acknowledgement_source = acknowledgement_search)
#'
#' @import tidyverse
#' @import httr
#'
#' @export





makePublicationTable <- function(theQuery = NULL,
                                 apiKey = NULL,
                                 acknowledgement_source = NULL,
                                 citations = FALSE,
                                 articleMetadata = FALSE,
                                 onlineDate = FALSE,
                                 subjectMetadata = TRUE,
                                 metrics = FALSE,
                                 percentileYearRange = 3){


  if(is.null(apiKey)){
    stop("Please provide an API Key")
  }
  
  if(!is.numeric(percentileYearRange)){
    stop("percentileYearRange must be numeric")
  }
  
  if(percentileYearRange > 10){
    stop("Maximum percentile year range is 10")
  }

  message("Running query")

  thePublications <- getPublication(theQuery = theQuery, theApiKey = apiKey)


  if(!is.null(acknowledgement_source)){

  thePublications <- addAcknowledgement(theData = thePublications, apiKey = apiKey, acknowledgement_source = acknowledgement_source)

  }


  # Create a list of Scopus ids to iterate over
  scopusID_list <- split(thePublications, f=thePublications$scopus_id)


  if(citations){

    message("Retrieving citations")

    citationData <- lapply(scopusID_list, function(scopusID_value){

      getCitation(theScopusID = scopusID_value$scopus_id,
                  start = as.numeric(substr(scopusID_value$cover_date, 1, 4)),
                  end = as.numeric(substr(Sys.Date(), 1, 4)),
                  theApiKey = apiKey)}) %>%
      bind_rows()

    thePublications <- thePublications %>%
                       select(-citation_extraction_date) %>%
                       left_join(citationData, by = "scopus_id")

  }



  if(articleMetadata){

  # Get the Author Data
  message("Retrieving metadata: author details")
    
  if(subjectMetadata){message("Retrieving metadata: subject classification")}

  theMetadata <- lapply(scopusID_list, function(x){getMetadata(theScopusID = x$scopus_id, theApiKey = apiKey, subjectMetadata = subjectMetadata)}) %>% bind_rows()

  thePublications <- left_join(thePublications, theMetadata, by = "scopus_id")

  
  if(subjectMetadata){
  # Clean where an undefined grouping is concatenated a asp classification
  thePublications <- thePublications %>%
    rowwise() %>%
    mutate(custom_subject_grouping = ifelse(str_detect(custom_subject_grouping, "Undefined;|;Undefined"),
                                            str_remove_all(custom_subject_grouping, "Undefined;|;Undefined"),
                                            custom_subject_grouping)) %>%
    ungroup()

  }
  
  }




  if(onlineDate){

  # Get Article Metadata
  message("Retrieving metadata: date first available online (ScienceDirect publications only)")

  # Create a list of Scopus ids to iterate over
  eid_list <- split(thePublications, f=thePublications$eid)

  availableOnline <- lapply(eid_list, function(eid_value){getOnlineDate(theEID = eid_value$eid, theApiKey = apiKey)}) %>% bind_rows()

  thePublications <- left_join(thePublications, availableOnline, by = "eid")

  }

  
  
  if(metrics){
    
    # Get Article Metadata
    message("Retrieving CiteScores and Percentiles")
    
    # Create a list of Scopus ids to iterate over
    id_list <- split(thePublications, f=thePublications$scopus_id)
    
    thePublications <- lapply(id_list, function(thePub){
      
      citeScore <- getCiteScore(theScopusID = thePub$scopus_id, theApiKey = apiKey)
      
      
      if(!is.null(nrow(citeScore))){
        
        thePub <- left_join(thePub, citeScore, by = "scopus_id")
        
      }
      
      percentileScores <- getPercentile(thePublicationID = thePub$publication_id, yearRange = percentileYearRange, theApiKey = apiKey) %>%
        select(publication_id, contains("percentile"))
      
      
      if(!is.null(nrow(percentileScores))){
        
        thePub <- left_join(thePub, percentileScores, by = "publication_id")
        
      }
      
      
      
  }) %>% bind_rows()
    
    
 }
    
  # Format column order
  citationStart <- min(as.numeric(substr(thePublications$cover_date, 1, 4)))
  citationEnd <- max(as.numeric(substr(thePublications$cover_date, 1, 4)))

  if(citations){
  citationNames <- c("total_citations", paste0("citations_", seq(citationStart, citationEnd, by = 1)), "citation_extraction_date")
  } else {
    citationNames <- c("total_citations", "citation_extraction_date")
  }

  
  if(onlineDate){
    thePublications <- thePublications %>%
      mutate(year = substr(cover_date, 1, 4)) %>%
      select(year, cover_date, first_available_online, title, !! citationNames, everything())
  } else {
    thePublications <- thePublications %>%
      mutate(year = substr(cover_date, 1, 4)) %>%
      select(year, cover_date, title, !! citationNames, everything())
  }

  
  message("Done!")

  return(thePublications)

}
