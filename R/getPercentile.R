#' getPercentile
#'
#' Function that uses the publication DOI or Scopus ID to get citations for a publication
#'
#' @param thePublicationID Character string with the journal's source id/publication id
#' @param yearRange The years to be returned. Possible values: 3, 5, 10.
#' @param theApiKey Character string with your API key
#'
#' @return Associated journal's percentiles (1st, 5th, 10th, 25th)
#'
#' @examples
#' getPercentiles(thePublicationID = "21100790929",
#'                yearRange = "3",   
#'                theApiKey = your_api_key)
#'
#'
#' @import tidyverse
#' @import httr
#'
#' @export


getPercentile <- function(thePublicationID, yearRange, theApiKey){
  
  if(is.na(thePublicationID)){
    return(NA)
  }
  
  yearRange <- as.character(yearRange)
  
  # if(!yearRange %in% c("3", "5")){
  #   stop("Only 3 or 5 year ranges can be specified")
  # }
  
  if(is.na(thePublicationID)){
    message("Metadata missing: no publication id")
    return(thePublicationID)
  } else {
  
  hdrs = c("Accept" = "application/json",
           "X-ELS-APIKey" = theApiKey)
  
  thePercentiles <- NULL
  
  while(rlang::is_empty(thePercentiles)){
    
    thePercentiles <- GET(paste0("https://api.elsevier.com/analytics/scival/scopusSource/metrics?journalImpactType=CiteScore&metricTypes=PublicationsInTopJournalPercentiles&showAsFieldWeighted=true&includeSelfCitations=true&yearRange=", yearRange, "yrs&httpAccept=application/json&byYear=true&includedDocs=AllPublicationTypes&sourceIds=", thePublicationID),
                          add_headers(hdrs)) %>%
      content()
  }
  

  percentile_1 <- lapply(thePercentiles$results[[1]]$metrics[[1]]$values[[1]]$percentageByYear, function(x){ 
    if(is.null(x)){
    result <- NA
  } else {
    result <- ifelse(x == 100, 1, 0)
    }
    return(result)
    }) %>% 
    bind_cols()
  
  percentile_5 <- lapply(thePercentiles$results[[1]]$metrics[[1]]$values[[2]]$percentageByYear, function(x){ 
    if(is.null(x)){
      result <- NA
    } else {
      result <- ifelse(x == 100, 1, 0)
    }
    return(result)
  }) %>% 
    bind_cols()
  
  percentile_10 <- lapply(thePercentiles$results[[1]]$metrics[[1]]$values[[3]]$percentageByYear, function(x){ 
    if(is.null(x)){
      result <- NA
    } else {
      result <- ifelse(x == 100, 1, 0)
    }
    return(result)
  }) %>% 
    bind_cols()
  
  percentile_25 <- lapply(thePercentiles$results[[1]]$metrics[[1]]$values[[4]]$percentageByYear,  function(x){ 
    if(is.null(x)){
      result <- NA
    } else {
      result <- ifelse(x == 100, 1, 0)
    }
    return(result)
  }) %>% 
    bind_cols()
  
  
  names(percentile_1) <- paste0("percentile_1--", names(percentile_1))
  names(percentile_5) <- paste0("percentile_5--", names(percentile_5))
  names(percentile_10) <- paste0("percentile_10--", names(percentile_10))
  names(percentile_25) <- paste0("percentile_25--", names(percentile_25))
  
  combinedData <- bind_cols(percentile_1, percentile_5, percentile_10, percentile_25)
  
  
  yearString <- str_remove_all(names(combinedData), "percentile_1--|percentile_5--|percentile_10--|percentile_25--")
  yearString <- unique(sort(yearString[!yearString %in% "publication_id"]))
  
  
  percentileData <- lapply(yearString, function(x){theResult <- combinedData %>% select(contains(x))}) %>% 
    bind_cols() %>%
    mutate(publication_id = thePublicationID) %>%
    select(publication_id, everything())
  
  return(percentileData)
  
  }
  
  
  
}





