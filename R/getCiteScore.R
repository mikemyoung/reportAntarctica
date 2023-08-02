#' getCiteScore
#'
#' Function that uses the publication DOI or Scopus ID to get citations for a publication
#'
#' @param theScopusID Character string with the publication's scopus id
#' @param theApiKey Character string with your API key
#'
#' @return Associated journal's CiteScore
#'
#' @examples
#' getCiteScore(theScopusID = "85101941134",
#'              theApiKey = your_api_key)
#'
#'
#' @import tidyverse
#' @import httr
#'
#' @export


getCiteScore <- function(theScopusID, theApiKey){
  
  if(is.na(theScopusID)){
    return(NA)
  }
  
  hdrs = c("Accept" = "application/json",
           "X-ELS-APIKey" = theApiKey)
  
  theScore <- NULL
  
  while(rlang::is_empty(theScore)){
    
    theScore <- GET(paste0("https://api.elsevier.com/analytics/scival/publication/metrics?metricTypes=JournalImpact&publicationIds=", theScopusID, "&httpAccept=application/json&byYear=trueyearRange=3yrs&includedDocs=AllPublicationTypes"),
                  add_headers(hdrs)) %>%
                  content()
            
    }
  
  if(!is_empty(theScore$results)){
    
  citeScore <- data.frame(scopus_id = theScopusID, 
                          cite_score = ifelse(!is.null(theScore$results[[1]]$metrics[[1]]$value), theScore$results[[1]]$metrics[[1]]$value, NA),
                          stringsAsFactors = FALSE)

  } else {
    
    citeScore <- data.frame(scopus_id = theScopusID,
                            cite_score = NA, 
                            stringsAsFactors = FALSE) 
  }
 

  return(citeScore)
  
}
