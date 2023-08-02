#' getAuthorMetadata
#'
#' Function that uses the publication DOI or Scopus ID to get citations for a publication
#'
#' @param theDOI Character string with the publication's doi
#' @param theScopusID Character string with the publications Scopus ID
#' @param start Numeric with the first citation year required
#' @param end Numeric with the final citation year required
#' @param theApiKey Character string with your API key
#'
#' @return Citation data with the input identifier
#'
#' @examples
#' getAuthorMetadata(theScopusID = 8044727400,
#'             theApiKey = your_api_key)
#'
#' @import tidyverse
#' @import httr
#'
#' @export
#' 
getAuthorMetadata <- function(theScopusID, theApiKey){
  
  hdrs = c("Accept" = "application/json",
           "X-ELS-APIKey" = theApiKey)
  
  thePubs <- NULL
  
  while(rlang::is_empty(thePubs)){
    
    thePubs <- GET(paste0("https://api.elsevier.com/content/author?author_id=", theScopusID, "&view=metrics"),
                   add_headers(hdrs)) %>%
      content()
    
  }
  
  
  
  theResult <- data.frame(scopus_id = theScopusID,
                          citation_count = thePubs$`author-retrieval-response`[[1]]$coredata$`citation-count`,
                          cited_by_count = thePubs$`author-retrieval-response`[[1]]$coredata$`cited-by-count`,
                          document_count = thePubs$`author-retrieval-response`[[1]]$coredata$`document-count`,
                          h_index = thePubs$`author-retrieval-response`[[1]]$`h-index`,
                          retrieval_date = Sys.Date()) %>%
    mutate(retrieval_date = str_replace_all(retrieval_date, "-", "_")) %>%
    rename_at(vars(h_index, citation_count, cited_by_count, document_count), ~ c(paste0("h_index_", substr(Sys.Date(), 1, 4)), paste0("citation_count_", substr(Sys.Date(), 1, 4)), paste0("cited_by_count_", substr(Sys.Date(), 1, 4)), paste0("document_count_", substr(Sys.Date(), 1, 4))))
  
  
  return(theResult)
  
  
}
