#' makeAntarcticDatabase
#'
#' High level wrapper function which builds NZ Antarctic Database
#'
#' @param apiKey Character string with your Scopus API key
#'
#' @return Data frame of NZ Antarctic Science publications
#'
#' @examples
#'
#' nzAntScience <- makeAntarcticDatabase(apiKey = yourApiKey)
#'
#' @import tidyverse
#' @import httr
#'
#' @export

makeAntarcticDatabase <- function(apiKey){
  
  eid_list <- split(subjectGroupMetadata, f = subjectGroupMetadata$eid)
  
  theDatabase <- lapply(eid_list, function(theEID){
    
    thePublication  <- makePublicationTable(theQuery = paste0('EID(', theEID$eid,')'),
                                            apiKey = apiKey,
                                            citations = TRUE,
                                            articleMetadata = TRUE)
    
    return(thePublication)
    
  }) %>% bind_rows()
  
  theDatabase <- left_join(theDatabase, subjectGroupMetadata, by = "eid")
  
  return(theDatabase)
  
}
