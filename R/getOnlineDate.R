#' getOnlineDate
#'
#' Function that gets the first available online date for Science Direct publications
#'
#' @param theDOI Character string with the publication's doi
#' @param theEID Character string with the publication's EID
#' @param theApiKey Character string with your API key
#'
#' @return Data frame with the online date and the input identifier
#'
#' @examples
#' getCitation(theDOI = "10.1080/16000889.2021.1933783",
#'             start = 2021,
#'             end = 2022,
#'             theApiKey = your_api_key)
#'
#'
#' @import tidyverse
#' @import httr
#'
#' @export

getOnlineDate <- function(theDOI = NULL, theEID = NULL, theApiKey){


if(!is.null(theDOI) && is.na(theDOI)){ availableOnline <- data.frame(doi = theDOI, first_available_online = availableOnline)}


if(!is.null(theDOI) && is.null(theEID)){

theURL <- paste0("https://api.elsevier.com/content/metadata/article?query=doi(", theDOI, ")", "&field=available-online-date")

} else if(!is.null(theEID) && is.null(theDOI)){

theURL <- paste0("https://api.elsevier.com/content/metadata/article?query=eid(", theEID, ")", "&field=available-online-date")

}

hdrs = c("Accept" = "application/json",
  "X-ELS-APIKey" = theApiKey)


theMetadata <- GET(theURL,
               add_headers(hdrs)) %>%
               content()



availableOnline <- ifelse(theMetadata$`search-results`$`opensearch:totalResults` == "0", NA, theMetadata$`search-results`$entry[[1]]$`available-online-date`)

if(!is.null(theDOI) && is.null(theEID)){

availableOnline <- data.frame(doi = theDOI,
                              first_available_online = availableOnline)

} else if(!is.null(theEID) && is.null(theDOI)){

availableOnline <- data.frame(eid = theEID,
                              first_available_online = availableOnline)


}


return(availableOnline)


}
