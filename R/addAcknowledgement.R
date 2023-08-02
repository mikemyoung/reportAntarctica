#' addAcknowledgement
#'
#' Function used to identify publications that have acknowledged a specified organisation.
#'
#' @param theData Data frame of the data returned from getPublication.R
#' @param apiKey Character string with your Scopus API key
#' @param acknowledgement_source List of organisations and the keywords to be searched signalling their acknowledgement
#'
#' @return Data frame of publications returned by search query
#'
#' @examples
#'
#' acknowledgement_search <- list("ANTNZ" = 'FUND-ALL("Antarctica New Zealand" OR "Antarctica NZ") AND PUBYEAR > 2021')
#'
#' addAcknowledgement(theData = df,
#'                    theApiKey = your_api_key,
#'                    acknowledgement_source = acknowledgement_search)
#'
#'
#' @import tidyverse
#' @import httr
#'
#' @export


addAcknowledgement <- function(theData = NULL, apiKey, acknowledgement_source){


  message("Running acknowledgement search")

  theAcknowledgements <- list()

  for(i in seq_along(acknowledgement_source)){

    if(!is.null(theData)){

    message(paste0("Searching for " , "'", names(acknowledgement_source)[i]), "'")
    theAcknowledgements[[i]] <- getPublication(theQuery = acknowledgement_source[[i]], theApiKey = apiKey) %>%
      select(eid) %>%
      mutate(acknowledgement_name = names(acknowledgement_source)[[i]])

    } else {

      message(paste0("Searching for " , "'", names(acknowledgement_source)[i]), "'")
      theAcknowledgements[[i]] <- getPublication(theQuery = acknowledgement_source[[i]], theApiKey = apiKey) %>%
        mutate(acknowledgement_name = names(acknowledgement_source)[[i]])

    }

  }


  publication_list <- bind_rows(theAcknowledgements) %>%
    split(f = .$eid)

  acknowledgementData <- lapply(publication_list, function(theRecord){

    theResult <- theRecord %>%
      pivot_longer(., -all_of(names(theRecord)[!names(theRecord) %in% c("acknowledgement_name","acknowledgement_source")]), names_to = "acknowledgement_name", values_to = "theAcknowledgements") %>%
      group_by(eid) %>%
      mutate(acknowledgement_source = paste(theAcknowledgements, collapse = ";")) %>%
      ungroup() %>%
      select(-theAcknowledgements, -acknowledgement_name) %>%
      distinct()

  }) %>% bind_rows()


  if(!is.null(theData) & !is_empty(acknowledgementData)){

      acknowledgementData <- suppressMessages(left_join(theData, acknowledgementData, by = "eid"))

  } else if(!is.null(theData) & is_empty(acknowledgementData)){

    acknowledgementData <- theData %>%
      mutate(acknowledgement_source = NA)

  }

  return(acknowledgementData)

}
