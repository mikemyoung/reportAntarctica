#' getCitation
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


getCitation <- function(theDOI = NULL, theScopusID = NULL, start, end, theApiKey){

if(!is.null(theDOI)){theDOI <- as.character(theDOI)}

hdrs = c("Accept" = "application/json",
         "X-ELS-APIKey" = theApiKey)

theCites <- NULL

while(rlang::is_empty(theCites)){

if(!is.null(theDOI) & is.null(theScopusID)){

theCites <- GET(paste0("https://api.elsevier.com/content/abstract/citations?doi=", theDOI, "&date=", start, "-", end),
                add_headers(hdrs)) %>%
            content()

  } else if(!is.null(theScopusID) & is.null(theDOI)){

    theCites <- GET(paste0("https://api.elsevier.com/content/abstract/citations?scopus_id=", theScopusID, "&date=", start, "-", end),
                    add_headers(hdrs)) %>%
                content()

  }

}

if(start != end){


  if(!is.null(theDOI) & is.null(theScopusID)){

    theResult <- bind_rows(theCites[[1]]$citeColumnTotalXML$citeCountHeader$columnTotal) %>%
                 rename_at(vars(names(.)), ~("citations")) %>%
                 mutate(year = paste0("citations_", as.character(seq(start, end, 1)))) %>%
                 pivot_wider(., names_from = year, values_from = citations) %>%
                 bind_cols(data.frame(doi = theCites[[1]]$`identifier-legend`$identifier[[1]]$`prism:doi`), .)

  } else if(is.null(theDOI) & !is.null(theScopusID)){

    theResult <- bind_rows(theCites[[1]]$citeColumnTotalXML$citeCountHeader$columnTotal) %>%
      rename_at(vars(names(.)), ~("citations")) %>%
      mutate(year = paste0("citations_", as.character(seq(start, end, 1)))) %>%
      pivot_wider(., names_from = year, values_from = citations) %>%
      bind_cols(data.frame(scopus_id = theCites[[1]]$`identifier-legend`$identifier[[1]]$scopus_id), .)

  }


} else {


  if(!is.null(theDOI) & is.null(theScopusID)){

  theResult <- data.frame(citations = theCites[[1]]$citeColumnTotalXML$citeCountHeader$columnTotal) %>%
    mutate(year = paste0("citations_", as.character(seq(start, end, 1)))) %>%
    pivot_wider(., names_from = year, values_from = citations) %>%
    bind_cols(data.frame(doi = theCites[[1]]$`identifier-legend`$identifier[[1]]$`prism:doi`), .)

  } else if(is.null(theDOI) & !is.null(theScopusID)){

    theResult <- data.frame(citations = theCites[[1]]$citeColumnTotalXML$citeCountHeader$columnTotal) %>%
      mutate(year = paste0("citations_", as.character(seq(start, end, 1)))) %>%
      pivot_wider(., names_from = year, values_from = citations) %>%
      bind_cols(data.frame(scopus_id = theCites[[1]]$`identifier-legend`$identifier[[1]]$scopus_id), .)

  }


}

theResult <- theResult %>%
  mutate(citation_extraction_date = str_replace_all(Sys.Date(), "-", "_"))

return(theResult)

}

