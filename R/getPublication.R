#' getPublication
#'
#' Function used to pull publication records from Scopus Search API
#'
#' @param theQuery Character string with the Scopus query
#' @param apiKey Character string with your Scopus API key
#'
#' @return Data frame of publications returned by search query
#'
#' @examples
#' getPublication(theQuery = 'TITLE-ABS-KEY("Scott Base) AND PUBYEAR IS 2021',
#'                theApiKey = your_api_key)
#'
#'
#' @import tidyverse
#' @import httr
#'
#' @export


getPublication <- function(theQuery = NULL, theApiKey){

  if(is.null(theQuery)){
    stop("Please specify a query")
  }

  theResult <- NULL

  hdrs = c("Accept" = "application/json",
           "X-ELS-APIKey" = theApiKey)


 if(!is.null(theQuery)){

    while(rlang::is_empty(theResult)){

      theResult <- GET(paste0('https://api.elsevier.com/content/search/scopus?query=', theQuery, '&count=200'),
                   add_headers(hdrs)) %>%
                   content()

      theList <- list(theResult)

    }


    iterations <- as.numeric(theList[[1]]$`search-results`$`opensearch:totalResults`)/200

    message(paste0("Returned ", theList[[1]]$`search-results`$`opensearch:totalResults`, " results"))

    tryCatch( {
    
    if(iterations > 1){

        message("Retrieved publications: 1 to 200")

        iterations <- ceiling(iterations - 1)

        for(i in 1:iterations){

          startIndex <- 200*i

          if(i < iterations){
            countIndex <- 200
          } else {
            countIndex <- as.numeric(theList[[1]]$`search-results`$`opensearch:totalResults`) - (iterations*200)
          }

          message(paste0("Retrieving publications: ", startIndex + 1, " to ", startIndex + countIndex))

          theResult <- NULL

          while(rlang::is_empty(theResult)){
            
            

            theResult <- GET(paste0('https://api.elsevier.com/content/search/scopus?query=', theQuery, '&count=', countIndex, '&start=', startIndex),
                                  add_headers(hdrs)) %>%
                                  content()

          }

          theList[[i+1]] <- theResult

        }

    }
      
    },
    
    error = function(e) { 
      stop("INVALID CALL: Check parameter inputs. Check connection to an institutional IP Address")
      })

  }

        thePublications <- lapply(theList, function(x){

          thePublications_y <- lapply(x$`search-results`$entry, function(y){

            publications <- data.frame(cover_date = ifelse(!is.null(y$`prism:coverDate`), y$`prism:coverDate`, NA),
                                       title = ifelse(!is.null(y$`dc:title`), y$`dc:title`, NA),
                                       total_citations = ifelse(!is.null(y$`citedby-count`), y$`citedby-count`, NA),
                                       publication_name = ifelse(!is.null(y$`prism:publicationName`), y$`prism:publicationName`, NA),
                                       aggregation_type = ifelse(!is.null(y$`prism:aggregationType`), y$`prism:aggregationType`, NA),
                                       subtype_description = ifelse(!is.null(y$subtypeDescription), y$subtypeDescription, NA),
                                       volume = ifelse(!is.null(y$`prism:volume`), y$`prism:volume`, NA),
                                       article_number = ifelse(!is.null(y$`article-number`), y$`article-number`, NA),
                                       issue_identifier = ifelse(!is.null(y$`prism:issueIdentifier`), y$`prism:issueIdentifier`, NA),
                                       page_range = ifelse(!is.null(y$`prism:pageRange`), y$`prism:pageRange`, NA),
                                       open_access = ifelse(!is.null(y$openaccess), y$openaccess, NA),
                                       eid = ifelse(!is.null(y$eid), y$eid, NA),
                                       doi = ifelse(!is.null(y$`prism:doi`), y$`prism:doi`, NA),
                                       scopus_id = ifelse(!is.null(y$`dc:identifier`), str_remove_all(y$`dc:identifier`, "SCOPUS_ID:"), NA),
                                       publication_id = ifelse(!is.null(y$`source-id`), y$`source-id`, NA))


            }) %>% bind_rows()

          return(thePublications_y)

        }) %>% bind_rows()




        if(!all(is.na(thePublications))){

        thePublications <- thePublications %>%
          mutate(citation_extraction_date = Sys.Date())

        } else {

          thePublications <- thePublications %>%
            mutate(citation_extraction_date = NA)

        }

  return(thePublications)

}
