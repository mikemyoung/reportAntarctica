#' getScienceDirect
#'
#' Function used to pull publication records from ScienceDirect API
#'
#' @param theQuery Character string with the ScienceDirect query
#' @param apiKey Character string with your Scopus API key
#'
#' @return Data frame of publications returned by search query
#'
#' @examples
#' getScienceDirect(theQuery = 'doi(10.1016/B978-0-12-819109-5.00008-6)',
#'                  theApiKey = your_api_key)
#'
#'
#' @import tidyverse
#' @import httr
#'
#' @export

getScienceDirect <- function(theQuery = NULL, theApiKey){
  
    if(is.null(theQuery)){
      stop("Please specify a query")
    }
    
    theResult <- NULL
    
    hdrs = c("Accept" = "application/json",
             "X-ELS-APIKey" = theApiKey)
    
    
    while(rlang::is_empty(theResult)){
        
        theResult <- GET(paste0('https://api.elsevier.com/content/metadata/article?query=', theQuery),
                         add_headers(hdrs)) %>%
          content()
        
        theList <- list(theResult)
        
      }
      
      thePublications <- lapply(theList, function(x){
        
        thePublications_y <- lapply(x$`search-results`$entry, function(y){
          
          publications <- data.frame(cover_date = ifelse(!is.null(y$`prism:coverDate`), y$`prism:coverDate`, NA),
                                     title = ifelse(!is.null(y$`dc:title`), y$`dc:title`, NA),
                                     publication_name = ifelse(!is.null(y$`prism:publicationName`), y$`prism:publicationName`, NA),
                                     open_access = ifelse(!is.null(y$openaccess), y$openaccess, NA),
                                     aggregation_type = ifelse(!is.null(y$pubType), y$pubType, NA),
                                     eid = ifelse(!is.null(y$eid), y$eid, NA),
                                     doi = ifelse(!is.null(y$`prism:doi`), y$`prism:doi`, NA),
                                     starting_page = ifelse(!is.null(y$`prism:startingPage`), y$`prism:startingPage`, NA),
                                     ending_page = ifelse(!is.null(y$`prism:endingPage`), y$`prism:endingPage`, NA))
          
          
          theAuthors <- lapply(y$authors$author, function(z){authors <- data.frame(author_name = z$`$`)}) 
          
          authors <- data.frame(author_name = do.call("paste", c(theAuthors, sep = "., ")))
          
          publications <- bind_cols(publications, authors)
          
          return(publications)
          
        }) %>% bind_rows()
        
        return(thePublications_y)
        
      }) %>% bind_rows()%>%
             mutate_all(as.character) %>%
             mutate(year = substr(cover_date, 1, 4)) %>%
             select(year,
                    cover_date,
                    title,
                    author_name,
                    doi,
                    publication_name,
                    open_access,
                    aggregation_type,
                    starting_page,
                    ending_page,
                    eid)
      

      
      
    return(thePublications)
      
}