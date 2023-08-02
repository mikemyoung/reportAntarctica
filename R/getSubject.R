#' getSubject
#'
#' Function that uses data returned from abstract API within getMetadata to get subject metadata
#'
#' @param abstractData Data frame of data prepared by getMetadata.R
#' @param theScopusID Character string with the publications Scopus ID
#' @param theApiKey Character string with your API key
#'
#' @return Subject metadata with the scoups id
#'
#' @import tidyverse
#' @import httr
#'
#' @export




getSubject <- function(abstractData = NULL, theScopusID = NULL, theApiKey){

  hdrs = c("Accept" = "application/json",
           "X-ELS-APIKey" = theApiKey)

  if(!is.null(theScopusID) & is.null(abstractData)){

    while(rlang::is_empty(abstractData)){

    abstractData <- GET(paste0("https://api.elsevier.com/content/abstract/scopus_id/", theScopusID),
                    add_headers(hdrs)) %>%
                    content()

    }

  }


    if(!is.null(abstractData$`abstracts-retrieval-response`$`subject-areas`)){

      publicationSubjects <- lapply(abstractData$`abstracts-retrieval-response`$`subject-areas`$`subject-area`, function(y){

      data.frame(scopus_subject_child_id = ifelse(!is.null(y$`@code`), as.character(y$`@code`), NA))

      }) %>% bind_rows()


    } else {

      publicationSubjects <- data.frame(scopus_subject_child_id = NA)

    }


    publicationSubjects <- left_join(publicationSubjects, subjectTable_2022, by = c("scopus_subject_child_id")) %>%
                           select(custom_subject_grouping, scopus_subject_name, scopus_subject_child_name)


    publicationMetadata <- data.frame(scopus_id = theScopusID,
                                      custom_subject_grouping = paste(unique(publicationSubjects$custom_subject_grouping), collapse = ";"),
                                      scopus_subject_name = paste(unique(publicationSubjects$scopus_subject_name), collapse = ";"),
                                      scopus_subject_child_name = paste(unique(publicationSubjects$scopus_subject_child_name), collapse = ";"),
                                      description = ifelse(!is.null(abstractData$`abstracts-retrieval-response`$coredata$`dc:description`), abstractData$`abstracts-retrieval-response`$coredata$`dc:description`, NA))


  return(publicationMetadata)

}

















