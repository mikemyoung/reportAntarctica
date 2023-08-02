# Make Metadata

subject_table <- lapply(10:36, function(scopus_subject_id){


  hdrs = c("Accept" = "application/json",
           "X-ELS-APIKey" = your_api_key)

  subject <- GET(paste0("https://api.elsevier.com/analytics/scival/subjectArea/", scopus_subject_id),
                 add_headers(hdrs)) %>%
    content()

  subjectOutput <- lapply(subject$subjectArea$children, function(x){

    subjectMeta <- data.frame(scopus_subject_child_id = x$id,
                              scopus_subject_child_name = x$name)

  }) %>% bind_rows() %>%
    mutate(scopus_subject_id = subject$subjectArea$id,
           scopus_subject_name = subject$subjectArea$name)


}) %>% bind_rows() %>%
       select(scopus_subject_id, scopus_subject_name, scopus_subject_child_id, scopus_subject_child_name) %>%
       mutate_all(as.character) %>%
       mutate(custom_subject_grouping = case_when(scopus_subject_id == "10" ~ "Multidisciplinary",
                                                  scopus_subject_id == "11" ~ "Life",
                                                  scopus_subject_id == "12" ~ "Social",
                                                  scopus_subject_id == "13" ~ "Life",
                                                  scopus_subject_id == "14" ~ "Social",
                                                  scopus_subject_id == "15" ~ "Physical",
                                                  scopus_subject_id == "16" ~ "Physical",
                                                  scopus_subject_id == "17" ~ "Physical",
                                                  scopus_subject_id == "18" ~ "Social",
                                                  scopus_subject_id == "19" ~ "Physical",
                                                  scopus_subject_id == "20" ~ "Social",
                                                  scopus_subject_id == "21" ~ "Physical",
                                                  scopus_subject_id == "22" ~ "Physical",
                                                  scopus_subject_id == "23" ~ "Physical",
                                                  scopus_subject_id == "24" ~ "Life",
                                                  scopus_subject_id == "25" ~ "Physical",
                                                  scopus_subject_id == "26" ~ "Physical",
                                                  scopus_subject_id == "27" ~ "Health",
                                                  scopus_subject_id == "28" ~ "Life",
                                                  scopus_subject_id == "29" ~ "Health",
                                                  scopus_subject_id == "30" ~ "Life",
                                                  scopus_subject_id == "31" ~ "Physical",
                                                  scopus_subject_id == "32" ~ "Social",
                                                  scopus_subject_id == "33" ~ "Social",
                                                  scopus_subject_id == "34" ~ "Health",
                                                  scopus_subject_id == "35" ~ "Health",
                                                  scopus_subject_id == "36" ~ "Health",
                                                  TRUE ~ scopus_subject_id))


rowNA <- data.frame(scopus_subject_id = NA,
                    scopus_subject_name = "Unclassified",
                    scopus_subject_child_id = NA,
                    scopus_subject_child_name = "Unclassified",
                    custom_subject_grouping = "Unclassified",
                    stringsAsFactors = FALSE)

subject_table <- bind_rows(subject_table, rowNA)

write.csv(subject_table, "data-raw/subjectMetadata.csv", row.names = FALSE)


subjectTable_2022 <- read.csv("data-raw/subjectMetadata.csv") %>% mutate_all(as.character)
use_data(subjectTable_2022, overwrite = TRUE)

subjectGroupingMetadata <- read.csv("data-raw/subjectGroupingMetadata.csv") %>% mutate_all(as.character)
use_data(subjectGroupingMetadata, overwrite = TRUE)




