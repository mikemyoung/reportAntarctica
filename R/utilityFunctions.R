
getFinancialYear <- function(theDate){
  
  tryCatch(as.Date(theDate),  
           error = function(err) {stop(message("Please provide date format (yyyy-mm-dd)"))})  
  
  theMonth <- as.numeric(substr(theDate, 6, 7))
  
  if(theMonth < 7){
    
    fy <- paste0(as.numeric(substr(theDate, 1, 4)) -1, "_", substr(theDate, 3, 4))
    
  } else {
    
    fy <- paste0(substr(theDate, 1, 4), "_", as.numeric(substr(theDate, 3, 4)) + 1)
    
  }
  
  return(fy)
}



identifyCollaboration <- function(theCountries){
  
  if(!is.na(theCountries)){
  
  country_list <- str_split(theCountries, "--")
  
  output <- lapply(country_list[[1]], function(x){
    
    data.frame(country = unique(str_split(x, ".,")[[1]]))
    
  }) %>% bind_rows() 
  
  
  nz <- output %>% 
    filter(country == "New Zealand")
  
  
  if(!FALSE %in% (unique(output$country) == "New Zealand")){
    theResult <- "domestic "
  } else if(nrow(nz) > 1 && TRUE %in% unique(output$country) != "New Zealand") {
    theResult <- "domestic & international"
  } else if (nrow(nz) > 0  && TRUE %in% unique(output$country) != "New Zealand") {
    theResult <- "international"
  } else {
    theResult <- "undefined"
  }
  
  } else {
    
    theResult <- NA
    
  }
  
  
  return(theResult)
  
}



getAuthorPubs <- function(thePublications, nameList){

  # Break-up date to be merged onto names list
  thePubs <- thePublications %>%
    mutate(authors = author_names,
           author_names = str_replace_all(author_names, "\\.;", ";"),
           author_names = str_replace_all(author_names, "\\.,", "\\."),
           author_names = str_replace_all(author_names, ";", "\\.,"),
           author_names = str_replace_all(author_names, " &", ",")) %>%
    mutate(author_names = strsplit(as.character(author_names), "\\.,")) %>%
    unnest(author_names) %>%
    mutate(author_names = ifelse(str_detect(author_names, " and "), strsplit(author_names, " and "), author_names)) %>%
    unnest(author_names) %>%
    mutate(author_names = str_trim(author_names, "both")) %>%
    filter(author_names != "") %>%
    select(author_names, authors, title, eid)


  # Clean data
  thePubs <- thePubs %>%
    mutate(author_names = stri_trans_general(author_names, "Latin-ASCII")) %>%
    rowwise() %>%
    mutate(author_names = addComma(author_names),
           author_names = addComma_fullName(author_names),
           author_names = removeStop(author_names),
           author_names = removeSpace(author_names),
           author_names_original = author_names,
           author_names = trimFirstName(author_names),
           author_names = removeLeadInitial(author_names),
           name_join = str_to_upper(author_names),
           name_join = str_replace_all(name_join, "-", " "),
           name_join = str_trim(name_join, "both")) %>%
    ungroup() %>%
    select(author_names_original, author_names, name_join, title, authors, eid)




  # IMPT. COUNTING PUBLICATIONS - using eid
  # It doesn't matter if there are multiple matches where individuals
  # with the same join name appear on the same publication.
  # We're counting publications so we only need one of them to match with a
  # publication to end up being counted
  # So,it doesn't matter if one of them is being dropped of with the distinct(eid)

  theMaster <- full_join(nameList, thePubs, by = c("name_join")) %>%
    full_join(thePublications, by = "eid")


  withPubs <- full_join(nameList, thePubs, by = c("name_join")) %>%
    drop_na()


  noPubs <-  full_join(nameList, thePubs, by = c("name_join")) %>%
    filter(is.na(passenger_first_name) | is.na(author_names))


  nrow(withPubs) + nrow(noPubs) == nrow(theMaster) #TRUE!



  withPubs <- left_join(withPubs, thePublications, by = "eid") %>%
    select(-title.x, -author_names.x) %>%
    rename(title = "title.y",
           author_names = "author_names.y")



  soiPubs <- withPubs %>% distinct(eid, .keep_all = TRUE)
  nrow(soiPubs)


  return(soiPubs)

}


flipNames <- function(authorString){

  authorString <- str_split(authorString, ".,")[[1]]

  authorString2 <- c()
  errorLog <- data.frame()
  cnt <- 0

  for(i in 1:length(authorString)){

    tempString <- str_split(authorString[i], " ")[[1]]

    if(length(tempString) < 3){
      cnt <- cnt + 1
      errorLog[cnt,1] <- i
      next
    }

    if(length(tempString) == 4){
      tempString <- str_split(paste0(tempString[2], ",", tempString[4]), ",")[[1]]

    } else if(length(tempString) == 3 && tempString[1] != ""){
      tempString <- str_split(paste0(tempString[3], ",", tempString[4]), ",")[[1]]

    }

    for(j in 1:length(tempString)){

      if(tempString[j] == ""){
        next

      } else if(j == 2){

        theInitial <- strsplit(tempString[j], "")[[1]][1]

      } else if(j == length(tempString)){

        lastName <- tempString[j]
        authorString2 <- paste0(authorString2, "., ", tempString[j],", ", theInitial)

      }

    }

  }

  return(authorString2)
}



reverseInitials <- function(authorString){

  tempString_a <- strsplit(authorString, "")[[1]][1]

  authorString <-  paste0(paste(strsplit(authorString, "")[[1]][2], collapse = ""),
                          paste(strsplit(authorString, "")[[1]][1], collapse = ""))

  return(authorString)
}


createInitials <- function(authorString){

  authorString <- strsplit(authorString, ",")

  firstInitial <- strsplit(authorString[[1]][1], "")[[1]][1]
  secondInitial <- authorString[[1]][2]

  authorString <- str_remove_all(paste(firstInitial, secondInitial, collapse = ""), " ")
  return(authorString)

}



sentenceCase <- function(authorString){

newString <- c()
authorString <- strsplit(authorString, ",")[[1]]
theSeq <- seq(1, length(authorString), by = 2)

  for(i in theSeq){

  surName <- str_to_sentence(authorString[i])

  newString <- paste0(newString, str_trim(surName, "left"), ", ", str_trim(authorString[i+1], "left"), ", ")

}

return(newString)

}

trimFirstName <- function(authorString){

  authorString <- strsplit(authorString, "")[[1]]

  for(i in 1:length(authorString)){

    if(i == length(authorString) - 1){
      break
    }

    currentChar <- authorString[i]
    nextChar <- authorString[i+1]

    if(currentChar == "," && nextChar == " "){

      authorString <-  paste(authorString[1:(i+2)], collapse = "")
      break
    }

  }

  authorString <-  paste(authorString, collapse = "")

  return(authorString)

}


removeWhite <- function(authorString){

  authorString <- strsplit(authorString, "")[[1]]


  for(i in 1:length(authorString)){

    if(i == length(authorString) - 1){
      break
    }

    currentChar <- authorString[i]
    nextChar <- authorString[i+1]

    if(is.na(currentChar) & is.na(nextChar)){

      break

      } else if(currentChar == " " && nextChar == " "){

        authorString <-  paste0(paste(authorString[1:i], collapse = ""), paste(authorString[(i+2):length(authorString)], collapse = ""))
        break
    }

  }

  authorString <-  paste(authorString, collapse = "")

  return(authorString)

}


mcCapitalise <- function(authorString){

  authorString <- strsplit(authorString, "")[[1]]

  if(authorString[1] == "M" && authorString[2] == "c"){

      authorString[3] <- str_to_upper(authorString[3])

  }

  authorString <- paste(authorString, collapse = "")

  return(authorString)

}





addComma <- function(authorString){

  if(any(str_detect(authorString, ","))){
    return(authorString)
  }

  authorString <- strsplit(authorString, "")[[1]]

  for(i in 1:length(authorString)){

    if(i == 1){
      next
    } else if(i == length(authorString)){
      break
    }

    currentChar <- authorString[i]
    charPlus1 <- authorString[i + 1]
    charPlus2 <- authorString[i + 2]
    charPlus3 <- authorString[i + 3]


    if((str_detect(currentChar,"[a-z]") | str_detect(currentChar,"[A-Z]")) && charPlus1 == " " &&
       ((str_detect(charPlus2, "[A-Z]") | str_detect(charPlus2, "[a-z]"))) &&
       (!(str_detect(charPlus3, "[A-Z]") | str_detect(charPlus3, "[a-z]")) | is.na(charPlus3))){

      tempString_A <- authorString[1:i]
      tempString_B <- authorString[(i+1):length(authorString)]

      authorString <- paste0(paste(tempString_A, collapse =""), ",", paste(tempString_B, collapse =""))

      return(authorString)

     }

  }

  authorString <- paste(authorString, collapse ="")

  return(authorString)

}


addComma_fullName <- function(authorString){

  if(any(str_detect(authorString, ","))){
    return(authorString)
  }

  authorString <- strsplit(authorString, "")[[1]]

  for(i in 1:length(authorString)){

    if(i == 1){
      next
    } else if(i == length(authorString)){
      break
    }

    currentChar <- authorString[i]
    charPlus1 <- authorString[i + 1]
    charPlus2 <- authorString[i + 2]
    charPlus3 <- authorString[i + 3]


    if((str_detect(currentChar,"[a-z]") | str_detect(currentChar,"[A-Z]")) && charPlus1 == " " &&
       ((str_detect(charPlus2, "[A-Z]") | str_detect(charPlus2, "[a-z]"))) &&
       (str_detect(charPlus3, "[A-Z]") | str_detect(charPlus3, "[a-z]"))){

      tempString_A <- authorString[1:i]
      tempString_B <- authorString[(i+1):length(authorString)]

      authorString <- paste0(paste(tempString_A, collapse =""), ",", paste(tempString_B, collapse =""))

      return(authorString)

    }

  }

  authorString <- paste(authorString, collapse ="")

  return(authorString)

}


removeLeadInitial <- function(authorString){

  authorString <- strsplit(authorString, "")[[1]]

  currentChar <- authorString[1]
  charPlus1 <- authorString[2]


  if((str_detect(currentChar,"[a-z]") | str_detect(currentChar,"[A-Z]")) && charPlus1 == "."){

    authorString <- paste(authorString[3:length(authorString)], collapse = "")

  } else {

    authorString <- paste(authorString, collapse = "")
  }

  return(authorString)

}

checkScopusError <- function(authorString){
  
  scopusError = NULL
  authorString <- strsplit(authorString, "")[[1]]
  
  for(i in 1:length(authorString)){
    
    if(TRUE %in% authorString[i] == "." & authorString[i+1] == " " | authorString[i] == "," & authorString[i+1] == " "){
      scopusError = TRUE
      
      #authorString <- paste(authorString, collapse = "")
      #output <- list(error = TRUE, authors = authorString)
      
    }
    
  }
  
  if(!is.null(scopusError)){
    return(TRUE)
  
} else {
  authorString <- paste(authorString, collapse = "")
  return(FALSE)
  
}
  
}


removeSecondInitial <- function(authorString){
  
  if(!any(str_detect(authorString, "\\."))){
    return(authorString)
  }
  
  authorString <- strsplit(authorString, "")[[1]]
  
  for(i in 1:length(authorString)){
    
    if(authorString[i] == " " & (str_detect(authorString[i+1], "[A-Z]") & str_detect(authorString[i+2], "[\\.]"))){
      
      authorString <- paste(authorString[1:(i+1)], collapse = "")
      
      break
      
    } else if(i == length(authorString)){
      
      authorString <- paste(authorString, collapse = "")
      
    }
  }

  return(authorString)
}


getPubName <- function(authorString){

  authorString <- strsplit(authorString, "")[[1]]


  for(i in 1:length(authorString)){

    if(authorString[i] == " " & (str_detect(authorString[i+1], "[a-z]") |  str_detect(authorString[i+1], "[A-Z]"))){

      last_name <- paste(paste0(authorString[(i+1):length(authorString)]), collapse = "")

    }
  }


  authorString <- paste0(last_name, ", ", authorString[1])

  return(authorString)

}

addStop <- function(authorString){
  
  authorString <- strsplit(authorString, "")[[1]]
  
  if(authorString[length(authorString)] != "."){
    
    authorString <- paste0(paste(authorString, collapse = ""), ".")
  
    } else {
    
    authorString <- paste(authorString, collapse = "")
    
  }

  return(authorString)
  
}


removeStop <- function(authorString){

  if(!any(str_detect(authorString, "."))){
    return(authorString)
  }

  authorString <- strsplit(authorString, "")[[1]]

  if(authorString[length(authorString)] == "."){

    authorString <- authorString[1:(length(authorString)-1)]

  }

  authorString <- paste(authorString, collapse ="")

  return(authorString)

}


removeSpace <- function(authorString){

  if(!any(str_detect(authorString, "."))){
    return(authorString)
  }

  authorString <- strsplit(authorString, "")[[1]]

  for(i in 1:length(authorString)){

    currentChar <- authorString[i]
    charPlus1 <- authorString[i + 1]


    if(currentChar == "." && charPlus1 == " "){

      tempString_a <- authorString[1:i]
      tempString_b <- authorString[(i+2):(length(authorString))]

      authorString <- paste0(paste(tempString_a, collapse =""), paste(tempString_b, collapse =""))

      return(authorString)
    }

  }

  authorString <- paste(authorString, collapse ="")

  return(authorString)

}




attachInitials <- function(surname, firstName_initial, middleName_initial){

  if(is.na(firstName_initial) & is.na(middleName_initial)){

    authorFullName <- surname

  } else if(!is.na(firstName_initial) & is.na(middleName_initial)) {

    authorFullName <- paste0(surname,", ", firstName_initial)

  } else if(!is.na(firstName_initial) & !is.na(middleName_initial)){

    authorFullName <- paste0(surname,", ", firstName_initial, ".", middleName_initial)
  }

  return(authorFullName)

}
