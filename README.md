# reportAntarctica

Access, aggregate and report Scopus bibliometrics with R (minimal R experience required).


# Installation

Install reportAntarctica from github with:

``` 
install.packages("devtools")
library(devtools)
devtools::install_github("mikemyoung/reportAntarctica")
```


# API Key

Access to the Scopus APIs require an API key. Please contact Scopus Support to obtain a key (https://service.elsevier.com/app/overview/scopus/).


# Institutional Access

Access to Scopus APIs require the IP address of the institution subscribed to Scopus. If you are offsite, you will need to connect to your institution's IP address with a VPN.


# Citation API

Access to citation data is requested through Scopus Support (https://service.elsevier.com/app/overview/scopus/).

# Example

```

# 2023 Antarctica NZ publications - see https://www.scopus.com/search/form.uri?display=advanced for advanced search guidance

antnz <- makePublicationTable(theQuery = 'FUND-ALL("Antarctica New Zealand" OR "Antarctica NZ" OR "Scott Base" OR NZARI OR "New Zealand Antarctic Research Institute") AND PUBYEAR IS 2023',
                              apiKey = yourApiKey)
                              
```

# Build NZ Authored Antarctic and Southern Ocean Science Publications (1996 - 2021)

Build a list of publications featuring New Zealand Antarctic Science from 1996 - 2021 with at least one New Zealand author.

Includes custom subject grouping:

  1) Biosphere - Terrestrial
  2) Bioshpere - Marine
  3) Bioshpere
  4) Atmosphere
  5) Cryosphere
  6) Geosphere
  7) Oceans
  8) Space
  9) Human


# Build a Relational Database

Transform publications into a relational database with:

1) Author table
2) Publication table
3) Affiliation table


# Example

```
# Build the NZ Antarctic Science Publications
nzAntDatabase <- makeAntarcticDatabase(apiKey = yourApiKey)
                                      
# Write the publications to Excel                                 
write.xlsx(nzAntDatabase, "antsciDatabase.xlsx")  

# Build a relational database with metadata
db <- makeRelationalDatabase(nzAntDatabase)

View(db$author_table)
View(db$publication_table)
View(db$affiliation_table)

```

# Example

```
# Add your query - identical syntax used when searching via Scopus Advanced Search
query = '(TITLE-ABS-KEY(antarct* OR "southern ocean" OR "ross sea" OR "amundsen sea" OR "weddell sea" OR "ross ice shelf") AND NOT TITLE-ABS-KEY(candida OR "except antarctica" OR "not antarctica")) AND PUBYEAR IS 2021 AND AFFILCOUNTRY("New Zealand")'



# Run the query
theOutput <- makePublicationTable(theQuery = query,
                                  apiKey = your_api_key,
                                  articleMetadata = TRUE,
                                  citations = TRUE,
                                  metrics = TRUE)
                                  
# Write the bibliometric metadata to Excel                                 
write.xlsx(theOutput, "yourBibliometricData.xlsx")       
```
