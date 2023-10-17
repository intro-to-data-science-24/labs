# load packages
library(rvest)
library(stringr)
library(tidyverse)


# Example 1: Wikipedia (text)  -------------------------------------------------
parsed_url <- read_html("https://en.wikipedia.org/wiki/Cologne")

parsed_url |> 
  html_element(xpath = '//p[(((count(preceding-sibling::*) + 1) = 170) and parent::*)]') |> 
  html_text()


# Example 2: New York Public Library -------------------------------------------
nypl_url <- "https://www.nypl.org/books-more/recommendations/best-books/adults?year=2021"
nypl100 <- read_html(nypl_url)

title <- nypl100 |> 
  html_elements(xpath = '//ul/li/div/div/h4') |> 
  html_text2()

author <- nypl100 |> 
  html_elements(css = '.spbb-card__byline--grid') |> 
  html_text2()

summary <- nypl100 |> 
  html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "spbb-card__description--grid", " " ))]') |> 
  html_text2()

data.frame(title, author, summary) |> head(3)


# Example 3: Wikipedia (tables)  -----------------------------------------------
url_p <- read_html("https://en.wikipedia.org/wiki/Germany")
deu_table <- html_table(url_p, header = TRUE, fill = TRUE) |> 
  pluck(4)
deu_table |> 
  select(State, Capital) |> 
  head(5)


# Example 4: Journal of Statistical Software (multiple pages) ------------------
#browseURL("http://www.jstatsoft.org/issue/archive") # inspect webpage

baseurl <- "http://www.jstatsoft.org/article/view/v" # base url (stays constant across urls)
volurl <- paste0("0", 1:99) # vector of volume numbers
volurl[1:9] <- paste0("00", 1:9) 
brurl <- paste0("0", 1:9) # vector of article numbers

# list of unique urls
urls_list <- cross2(volurl, brurl) |> 
  map_chr(~paste0(baseurl, .[[1]], 'i', .[[2]])) 

# list of unique file names
names <- cross2(volurl, brurl) |> 
  map_chr(~paste0(.[[1]], '_', .[[2]], '.html'))

tempwd <- here::here("session-07-web-scraping/data/jstatsoftStats") # set temporary working directory
dir.create(tempwd, recursive = TRUE) # create folder structure if filepath doesn't already exist
setwd(tempwd) # set working directory

folder <- paste0(tempwd, "/html_articles/") # where to store data
dir.create(folder, recursive = TRUE) # create folder structure if filepath doesn't already exist

# download html files to local device
for (i in seq_along(urls_list)) {
  
  # only update, don't replace
  if (!file.exists(paste0(folder, names[i]))) {
    
    # skip article when we run into an error
    tryCatch(
      download.file(urls_list[i], destfile = paste0(folder, names[i])),
      error = function(e)
        e
    )
    
    # don't kill their server --> be polite!
    Sys.sleep(runif(1, 0, 1))
    
  }
}

list_files <- list.files(folder, pattern = "0.*") # list of downloaded files
list_files_path <- list.files(folder, pattern = "0.*", full.names = TRUE) # full.names=TRUE shows entire filepath
length(list_files) # number of downloaded files

# initialize empty char vecs to store output for each variable
authors <- character()
title <- character()
datePublish <- character()

# for loop to extract info from html files
for (i in seq_along(list_files_path)) {
  
  # parse html file
  html_out <- read_html(list_files_path[i])
  
  # extract author info
  authors[i] <- html_out |>
    html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "authors_long", " " ))]//strong') |>
    html_text2()

  # extract title info
  title[i] <- html_out |>
    html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "page-header", " " ))]') |>
     html_text2()
 
  # extract publishing date info
  datePublish[i] <- html_out |>
     html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "article-meta", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "row", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "col-sm-8", " " ))]') |>
     html_text2()
 
 }
 
# inspect data
authors[1:3]
title[1:3]
datePublish[1:3]
 
# create a data frame
dat <- data.frame(authors = authors, title = title, datePublish = datePublish)
dim(dat)
dat
 

# Example 5: Star Wars API -----------------------------------------------------
# load packages
library(httr)
library(jsonlite)
library(xml2)

# define base url
baseurl <- "https://swapi.dev/api/"

# query Star Wars films
query <- 'films'
GET(paste0(baseurl, query)) |>
  content(as = 'text') |>
  fromJSON() |> 
  pluck(4) |> 
  as.data.frame()

# query Star Wars characters named 'skywalker'
query <- 'people/?search=skywalker'
GET(paste0(baseurl, query)) |>
  content(as = 'text') |>
  fromJSON() |> 
  pluck(4) |> 
  as.data.frame()

# check query type
query <- 'starships/?search=death/?format=wookiee'
GET(paste0(baseurl, query)) |>
  http_type()

## ---- eval=FALSE-------------------
## baseurl <- 'https://api.congress.gov/v3/'
## 
## query <- 'nomination/115/2259/actions'
## 
## GET(paste0(baseurl, query),
##     authenticate(user = Sys.getenv('US.GOV_API'), password = '')) |>
##   content(as = 'text') |>
##   fromJSON() |>
##   pluck(1) |>
##   as.data.frame()


## ---- eval=FALSE-------------------
## query <- paste0('summaries/117/hr?fromDateTime=2022-10-01T00:00:00Z&toDateTime=2022-11-01T00:00:00Z&sort=updateDate+desc')
## 
## GET(paste0(baseurl, query), query = list(api_key = Sys.getenv('US.GOV_API'))) |>
##   content(as = 'text') |>
##   fromJSON() |>
##   pluck(3) |>
##   as.data.frame()


## ---- eval=FALSE-------------------
## query <- 'member/O000167/sponsored-legislation.xml'
## 
## GET(paste0(baseurl, query),
##     add_headers('X-Api-Key' = Sys.getenv('US.GOV_API'))) |>
##   content(as = 'text') |>
##   read_xml() |>
##   xml_find_all('//sponsoredLegislation//title') |> xml_text()

