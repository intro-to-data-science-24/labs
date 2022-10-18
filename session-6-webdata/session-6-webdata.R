## ----setup, include=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## <!DOCTYPE html>

##   <html>

##     <head>

##       <title id=1>First HTML</title>

##     </head>

##   <body>

##       <div>

##           <h1>

##             I am your first HTML file!

##           </h1>

##       </div>

##   </body>

## </html>


## ---- fig.align='center', echo=F, out.width = "90%"----
knitr::include_graphics("pics/inspect.png")


## ---- message=F--------------------------------
library(rvest)
library(stringr)


## ----------------------------------------------
parsed_doc <- read_html("http://www.r-datacollection.com/materials/ch-4-xpath/fortunes/fortunes.html") 


## ----------------------------------------------
html_elements(parsed_doc, xpath = "/html/body/div/p/i")


## ---- fig.align='center', echo=F, out.width = "90%"----
knitr::include_graphics("pics/Xpath.png")


## ----------------------------------------------
html_elements(parsed_doc, xpath = "//p/preceding-sibling::h1")


## ----------------------------------------------
html_elements(parsed_doc, xpath = "//p[2]")


## ----------------------------------------------
html_elements(parsed_doc, xpath = "//h1[contains(., 'Rolf')] | //h1[contains(., 'Robert')]" )


## ----------------------------------------------
html_elements(parsed_doc, xpath = "//a[@href]" )


## ----------------------------------------------
x <- c("apple", "banana", "pear")

str_detect(x, "e")


## ----------------------------------------------
str_extract_all(x, 'a')


## ----------------------------------------------
str_replace_all(x, c("a" = "A", "b" = "B", "p" = "P"))


## ----------------------------------------------
str_locate(x, 'a')


## ---- fig.align='center', echo=F, out.width = "90%"----
knitr::include_graphics("pics/xkcd_regex.png")


## ----------------------------------------------
str_view(x, "a.")


## ----------------------------------------------
str_view(x, "p+")


## ----------------------------------------------
y <- "1. A small sentence. - 2. Another tiny sentence."
str_view_all(y, "\\s(\\w{4,5})\\s")
str_view_all(y, "\\d\\W")


## ----------------------------------------------
str_view(x, "^a|b")


## ----------------------------------------------
fruits <- c("apple", "orange", "pear")
fruit_match <- str_c(fruits, collapse = "|")
str_subset(stringr::sentences, fruit_match) %>% head(3)


## ----------------------------------------------
z <- c('abc?defg', '123456?89', "[?.{!]\\")
str_view(z, '\\?')


## ----------------------------------------------
str_detect(stringr::words, '^\\w{2}y$') %>% sum


## ----------------------------------------------
str_subset(stringr::words, "[^e]ed$")


## ----------------------------------------------
hertie_emails <- c('456123@students.hertie-school.org', 'h.simpson@students.hertie-school.org')
str_view(hertie_emails, '(^\\d{6}|^\\w\\.\\w{1,})@students\\.hertie-school\\.org')


## ----------------------------------------------
raw_data <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5553642Dr. Julius Hibbert"

str_view_all(raw_data, "[[:alpha:]., ]{2,}")

str_view_all(raw_data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}")


