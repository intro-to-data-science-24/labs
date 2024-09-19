## ----setup, include=FALSE-------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

pacman::p_load(tidyverse, purrr)


## ---- echo = F------------------------
library(nycflights13)
#flights 
#planes


## -------------------------------------
left_join(flights, planes) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, type, model)%>%
  head(3) ## Just to save vertical space in output


## -------------------------------------
left_join(
  flights,
  planes %>% rename(year_built = year), ## Not necessary w/ below line, but helpful
  by = "tailnum" ## Be specific about the joining column
  ) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, year_built, type, model) %>%
  head(3) 


## ----join3----------------------------
left_join(
  flights,
  planes, ## Not renaming "year" to "year_built" this time
  by = "tailnum"
  ) %>%
  select(contains("year"), month, day, dep_time, arr_time, carrier, flight, tailnum, type, model) %>%
  head(3)


## ---- include=FALSE-------------------
pacman::p_load(RSQLite, DBI, bigrquery, dbplyr, nycflights13)


## ---- eval=FALSE----------------------
## con <- DBI::dbConnect(RMariaDB::MariaDB(),
##                       host = "database.rstudio.com",
##                       user = "tom",
##                       password = rstudioapi::askForPassword("Tom's password")
## )
## 


## -------------------------------------
# set up connection with DBI and RSQLite
con <- dbConnect(RSQLite::SQLite(), ":memory:")


## -------------------------------------
summary(con)


## -------------------------------------

# upload local data frame into remote data source; here: database
copy_to(
  dest = con, 
  df = nycflights13::flights, 
  name = "flights")



## -------------------------------------

copy_to(
  dest = con, 
  df = nycflights13::flights, 
  name = "flights",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum",
    "dest"
  ),
  overwrite = T # throws error as table already exists
)



## -------------------------------------
DBI::dbListTables(con)


## -------------------------------------
# generate reference table from the database
flights_db <- tbl(con, "flights")
flights_db 


## -------------------------------------
# perform various queries
flights_db %>% select(year:day, dep_delay, arr_delay)



## -------------------------------------

flights_db %>% filter(dep_delay > 240)


## -------------------------------------
flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time))


## ----echo=TRUE, results='hide', fig.keep='all'----

flights_db %>% 
  filter(distance > 75) %>%
  group_by(origin, hour) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(hour, delay, color = origin)) + geom_line()



## -------------------------------------
copy_to(
  dest = con, 
  df = nycflights13::planes, 
  name = "planes",
  temporary = FALSE, 
  indexes = "tailnum"
)

copy_to(
  dest = con, 
  df = nycflights13::airlines, 
  name = "airlines",
  temporary = FALSE, 
  indexes = "carrier"
)

copy_to(
  dest = con, 
  df = nycflights13::airports, 
  name = "airports",
  temporary = FALSE, 
  indexes = "faa"
)

copy_to(
  dest = con, 
  df = nycflights13::weather, 
  name = "weather",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day", "hour", "origin")
  )
)


## -------------------------------------
dbListTables(con)


## -------------------------------------
planes_db = tbl(con, 'planes')
left_join(
  flights_db,
  planes_db %>% rename(year_built = year),
  by = "tailnum" ## Important: Be specific about the joining column
) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum,
         year_built, type, model) 



## ---- warning=TRUE--------------------

tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>% 
  arrange(desc(delay)) %>%
  filter(n > 100)



## -------------------------------------
nrow(tailnum_delay_db)


## ---- error=TRUE----------------------
tail(tailnum_delay_db)


## -------------------------------------

tailnum_delay_db %>% show_query()


## -------------------------------------
tailnum_delay <- tailnum_delay_db %>% collect()
tailnum_delay


## -------------------------------------
sql_query <- "SELECT * FROM flights WHERE dep_delay > 240.0 LIMIT 5"
dbGetQuery(con, sql_query)


## -------------------------------------

DBI::dbDisconnect(con)



## ---- eval=F--------------------------
## con <- dbConnect(
##   bigrquery::bigquery(),
##   project = "publicdata",
##   dataset = "samples",
##   billing = google_cloud_project_name # This will tell Google whom to charge
## )

