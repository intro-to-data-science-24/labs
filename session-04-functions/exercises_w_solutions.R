# load packages
pacman::p_load(tidyverse)

# Keep in mind, there are multiple ways to solve problems with R. The solutions 
# presented below may not be the only way to get the desired result.


# Exercise 1 ####
# Way 1: base R solution
get_mode <- function(v) {
  
  v <- na.omit(v)
  uniq_v <- unique(v)
  
  return(uniq_v[which.max(tabulate(match(v, uniq_v)))])
  
}

get_mode(study$age) #example

# Way 2: tidyverse solution
get_mode <- function(v) {
  
  out <- v |> 
    janitor::tabyl() |> 
    filter(n == max(n)) |> 
    pull(v)
  
  return(out)
  
}

get_mode(study$age) #example

# Way 3:
get_mode <- function(x){
freq_table <- table(x)
my_mode <- as.numeric(names(which.max(freq_table)))
return(my_mode)
}

get_mode(study$age) #example

# Exercise 2 ####
get_mode(study$bmi_3cat) 


# Exercise 3 ####

# Part 1: fix 'netural' typo

# Way 1: base R solution
study$emotions[study$emotions == 'netural'] <- 'neutral'

# Way 2: tidyverse solution
study <- study |> 
  mutate(
    #emotions = stringr::str_replace(emotions, "netural", "neutal") # using stringr package (optional)
    emotions = ifelse(emotions == "netural", "neutral", emotions)
    ) 

# Part 2: write emoticon function

# Way 1:
replace_w_emoticons <- function(x) {
  if (x == "happy") {
    ":)"
  } else if (x == "sad") {
    ":("
  } else {
    ":/"
  }
}

#replace_w_emoticons(study$emotions) - when you run this, you get an error saying the condition has length >1. This is because if and else statements are designed to work with scalar (single) values or logical vectors with a length of 1.

# therefore, we vectorize the function to create an emoticons column

## base R solution:
replace_w_emoticons_vectorized <- Vectorize(replace_w_emoticons) 
study$emoticons <- study$emotions |> replace_w_emoticons() 

#OR

#we use the map function
map_vec(study$emotions, replace_w_emoticons)

## tidyverse solutions:
study <- study |> 
  rowwise() |> # use rowwise(), otherwise the function will only evaluate the first argument
  mutate(emoticons = replace_w_emoticons(emotions))

study <- study |> 
  mutate(emoticons2 = map_chr(emotions, replace_w_emoticons))


# Way 2:
replace_emotions <- function(x) {
  result <- recode(x, "happy" = ":)", "sad" = ":(", "neutral" = ":|")
  return(result)
}

replace_emotions(study$emotions)


# Exercise 4 ####
is.vector(map_dbl(df, mean, na.rm = TRUE))

# Exercise 5 ####
mean_sd <- function(x){
  if(!(is.numeric(x))) {
   # stop("x is not a numeric value")
   return(NULL)
  } else{ 
    list("mean" = mean(x, na.rm = TRUE),
         "sd" = sd(x, na.rm = TRUE))
  }  
}


# Exercise 6 ####

#Way 1: list
map(study %>% select_if(is.numeric), mean_sd)


#Way 2: dataframe
map_df(study |> select(where(is.numeric)), mean_sd) |> 
  cbind(
    'col' = study |> 
      select(where(is.numeric)) |> 
      colnames()
  )

#Way 3: 
map_vec(study$emotions, replace_w_emoticons)

# or just use any other map

# Exercise 7 ####
study |> select(where(is.character)) |> map_df(tolower)


# Debugging solutions ####

# solutions are now also commented in line
geod_dist <- function(lat1, lon1, lat2, lon2, earth.radius = 6371) {
  
  # from degrees to radians
  deg2rad <- function(deg) return(deg*pi/180)
  lon1 <- deg2rad(lon1)
  lat1 <- deg2rad(lat1)
  lon2 <- deg2rad(lon2) # replace with lon2
  lat2 <- deg2rad(lat2)
  
  # calculation
  delta.long <- (lon2 - lon1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2 # replace sing with sin
  c <- 2 * asin(min(1,sqrt(a)))
  d = earth.radius * c # replace earth_radius with earth.radius
  return(d)
}

geod_dist(lat1 = 49.5, lon1 = 8.4, lat2 = 52.5, lon2 = 13.4)


# Take home debugging exercise ####
# load packages
library(tidyverse)
library(legislatoR) 

# get political data on German legislators
political_df <- left_join(x = get_political(legislature = 'deu') |> filter(session == 18), 
            y = get_core(legislature = "deu"), by = "pageid")

# wiki traffic data
traffic_df <- get_traffic(legislature = "deu") |> 
  filter(date >= "2013-10-22" & date <= "2017-10-24") |> 
  group_by(pageid) |> 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# sessions served
sessions_served_df <- get_political(legislature = "deu") |> 
  group_by(pageid) |> 
  dplyr::summarize(sessions_served = n())

# merge
legislator_df <- 
  left_join(political_df, sessions_served_df, by = "pageid") |> 
  left_join(traffic_df, by = "pageid") 

# compute age
get_age <- function(birth, date_at) {
  date_at_fmt <- date_at
  birth_fmt <- birth
  diff <- difftime(lubridate::ymd(date_at_fmt), lubridate::ymd(birth_fmt))
  diff_years <- lubridate::time_length(diff, "years")
  diff_years
}
legislator_df$age_in_years <- round(get_age(legislator_df$birth, "2017-10-24"), 0)

# plot top 10 pageviews
legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_table <- legislator_df |> dplyr::select(rank, name, traffic_mean, traffic_max)
colnames(legislator_df_table) <- c("Rank", "Representative", "Mean", "Maximum")
legislator_df_table <- head(legislator_df_table, 10)

ggplot(legislator_df_table, aes(y = Mean, x = -Rank)) + 
  xlab("Rank") + ylab("Avg. daily page views") + 
  labs(title = "Top 10 representatives by average daily page views") + 
  geom_col(stats = "identity") + 
  scale_x_continuous(breaks = -nrow(legislator_df_table):-1, labels = rev(1:nrow(legislator_df_table))) +
  geom_text(aes(y = 10, label = Representative), hjust = 0, color = "white", size = 2) + 
  coord_flip() + 
  theme_minimal()

# run model of page views as a function of sessions served, party, sex, and age in years
legislator_df$traffic_log <- log(legislator_df$traffic_mean)

covars <- c("sessions_served", "party", "sex", "age_in_years")
fmla <- paste("traffic_log", paste(covars, collapse = " + "), sep = " ~ ") 
summary(log_traffic_model <- lm(fmla, legislator_df))

# plot table
sjPlot::tab_model(log_traffic_model)
