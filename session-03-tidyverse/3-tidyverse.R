## LAB SESSION 3


## -----------------------------------------------------------------------------------------------------
library(tidyverse)


## -----------------------------------------------------------------------------------------------------
tidyverse_packages()


 
## ## These next two lines of code do exactly the same thing.
## 



# The tidyverse loads its a pipe operator, denoted `%>%`.
# Using pipes can dramatically improve the experience of reading and writing code. Compare:
#   

## These next two lines of code do exactly the same thing.

mpg %>%
  filter(manufacturer=="audi") %>% 
  group_by(model) %>% 
  summarise(hwy_mean = mean(hwy))

summarise(group_by(filter(mpg, manufacturer=="audi"), model), hwy_mean = mean(hwy))
# 
# The first line reads from left to right, exactly how I thought of the operations in my head. 
# - Take this object (`mpg`), do this (`filter`), then do this (`group_by`), etc.
# 
# The second line totally inverts this logical order (the final operation comes first!) 
# - Who wants to read things inside out?
#   
#   The piped version of the code is even more readable if we write it over several lines. Here it is aga
#   in and, this time, I'll run it for good measure so you can see the output:
mpg %>%
  filter(manufacturer=="audi") %>%
  group_by(model) %>%
  summarise(hwy_mean = mean(hwy))


# Remember: Using vertical space costs nothing and makes for much more readable/writeable code than cramming things horizontally.


# a word on arguments within a function
scale(penguins$body_mass_g, scale = T, center = F)


################################### TIDYR ############################################################

## ---- echo = F----------------------------------------------------------------------------------------
library(nycflights13)
flights 
planes


## -----------------------------------------------------------------------------------------------------
left_join(flights, planes) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, type, model)%>%
  head(3) ## Just to save vertical space in output


## -----------------------------------------------------------------------------------------------------
left_join(
  flights,
  planes %>% rename(year_built = year), ## Not necessary w/ below line, but helpful
  by = "tailnum" ## Be specific about the joining column
) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, year_built, type, model) %>%
  head(3) 

left_join(main-data, secondary-data, by = "key-variable")

## ----join3--------------------------------------------------------------------------------------------
left_join(
  flights,
  planes, ## Not renaming "year" to "year_built" this time
  by = "tailnum"
) %>%
  select(contains("year"), month, day, dep_time, arr_time, carrier, flight, tailnum, type, model) %>%
  head(3)


## -----------------------------------------------------------------------------------------------------
stocks = data.frame( ## Could use "tibble" instead of "data.frame" if you prefer
  time = as.Date('2009-01-01') + 0:1,
  X = rnorm(2, 0, 1),
  Y = rnorm(2, 0, 2),
  Z = rnorm(2, 0, 4)
)
stocks

stocks %>% pivot_longer(c("X", "Y", "Z"), names_to="stock", values_to="price")

## other example

panel = data.frame( ## Could use "tibble" instead of "data.frame" if you prefer
  unit = c("A","B"),
  var1 = rnorm(2, 0, 1),
  var2 = rnorm(2, 0, 2),
  var3 = rnorm(2, 0, 4)
)
panel

pivot_longer(panel, c("var1", "var2", "var3"), names_to="time", values_to="rating")
panel %>% pivot_longer(-unit, names_to="time", values_to="rating", names_prefix = "var")


## -----------------------------------------------------------------------------------------------------
## Write out the argument names this time: i.e. "names_to=" and "values_to="
tidy_stocks <- stocks %>% 
  pivot_longer(-time, names_to="stock", values_to="price")

tidy_panel <- panel %>% pivot_longer(-unit , names_to="time", values_to="rating", names_prefix = "var")

## ----pivot_wider1, dependson=tidy_stocks--------------------------------------------------------------
tidy_stocks %>% pivot_wider(names_from=stock, values_from=price)
tidy_stocks %>% pivot_wider(names_from=time, values_from=price)

tidy_panel %>% pivot_wider(names_from = time, values_from = rating)
tidy_panel %>% pivot_wider(names_from = unit, values_from = rating)

## ----sep1---------------------------------------------------------------------------------------------
economists = data.frame(name = c("Adam.Smith", "Paul.Samuelson", "Milton.Friedman"))
economists
economists %>% separate(name, c("first_name", "last_name")) 


## ----sep2---------------------------------------------------------------------------------------------
jobs = data.frame(
  name = c("Jack", "Jill"),
  occupation = c("Homemaker", "Philosopher, Philanthropist, Troublemaker") 
) 
jobs
## Now split out Jill's various occupations into different rows
jobs %>% separate_rows(occupation)


## ----unite1-------------------------------------------------------------------------------------------
gdp = data.frame(
  yr = rep(2016, times = 4),
  mnth = rep(1, times = 4),
  dy = 1:4,
  gdp = rnorm(4, mean = 100, sd = 2)
)
gdp 
## Combine "yr", "mnth", and "dy" into one "date" column
gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-")


## ----unite2-------------------------------------------------------------------------------------------
gdp_u = gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-")
gdp_u = gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-") %>% as_tibble()
gdp_u


## ----unite3, message=F--------------------------------------------------------------------------------
library(lubridate)
gdp_u %>% mutate(date = ymd(date))



################################### DPLYR ############################################################

## ---- echo=FALSE--------------------------------------------------------------------------------------
library(palmerpenguins)

penguins <- penguins

## -----------------------------------------------------------------------------------------------------
glimpse(penguins)
names(penguins)


## ---- eval=F------------------------------------------------------------------------------------------
penguins %>% select(species, island, year, sex, body_mass_g)

penguins_no_yr <- penguins %>%
  select(-year) 


## ---- eval=F------------------------------------------------------------------------------------------
dplyr::filter(penguins, year == 2007)

penguins %>%filter(year == 2007 | species == "Chinstrap")


final_df <- penguins %>%
  filter(year == 2009 & species == "Chinstrap") %>% 
  select(species, sex, year)

final_df %>% print()





## ---- echo=FALSE, eval=F------------------------------------------------------------------------------
## # you just need to utilize & and type the logical operator for the species
dplyr::filter(penguins, year == 2007 & species == "Chinstrap" & )
dplyr::filter(penguins, year == 2007 , species == "Chinstrap")
dplyr::filter(penguins, year == 2007 | species == "Chinstrap")


## ---- echo=FALSE, eval=F------------------------------------------------------------------------------ penguins %>%
penguins %>% 
  dplyr::filter(year == 2009 & species == "Chinstrap") %>%
  dplyr::select(species, sex, year) 


## ---- eval=F------------------------------------------------------------------------------------------
names(penguins)

penguins_n <- penguins %>%
  mutate(body_mass_g = body_mass_g - 2,
         body_mass_kg = body_mass_g/1000,
         body_mass_scaled = scale(body_mass_g)) %>% 
  select(species, body_mass_g, body_mass_kg)



penguins_n$body_mass_2 <- (penguins_n$body_mass_g - mean(penguins_n$body_mass_g))/sd(penguins_n$body_mass_g)

attach(penguins_n)
body_mass_2 <- (body_mass_g - mean(body_mass_g))/sd(body_mass_g)


rm(penguins_n)

# useful stuff for missing values
penguins_n %>%
  mutate(body_mass_g = replace_na(body_mass_g, 0))

penguins_n %>%
  replace_na(0)

penguins_n %>%
  na.omit()

penguins_n%>%
  filter(!is.na(body_mass_g))

# the very careful way of adding new variables to a dataframe
body_mass_kg <- penguins$body_mass_g/1000

penguins$body_Mass_kg <- body_mass_kg

## -----------------------------------------------------------------------------------------------------
# compare this output with the one below
penguins %>%
  dplyr::summarize(heaviest_penguin = max(body_mass_g, na.rm = T)) 

## -----------------------------------------------------------------------------------------------------

penguins <- penguins %>%
  dplyr::group_by(species) %>%
  mutate(max_weight_species = max(body_mass_g, na.rm = T))
  dplyr::summarize(heaviest_penguin = max(body_mass_g, na.rm = T))

penguins %>%
  dplyr::group_by(species, year) %>%
  dplyr::summarize(lightest_penguin = min(body_mass_g, na.rm = T))


## ---- eval=F------------------------------------------------------------------------------------------
## 
penguins %>%
  dplyr::arrange(bill_length_mm)



## ---- eval=F------------------------------------------------------------------------------------------
## 
penguins %>%
  dplyr::arrange(desc(bill_length_mm))



## ---- echo = FALSE, eval=F----------------------------------------------------------------------------
penguins %>%
  dplyr::filter(island == "Dream") %>%
  dplyr::arrange(body_mass_g)



