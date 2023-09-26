#Coding Solutions - Session 3

#Ex 3
dplyr::select(penguins, species, island, year, body_mass_g, sex)

#Ex 4
# you just need to utilize & and type the logical operator for the species
dplyr::filter(penguins, year == 2007 & species == "Chinstrap")

#Ex 5
penguins |>
  dplyr::filter(year == 2009 & species == "Chinstrap") |>
  dplyr::select(species, sex, year)

#Ex 6
penguins |>
  dplyr::group_by(species, year) |>
  dplyr::summarize(lightest_penguin = min(body_mass_g, na.rm = T))

#Ex 7

penguins |>
  dplyr::filter(island == "Dream") |>
  dplyr::arrange(body_mass_g)


#Quiz (3)
penguins %>% 
  filter(species == "Gentoo" & year == 2008) %>% 
  summarize(deepest_bill = max(bill_depth_mm))


#Quiz (4)
penguins %>% 
  filter(species == "Chinstrap" & year == 2009) %>% 
  summarize(deepest_bill = max(bill_depth_mm))


