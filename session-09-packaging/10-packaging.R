###################### PACKAGING ######################################################

library(devtools)
library(roxygen2)


create_package("~/Desktop/hertie", open = FALSE)


setwd("~/Desktop/hertie")


theme_hertie <- ggplot2::theme_classic() +
  ggplot2::theme(text =  element_text(family = "Georgia", size = 23, color = "darkred"))
  


use_r("theme_hertie")


document()

?log
?theme_hertie



setwd("~/Desktop")
install("hertie")


setwd("~/Desktop/hertie")

check()


use_mit_license("Lisa Oswald")


#devtools::install_github("lfoswald/heRtie")
#library(heRtie)


# test
df <- data.frame(cbind(x = rnorm(100, 0, 1),
                       y = rnorm(100, 5, 3),
                       z = rbinom(100, 6, 0.5)))

ggplot(df, aes(x,y))+
  geom_point()+
  theme_hertie




