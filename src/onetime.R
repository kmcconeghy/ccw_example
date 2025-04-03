# To be run once at project start
  renv::init()
  install.packages('remotes')
  install.packages(c('tidyverse', 'lubridate', 'here', 'knitr', 'quarto'))