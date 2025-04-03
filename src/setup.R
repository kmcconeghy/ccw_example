cat('loading canon packages....')
  #source(here('r', 'onetime.R'))

  pkg_toload <- c('tidyverse', 
                  'lubridate', 'here', 'knitr', 'quarto',
                  'survival')
  
  hold_del <- sapply(pkg_toload, require, 
                     warn.conflicts=F, quietly=T,
                     character.only=T)
  here()

cat('Done', '\n')