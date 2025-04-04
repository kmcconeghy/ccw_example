library(here())

source(here('src', 'setup.R'), echo=F)


# run all data-files
  source(here('src', '01_syndata.R'))
  source(here('src', '02_clone.R'))
  source(here('src', '03_est_kmnaive.R'))
  source(here('src', '04_expdata.R'))

