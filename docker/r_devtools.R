#!/usr/bin/Rscript

utils::install.packages(
  pkgs = c('udunits2', 'units', 'devtools', 'caTools'),
  configure.args = '--with-udunits2-lib=/usr/local/lib',
  quiet = TRUE,
  repos = c('http://cran.us.r-project.org', 'https://cran.rstudio.com/')
)

utils::install.packages(
  pkgs = 'opal', 
  repos = c('https://cran.obiba.org', 'https://cran.rstudio.com/'), 
  dependencies = TRUE, 
  quiet = TRUE
)
