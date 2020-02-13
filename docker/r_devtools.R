#!/usr/bin/Rscript

utils::install.packages(
  pkgs = c('udunits2', 'units', 'remotes', 'devtools', 'caTools'),
  configure.args = '--with-udunits2-lib=/usr/local/lib',
  # repos = c('http://cran.us.r-project.org', 'https://cran.rstudio.com/'),
  quiet = TRUE
)

utils::install.packages(
  pkgs = 'opal', 
  repos = 'https://cran.obiba.org', 
  dependencies = TRUE, 
  quiet = TRUE
)
