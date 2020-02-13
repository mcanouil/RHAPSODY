#!/usr/bin/Rscript

options(stringsAsFactors = FALSE)

list_packages <- c(
  #"parallel", "grid", "methods", "utils", "opal",
  "tidyverse", "devtools", "scales",
  "broom", "viridis", "readxl", "writexl",
  "cowplot", "knitr", "kableExtra", "lme4",
  "lmerTest", "Hmisc", "data.tree"
)
utils::install.packages(pkgs = list_packages, quiet = TRUE)

# list_packages <- utils::read.table(header = TRUE, text =
#   "package version
#   broom   0.5.0
#   scales   1.0.0
#   cowplot   0.9.3
#   viridis   0.5.1
#   data.tree   0.7.8
#   devtools  1.13.6
#   grid   3.4.2
#   Hmisc   4.1-1
#   kableExtra   0.9.0
#   knitr    1.20
#   lme4  1.1-18-1
#   lmerTest   3.0-1
#   parallel   3.4.2
#   readxl   1.1.0
#   writexl     01.0
#   tidyverse   1.2.1"
# )
# list_packages[, "local_version"] <- sapply(list_packages[, "package"], utils::packageDescription, fields = "Version")
# list_packages[, "local_version"] <- ifelse(is.na(list_packages[, "local_version"]), "", list_packages[, "local_version"])
# list_packages <- list_packages[list_packages[, "local_version"]!=list_packages[, "version"], ]
# 
# mapply(
#   x = list_packages[, "package"],
#   y = list_packages[, "version"],
#   FUN = function(x, y) {
#     remotes::install_version(
#       package = x,
#       version = as.character(y),
#       dependencies = FALSE,
#       quiet = FALSE,
#       upgrade = "never"
#     )
#   }
# )
