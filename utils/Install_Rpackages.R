# Install R packages and load them
utils::install.packages(
  pkgs = c("udunits2", "units", "devtools", "caTools"),
  configure.args = "--with-udunits2-lib=/usr/local/lib",
  quiet = TRUE,
  repos = c("http://cran.us.r-project.org", "https://cran.rstudio.com/")
)
utils::install.packages(
  pkgs = "opal", 
  repos = c("https://cran.obiba.org", "https://cran.rstudio.com/"), 
  dependencies = TRUE, 
  quiet = TRUE
)
list_packages <- read.table(
  col.names = c("package", "version"), 
  text = '"acepack"	"1.4.1"
  "assertthat"	"0.2.0"
  "backports"	"1.1.1"
  "base64enc"	"0.1-3"
  "BH"	"1.66.0-1"
  "bindr"	"0.1.1"
  "plogr"	"0.2.0"
  "bindrcpp"	"0.2.2"
  "bitops"	"1.0-6"
  "brew"	"1.0-6"
  "foreign"	"0.8-69"
  "mnormt"	"1.5-5"
  "lattice"	"0.20-35"
  "nlme"	"3.1-131"
  "psych"	"1.7.8"
  "plyr"	"1.8.4"
  "pkgconfig"	"2.0.1"
  "utf8"	"1.1.3"
  "pillar"	"1.2.2"
  "tibble"	"1.4.2"
  "purrr"	"0.2.4"
  "tidyselect"	"0.2.4"
  "dplyr"	"0.7.5"
  "reshape2"	"1.4.3"
  "tidyr"	"0.8.1"
  "broom"	"0.4.4"
  "callr"	"1.0.0"
  "rematch"	"1.0.1"
  "cellranger"	"1.1.0"
  "checkmate"	"1.8.5"
  "cli"	"1.0.0"
  "cluster"	"2.0.6"
  "colorspace"	"1.3-2"
  "compiler"	"3.4.2"
  "RColorBrewer"	"1.1-2"
  "dichromat"	"2.0-0"
  "munsell"	"0.4.3"
  "labeling"	"0.3"
  "viridisLite"	"0.3.0"
  "scales"	"0.5.0"
  "gtable"	"0.2.0"
  "lazyeval"	"0.2.1"
  "ggplot2"	"2.2.1"
  "cowplot"	"0.9.1"
  "crayon"	"1.3.4"
  "curl"	"3.0"
  "data.table"	"1.10.4-3"
  "downloader"	"0.4"
  "htmlwidgets"	"0.9"
  "Matrix"	"1.2-12"
  "irlba"	"2.3.1"
  "igraph"	"1.1.2"
  "influenceR"	"0.1.0"
  "Rook"	"1.1-1"
  "XML"	"3.98-1.9"
  "rgexf"	"0.15.3"
  "gridExtra"	"2.3"
  "viridis"	"0.5.1"
  "visNetwork"	"2.0.1"
  "hms"	"0.4.2"
  "readr"	"1.1.1"
  "DiagrammeR"	"0.9.2"
  "data.tree"	"0.7.4"
  "DBI"	"0.7"
  "dbplyr"	"1.2.1"
  "devtools"	"1.13.5"
  "digest"	"0.6.15"
  "evaluate"	"0.10.1"
  "forcats"	"0.3.0"
  "Formula"	"1.2-2"
  "git2r"	"0.19.0"
  "glue"	"1.2.0"
  "graphics"	"3.4.2"
  "grDevices"	"3.4.2"
  "grid"	"3.4.2"
  "haven"	"1.1.1"
  "highr"	"0.6"
  "rpart"	"4.1-11"
  "latticeExtra"	"0.6-28"
  "survival"	"2.41-3"
  "nnet"	"7.3-12"
  "htmlTable"	"1.11.0"
  "Hmisc"	"4.1-1"
  "htmltools"	"0.3.6"
  "httr"	"1.3.1"
  "jsonlite"	"1.5"
  "xml2"	"1.2.0"
  "selectr"	"0.3-1"
  "rvest"	"0.3.2"
  "kableExtra"	"0.6.1"
  "knitr"	"1.20"
  "minqa"	"1.2.4"
  "nloptr"	"1.0.4"
  "RcppEigen"	"0.3.3.3.1"
  "lme4"	"1.1-14"
  "numDeriv"	"2016.8-1"
  "lmerTest"	"3.0-1"
  "lubridate"	"1.7.4"
  "magrittr"	"1.5"
  "markdown"	"0.8"
  "MASS"	"7.3-47"
  "memoise"	"1.1.0"
  "methods"	"3.4.2"
  "mgcv"	"1.8-22"
  "mime"	"0.5"
  "modelr"	"0.1.1"
  "openssl"	"0.9.9"
  "parallel"	"3.4.2"
  "R6"	"2.2.2"
  "Rcpp"	"0.12.17"
  "RCurl"	"1.95-4.8"
  "readxl"	"1.1.0"
  "reprex"	"0.1.2"
  "rjson"	"0.2.15"
  "rlang"	"0.2.1"
  "rmarkdown"	"1.9"
  "rprojroot"	"1.2"
  "rstudioapi"	"0.7"
  "splines"	"3.4.2"
  "stats"	"3.4.2"
  "stringi"	"1.2.3"
  "stringr"	"1.3.1"
  "tools"	"3.4.2"
  "utils"	"3.4.2"
  "whisker"	"0.3-2"
  "withr"	"2.1.2"
  "writexl"	"0.2"
  "yaml"	"2.1.15"
  "tidyverse" "1.2.1"
  "ggsignif" "0.4.0"
  "ggrepel" "0.8.0"
  "ggsci" "2.9"
  "polynom" "1.3-9"
  "ggpubr" "0.2"'
)
list_packages[, "local_version"] <- sapply(list_packages[, "package"], utils::packageDescription, fields = "Version")
list_packages[, "local_version"] <- ifelse(is.na(list_packages[, "local_version"]), "", list_packages[, "local_version"])
list_packages <- list_packages[list_packages[, "local_version"]!=list_packages[, "version"], ]
mapply(
  x = list_packages[, "package"], 
  y = list_packages[, "version"], 
  FUN = function(x, y) {
    devtools::install_version( 
      package = x,
      version = as.character(y),
      repos = c("http://cran.us.r-project.org", "https://cran.rstudio.com/"),
      dependencies = FALSE,
      quiet = FALSE,
      upgrade = "never"
    )
  }
)

list_packages <- c(
  "tidyverse",
  "devtools", "parallel", "grid", "scales",
  "broom", "viridis", "readxl", "writexl",
  "cowplot", "knitr", "kableExtra", "lme4",
  "lmerTest", "Hmisc", "data.tree", "opal"
)
invisible(sapply(X = list_packages, FUN = library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
