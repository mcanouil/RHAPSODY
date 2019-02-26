#---------------------------------------------------------------------------------------------------
# Name - RHAPSODY_WP3_PreDiab_DEBUG
# Desc - Copy of R code from "RHAPSODY_WP3_PreDiab.Rmd"
# Author - Mickaël Canouil, Ph.D.
# Version - 1.2.1
#---------------------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)


###############
# Node settings
###############
opal_credentials <- as.data.frame(t(read.table(
  file = "opal_credentials.txt",
  stringsAsFactors = FALSE,
  row.names = c("opal_server", "opal_login", "opal_password")
)), stringsAsFactors = FALSE)


###############
# Functions
###############
# function to convert "Vital Signs" (VS) OPAL table (CDISC) to "long format"
format_vs <- function(data) {
  data <- dplyr::mutate(
    .data = data,
    KEY_FACTOR = paste(DOMAIN, STUDYID, SUBJID, VISIT, sep = "_")
  )
  rownames(data) <- NULL
  
  descriptive_data <- dplyr::distinct(.data = dplyr::select(.data = data, DOMAIN, STUDYID, VSORRESU, VSTEST, VSTESTCD))
  
  if (!"VSTPTNUM"%in%colnames(data)) {
    data <- dplyr::mutate(.data = data, VSTPTNUM = NA)
  }
  if (!"VSTPT"%in%colnames(data)) {
    data <- dplyr::mutate(.data = data, VSTPT = NA)
  }
  
  data_values <- dplyr::full_join(
    x = dplyr::full_join(
      x = data %>%
        subset(is.na(VSTPTNUM)) %>%
        dplyr::select(KEY_FACTOR, VSORRES, VSTESTCD) %>%
        tidyr::spread(key = "VSTESTCD", value = "VSORRES"),
      y = data %>%
        subset(!is.na(VSTPTNUM)) %>%
        dplyr::select(KEY_FACTOR, VSORRES, VSTESTCD, VSTPTNUM) %>%
        dplyr::mutate(KEY_MEASURES = paste(VSTESTCD, VSTPTNUM, sep = "_")) %>%
        dplyr::select(KEY_FACTOR, KEY_MEASURES, VSORRES) %>%
        tidyr::spread(key = "KEY_MEASURES", value = "VSORRES"),
      by = c("KEY_FACTOR" = "KEY_FACTOR")
    ),
    y = data %>%
      dplyr::select(-c(VSORRES, VSTPT, VSTPTNUM, VSORRESU, VSTEST, VSTESTCD)) %>%
      dplyr::distinct(),
    by = c("KEY_FACTOR" = "KEY_FACTOR")
  ) %>%
    dplyr::mutate(
      VISIT = factor(
        x = VISIT,
        levels = c(
          "BASELINE", 
          sort(grep("VISIT", unique(VISIT), value = TRUE)), 
          "LAST"
        )
      )
    ) %>%
    dplyr::group_by(DOMAIN, STUDYID, SUBJID) %>%
    dplyr::arrange(VISIT) %>%
    dplyr::select(-KEY_FACTOR) %>%
    data.frame()
  
  list(data = data_values, annotation = descriptive_data)
}

# function to convert "Laboratory Measurements" (LB) OPAL table (CDISC) to "long format"
format_lb <- function(data) {
  check_multiple_hba1c <- data %>% 
    dplyr::select(LBTESTCD, LBORRESU) %>% 
    dplyr::filter(LBTESTCD=="HBA1C") %>% 
    dplyr::distinct()
  if (nrow(check_multiple_hba1c)>1) {
    if (any(grepl("%", check_multiple_hba1c[["LBORRESU"]]))) {
      data <- data %>% 
        dplyr::filter(
          !(LBTESTCD=="HBA1C" & LBORRESU %in% setdiff(check_multiple_hba1c[["LBORRESU"]], "%"))
        )
    } else {
       data <- data %>% 
        dplyr::filter(
          !(LBTESTCD=="HBA1C" & LBORRESU %in% setdiff(check_multiple_hba1c[["LBORRESU"]], "mmol/mol"))
        )
    }
  }
  
  data <- dplyr::mutate(
    .data = data,
    LBFAST = if ("LBFAST" %in% colnames(data)) LBFAST else NA,
    LBTPTNUM = if ("LBTPTNUM" %in% colnames(data)) LBTPTNUM else NA,
    LBTPTNUM = ifelse(LBTESTCD == "GLUC" & is.na(LBTPTNUM), 0, LBTPTNUM),
    KEY_FACTOR = paste(DOMAIN, STUDYID, SUBJID, VISIT, sep = "_")
  )
  rownames(data) <- NULL

  descriptive_variables <- c(
    "LBORRESU", "LBTEST", "LBTESTCD", "LBSPEC", 
    "LBCAT", "LBMETHOD", "LBTPT", "LBTPTNUM", "AGCAT"
  )
  
  descriptive_data <- data %>%
    dplyr::select(intersect(c("DOMAIN", "STUDYID", !!descriptive_variables), colnames(.))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      KEY_MEASURES = paste(LBTESTCD, LBSPEC, LBCAT, LBTPTNUM, sep = "_")
    )

  data_values <- dplyr::full_join(
    x = (data %>%
      dplyr::select(KEY_FACTOR, LBORRES, LBTESTCD, LBSPEC, LBCAT, LBTPTNUM) %>%
      dplyr::mutate(
        KEY_MEASURES = paste(LBTESTCD, LBSPEC, LBCAT, LBTPTNUM, sep = "_")
      ) %>%
      dplyr::select(KEY_FACTOR, KEY_MEASURES, LBORRES) %>%
      tidyr::spread(key = "KEY_MEASURES", value = "LBORRES")),
    y = (data %>%
      dplyr::select(LBDTC, SUBJID, VISIT, DOMAIN, STUDYID, LBFAST, KEY_FACTOR) %>%
      dplyr::distinct()),
    by = c("KEY_FACTOR" = "KEY_FACTOR")
  ) %>%
    dplyr::mutate(
      VISIT = factor(
        x = VISIT,
        levels = c(
          "BASELINE", 
          sort(grep("VISIT", unique(VISIT), value = TRUE)), 
          "LAST"
        )
      )
    ) %>%
    dplyr::group_by(DOMAIN, STUDYID, SUBJID) %>%
    dplyr::arrange(VISIT) %>%
    dplyr::select(-KEY_FACTOR) %>%
    data.frame()

  list(data = data_values, annotation = descriptive_data)
}

# function to compute bmi if not available
compute_bmi <- function(data) {
  if (!"BMI"%in%data$annotation[["VSTESTCD"]]) {
    weight_unit <- data$annotation %>% 
      dplyr::filter(VSTESTCD == "WEIGHT") %>% 
      dplyr::select(VSORRESU) %>% 
      unlist()
    convert_weight_unit <- switch(
      EXPR = weight_unit,
      "kg" = {function(x) x},
      "lb" = {function(x) x * 0.45359237}
    )
    
    height_unit <- data$annotation %>% 
      dplyr::filter(VSTESTCD=="HEIGHT") %>% 
      dplyr::select(VSORRESU) %>% 
      unlist()
    convert_height_unit <- switch(
      EXPR = height_unit,
      "cm" = {function(x) x / 100},
      "m" = {function(x) x},
      "in" = {function(x) (x * 2.54) / 100}
    )
    
    data$data <- dplyr::mutate(
      .data = data$data,
      BMI = convert_weight_unit(WEIGHT) / convert_height_unit(HEIGHT)
    )
    default_CDISC_BMI <- structure(
      list(
        DOMAIN = "VS", 
        STUDYID = unique(data$annotation[["STUDYID"]]), 
        VSORRESU = "kg/m2", 
        VSTEST = "Body Mass Index", 
        VSTESTCD = "BMI"
      ), 
      .Names = c("DOMAIN",  "STUDYID", "VSORRESU", "VSTEST", "VSTESTCD"), 
      row.names = 1L, 
      class = "data.frame"
    )
    data$annotation <- dplyr::bind_rows(data$annotation, default_CDISC_BMI)
  }
  data
}


###############
# Load/install libraries
###############
list_packages <- c(
  "tidyverse",
  "devtools", "parallel", "grid", "scales",
  "broom", "viridis", "readxl", "writexl",
  "cowplot", "knitr", "kableExtra", "lme4",
  "lmerTest", "Hmisc", "data.tree", "opal"
)

all_installed <- all(sapply(
  X = list_packages, 
  FUN = require, 
  character.only = TRUE, 
  quietly = TRUE, 
  warn.conflicts = FALSE
))

if (!all_installed) {
  # Install R packages and load them
  if (!all(c("udunits2", "units", "devtools", "caTools")%in%rownames(utils::installed.packages()))) {
    utils::install.packages(
      pkgs = setdiff(c("udunits2", "units", "devtools", "caTools"), rownames(utils::installed.packages())),
      configure.args = "--with-udunits2-lib=/usr/local/lib",
      quiet = TRUE,
      repos = c("http://cran.us.r-project.org", "https://cran.rstudio.com/")
    )
  }
  if (!all(c("opal")%in%rownames(utils::installed.packages()))) {
    utils::install.packages(
      pkgs = "opal", 
      repos = c("https://cran.obiba.org", "https://cran.rstudio.com/"), 
      dependencies = TRUE, 
      quiet = TRUE
    )
  }
  list_packages <- utils::read.table(
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
  if (nrow(list_packages)>0) {
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
  }
}


###############
# Do the funny stuff
###############
o <- opal::opal.login(
  username = opal_credentials$opal_login,
  password = opal_credentials$opal_password,
  url = opal_credentials$opal_server
)

available_tables <- intersect(opal::opal.datasources(opal = o)[[1]]$table, c("DM", "VS", "LB", "APMH", "CM", "MH"))

for (itable in available_tables) {
  opal::opal.assign(opal = o, symbol = itable, value = paste0("rhapsody.", itable))
  assign(x = itable, value = opal::opal.execute(o, itable))
}
opal::opal.logout(o)
rm(list = c("o", "itable", "available_tables"))


vs_tidy <- try(format_vs(data = VS))
if (is(vs_tidy, "try-error")) {
  warning(vs_tidy)
} else {
  message("VS table was successfully formated!")
}

vs_tidybmi <- try(vs_tidy <- compute_bmi(data = vs_tidy))
if (is(vs_tidybmi, "try-error")) {
  warning(vs_tidybmi)
} else {
  message("BMI computation was successfull!")
}

lb_tidy <- try(format_lb(data = LB))
if (is(lb_tidy, "try-error")) {
  warning(lb_tidy)
} else {
  message("LB table was successfully formated!")
}

if (any(c(is(vs_tidy, "try-error"), is(vs_tidybmi, "try-error"), is(lb_tidy, "try-error")))) {
  stop("LB or VS formatting failed! Check previous warnings!)")
}
  

DMVSLB <- dplyr::full_join(
  x = dplyr::select(.data = dplyr::mutate(.data = DM, VISIT = "BASELINE"), -DOMAIN),
  y = dplyr::full_join(
    x = dplyr::select(.data = dplyr::mutate(.data = vs_tidy$data, VISIT = as.character(VISIT)), -DOMAIN),
    y = dplyr::select(.data = dplyr::mutate(.data = lb_tidy$data, VISIT = as.character(VISIT)), -DOMAIN),
    by = c("STUDYID", "SUBJID", "VISIT")
  ),
  by = c("STUDYID", "SUBJID", "VISIT")
) %>%
  dplyr::mutate(
    VISIT = factor(
      x = VISIT,
      levels = c(
        "BASELINE", 
        sort(grep("VISIT", unique(VISIT), value = TRUE)), 
        "LAST"
      )
    )
  ) %>% 
  dplyr::group_by(STUDYID, SUBJID) %>%
  dplyr::filter(
    (VISIT=="BASELINE" & !is.na(AGE) & !is.na(SEX)) | VISIT!="BASELINE"
  ) %>% 
  dplyr::arrange(VISIT) %>% 
  dplyr::mutate(
    AGE0 = AGE[1],
    BMI0 = BMI[1],
    SEX0 = as.numeric(SEX=="M")[1]
  ) %>% 
  as.data.frame()


if ("FAMILYID" %in% colnames(APMH)) {
  DMVSLB <- dplyr::left_join(
    x = DMVSLB,
    y = APMH %>% 
      dplyr::select(FAMILYID, SUBJID, STUDYID) %>% 
      dplyr::distinct(),
    by = c("STUDYID", "SUBJID")
  )
}

if (
  all(
    nrow(lb_tidy$data) == length(unique(LB[["SUBJID"]]))*length(unique(LB[["VISIT"]])),
    nrow(vs_tidy$data) == length(unique(VS[["SUBJID"]]))*length(unique(VS[["VISIT"]])),
    nrow(lb_tidy$data) == nrow(vs_tidy$data)
  )
) {
  message("Good to go! Try to run the whole script.")
} else {
  stop("Something went wrong, test each of the condition above to locate where is the issue!")
}

if (
  DMVSLB %>%
    dplyr::group_by(SUBJID) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::select(n) %>%
    all()
) {
  DMVSLB <- dplyr::mutate(
    .data = DMVSLB,
    VISIT_YEARS = as.numeric(
      difftime(as.Date(VSDTC), min(as.Date(VSDTC), na.rm = TRUE), unit = "weeks") / 52.25
    )
  )
} else {
  message("The number of visits per individuals differs!")
}
