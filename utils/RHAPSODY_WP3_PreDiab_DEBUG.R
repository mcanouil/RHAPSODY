#---------------------------------------------------------------------------------------------------
# Name - RHAPSODY_WP3_PreDiab_DEBUG
# Desc - Copy of R code from "RHAPSODY_WP3_PreDiab.Rmd"
# Author - Mickaël Canouil, Ph.D.
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
  "parallel", "grid", "methods", "utils", "opal",
  "tidyverse", "devtools", "scales",
  "broom", "viridis", "readxl", "writexl",
  "cowplot", "knitr", "kableExtra", "lme4",
  "lmerTest", "Hmisc", "data.tree"
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
  options(repos = c(CRAN = "https://mran.microsoft.com/snapshot/2018-10-19"))
  list_packages <- c(
    "udunits2", "units", "remotes", "devtools", "caTools", 
    "RCurl", "rjson", "e1071", "knitrBootstrap",
    #"parallel", "grid", "methods", "utils", "opal",
    "tidyverse", "devtools", "scales",
    "broom", "viridis", "readxl", "writexl",
    "cowplot", "knitr", "kableExtra", "lme4",
    "lmerTest", "Hmisc", "data.tree"
  )
  utils::install.packages(pkgs = list_packages, quiet = TRUE)
  utils::install.packages(pkgs = "opal", repos = "https://cran.obiba.org", quiet = TRUE)
  
  list_packages <- c(
    "parallel", "grid", "methods", "utils", "opal",
    "tidyverse", "devtools", "scales",
    "broom", "viridis", "readxl", "writexl",
    "cowplot", "knitr", "kableExtra", "lme4",
    "lmerTest", "Hmisc", "data.tree"
  )
  invisible(sapply(X = list_packages, FUN = library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

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


if (exists("APMH") && "FAMILYID" %in% colnames(APMH)) {
  DMVSLB <- dplyr::left_join(
    x = DMVSLB,
    y = APMH %>% 
      dplyr::select(FAMILYID, SUBJID, STUDYID) %>% 
      dplyr::distinct(),
    by = c("STUDYID", "SUBJID")
  )
} else {
  DMVSLB <- dplyr::mutate(.data = DMVSLB, FAMILYID = SUBJID)
}

n_LB <- LB %>% 
  dplyr::select(STUDYID, SUBJID, VISIT) %>% 
  dplyr::distinct() %>% 
  dplyr::count(STUDYID, SUBJID) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  unlist()

n_VS <- VS %>% 
  dplyr::select(STUDYID, SUBJID, VISIT) %>% 
  dplyr::distinct() %>% 
  dplyr::count(STUDYID, SUBJID) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  unlist() 

if (all(c(nrow(lb_tidy$data), n_LB, nrow(vs_tidy$data), n_VS) == nrow(DMVSLB))) {
  message("Good to go! Try to run the whole script.")
} else {
  stop("Something went wrong, test each of the condition above to locate where is the issue!")
}


DMVSLB <- dplyr::mutate(
  .data = DMVSLB,
  VISIT_YEARS = as.numeric(
    difftime(as.Date(VSDTC), min(as.Date(VSDTC), na.rm = TRUE), unit = "weeks") / 52.25
  )
)
