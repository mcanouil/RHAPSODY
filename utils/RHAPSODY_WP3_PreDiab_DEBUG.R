#---------------------------------------------------------------------------------------------------
# Name - RHAPSODY_WP3_PreDiab_DEBUG
# Desc - Copy of R code from 'RHAPSODY_WP3_PreDiab.Rmd'
# Author - Mickaël Canouil, Ph.D.
# Version - 1.0.0
#---------------------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)


###############
# Node settings
###############
opal_credentials <- as.data.frame(t(read.table(
  file = 'opal_credentials.txt',
  stringsAsFactors = FALSE,
  row.names = c('opal_server', 'opal_login', 'opal_password')
)), stringsAsFactors = FALSE)


###############
# Functions
###############
if (!'tidyverse'%in%installed.packages()[, 'Package']) {
  install.package(pkgs = 'tidyverse', repos = 'http://cran.us.r-project.org')
}
library(tidyverse)
check_packages_version <- function(list_packages, session_info_csv = './utils/session_info.csv') {
  check_packages <- function(package) {
    if (!package%in%installed.packages()[, 'Package']) {
      install.packages(
  			pkgs = package, 
  			repos = c(
  			  'http://cran.us.r-project.org',
  			  'https://cran.rstudio.com/'
  		  ),
  			dependencies = TRUE
  		)
    }
    library(
      package = package, 
      character.only = TRUE, 
      quietly = TRUE, 
      warn.conflicts = FALSE
    )
  }
  
  invisible(sapply(c('devtools', 'tidyverse'), check_packages))

  read_csv(file = session_info_csv) %>% 
    dplyr::mutate(
      local_version = purrr::map(
        .x = package, 
        .f = function(x) {
          if (x %in% utils::installed.packages()[, 'Package']) {
            utils::packageDescription(pkg = x, fields = 'Version')
          }
        }
      )
    ) %>% 
    filter(local_version!=version) %>% 
    dplyr::arrange(package) %>% 
    dplyr::mutate(
      install = purrr::map2(
        .x = package, 
        .y = version,
        .f = function(x, y) {
          devtools::install_version(
            package = x, 
            version = as.character(y), 
            repos = c(
              'https://rhap-fdb01.vital-it.ch/repo/', 
              'http://cran.us.r-project.org',
              'https://cran.rstudio.com/'
            )
          )
        }
      )
    )
  
  invisible(sapply(list_packages, check_packages))
  tidyverse::tidyverse_conflicts()
}

format_pval <- function (x, thresh = 10^-2, digits = 3, eps = 1e-50) {
  pout <- ifelse(
    x>=thresh, 
    Hmisc::format.pval(x, digits = digits, eps = eps, nsmall = digits), 
    base::format.pval(x, digits = digits, eps = eps, scientific = TRUE, nsmall = digits)
  )
  return(pout)
}

# reference table to convert HbA1c from percentage to mmol/mol
convert_hba1c <- function(x, unitFrom = '%') {
  if (unitFrom == '%') {
    hba1c_unit_table <- structure(list(
      V1 = c(
        10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L,
        26L, 28L, 30L, 32L, 34L, 36L, 38L, 40L, 42L, 44L, 46L, 48L, 50L,
        52L, 54L, 56L, 58L, 60L, 62L, 64L, 66L, 68L, 70L, 72L, 74L, 76L,
        78L, 80L, 82L, 84L, 86L, 88L, 90L, 92L, 94L, 96L, 98L, 100L,
        102L, 104L, 106L, 108L, 110L, 112L, 114L, 116L, 118L, 120L, 122L,
        124L, 126L, 128L, 130L, 132L, 134L, 136L, 138L, 140L, 142L, 144L,
        146L, 148L, 150L, 152L, 154L, 156L, 158L, 160L, 162L, 164L, 166L,
        168L, 170L, 172L, 174L, 176L, 178L, 180L, 182L, 184L, 186L, 188L,
        190L, 192L, 194L, 196L, 198L, 200L, 202L, 204L, 208L, 210L
      ),
      V2 = c(
        3.1, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.3, 4.5, 4.7, 4.9,
        5.1, 5.3, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.5, 6.7, 6.9, 7.1,
        7.3, 7.5, 7.6, 7.8, 8, 8.2, 8.4, 8.6, 8.7, 8.9, 9.1, 9.3,
        9.5, 9.7, 9.8, 10, 10.2, 10.4, 10.6, 10.8, 10.9, 11.1, 11.3,
        11.5, 11.7, 11.8, 12, 12.2, 12.4, 12.6, 12.8, 12.9, 13.1,
        13.3, 13.5, 13.7, 13.9, 14, 14.2, 14.4, 14.6, 14.8, 15, 15.1,
        15.3, 15.5, 15.7, 15.9, 16.1, 16.2, 16.4, 16.6, 16.8, 17,
        17.2, 17.3, 17.5, 17.7, 17.9, 18.1, 18.3, 18.4, 18.6, 18.8,
        19, 19.2, 19.4, 19.5, 19.7, 19.9, 20.1, 20.3, 20.4, 20.6,
        20.8, 21, 21.2
      )
    ), .Names = c('V1', 'V2'), class = 'data.frame', row.names = c(NA, -100L))
    return(sapply(x, function(y) {
      round(sum(tidy(lm(V1~V2, data = hba1c_unit_table))[, 'estimate'] * c(1, y)))
    }))
  } else {
    return(x)
  }
}

# function to convert g/l to mmol/l
convert_2mmoll <- function(x, from = 'g/l') {
  switch(EXPR = tolower(from),
    'g/l' = {x / 0.18},
    'mg/l' = {x / 0.18 / 100},
    'mmol/l' = {x},
    stop('Please check Units: g/l, g/L, mg/l, mg/Ln, mmol/l or mmol/L!')
  )
}

# function to convert 'Vital Signs' (VS) OPAL table (CDISC) to 'long format'
format_vs <- function(data) {
  data <- data %>%
    mutate(
      KEY_FACTOR = paste(DOMAIN, STUDYID, SUBJID, VISIT, sep = '_')
    ) %>%
    `rownames<-`(NULL)
  
  descriptive_data <- data %>%
    select(DOMAIN, STUDYID, VSORRESU, VSTEST, VSTESTCD) %>%
    distinct()
  
  if (!'VSTPTNUM'%in%colnames(data)) {
    data <- data %>% 
      mutate(VSTPTNUM = NA)
  }
  if (!'VSTPT'%in%colnames(data)) {
    data <- data %>% 
      mutate(VSTPT = NA)
  }
  
  data_values <- full_join(
    x = full_join(
      x = data %>%
        subset(is.na(VSTPTNUM)) %>%
        select(KEY_FACTOR, VSORRES, VSTESTCD) %>%
        spread(key = 'VSTESTCD', value = 'VSORRES'),
      y = data %>%
        subset(!is.na(VSTPTNUM)) %>%
        select(KEY_FACTOR, VSORRES, VSTESTCD, VSTPTNUM) %>%
        mutate(KEY_MEASURES = paste(VSTESTCD, VSTPTNUM, sep = '_')) %>%
        select(KEY_FACTOR, KEY_MEASURES, VSORRES) %>%
        spread(key = 'KEY_MEASURES', value = 'VSORRES'),
      by = c('KEY_FACTOR' = 'KEY_FACTOR')
    ),
    y = data %>%
      select(-c(VSORRES, VSTPT, VSTPTNUM, VSORRESU, VSTEST, VSTESTCD)) %>%
      distinct(),
    by = c('KEY_FACTOR' = 'KEY_FACTOR')
  ) %>%
    mutate(
      VISIT = factor(
        x = VISIT,
        levels = c(
          'BASELINE', 
          sort(grep('VISIT', unique(VISIT), value = TRUE)), 
          'LAST'
        )
      )
    ) %>%
    group_by(DOMAIN, STUDYID, SUBJID) %>%
    arrange(VISIT) %>%
    select(-KEY_FACTOR) %>%
    data.frame()
  
  return(list(data = data_values, annotation = descriptive_data))
}

# function to convert 'Laboratory Measurements' (LB) OPAL table (CDISC) to 'long format'
format_lb <- function(data) {
  data <- data %>%
    mutate(
      LBFAST = if ('LBFAST' %in% colnames(.)) {LBFAST} else {NA},
      LBTPTNUM = if ('LBTPTNUM' %in% colnames(.)) {LBTPTNUM} else {NA},
      LBTPTNUM = ifelse(LBTESTCD == 'GLUC' & is.na(LBTPTNUM), 0, LBTPTNUM),
      KEY_FACTOR = paste(DOMAIN, STUDYID, SUBJID, VISIT, sep = '_')
    ) %>%
    `rownames<-`(NULL)

  descriptive_variables <- c(
    'LBORRESU', 'LBTEST', 'LBTESTCD', 'LBSPEC', 
    'LBCAT', 'LBMETHOD', 'LBTPT', 'LBTPTNUM', 'AGCAT'
  )
  
  descriptive_data <- data %>%
    select(intersect(c('DOMAIN', 'STUDYID', !!descriptive_variables), colnames(.))) %>%
    distinct() %>%
    mutate(
      KEY_MEASURES = paste(LBTESTCD, LBSPEC, LBCAT, LBTPTNUM, sep = '_')
    )

  data_values <- full_join(
    x = (data %>%
      select(KEY_FACTOR, LBORRES, LBTESTCD, LBSPEC, LBCAT, LBTPTNUM) %>%
      mutate(
        KEY_MEASURES = paste(LBTESTCD, LBSPEC, LBCAT, LBTPTNUM, sep = '_')
      ) %>%
      select(KEY_FACTOR, KEY_MEASURES, LBORRES) %>%
      spread(key = 'KEY_MEASURES', value = 'LBORRES')),
    y = (data %>%
      select(LBDTC, SUBJID, VISIT, DOMAIN, STUDYID, LBFAST, KEY_FACTOR) %>%
      distinct()),
    by = c('KEY_FACTOR' = 'KEY_FACTOR')
  ) %>%
    mutate(
      VISIT = factor(
        VISIT,
        levels = c('BASELINE', sort(grep('VISIT', unique(VISIT), value = TRUE)), 'LAST')
      )
    ) %>%
    group_by(DOMAIN, STUDYID, SUBJID) %>%
    arrange(VISIT) %>%
    select(-KEY_FACTOR) %>%
    data.frame()

  return(list(data = data_values, annotation = descriptive_data))
}

# function to compute bmi if not available
compute_bmi <- function(data) {
  if (!'BMI'%in%data$annotation[['VSTESTCD']]) {
    weight_unit <- data$annotation %>% 
      filter(VSTESTCD=='WEIGHT') %>% 
      select(VSORRESU) %>% 
      unlist()
    convert_weight_unit <- switch(
      EXPR = weight_unit,
      'kg' = {function(x) {x}},
      'lb' = {function(x) {x*0.45359237}}
    )
    
    height_unit <- data$annotation %>% 
      filter(VSTESTCD=='HEIGHT') %>% 
      select(VSORRESU) %>% 
      unlist()
    convert_height_unit <- switch(
      EXPR = height_unit,
      'cm' = {function(x) {x/100}},
      'm' = {function(x) {x}},
      'in' = {function(x) {(x*2.54)/100}}
    )
    
    data$data <- data$data %>% 
      mutate(
        BMI = convert_weight_unit(WEIGHT) / convert_height_unit(HEIGHT)
      )
    default_CDISC_BMI <- structure(
      list(
        DOMAIN = 'VS', 
        STUDYID = unique(data$annotation[['STUDYID']]), 
        VSORRESU = 'kg/m2', 
        VSTEST = 'Body Mass Index', 
        VSTESTCD = 'BMI'
      ), 
      .Names = c('DOMAIN',  'STUDYID', 'VSORRESU', 'VSTEST', 'VSTESTCD'), 
      row.names = 1L, 
      class = 'data.frame'
    )
    data$annotation <- bind_rows(data$annotation, default_CDISC_BMI)
  }
  return(data)
}


###############
# Load/install libraries
###############
list_packages <- c(
  'devtools', 'parallel', 'grid', 'scales',
  'broom', 'viridis', 'readxl', 'writexl',
  'cowplot', 'knitr', 'kableExtra', 'lme4',
  'lmerTest', 'Hmisc', 'data.tree', 'opal'
)
check_packages_version(
  list_packages = list_packages, 
  session_info_csv = 'session_info.csv'
)


###############
# Do the funny stuff
###############
o <- opal.login(
  username = opal_credentials$opal_login,
  password = opal_credentials$opal_password,
  url = opal_credentials$opal_server
)


available_tables <- intersect(opal.datasources(opal = o)[[1]]$table, c('DM', 'VS', 'LB', 'MH', 'CM'))
for (itable in available_tables) {
  opal.assign(opal = o, symbol = itable, value = paste0('rhapsody.', itable))
  assign(x = itable, value = opal.execute(o, itable))
}
opal.logout(o)
rm(list = c('o', 'itable', 'available_tables'))


vs_tidy <- try(format_vs(data = VS))
vs_tidybmi <- try(vs_tidy <- compute_bmi(data = vs_tidy))
lb_tidy <- try(format_lb(data = LB))

if (is(vs_tidy, 'try-error')) {
  warning(vs_tidy)
} else {
  message('VS table was successfully formated !')
}
if (is(vs_tidybmi, 'try-error')) {
  warning(vs_tidybmi)
} else {
  message('BMI computation was successfull !')
}
if (is(lb_tidy, 'try-error')) {
  warning(lb_tidy)
} else {
  message('LB table was successfully formated !')
}

if (any(c(is(vs_tidy, 'try-error'), is(vs_tidybmi, 'try-error'), is(lb_tidy, 'try-error')))) {
  stop('LB or VS formatting failed ! Check previous warnings !)')
}
  

DMVSLB <- merge(
  x = DM %>% mutate(VISIT = 'BASELINE') %>% select(-DOMAIN),
  y = merge(
    vs_tidy$data %>% select(-DOMAIN),
    lb_tidy$data %>% select(-DOMAIN),
    by = c('STUDYID', 'SUBJID', 'VISIT'),
    all = TRUE
  ),
  by = c('STUDYID', 'SUBJID', 'VISIT'),
  all = TRUE
) %>%
  group_by(STUDYID, SUBJID) %>%
  filter(
    (VISIT=='BASELINE' & !is.na(AGE) & !is.na(SEX)) | VISIT!='BASELINE'
  ) %>% 
  arrange(VISIT) %>% 
  mutate(
    AGE0 = AGE[1],
    BMI0 = BMI[1],
    SEX0 = as.numeric(SEX=='M')[1]
  ) %>% 
  as.data.frame()

if (
  all(
    nrow(lb_tidy$data) == length(unique(LB[['SUBJID']]))*length(unique(LB[['VISIT']])),
    nrow(vs_tidy$data) == length(unique(VS[['SUBJID']]))*length(unique(VS[['VISIT']])),
    nrow(lb_tidy$data) == nrow(vs_tidy$data)
  )
) {
  message('Good to go! Try to run the whole script.')
} else {
  stop('Something went wrong, test each of the condition above to locate where is the issue!')
}

### To check if all individuals have the same number of visits before convert
if (
  DMVSLB %>%
    group_by(SUBJID) %>%
    summarise(n = length(SUBJID)) %>%
    select(n) %>%
    all()
) {
  DMVSLB <- DMVSLB %>%
    mutate(
      VISIT_YEARS = as.numeric(
        difftime(as.Date(VSDTC), min(as.Date(VSDTC), na.rm = TRUE), unit = 'weeks') / 52.25
      )
    )
} else {
  message('The number of visits per individuals differs!')
}

