list_packages <- c(
  "devtools",
  "parallel",
  "grid",
  "scales",
  "broom",
  "viridis",
  "readxl",
  "writexl",
  "cowplot",
  "knitr",
  "kableExtra",
  "lme4",
  "lmerTest",
  "Hmisc",
  "data.tree",
  "opal",
  "tidyverse"
)

## To set properly the session_info file based on Mickaël Canouil (mickael.canouil@cnrs.fr) setup
# list_packages %>%
#   devtools::session_info(pkgs = .) %>%
#   .$packages %>%
#   dplyr::select(package, version) %>%
#   readr::write_csv(path = "session_info.csv")

## Function to check, install and load packages based on session_info file
check_packages_version <- function(list_packages, session_info_csv = "session_info.csv") {
  
  check_packages <- function(package) {
    if (!package%in%installed.packages()[, "Package"]) {
      install.packages(
  			package, 
  			repos = c(
  			  "https://rhap-fdb01.vital-it.ch/repo/", 
  			  "http://cran.us.r-project.org",
  			  "https://cran.rstudio.com/", 
  			  "http://cran.obiba.org"
  		  ),
  			dependencies = TRUE
  		)
    }
    library(package = package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
  
  invisible(sapply(c("devtools", "tidyverse"), check_packages))

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
    dplyr::mutate(
      install = purrr::map2(
        .x = package, 
        .y = version,
        .f = function(x, y) {
          devtools::install_version(
            package = x, 
            version = as.character(y), 
            repos = c(
      			  "https://rhap-fdb01.vital-it.ch/repo/", 
      			  "http://cran.us.r-project.org",
      			  "https://cran.rstudio.com/", 
      			  "http://cran.obiba.org"
      		  )
          )
        }
      )
    )
  
  invisible(sapply(list_packages, check_packages))
  tidyverse::tidyverse_conflicts()
}

## Do the job
check_packages_version(list_packages = list_packages, session_info_csv = "session_info.csv")
