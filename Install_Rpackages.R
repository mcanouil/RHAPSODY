library(tidyverse)
library(devtools)
session_info <- read.delim(stringsAsFactors = FALSE, header = FALSE, col.names = c("Package", "Version"), sep = "_", text = 
"forcats_0.3.0
stringr_1.3.0
dplyr_0.7.5
purrr_0.2.4
readr_1.1.1
tidyr_0.8.0
tibble_1.4.2
tidyverse_1.2.1
mime_0.5
rjson_0.2.15
RCurl_1.95-4.8
bitops_1.0-6
data.tree_0.7.4
Hmisc_4.1-1
Formula_1.2-2
survival_2.41-3
lattice_0.20-35
lmerTest_2.0-36
lme4_1.1-14
Matrix_1.2-12
kableExtra_0.6.1
knitr_1.20
cowplot_0.9.1
ggplot2_2.2.1
writexl_0.2
readxl_1.1.0
viridis_0.5.1
viridisLite_0.3.0
broom_0.4.4
scales_0.5.0
nlme_3.1-131
lubridate_1.7.4
RColorBrewer_1.1-2
httr_1.3.1
rprojroot_1.2
tools_3.4.2
backports_1.1.1
R6_2.2.2
rpart_4.1-11
lazyeval_0.2.1
colorspace_1.3-2
nnet_7.3-12
withr_2.1.2
tidyselect_0.2.4
gridExtra_2.3
mnormt_1.5-5
compiler_3.4.2
cli_1.0.0
rvest_0.3.2
htmlTable_1.11.0
xml2_1.2.0
influenceR_0.1.0
checkmate_1.8.5
psych_1.7.8
digest_0.6.15
foreign_0.8-69
minqa_1.2.4
rmarkdown_1.9
base64enc_0.1-3
pkgconfig_2.0.1
htmltools_0.3.6
htmlwidgets_0.9
rlang_0.2.0
rstudioapi_0.7
bindr_0.1.1
visNetwork_2.0.1
jsonlite_1.5
acepack_1.4.1
rgexf_0.15.3
magrittr_1.5
Rcpp_0.12.16
munsell_0.4.3
stringi_1.1.6
yaml_2.1.15
MASS_7.3-47
plyr_1.8.4
crayon_1.3.4
haven_1.1.1
splines_3.4.2
hms_0.4.2
pillar_1.2.2
igraph_1.1.2
reshape2_1.4.3
XML_3.98-1.9
glue_1.2.0
evaluate_0.10.1
downloader_0.4
latticeExtra_0.6-28
modelr_0.1.1
data.table_1.10.4-3
nloptr_1.0.4
cellranger_1.1.0
gtable_0.2.0
assertthat_0.2.0
bindrcpp_0.2.2
cluster_2.0.6
Rook_1.1-1
DiagrammeR_0.9.2
brew_1.0-6") %>% 
  mutate(
    local_version = map(
      .x = Package, 
      .f = function(x) {
        if (x %in% installed.packages()[, "Package"]) {
          packageDescription(pkg = x, fields = "Version")
        } else {
          NULL
        }
      }
    ),
    needs_update = local_version!=Version
  ) %>% 
  filter(needs_update) %>% 
  mutate(
    install = map2(
      .x = Package, 
      .y = Version, 
      .f = function(x, y) {
        install_version(
          package = x, 
          version = as.character(y), 
          repos = "http://cran.us.r-project.org" # "https://cran.rstudio.com/"
        )
      }
    )
  )

