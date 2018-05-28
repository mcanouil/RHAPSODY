#!/bin/bash

R --slave --silent <<RSCRIPT
# set the output directory or leave as is; 
# output will be generated where the Rmarkdown file is
working_directory <- './' 

# Analysis
rmarkdown::render(
  input = "RHAPSODY_WP3_PreDiab.Rmd",
  output_format = "html_document",
  output_file = paste0(reportName, ".html"),
  output_dir = working_directory,
  params = list(
    cohort_name = 'CohortName',
    author_name = 'Firstname LASTNAME',
    opal_credentials = 'opal_credentials.txt',
    vcf_input_directory = './vcfs',
    imputation_quality_tag = 'INFO', # To be set according to VCF (could also be "R2")
    vcftools_binary_path = './vcftools/vcftools_latest/bin',
    output_directory = working_directory,
    analysis_step = 2,
    format_vcfs = FALSE,
    variants_analysis = FALSE,
    n_cpu = 2,
    echo = FALSE, # Should R code be printed in the report
    warning = TRUE, # Should warnings be printed in the report
    message = TRUE # Should messages be printed in the report
  ),
  encoding = "UTF-8"
)

RSCRIPT

