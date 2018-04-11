#!/bin/bash

R --slave --silent <<RSCRIPT
# set the output directory or leave at is; 
# output will be generated where the Rmarkdown file is
wd <- './' 

# Analysis
rmarkdown::render(
  input = 'RHAPSODY_WP3_PreDiab.Rmd', 
  output_format = "html_document", 
  output_dir = wd,
  params = list(
    cohort_name = 'CohortName',
    author_name = 'Firstname LASTNAME',
    opal_server = 'http://localhost:8080',
    opal_login = 'administrator', 
    opal_password = 'password', 
    output_directory = wd,
    vcf_input_directory = './vcfs',
    imputation_quality_tag = 'INFO', 
    output_code = FALSE, 
    vcftools_binary_path = '/vcftools/vcftools_latest/bin', 
    format_vcfs = FALSE,
    variants_analysis = FALSE
  ),
  encoding = 'UTF-8'
)

RSCRIPT

