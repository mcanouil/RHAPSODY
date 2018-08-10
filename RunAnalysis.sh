#!/bin/sh

R --slave --silent <<RSCRIPT

# set the output directory or leave as is; 
# output will be generated where the Rmarkdown file is
working_directory <- './' 

analysis_step <- 7
cohort_name <- 'Cohort_Name'
author_name <- 'Firstname LASTNAME'
opal_credentials <- 'opal_credentials.txt'
vcf_directory <- '/media/.../vcf'
imputation_quality_tag <- 'INFO' # To be set according to VCF (could also be "R2")


# Run the analysis
dir.create(path = working_directory, showWarnings = FALSE)
rmarkdown::render(
  input = '/home/rhapsody/WP3/scripts/RHAPSODY_WP3_PreDiab.Rmd', 
  output_format = output_format, 
  output_file = paste0(
    'RHAPSODY_WP3_PreDiab_', 
    cohort_name, '_step', 
    analysis_step, '.html'
  ), 
  output_dir = working_directory,
  params = list(
    cohort_name = cohort_name, 
    author_name = author_name, 
    opal_credentials = opal_credentials,
    vcf_input_directory = vcf_directory, 
    imputation_quality_tag = imputation_quality_tag, 
    vcftools_binary_path = '/usr/local/bin',
    output_directory = working_directory,
    analysis_step = analysis_step,
    format_vcfs = TRUE,
    variants_analysis = TRUE,
    n_cpu = 2,
    echo = FALSE, # Should R code be printed in the report
    warning = FALSE, # Should warnings be printed in the report
    message = FALSE # Should messages be printed in the report
  ),
  encoding = 'UTF-8'
)

RSCRIPT
