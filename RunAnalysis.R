# set the output directory or leave as is; 
# output will be generated where the Rmarkdown file is
working_directory <- './' 

cohort_name <- 'Cohort_Name'
author_name <- 'Firstname LASTNAME'

n_cpu <- 2

opal_credentials <- 'opal_credentials.txt'

vcf_directory <- '/media/.../vcf'
imputation_quality_tag <- 'INFO' # To be set according to VCF (could also be 'R2')
vcftools_binary_path <- '/usr/local/bin'
format_vcfs <- TRUE

analysis_step <- 7
variants_analysis <- TRUE



# Run the analysis
dir.create(path = working_directory, showWarnings = FALSE, mode = '0777')
rmarkdown::render(
  input = '~/WP3/scripts/RHAPSODY_WP3_PreDiab.Rmd', 
  output_format = 'html_document', 
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
    vcftools_binary_path = vcftools_binary_path,
    output_directory = working_directory,
    analysis_step = analysis_step,
    format_vcfs = format_vcfs,
    variants_analysis = variants_analysis,
    chunk_size = 1000,
    n_cpu = n_cpu,
    echo = FALSE, # Should R code be printed in the report
    warning = FALSE, # Should warnings be printed in the report
    message = FALSE # Should messages be printed in the report
  ),
  encoding = 'UTF-8'
)
