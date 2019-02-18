args <- commandArgs(trailingOnly = TRUE)
# set the output directory or leave as is; 
# output will be generated where the Rmarkdown file is
working_directory <- '/media/rhapsody_output'

cohort_name <- as.character(args[1])
analysis_step <- as.numeric(args[2])
format_vcfs <- as.logical(args[3])
variants_analysis <- TRUE
author_name <- 'Firstane Lastname'
opal_credentials <- '/media/docker_analysis/opal_credentials.txt'
vcf_directory <- '/media/vcf/'
imputation_quality_tag <- 'INFO' # To be set according to VCF (could also be 'R2')


# Run the analysis
# dir.create(path = working_directory, showWarnings = FALSE, mode = '0777')
capture.output(file = paste0(working_directory, cohort_name, ".log"), {
  rmarkdown::render(
    input = '/home/rhapsody/WP3/scripts/RHAPSODY_WP3_PreDiab.Rmd', 
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
      vcftools_binary_path = '/usr/local/bin',
      output_directory = working_directory,
      analysis_step = analysis_step,
      format_vcfs = TRUE,
      variants_analysis = variants_analysis,
      chunk_size = 1000,
      n_cpu = parallel::detectCores(),
      echo = FALSE, # Should R code be printed in the report
      warning = FALSE, # Should warnings be printed in the report
      message = FALSE # Should messages be printed in the report
    ),
    encoding = 'UTF-8'
  )
  
  if (variants_analysis) {
    rmarkdown::render(
      input = '/home/rhapsody/WP3/scripts/utils/check_analysis.Rmd',
      output_format = 'pdf_document',
      output_file = paste0('RHAPSODY_WP3_PreDiab_', cohort_name, '_check.pdf'),
      output_dir = working_directory,
      params = list(
        cohort_name = cohort_name,
        author_name = author_name,
        output_directory = working_directory
      ),
      encoding = 'UTF-8'
    )
  }
})
