### global libraries ===============================================================================
library(targets)
library(tarchetypes)
library(here)
library(data.table)
library(future)
library(future.callr)

# tar_option_set(cue = tar_cue(mode = "never"))

# targets::tar_renv(extras = "visNetwork", path = "scripts/_dependencies.R")


### project setup ==================================================================================
invisible(sapply(list.files(here("scripts"), pattern = "^tar-.*R$", full.names = TRUE), source, echo = FALSE))

plan(future.callr::callr, workers = 3)
# plan(multicore, workers = 40)
message(sprintf("Number of workers: %d", future::nbrOfWorkers()))
# setDTthreads(threads = 1)


### targets setup ==================================================================================
tar_setup <- list(
  tar_target(project, sub("(.*)_[^_]*\\.Rproj$", "\\1", list.files(here(), pattern = ".Rproj$")), packages = "here"),
  tar_target(author, "Mickaël Canouil, *Ph.D.*"),
  tar_target(output_directory, here::here("outputs"), packages = "here"),
  tar_target(results_directory, file.path(output_directory, "results_meta")),
  tar_target(vep_symbol, 
    command = get_symbol_vep(
      input_directory = results_directory,
      output_directory = output_directory,
      genome_assembly = "GRCh37",
      ensembl_version = "104",
      ensembl_species = "homo_sapiens",
      vep_cache = c(
        "server" = "/media/Data/ExternalData/vep_data", 
        "docker" = "/disks/DATA/ExternalData/vep_data"
      )
    ),
    packages = c("here", "data.table")
  ),
  tar_target(veb_symbol_file,
    command = format_symbol_vep(vep_symbol),
    format = "file"
  )
)


### targets ========================================================================================
list(
  tar_setup,
  tar_target(traits_dt, 
    command = tar_group(dplyr::group_by(get_results_files(results_directory), dates, traits)),
    packages = "dplyr",
    iteration = "group"
  ),
  tar_target(traits_gg,
    command = plot_manhattan(
      data = traits_dt,
      path = here("outputs"),
      width = 16,
      height = 10,
      units = "cm",
      res = 300,
      scaling = 0.75,
      annotation = veb_symbol_file
    ),
    pattern = map(traits_dt),
    iteration = "list",
    format = "file", 
    packages = c(
      "ggplot2", "ggtext", "patchwork", "ragg", "data.table", 
      "grDevices", "ggrepel", "scales", "here"
    )
  )
)
