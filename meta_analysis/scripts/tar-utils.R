#' get_results_files
#' @import data.table
get_results_files <- function(path) {
  data.table::data.table(
    file = list.files(path = path, pattern = "\\.csv.gz$", full.names = TRUE)
  )[
    j = `:=`(
      dates = sub(".*_([^_]+).csv.gz", "\\1", basename(file)),
      traits = sub("_.*", "", basename(file)),
      effects = sub("[^_]+_(.+)_[^_]+.csv.gz", "\\1", basename(file))
    )
  ]
}

#' get_symbol_vep
#' @import data.table
get_symbol_vep <- function(
  input_directory,
  output_directory = here::here("outputs"),
  genome_assembly = "GRCh38",
  ensembl_version = "104",
  ensembl_species = "homo_sapiens",
  vep_cache = c(
    "server" = "/media/Data/ExternalData/vep_data", 
    "docker" = "/disks/DATA/ExternalData/vep_data"
  )
) {
  data.table::fwrite(
    x = unique(data.table::rbindlist(lapply(
      X = list.files(path = input_directory, pattern = "\\.csv.gz$", full.names = TRUE),
      FUN = data.table::fread, select = c("CHR", "BP", "reference_allele", "other_allele")
    )))[
      i = order(CHR, BP),
      j = list(CHR, start = BP, end = BP, alleles = paste(reference_allele, other_allele, sep = "/"), strand = "+")
    ], 
    file = file.path(output_directory, "snps_locations.txt.gz"), 
    col.names = FALSE, row.names = FALSE, sep = "\t"
  )

  input_docker <- sub(
    "/disks/PROJECT",
    "/media/Datatmp",
    file.path(output_directory, "snps_locations.txt.gz")
  )
  output_docker <- paste0("snps_vep_", ensembl_version, ".0_", genome_assembly, ".txt")
  
  if (
    !file.exists(file.path(
      vep_cache[["docker"]], "homo_sapiens", paste0(ensembl_version, "_", genome_assembly)
    ))
  ) {
    system(sprintf(
      paste(
        "cd %s", 
        "curl -sO ftp://ftp.ensembl.org/pub/release-%s/variation/vep/%s_vep_%s_%s.tar.gz",
        "tar xzf %s_vep_%s_%s.tar.gz",
        "rm %s_vep_%s_%s.tar.gz",
        sep = " && "
      ),
      vep_cache[["docker"]],
      ensembl_version, ensembl_species, ensembl_version, genome_assembly,
      ensembl_species, ensembl_version, genome_assembly,
      ensembl_species, ensembl_version, genome_assembly
    ))
  }
  
  cat(paste(
    '#!/bin/bash',
    '\n\nchmod 777', dirname(input_docker),
    '\n\ndocker run',
    '--rm',
    '--name vep',
    '--volume', paste0(dirname(input_docker), ':/data_dir'),
    paste0('--volume ', vep_cache[["server"]], ':/opt/vep/.vep'),
    paste0('ensemblorg/ensembl-vep:release_', ensembl_version, '.0'),
    '/bin/bash -c "./vep',
    '--input_file', file.path("/data_dir", basename(input_docker)),
    '--cache',
    '--offline',
    '--fork 70',
    '--force_overwrite',
    '--assembly', genome_assembly, 
    '--check_existing',
    '--no_check_alleles',
    '--symbol',
    '--output_file', file.path("/data_dir", output_docker),
    '&& cut -f 1-4,13-14', file.path("/data_dir", output_docker), 
    '| bgzip --thread 70 -f >', file.path("/data_dir", paste0(output_docker, ".gz")),
    '"',
    '\n\nchmod 775', dirname(input_docker),
    '\n'
  ), file = file.path(output_directory, "run_docker_vep.sh"))

  file.path(output_directory, output_docker)
}

#' format_symbol_vep
#' @import data.table
format_symbol_vep <- function(file) {
  default_file <- file
  file <- paste0(file, ".gz")
  stopifnot(
    file.exists(file) && file.mtime(file.path(dirname(file), "run_docker_vep.sh")) < file.mtime(file)
  )
  if (file.exists(file)) {
    on.exit(unlink(c(
      file.path(dirname(file), "snps_locations.txt.gz"),
      default_file,
      paste0(default_file, "_summary.html")
    )))
  }
  vep_annotation <- data.table::fread(file = file,  skip = "#U")[ 
    j = c("CHR", "POS") := data.table::tstrsplit(Location, ":", fixed = TRUE)
  ][
    j = (c("Gene", "Symbol", "rsid")) :=
      list(
        paste(unique(Gene), collapse = ";"),
        data.table::fifelse(
          test = grepl("SYMBOL=", Extra), 
          yes = paste(unique(gsub("^.*SYMBOL=([^;]*);.*$", "\\1", Extra)), collapse = ";"),
          no = NA_character_
        ),
        paste(unique(Existing_variation), collapse = ";")
      ), 
    by = "#Uploaded_variation"
  ][j = list(CHR, POS, `#Uploaded_variation`, Gene, Symbol, rsid)]
  
   data.table::fwrite(
    x = data.table::setnames(unique(vep_annotation), "#Uploaded_variation", "chr_pos_ref_alt"), 
    file = sub(".txt.gz", "_formated.txt.gz", file)
  )
   
  sub(".txt.gz", "_formated.txt.gz", file)
}

#' fortify.manhattan
#' @import data.table
fortify.manhattan <- function(data, x, y, group) {
  map_chro <- c(seq(22), "X", "Y", "X", "Y")
  names(map_chro) <- c(seq(24), "X", "Y")
  
  `:=` <- data.table::`:=`
  
  out <- data.table::setnames(
    x = data.table::as.data.table(data), 
    old = c(x, y, group), 
    new = c("x_pos", "y_pval", "x_chr")
  )
  out[j = x_chr := as.character(x_chr)]
  out[j = x_chr := map_chro[gsub("^chr", "", x_chr, ignore.case = TRUE)]]
  out[j = x_chr := factor(x_chr, levels = intersect(c(seq(22), "X", "Y"), x_chr))]
  out[j = x_pos := as.double(x_pos)]
  out[order(x_chr, x_pos)]
  out[j = x_pos := scales::rescale(x = x_pos, to = c(-0.4, 0.4)), by = "x_chr"]
  out[j = x_pos := x_pos + as.integer(x_chr)]
  data.table::setnames(out, c("x_pos", "y_pval", "x_chr"), c("x", "y", "group"))
}

#' StatManhattan
#' @import ggplot2
StatManhattan <- ggplot2::ggproto("StatManhattan", ggplot2::Stat,
  required_aes = c("x", "y", "group"),
  setup_data = function(data, params) {
    fortify.manhattan(data, "x", "y", "group")
  },
  compute_layer = function(data, scales, params) {
    data
  }
)

#' pval_trans
#' @importFrom scales trans_new
pval_trans <- function(alpha = NULL, md = FALSE, prefix = FALSE, colour = "#b22222") {
  scales::trans_new(
    name = "pval",
    domain = c(0, 1),
    transform = function(x) {x[x < .Machine$double.xmin] <- .Machine$double.xmin; -log(x, 10)},
    inverse = function(x) {10^-x},
    breaks = (function(n = 5) {
      function(x) {
        max <- floor(-log(min(c(x, alpha), na.rm = TRUE), base = 10))
        if (max == 0) 1 else sort(unique(c(10^-seq(0, max, by = floor(max / n) + 1), alpha)))
      }
    })(),
    format = (function(x) {
      if (md & nchar(system.file(package = "ggtext")) != 0) {
        prefix_text <- if (prefix) "&alpha; = " else ""
        x_fmt <- gsub(
          "^(.*)e[+]*([-]*)0*(.*)$", 
          "\\1 &times; 10<sup>\\2\\3</sup>", 
          format(x, scientific = TRUE, digits = 3)
        )
        x_fmt[x %in% c(0, 1)] <- x[x %in% c(0, 1)]
        x_fmt <- gsub("^1 &times; ", "", x_fmt)
        alpha_idx <- format(x, scientific = TRUE, digits = 3) == format(alpha, scientific = TRUE, digits = 3)
        x_fmt[alpha_idx] <- paste0("<b style='color:", colour, ";'>", prefix_text, x_fmt[alpha_idx], "</b>")
        x_fmt
      } else {
        prefix_text <- if (prefix) "alpha == " else ""
        x_fmt <- gsub(
          "^(.*)e[+]*([-]*)0*(.*)$", 
          "\\1 %*% 10^\\2\\3", 
          format(x, scientific = TRUE)
        )
        x_fmt[x %in% c(0, 1)] <- x[x %in% c(0, 1)]
        x_fmt <- gsub("^1 \\%\\*\\% ", "", x_fmt)
        alpha_idx <- format(x, scientific = TRUE, digits = 3) == format(alpha, scientific = TRUE, digits = 3)
        x_fmt[alpha_idx] <- paste0(prefix_text, x_fmt[alpha_idx])
        parse(text = x_fmt)
      }
    })
  )
}

#' draw_manhattan
#' @import ggplot2
#' @import data.table
#' @importFrom scales viridis_pal
draw_manhattan <- function(data, x, y, chr, label_y = "P-value", alpha = 0.05) {
  data <- data.table::as.data.table(data)#[, .SD, .SDcols = c(x, y, chr)]
  data.table::setnames(data, c(x, y, chr), c("pos", "pvalue", "chr"), skip_absent = TRUE)
  if (is.numeric(data[["chr"]])) data[, "chr" := lapply(.SD, as.character), .SDcols = "chr"]
  ggplot2::ggplot(data = data)  +
    ggplot2::aes(x = .data[["pos"]], y = .data[["pvalue"]], colour = .data[["chr"]]) +
    ggplot2::geom_point(stat = "manhattan", size = 0.60, na.rm = TRUE) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf, xmax = Inf, ymin = 1, ymax = alpha,
      fill = "#b22222", alpha = 0.2, colour = NA
    ) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2, colour = "#b22222") +
    ggplot2::scale_x_continuous(
      breaks = 1:24,
      labels = c(1:22, "X", "Y"),
      expand = ggplot2::expansion(add = 0.25)
    ) +
    ggplot2::scale_y_continuous(
      trans = pval_trans(alpha = NULL, md = TRUE, colour = "#b22222"), 
      expand = ggplot2::expansion(mult = c(0, 0.2)), 
      limits = c(0.05, NA)
    ) +
    ggplot2::scale_colour_manual(values = rep(scales::viridis_pal(begin = 1/4, end = 3/4)(2), 12)) +
    ggplot2::labs(colour = "Chromosome", x = "Chromosome", y = label_y) +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), legend.position = "none") 
}

#' plot_manhattan
#' @import ggplot2
#' @import ggtext
#' @import patchwork
#' @import ragg
#' @import data.table
#' @import grDevices
#' @import ggrepel
plot_manhattan <- function(data, path, width, height, units, res, scaling, annotation = NULL) {
  filename <- sprintf("%s/%s", path, tolower(unique(data[["traits"]])))
  data <- data.table::as.data.table(data)[
    order(factor(effects, levels = c("not_Interaction", "Interaction")))
  ]
  list_dt <- lapply(data[["file"]], annotation_file = annotation, function(file, annotation_file) {
    merge(
      x = data.table::fread(file),
      y = data.table::fread(annotation_file),
      by.x = c("CHR", "BP"),
      by.y = c("CHR", "POS")
    )[
      i = `p-value` <= 0.05 / .N, 
      j = gene_label := data.table::fifelse(Symbol == "", NA_character_, Symbol)
    ][
      j = gene_label_min := data.table::fifelse(`p-value` == min(`p-value`), gene_label, NA_character_),
      by = "gene_label"
    ][`p-value` > 0.05, `p-value` := NA_real_][
      j = file := basename(file)
    ]
  })
  
  list_gg <- lapply(list_dt, function(dt, annotation_file) {
    draw_manhattan(
      data = dt[j = c("CHR", "BP", "p-value", "gene_label_min")][`p-value` > 0.05, `p-value` := NA_real_], 
      x = "BP", 
      y = "p-value", 
      chr = "CHR", 
      label_y = "P-value", 
      alpha = 0.05 / nrow(dt)
    ) +
      ggrepel::geom_label_repel(
        mapping = ggplot2::aes(label = gene_label_min), 
        stat = "manhattan", 
        show.legend = FALSE,
        min.segment.length = 0,
        # direction = "x", 
        size = 1.75
      ) +
      ggplot2::theme_minimal(base_family = "Verdana") +
      ggplot2::theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown(face = "italic"),
        axis.text.y = ggtext::element_markdown(), 
        panel.grid.major.x = ggplot2::element_blank(), 
        panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = "none"
      )
  })
  
  data.table::fwrite(
    x = data.table::rbindlist(lapply(
      X = list_dt, 
      FUN = function(dt) {
        dt[`p-value` <= 0.05 / .N, -c("gene_label", "gene_label_min")]
      }
    )), 
    file = sprintf("%s.csv", filename)
  )
  
  ragg::agg_png(
    filename = sprintf("%s.png", filename), 
    width = width,
    height = height, 
    units = units, 
    res = res, 
    scaling = scaling
  )
  print(
    patchwork::wrap_plots(list_gg, ncol = 1, nrow = 2) + 
      patchwork::plot_annotation(
        title = sprintf("Trait: %s", unique(data[["traits"]])),
        subtitle = paste0(
          paste(
            sprintf(
              "<b>%s)</b> %s",
              LETTERS[seq_len(nrow(data))],
              c(
                "Interaction" = "SNP &times; Time", 
                "not_Interaction" = "SNP"
              )[data[["effects"]]]
            ), 
          collapse = " and "
          ), 
          "."
        ),
        tag_levels = "A", 
        theme = ggplot2::theme_minimal(base_family = "Verdana") +
          ggplot2::theme(
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.title = ggtext::element_markdown(),
            plot.subtitle = ggtext::element_markdown(face = "italic")
          )
      )
  )
  invisible(grDevices::dev.off())
  
  paste0(filename, c(".csv", ".png"))
}
