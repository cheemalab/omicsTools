#' Load an omics dataset
#'
#' Reads common delimited, Excel, and serialized R formats without attaching
#' additional packages. Tabular inputs are returned as tibbles.
#'
#' @param file_path Path to the input file.
#' @param format Optional format override. By default the file extension is
#'   used.
#' @param ... Additional arguments passed to the corresponding reader.
#'
#' @return A tibble for tabular data, or the stored R object for `rds`, `rda`,
#'   and `rdata` files. An R data file containing multiple objects returns a
#'   named list.
#' @export
#' @author Yaoxiang Li
load_omics_data <- function(file_path, format = NULL, ...) {
  if (length(file_path) != 1L || !file.exists(file_path)) {
    stop("`file_path` must identify one existing file.", call. = FALSE)
  }

  if (is.null(format)) {
    format <- tools::file_ext(file_path)
  }
  format <- tolower(sub("^\\.", "", format))

  if (format %in% c("xls", "xlsx") && !requireNamespace("readxl", quietly = TRUE)) {
    stop("Install readxl to import Excel files.", call. = FALSE)
  }

  value <- switch(
    format,
    csv = readr::read_csv(file_path, show_col_types = FALSE, ...),
    tsv = readr::read_tsv(file_path, show_col_types = FALSE, ...),
    txt = readr::read_delim(file_path, show_col_types = FALSE, ...),
    xlsx = readxl::read_xlsx(file_path, ...),
    xls = readxl::read_xls(file_path, ...),
    rds = readRDS(file_path),
    rda = .load_r_data_file(file_path),
    rdata = .load_r_data_file(file_path),
    stop("Unsupported file format: ", format, call. = FALSE)
  )

  if (is.data.frame(value) || is.matrix(value)) {
    return(tibble::as_tibble(value))
  }
  value
}

#' Load omics data
#'
#' @inheritParams load_omics_data
#' @return The value returned by [load_omics_data()].
#' @export
#' @author Yaoxiang Li
load_data <- function(file_path, format = NULL, ...) {
  load_omics_data(file_path, format = format, ...)
}

# Author: Yaoxiang Li
.load_r_data_file <- function(file_path) {
  env <- new.env(parent = baseenv())
  names_loaded <- load(file_path, envir = env)
  values <- mget(names_loaded, envir = env, inherits = FALSE)
  if (length(values) == 1L) {
    return(values[[1L]])
  }
  values
}

#' Locate a bundled plot example
#'
#' @param relative_path Path under the package's example directory.
#' @return The path to the example file.
#' @export
#' @author Yaoxiang Li
omics_example_file <- function(relative_path) {
  if (length(relative_path) != 1L || is.na(relative_path) ||
      !nzchar(relative_path) ||
      grepl("(^|[/\\\\])\\.\\.($|[/\\\\])|^[/\\\\]|^[A-Za-z]:", relative_path)) {
    stop("`relative_path` must identify a file inside the example directory.", call. = FALSE)
  }
  path <- system.file("extdata", relative_path, package = "omicsTools")
  if (!nzchar(path) || dir.exists(path)) {
    stop("No bundled example at `", relative_path, "`.", call. = FALSE)
  }
  path
}
