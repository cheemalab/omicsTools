#' Annotate MS1 mass-to-charge values
#'
#' Search for MS1 annotations.
#'
#' @param AnnotaData Numeric vector of observed m/z values.
#' @param masstole Non-negative mass tolerance.
#' @param toleUnit Tolerance unit, `1` or `"Da"`, or `2` or `"ppm"`.
#' @param annotaDB Database name, `"metlin"` or `"hmdb"`.
#' @param ionMode Ion mode, `"pos"`, `"neg"`, or `"neu"` for neutral masses.
#' @param adducts Optional CEU Mass Mediator adduct names. By default all
#'   compatible adducts are searched.
#' @return A tibble of CEU Mass Mediator matches.
#' @export
#' @author Yaoxiang Li
ms1_annotation <- function(AnnotaData,
                           masstole = 0.05,
                           toleUnit = 1,
                           annotaDB = "metlin",
                           ionMode = "pos",
                           adducts = NULL) {
  masses <- unique(suppressWarnings(as.numeric(AnnotaData)))
  masses <- masses[is.finite(masses) & masses > 0]
  if (!length(masses)) stop("Supply at least one positive m/z value.", call. = FALSE)
  if (!is.numeric(masstole) || length(masstole) != 1L ||
      !is.finite(masstole) || masstole < 0) {
    stop("`masstole` must be one non-negative number.", call. = FALSE)
  }
  unit <- match.arg(tolower(as.character(toleUnit)), c("1", "da", "2", "ppm"))
  ppm <- unit %in% c("2", "ppm")
  database <- match.arg(tolower(annotaDB), c("metlin", "hmdb"))
  mode <- match.arg(tolower(ionMode), c("pos", "neg", "neu"))

  result <- .search_ms1(
    databases = jsonlite::toJSON(database),
    masses_mode = if (mode == "neu") "neutral" else "mz",
    ion_mode = if (mode == "neg") "negative" else "positive",
    adducts = jsonlite::toJSON(if (is.null(adducts)) "all" else as.character(adducts)),
    tolerance = if (ppm) masstole else masstole * 1000,
    tolerance_mode = if (ppm) "ppm" else "mDa",
    unique_mz = masses
  )
  tibble::as_tibble(result)
}

# Author: Yaoxiang Li
.search_ms1 <- function(...) {
  if (!requireNamespace("cmmr", quietly = TRUE)) {
    stop("Install the cmmr package to search CEU Mass Mediator.", call. = FALSE)
  }
  cmmr::batch_search(...)
}
