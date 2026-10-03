#' Describes the genotype datasets bundled in humangentools
#'
#' `get_dataset_info` returns one row per source genotype dataset, with a
#' description, the genotyping technology, and how many populations and samples
#' it contributes. The `dataset` column is the vocabulary used by the
#' `source_dataset` column of [get_pop_info()] and [get_sample_information()],
#' and by their `dataset` arguments.
#'
#' Datasets are kept separate because their genotypes are not interchangeable:
#' they differ in platform, ascertainment and error profile. The same individual
#' appears under more than one dataset where more than one project genotyped
#' them, which is why [get_sample_information()] can return several rows for one
#' sample.
#'
#' @param dataset A character vector of dataset names to filter by. Default
#'   `NULL` (all datasets).
#' @return A data frame with columns `dataset`, `description`, `genotyping`,
#'   `n_pops`, `n_samples` and `reference`.
#' @importFrom utils read.table
#' @export
get_dataset_info <- function(dataset = NULL) {

  file_path <- system.file("extdata", "dataset_information.Rtable", package = "humangentools")

  if (file_path == "") {
    stop("Data file not found in package. Make sure it is in inst/extdata/ before building the package.")
  }

  d <- read.table(
    file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE,
    na.strings = "NA", quote = "\""
  )

  if (!is.null(dataset)) {
    missing_ds <- setdiff(dataset, d$dataset)
    if (length(missing_ds) > 0) {
      warning(sprintf(
        "Dataset(s) not found: %s. Available: %s",
        paste(missing_ds, collapse = ", "), paste(d$dataset, collapse = ", ")
      ))
    }
    d <- d[d$dataset %in% dataset, , drop = FALSE]
  }

  rownames(d) <- NULL
  d
}
