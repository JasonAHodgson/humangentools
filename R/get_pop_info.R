#' Returns a table of information about populations in a given dataset.
#'
#' `get_pop_info` returns a data frame containing information about populations in the specified dataset.
#' @param samples A gen_tibble object, or a string with sample id. Default is to return all populations in the package dataset.
#' @param region A character vector specifying regions to filter populations by. Default is NULL (no filtering).
#' @param population A character vector specifying populations to filter by. Default is NULL (no filtering).
#' @param dataset A string specifying the dataset to include. Default is all datasets in the package.
#' @param include A charcter vector specifying which columns to include in the output. Default is all columns.
#' @param exclude A character vector specifying which columns to exclude from the output. Default is no columns excluded.
#' @return A data frame with population information.
#' @export


get_pop_info <- function(
  samples = NULL,
  region = NULL,
  population = NULL,
  dataset = NULL,
  include = NULL,
  exclude = NULL
) {

  file_path <- system.file("extdata", "population_information.Rtable", package = "humangentools")

  # read in the data
  pop_info <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE)

  # filter by samples if provided
  if (!is.null(samples)) {
    # check if samples is a gen_tibble or a character vector
    if (inherits(samples, "gen_tibble")) {
      sample_ids <- unique(samples$id)
    } else if (is.character(samples)) {
      sample_ids <- samples
    } else {
      stop("samples must be a gen_tibble or a character vector of sample ids.")
    }
  }

    # get populations corresponding to sample ids if sample_ids is provided
  if (exists("sample_ids")) {
    sample_pops <- humangentools::get_sample_information(ID = sample_ids, na.fill = FALSE)$population

    # filter pop_info for these populations
    pop_info <- pop_info[pop_info$population %in% sample_pops, ]
  }

  # filter by region if provided
  if (!is.null(region)) {
    pop_info <- pop_info[pop_info$region %in% region, ]
  }

  # filter by population if provided
  if (!is.null(population)) {
    pop_info <- pop_info[pop_info$population %in% population, ]
  }

  # filter by dataset if provided
  if (!is.null(dataset)) {
    pop_info <- pop_info[pop_info$dataset %in% dataset, ]
  }

  # include only specified columns if provided
  if (!is.null(include)) {
    include <- intersect(include, colnames(pop_info))
    pop_info <- pop_info[, include, drop = FALSE]
  }

  # exclude specified columns if provided
  if (!is.null(exclude)) {
    exclude <- intersect(exclude, colnames(pop_info))
    pop_info <- pop_info[, !(colnames(pop_info) %in% exclude), drop = FALSE]
  }

  return(pop_info)
}
