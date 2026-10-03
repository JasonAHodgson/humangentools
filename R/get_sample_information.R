#' Gets sample information for a list of sample IDs
#'
#' `get_sample_information` takes a list of sample IDs and returns the canonical
#' population code, human readable label, region and source dataset for each.
#'
#' `id` identifies the individual and is shared across datasets, so it joins;
#' `source_id` is the contributing dataset's own key, and `data_type` records the
#' AADR library type (.AG, .SG, .DG and so on), `NA` elsewhere. One AADR
#' individual sequenced by two methods yields two rows with the same `id` and
#' different `source_id`, so rows are unique on `(source_id, pop)` rather than on
#' `(id, pop)`.
#'
#' A sample can legitimately appear more than once: the Simons Genome Diversity
#' Project resequenced HGDP cell lines, so `HGDP01414` is present as both
#' `BantuKenyaHGDP` and `BantuKenyaSGDP` -- one individual, two genotype
#' datasets. The result therefore has *at least* one row per input ID rather
#' than exactly one. Pass `dataset` to restrict to a single source and recover a
#' one-row-per-ID result.
#'
#' @param ID a character vector of sample IDs.
#' @param dataset An optional character vector of source datasets to restrict to
#'   (matched against `source_dataset`). Default `NULL` returns every dataset a
#'   sample appears in.
#' @param temporal A character vector restricting samples by the age of their
#'   population: one or both of `"modern"` and `"ancient"`. Default `NULL` (no
#'   filtering).
#' @param na.fill Logical; if `TRUE` (the default), IDs not found in the sample
#'   information data are kept in the result with `NA` population/region. If
#'   `FALSE`, they are dropped instead.
#' @return A data frame of sample id, canonical population code, population
#'   label, region and source dataset, ordered to follow `ID`.
#' @importFrom utils read.table
#' @export
get_sample_information <- function(ID, dataset = NULL, temporal = NULL,
                                  na.fill = TRUE) {

  file_path <- system.file("extdata", "sample_information.Rtable", package = "humangentools")

  if (file_path == "") {
    stop("Data file not found in package. Make sure it is in inst/extdata/ before building the package.")
  }

  d <- read.table(
    file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE,
    na.strings = "NA", quote = "\""
  )

  if (!is.null(dataset)) {
    d <- d[d$source_dataset %in% dataset, , drop = FALSE]
  }

  if (!is.null(temporal)) {
    bad <- setdiff(temporal, c("modern", "ancient"))
    if (length(bad) > 0) {
      stop("`temporal` must be one or both of \"modern\" and \"ancient\"; got: ",
           paste(bad, collapse = ", "))
    }
    d <- d[d$temporal %in% temporal, , drop = FALSE]
  }

  missing_ID <- ID[!(ID %in% d$id)]
  if (length(missing_ID) > 0) {
    warning(sprintf(
      "The following IDs were not found in the sample information data: %s",
      paste(missing_ID, collapse = ", ")
    ))
  }

  result <- d[d$id %in% ID, , drop = FALSE]

  if (na.fill && length(missing_ID) > 0) {
    missing_rows <- data.frame(
      id = missing_ID, source_id = NA_character_, data_type = NA_character_,
      pop = NA_character_, population = NA_character_, region = NA_character_,
      source_dataset = NA_character_, temporal = NA_character_,
      stringsAsFactors = FALSE
    )
    result <- rbind(result, missing_rows[, names(result), drop = FALSE])
  }

  # follow the order of ID; where a sample appears in several datasets its rows
  # stay together (order() is stable for this)
  result <- result[order(match(result$id, ID)), , drop = FALSE]

  # with na.fill = TRUE every requested ID must be represented by at least one
  # row -- but not by exactly one, since a sample can belong to more than one
  # source dataset (see Details)
  if (na.fill && !all(ID %in% result$id)) {
    stop("Internal error: some requested IDs are absent from the result.")
  }

  rownames(result) <- NULL
  result
}
