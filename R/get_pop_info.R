#' Returns a table of information about populations in a given dataset.
#'
#' `get_pop_info` returns a data frame of information about populations in the
#' package's bundled genetic datasets. Populations are keyed by `pop`, a
#' canonical code following the convention of `kgp::allmeta`: a bare three
#' letter code for 1000 Genomes populations (`YRI`), and `<Name><DATASET>`
#' elsewhere (`YorubaHGDP`, `YorubaSGDP`, `YRIHapMap`). The same group sampled
#' by two projects therefore gets two codes, which keeps genotypes from
#' different sequencing platforms separable.
#'
#' Populations are also classified as present-day or ancient in the `temporal`
#' column, and can be filtered on it. This matters because the AADR contributes
#' several thousand ancient populations: an analysis that assumes a living
#' population, such as anything joined to ethnographic data, wants
#' `temporal = "modern"`.
#'
#' Two sets of coordinates are stored for each population: `origin_lat`/
#' `origin_lon`, the group's ethnographic homeland, and `sampling_lat`/
#' `sampling_lon`, where the samples were actually collected. These differ for
#' diaspora cohorts -- 1000 Genomes GIH was sampled in Houston but originates in
#' Gujarat -- and for some populations an origin is not a point at all, in which
#' case the origin coordinates are `NA` and `coord_note` says why. The
#' `location` argument chooses which pair is copied to the convenience columns
#' `lat` and `lon`.
#'
#' @param samples A gen_tibble object, or a character vector of sample ids.
#'   Default is to return all populations in the package dataset.
#' @param pop A character vector of canonical population codes to filter by.
#'   Default is `NULL` (no filtering).
#' @param population A character vector of human readable population labels to
#'   filter by. Each population has exactly one `population_label`; where source
#'   datasets named the same group differently, the other names are kept
#'   pipe-separated in `population_alt` (`GBR` is labelled `British` with
#'   `English` as an alternative). Matching considers both columns, so either
#'   name finds the population. Default is `NULL` (no filtering).
#' @param region A character vector specifying regions to filter populations by.
#'   Default is NULL (no filtering).
#' @param dataset A character vector of source datasets to include (matched
#'   against `source_dataset`; see `dataset_information.Rtable` for the
#'   vocabulary). Default is all datasets in the package.
#' @param temporal A character vector restricting populations by age: one or
#'   both of `"modern"` (present-day) and `"ancient"`. Every population from
#'   HGDP, 1000 Genomes, SGDP, HapMap and the individual-study datasets is
#'   `"modern"`; AADR contributes both. Default `NULL` (no filtering), which
#'   returns ancient populations alongside present-day ones -- pass
#'   `temporal = "modern"` for analyses that assume a living population.
#' @param location One of `"origin"` (the default) or `"sampling"`, choosing
#'   which coordinate pair is returned in the `lat` and `lon` columns. The
#'   explicit `origin_*` and `sampling_*` columns are always returned as well.
#' @param include A character vector specifying which columns to include in the
#'   output. Default is all columns.
#' @param exclude A character vector specifying which columns to exclude from
#'   the output. Default is no columns excluded.
#' @return A data frame with population information.
#' @importFrom utils read.table
#' @export
get_pop_info <- function(
  samples = NULL,
  pop = NULL,
  population = NULL,
  region = NULL,
  dataset = NULL,
  temporal = NULL,
  location = c("origin", "sampling"),
  include = NULL,
  exclude = NULL
) {

  location <- match.arg(location)

  file_path <- system.file("extdata", "population_information.Rtable", package = "humangentools")

  if (file_path == "") {
    stop("Data file not found in package. Make sure it is in inst/extdata/ before building the package.")
  }

  pop_info <- read.table(
    file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE,
    na.strings = "NA", quote = "\""
  )

  # convenience aliases, so callers that just want "the" coordinates do not have
  # to choose a column name; the explicit pairs stay in the output either way
  pop_info$lat <- if (location == "origin") pop_info$origin_lat else pop_info$sampling_lat
  pop_info$lon <- if (location == "origin") pop_info$origin_lon else pop_info$sampling_lon
  front <- c("pop", "population_label", "population_alt", "population_desc",
             "source_dataset", "temporal", "region", "lat", "lon")
  pop_info <- pop_info[, c(front, setdiff(names(pop_info), front)), drop = FALSE]

  # filter by samples if provided
  if (!is.null(samples)) {
    if (inherits(samples, "gen_tibble")) {
      sample_ids <- unique(samples$id)
    } else if (is.character(samples)) {
      sample_ids <- samples
    } else {
      stop("samples must be a gen_tibble or a character vector of sample ids.")
    }
    sample_pops <- humangentools::get_sample_information(
      ID = sample_ids, dataset = dataset, na.fill = FALSE
    )$pop
    pop_info <- pop_info[pop_info$pop %in% unique(sample_pops), , drop = FALSE]
  }

  if (!is.null(pop)) {
    pop_info <- pop_info[pop_info$pop %in% pop, , drop = FALSE]
  }

  # `population_label` is single-valued; any other name the source datasets used
  # for the same group lives in `population_alt`, pipe-separated. Both are
  # matched, so a caller can pass either name.
  if (!is.null(population)) {
    alt <- strsplit(ifelse(is.na(pop_info$population_alt), "",
                           pop_info$population_alt), "|", fixed = TRUE)
    keep <- pop_info$population_label %in% population |
      vapply(alt, function(labels) any(labels %in% population), logical(1))
    pop_info <- pop_info[keep, , drop = FALSE]
  }

  if (!is.null(region)) {
    pop_info <- pop_info[pop_info$region %in% region, , drop = FALSE]
  }

  if (!is.null(dataset)) {
    pop_info <- pop_info[pop_info$source_dataset %in% dataset, , drop = FALSE]
  }

  if (!is.null(temporal)) {
    bad <- setdiff(temporal, c("modern", "ancient"))
    if (length(bad) > 0) {
      stop("`temporal` must be one or both of \"modern\" and \"ancient\"; got: ",
           paste(bad, collapse = ", "))
    }
    pop_info <- pop_info[pop_info$temporal %in% temporal, , drop = FALSE]
  }

  if (!is.null(include)) {
    include <- intersect(include, colnames(pop_info))
    pop_info <- pop_info[, include, drop = FALSE]
  }

  if (!is.null(exclude)) {
    exclude <- intersect(exclude, colnames(pop_info))
    pop_info <- pop_info[, !(colnames(pop_info) %in% exclude), drop = FALSE]
  }

  rownames(pop_info) <- NULL
  pop_info
}
