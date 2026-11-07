#' Finds individuals present in more than one source dataset
#'
#' `get_sample_overlap` returns the individuals that the bundled genotype
#' datasets have in common, so that duplication is visible before a merge
#' rather than after it. 3,471 of the 24,579 individuals in the package's
#' holdings are present in more than one source dataset -- 2,514 of them shared
#' between the AADR and 1000 Genomes alone -- and a user who merges two datasets
#' without checking counts those individuals twice.
#'
#' The package is the only place that holds the cross-dataset individual key:
#' `id` joins across datasets, while `source_id` is the contributing dataset's
#' own key (see [get_sample_information()]). This function is the use that
#' distinction implies.
#'
#' @section What the result is, and is not:
#'
#' The return is one row per `(id, source_dataset)` pair, so `nrow()` is a count
#' of *rows*, not of individuals: 35 individuals shared between HGDP and SGDP
#' give 70 rows. Count individuals with `length(unique(x$id))`.
#'
#' Duplication is only the part of the problem that metadata can see. Three
#' further hazards are not visible here and are not reported by this function:
#'
#' * **Absence of a row does not mean the individuals are unrelated.** It means
#'   only that they are not recorded as the *same* individual. Cryptic
#'   relatedness across projects needs genotypes to detect.
#' * **Dropping a duplicate is a choice of platform, not a cleanup step.** The
#'   two copies of an individual were genotyped by different projects on
#'   different platforms with different SNP ascertainment and different genotype
#'   representation -- AADR 1240K data is pseudo-haploid, 1000 Genomes and SGDP
#'   data is diploid. Keeping the AADR copy rather than the 1000 Genomes copy
#'   changes the ascertainment profile and the representation of the merged set,
#'   and for the Axiom Human Origins array the ascertainment panels are not
#'   mutually exclusive, so panel membership cannot be used to reason about the
#'   result either. De-duplicating does not make two datasets comparable; it
#'   only stops one individual being counted twice.
#' * **Batch and population are nearly collinear in these holdings**, because
#'   each source dataset was genotyped in its own laboratory. The duplicated
#'   individuals returned here are the one place where the same person was
#'   measured twice, and so are the control that lets a batch effect be
#'   measured rather than assumed away.
#'
#' @section Matching is on the exact id string, so counts are lower bounds:
#'
#' Two individuals are treated as the same individual when their `id` strings
#' are identical. Where a dataset re-releases another project's individuals
#' under its own names, the link is missed. This is known to be material for
#' AADR against SGDP: 267 AADR individuals carry `S_`-prefixed ids following the
#' SGDP naming convention and **none of them matches an SGDP id exactly**,
#' against the 46 AADR-SGDP matches exact matching does find. The AADR `.anno`
#' source file carries an `Individual ID` column, not currently shipped in
#' `sample_information.Rtable`, that resolves at least 95 of them
#' (`S_Dusun-1.DG` has `Individual ID = Dusun22`, which is an SGDP id in the
#' package).
#'
#' **The AADR-SGDP figure in particular is therefore a lower bound.** When that
#' column is added to the shipped table this function will gain a `match_basis`
#' column and the counts will rise; that is the fix landing, not a regression.
#'
#' @section How the filters interact with the overlap:
#'
#' `dataset` restricts the candidate pool **before** overlap is computed, and is
#' the argument that asks a merge question. `dataset = c("HGDP", "SGDP")` asks
#' "which individuals are in both of these" and returns the 35 individuals those
#' two share, not the 929 that HGDP shares with the AADR. `dataset = NULL`
#' counts every dataset in the holdings, so `get_sample_overlap()` and
#' `get_sample_overlap(dataset = c("AADR", "KGP"))` are not the same question
#' restricted -- they are different questions.
#'
#' `samples`, `pop` and `temporal` select which of the overlapping individuals
#' are *reported*; they do not shrink the pool. An individual is kept when any
#' of its rows in the pool matches, and then all of its rows in the pool are
#' returned, so `n_datasets` and `datasets` keep describing the individual
#' rather than the filter. For `pop` this is the only workable reading:
#' population codes carry the dataset they came from, so every one of the 3,471
#' overlapping individuals has a *different* `pop` in each dataset, and a `pop`
#' filter applied to the pool before the overlap was computed would always
#' return nothing.
#'
#' Note that `temporal` describes the age of an individual's population, and the
#' AADR contributes several thousand ancient populations. Any analysis that
#' assumes a living group -- anything joined to ethnographic data through
#' [get_dplace_link()], for instance -- wants `temporal = "modern"`.
#'
#' @section Within-dataset duplication is a different problem:
#'
#' An individual sequenced twice by one project, which the AADR records as two
#' library types (`.AG` and `.SG`) under one `id`, has `n_datasets == 1` and is
#' **not** returned at the default `min_datasets = 2L`. That is deliberate:
#' within-dataset library duplication has a different fix, and folding it into
#' `n_datasets` would make the count uninterpretable. It is surfaced instead by
#' `n_source_ids`, which counts distinct `source_id` values for the individual
#' across the pool and is always returned.
#'
#' @param samples A gen_tibble object, or a character vector of sample ids.
#'   Default `NULL` considers all holdings. Ids absent from the candidate pool
#'   warn; they do not error.
#' @param dataset A character vector of source datasets defining the candidate
#'   pool (matched against `source_dataset`; see [get_dataset_info()] for the
#'   vocabulary). Overlap is computed **within** this pool. Default `NULL` (all
#'   datasets). A dataset name not in the holdings warns.
#' @param pop A character vector of canonical population codes to filter by.
#'   Applied after overlap is computed: an individual is kept when any of its
#'   rows in the pool carries one of these codes. Default `NULL` (no filtering).
#' @param temporal A character vector restricting individuals by the age of
#'   their population: one or both of `"modern"` and `"ancient"`. Applied after
#'   overlap is computed. Default `NULL` (no filtering). Anything other than
#'   `"modern"` or `"ancient"` is an error, matching [get_pop_info()].
#' @param min_datasets A single non-negative whole number: the number of
#'   distinct source datasets an individual must appear in to be returned.
#'   Default `2L`. `min_datasets = 1L` is legal and degenerates to a listing of
#'   the pool.
#' @param include A character vector specifying which columns to include in the
#'   output. Default is all columns.
#' @param exclude A character vector specifying which columns to exclude from
#'   the output. Default is no columns excluded.
#' @return A data frame with one row per `(id, source_dataset)` pair, with
#'   columns `id`, `source_dataset`, `source_id`, `data_type`, `pop`,
#'   `population`, `temporal`, `n_datasets`, `n_source_ids` and `datasets`.
#'   `datasets` is the pipe-separated sorted set of source datasets the
#'   individual appears in within the pool, following the convention of
#'   `population_alt` in [get_pop_info()]. Rows are grouped by `id` and ordered
#'   by descending `n_datasets`, then `id`. When nothing overlaps the return is
#'   a zero-row data frame with the same columns and the same column types, not
#'   `NULL`.
#' @seealso [get_sample_information()] for the per-sample metadata this is
#'   derived from, and [get_dataset_info()] for the dataset vocabulary.
#' @examples
#' # how badly would merging the AADR with 1000 Genomes double-count?
#' ov <- get_sample_overlap(dataset = c("AADR", "KGP"))
#' length(unique(ov$id))
#'
#' # the documented three-dataset individual
#' get_sample_overlap(samples = "HGDP01414")
#' @importFrom utils read.table
#' @export
get_sample_overlap <- function(
  samples = NULL,
  dataset = NULL,
  pop = NULL,
  temporal = NULL,
  min_datasets = 2L,
  include = NULL,
  exclude = NULL
) {

  if (!is.numeric(min_datasets) || length(min_datasets) != 1L ||
      is.na(min_datasets) || !is.finite(min_datasets) ||
      min_datasets < 0 || min_datasets != round(min_datasets)) {
    stop("`min_datasets` must be a single non-negative whole number.")
  }
  min_datasets <- as.integer(min_datasets)

  if (!is.null(temporal)) {
    bad <- setdiff(temporal, c("modern", "ancient"))
    if (length(bad) > 0) {
      stop("`temporal` must be one or both of \"modern\" and \"ancient\"; got: ",
           paste(bad, collapse = ", "))
    }
  }

  file_path <- system.file("extdata", "sample_information.Rtable", package = "humangentools")

  if (file_path == "") {
    stop("Data file not found in package. Make sure it is in inst/extdata/ before building the package.")
  }

  d <- read.table(
    file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE,
    na.strings = "NA", quote = "\""
  )

  # `dataset` defines the candidate pool, and overlap is counted inside it --
  # so asking about a pair of datasets gives what that pair shares, not what
  # either shares with everything else
  if (!is.null(dataset)) {
    missing_ds <- setdiff(dataset, d$source_dataset)
    if (length(missing_ds) > 0) {
      warning(sprintf(
        "Dataset(s) not found: %s. Available: %s",
        paste(missing_ds, collapse = ", "),
        paste(sort(unique(d$source_dataset)), collapse = ", ")
      ))
    }
    d <- d[d$source_dataset %in% dataset, , drop = FALSE]
  }

  pool_ids <- unique(d$id)

  # per-individual summaries over the pool. `n_source_ids` counts the dataset's
  # own keys, so AADR library duplication (.AG/.SG under one id) is visible
  # without being able to inflate `n_datasets`
  n_datasets   <- vapply(split(d$source_dataset, d$id),
                         function(x) length(unique(x)), integer(1))
  n_source_ids <- vapply(split(d$source_id, d$id),
                         function(x) length(unique(x)), integer(1))
  datasets     <- vapply(split(d$source_dataset, d$id),
                         function(x) paste(sort(unique(x)), collapse = "|"),
                         character(1))

  keep <- names(n_datasets)[n_datasets >= min_datasets]
  d <- d[d$id %in% keep, , drop = FALSE]

  # `samples`, `pop` and `temporal` choose which overlapping individuals to
  # report; they never change what the pool was, so an individual kept by any
  # one of its rows keeps all of its rows
  if (!is.null(samples)) {
    if (inherits(samples, "gen_tibble")) {
      sample_ids <- unique(samples$id)
    } else if (is.character(samples)) {
      sample_ids <- samples
    } else {
      stop("samples must be a gen_tibble or a character vector of sample ids.")
    }
    missing_ids <- sample_ids[!(sample_ids %in% pool_ids)]
    if (length(missing_ids) > 0) {
      warning(sprintf(
        "The following IDs were not found in the candidate pool: %s",
        paste(missing_ids, collapse = ", ")
      ))
    }
    d <- d[d$id %in% sample_ids, , drop = FALSE]
  }

  if (!is.null(pop)) {
    missing_pop <- setdiff(pop, d$pop)
    if (length(missing_pop) > 0) {
      warning(sprintf(
        "Population code(s) not found among overlapping individuals: %s",
        paste(missing_pop, collapse = ", ")
      ))
    }
    d <- d[d$id %in% unique(d$id[d$pop %in% pop]), , drop = FALSE]
  }

  if (!is.null(temporal)) {
    d <- d[d$id %in% unique(d$id[d$temporal %in% temporal]), , drop = FALSE]
  }

  result <- data.frame(
    id             = as.character(d$id),
    source_dataset = as.character(d$source_dataset),
    source_id      = as.character(d$source_id),
    data_type      = as.character(d$data_type),
    pop            = as.character(d$pop),
    population     = as.character(d$population),
    temporal       = as.character(d$temporal),
    n_datasets     = as.integer(n_datasets[d$id]),
    n_source_ids   = as.integer(n_source_ids[d$id]),
    datasets       = as.character(datasets[d$id]),
    stringsAsFactors = FALSE
  )

  result <- result[order(-result$n_datasets, result$id,
                         result$source_dataset, result$source_id), , drop = FALSE]

  if (!is.null(include)) {
    include <- intersect(include, colnames(result))
    result <- result[, include, drop = FALSE]
  }

  if (!is.null(exclude)) {
    exclude <- intersect(exclude, colnames(result))
    result <- result[, !(colnames(result) %in% exclude), drop = FALSE]
  }

  rownames(result) <- NULL
  result
}
