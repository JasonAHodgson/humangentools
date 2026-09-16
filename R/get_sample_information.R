#' Gets sample information for a list of sample IDs
#'
#' `get_sample_information` takes a list of sample IDs and returns population and region information for each sample
#' @param ID a list of sample IDs
#' @param na.fill Logical; if `TRUE` (the default), IDs not found in the
#'   sample information data are kept in the result with `NA` population/
#'   region. If `FALSE`, they are dropped instead.
#' @return a data frame of sample id, population, and region
#' @importFrom utils read.table
#' @export


get_sample_information <- function(ID, na.fill = TRUE){
  file_path <- system.file("extdata", "sample_information.Rtable", package = "humangentools")

  if (file_path == "") {
    stop("Data file not found in package. Make sure it is in inst/extdata/ before building the package.")
  }

  d <- read.table(file_path, header=TRUE)

  # check if all ID are present in d$id
  missing_ID <- ID[!(ID %in% d$id)]
  if(length(missing_ID) > 0){
    warning(sprintf("The following IDs were not found in the sample information data: %s", paste(missing_ID, collapse = ", ")))
  }

  # filter d for only the IDs in the input list, keep all missing_ID, but fill missing data with NA if na.fill is TRUE
  result <- d[d$id %in% ID, ]
  if(na.fill && length(missing_ID) > 0){
    missing_rows <- data.frame(
      id = missing_ID, population = NA_character_, region = NA_character_
    )
    result <- rbind(result, missing_rows)
  }

  # reorder result to match the order of input ID
  result <- result[match(ID, result$id), ]
  # remove any rows with NA if na.fill is FALSE (dropping unmatched IDs is
  # the whole point of na.fill = FALSE, so `result` legitimately ends up
  # shorter than `ID` here -- only na.fill = TRUE guarantees a 1:1 match)
  if(!na.fill){
    result <- result[!is.na(result$id), ]
  }

  # sanity check: with na.fill = TRUE every ID should have matched to
  # exactly one row (either real data or a filled-in NA row above)
  if (na.fill && nrow(result) != length(ID)) {
    stop("Error: The number of rows in the result does not match the number of input IDs.\nConsider using na.fill = TRUE to fill missing data with NA.")
  }

  result
}
