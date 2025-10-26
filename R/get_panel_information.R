#' Provides Axiom Human Origins panel ascertainment information
#'
#' `get_panel_information` returns ascertainment information for the various SNP panels.
#' @param panel A list of panels for which ascertainment information is desired
#' @return a table of ascertainment information
#' @export

get_panel_information <- function(panel = "all") {
  file_path <- system.file("extdata", "Axiom_Panel_info.Rtable", package = "tidypopgenTools")

  if (file_path == "") {
    stop("Data file not found in package. Make sure it is in inst/extdata/ before building the package.")
  }

  d <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE)

  if (!("all" %in% panel)) {
    panel <- as.character(panel)
    valid_factors <- panel[panel %in% unique(d$panel)]

    if (length(valid_factors) == 0) {
      warning(
        sprintf(
          "No valid panel names found in data file. Valid options are: %s",
          paste(unique(d$panel), collapse = ", ")
        )
      )
      return(d[0, ])
    }

    d <- d[d$panel %in% valid_factors, ]
  }

  return(d)
}
