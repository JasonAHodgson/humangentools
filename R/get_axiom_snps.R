#' Get a list of SNP RS ids from desired Axiom Human Origins ascertainment Panels
#'
#' `get_axiom_snps` returns a vector of snps corresponding to requested Human Origins ascertainment panels
#' @param panel a list of panels to be included.
#' @return a list of SNP RS ids.
#' @export




get_axiom_snps <- function(panel) {

  file_path <- system.file("extdata", "Axiom_Human_Origins_Panels_10-2025.Rtable", package = "tidypopgenTools")

  # read in the data
  d <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE)

  # filter d for the requested panels in the input panel list
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
      return(character(0))
    }

    d <- d[d$panel %in% valid_factors, ]
  }
  return(unique(d$RS))

}


