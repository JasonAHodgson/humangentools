#' Get a list of SNP RS ids from desired Axiom Human Origins ascertainment Panels
#'
#' `get_axiom_snps` returns a list of snps corresponding to requested Human Origins ascertainment panels
#' @param panel a list of panels to be included.
#' @return a list of SNP RS ids.
#' @export




get_axiom_snps <- function(panel) {

  file_path <- system.file("extdata", "Axiom_Human_Origins_Panels_10-2025.Rtable", package = "tidypopgenTools")

  if (file_path == "") {
    stop("Data file not found in package. Make sure it is in inst/extdata/ before building the package.")
  }

  d <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE)

  valid_factors <- panel[panel %in% unique(d$panel)]

  result <- unlist(lapply(valid_factors, function(f) {
    d$RS[d$panel == f]
  }))

  return(result)
}


