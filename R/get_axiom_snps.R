#' Get a list of SNP RS ids from desired Axiom Human Origins ascertainment Panels
#' 
#' `get_axiom_snps` returns a list of snps corresponding to requested Human Origins ascertainment panels
#' @param panel a list of panels to be included.
#' @return a list of SNP RS ids.
#' @export




get_axiom_snps <- function(panel) {
  
  d <- read.table(file=system.file("Axiom_Human_Origins_Panels_10-2025.Rtable"))
  
   # Ensure that factor_list elements actually exist in d$panel
  valid_factors <- panel[panel %in% levels(d$panel)]
  
  # Create a named list: each factor -> matching RS values
  result <- lapply(valid_factors, function(f) {
    d$RS[d$panel == f]
  })
  
  # Name the list elements by the factor levels
  names(result) <- valid_factors
  
  return(result)
}
