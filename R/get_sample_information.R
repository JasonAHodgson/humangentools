#' Gets sample information for a list of sample IDs
#'
#' `get_sample_information` takes a list of sample IDs and returns population and region information for each sample
#' @param ID a list of sample IDs
#' @return a data frame of sample id, population, and region
#' @export


get_sample_information <- function(ID, na.fill = TRUE){
  file_path <- system.file("extdata", "sample_information.Rtable",package = "tidypopgenTools")
  d <- read.table(file_path, header=TRUE)

  # check if all ID are present in d$id
  missing_ID <- ID[!(ID %in% d$id)]
  if(length(missing_ID) > 0){
    warning(sprintf("The following IDs were not found in the sample information data: %s", paste(missing_ID, collapse = ", ")))
  }

  # filter d for only the IDs in the input list, keep all missing_ID, but fill missing data with NA if na.fill is TRUE
  result <- d[d$id %in% ID, ]
  if(na.fill){
    missing_rows <- data.frame(id = missing_ID, population = NA, region = NA)
    result <- rbind(result, missing_rows)
  }

  # reorder result to match the order of input ID
  result <- result[match(ID, result$id), ]
  # remove any rows with NA if na.fill is FALSE
  if(!na.fill){
    result <- result[!is.na(result$id), ]
  }


  # return the result if the data frame is the same length as input ID, else return an error
  if (nrow(result) != length(ID)) {
    stop("Error: The number of rows in the result does not match the number of input IDs.\nConsider using na.fill = TRUE to fill missing data with NA.")
  } else {
    return(result)
  }




}
