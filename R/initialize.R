initialize_iea_data <- function(file){
  
}



#' Fix IEA data header
#' 
#' The IEA extended energy balance files headers are not conducive to creating data frames.
#' They contain two-line headers with blanks in the wrong places.
#' This function fixes the header to ensure that it works properly.
#'
#' @param unfixed_header the header (first two lines) of an IEA data file as a string.
#'
#' @return a fixed version of the header as a string.
#' 
#' @export
#'
#' @examples
#' fix_header(",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT")
fix_header <- function(unfixed_header, newlinechar = "\n", incorrect_first_line_lead = ",,TIME", correct_first_line_lead = "COUNTRY,FLOW,PRODUCT"){
  headerlines <- strsplit(unfixed_header, split = newlinechar) %>% unlist()
  nlines <- length(headerlines)
  # Reject this input if it doesn't have 1 or 2 lines.
  assertthat::assert_that(nlines %in% c(1,2), msg = paste("unfixed_header should have only 1 or 2 lines:", nlines, "lines were found."))

  # Check to see if this header is already "fixed"
  if (nlines == 1 & unfixed_header %>% startsWith(correct_first_line_lead)) {
    return(unfixed_header)
  }
  
  if (nlines == 2 & headerlines[[1]] %>% startsWith(correct_first_line_lead) & headerlines[[2]] %>% startsWith(correct_first_line_lead)) {
    # Return only the first line.
    return(headerlines[[1]])
  }
  # Ensure that we have 2 lines
  assertthat::assert_that(nlines == 2, msg = "length of unfixed_header != 2 in fix_header()")
  out <- gsub(pattern = incorrect_first_line_lead, replacement = correct_first_line_lead, x = headerlines[[1]])
  return(out)
}