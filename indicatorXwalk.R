#' indicatorXwalk
#' 
#' indicatorXwalk converts a dataset from one format to another given a cross-walk
#' definition.
#'
#' @param data.input A data.frame of data to convert
#' @param data.xwalk A data.frame containing 3 columns: 
#'   OutputColumnHeading, InputColumnHeading, order
#' @param noMatch.to.blankColumn A logical(TRUE): TRUE or FALSE as to if all output 
#'   columns are included or only those matching the input columns
#'
#' @return A data.frame containing the converted data
#' @export
#'
#' @examples
#'
indicatorXwalk <- function(data.input, data.xwalk, noMatch.to.blankColumn=TRUE){
  
  # Purge non-matching input data
  index.inputColMatch = match( names(data.input), data.xwalk$InputColumnHeading )
  data.input = data.input[ , !is.na(index.inputColMatch) ]
  
  # Convert input to output column names
  names(data.input) = data.xwalk$OutputColumnHeading[ na.exclude(index.inputColMatch) ]
  
  # Pad output with empty unmatched columns if desired
  if(noMatch.to.blankColumn){
    index.outputColNoMatch = setdiff(1:nrow(data.xwalk), index.inputColMatch)
    data.input[ , data.xwalk$OutputColumnHeading[ index.outputColNoMatch ] ] = NA
  }
  
  # Re-order the output columns
  index.outputColMatch = match(data.xwalk$OutputColumnHeading, names(data.input) )
  data.input = data.input[ , data.xwalk$order[ na.exclude(index.outputColMatch) ] ]
  
  return( data.input )
}
