#' A wrapper around read_ods to read multiple sheets in a .ods file
#'
#' @param filename File to import
#'
#' @return A list of data frames--one data frame for each sheet
#' @export
read_ods_multsheets <- function(filename) {
  sheets <- readODS::list_ods_sheets(filename)
  x <- lapply(sheets, function(X) readODS::read_ods(filename, sheet = X))
  names(x) <- sheets
  x
}
