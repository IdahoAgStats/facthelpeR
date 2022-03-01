#' A wrapper around read_ods to read multiple sheets in a .ods file
#'
#' @param filename File to import
#'
#' @return A list of data frames--one data frame for each sheet
#' @export
#'
#' @examples
#' dat <- c("iris", "mtcars")
#' path <- "/home/em/Desktop/"
#'
#' readODS::write_ods(get(dat[1]), paste(path, "x.ods"), row_names = FALSE, sheet = dat[1])
#'
#' for(i in 2:length(dat)) {
#' readODS::write_ods(get(dat[i]), paste(path, "x.ods"), row_names = FALSE, sheet = dat[i],
#'   append = TRUE)}
#'
#' read_ods_multsheets(paste(path, "x.ods"))
#'
read_ods_multsheets <- function(filename) {
  sheets <- readODS::list_ods_sheets(filename)
  x <- lapply(sheets, function(X) readODS::read_ods(filename, sheet = X))
  names(x) <- sheets
  x
}
