#' A wrapper around read_ods to read multiple sheets in a .ods file
#'
#' Read in multiple sheets in a .ods file.
#'
#' Currently, read_ods will only read one sheet at a time. read_ods_multsheets
#' can handle multiple sheets in one .ods workbook, or, when used with
#' list.files and lapply, multiple workbooks with multiple sheets.
#'
#' The result is a named list of data frames, with the sheet name appended
#' to the basename of the filename with the file extension removed.
#'
#' @param path Path to the .ods file to import.
#'
#' @return A list of data frames--one data frame for each sheet.
#' @family readin functions
#' @examples
#' \dontrun{
#' write_ods(iris[iris$Species=="setosa",], "iris.ods", sheet = "setosa")
#'
#' write_ods(iris[iris$Species=="versicolor",], "iris.ods",
#'           sheet = "versicolor", append = TRUE)
#'
#' write_ods(iris[iris$Species=="virginica",],
#'           "iris.ods", sheet = "virginica", append = TRUE)
#'
#' read_ods_multsheets("iris.ods")
#'
#' # Read multiple files with multiple sheets
#' files <- list.files(data_folder, recursive = TRUE, pattern = "*.ods")
#'
#' # Use lapply() with the result of list.files
#' lapply(files, read_ods_multsheets)
#' }
#' @export
#'
read_ods_multsheets <- function(path) {
  sheets <- readODS::list_ods_sheets(path)
  x <- lapply(sheets, function(X) readODS::read_ods(path, sheet = X))
  list_names <- stringr::str_remove(basename(path), ".ods$")
  names(x) <- paste(list_names, sheets, sep = "_")
  x
}
