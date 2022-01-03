#' List the names of all sheets within given files
#'
#' Note: this function was previously list.sheetnames()
#' @param data_folder A string pointing to the parent path
#' @param files A vector of filenames that will be pasted to the parent path
#' @param reg_ex A regular expression to select sheets using
#' @import purrr
#' @import dplyr
#' @import stringr
#' @export
#' @family readin functions
list_sheetnames <- function(data_folder, files, reg_ex = NULL){

  sheet_info <- map(files, ~ data.frame(filename = .x,
                                        sheets = excel_sheets(paste(data_folder, .x, sep = "/"))))
  sheet_info <- bind_rows(sheet_info)

  if (!is.null(reg_ex)){
    sheet_info <- sheet_info %>%
      filter(str_detect(sheets, regex(reg_ex, ignore_case = T)))
  }
  return(sheet_info)
}

#' A wrapper for list_sheetnames to maintain backwards compatibility
#'
#' @rdname list_sheetnames
#' @export
list.sheetnames <- function(data_folder, files, reg_ex = NULL){
  message("This function name is being retained for backwards compatibility.
          Please use list_sheetnames()")
  list_sheetnames(data_folder = data_folder,
                  files = files,
                  reg_ex = NULL)

}


