#' A wrapper for read_excelheader to maintain backwards compatibility
#'
#'@inheritParams read_header
#' @rdname read_excelheader
#' @export
read.excelheader <- function(sheet_name, path, header_start, header_end, unique_names){
  message("This function name is being retained for backwards compatibility.
          Please use read_excelheader()")
  read_excelheader(sheet_name = sheet_name,
                   path = path,
                   header_start = header_start,
                   header_end = header_end,
                   unique_names = unique_names)

}


#' A wrapper for read_sheets to maintain backwards compatibility
#'
#' @rdname read_sheets
#' @export
read.sheets <- function(file_name, data_folder, skip_df){
  message("This function name is being retained for backwards compatibility.
          Please use read_sheets()")
  read_sheets(file_name = file_name,
              data_folder = data_folder,
              skip_df = skip_df)

}

#' A wrapper for read_excelsheet to maintain backwards compatibility
#'
#' @rdname read_excelsheet
#' @export
read.excelsheet <- function(path, sheet, skip, na, col_names, guess_max,
                            complete_cases = TRUE){
  message("This function name is being retained for backwards compatibility.
          Please use read_excelsheet()")
  read_excelsheet(path = path,
                  sheet = sheet,
                  skip = skip,
                  na = na,
                  col_names = col_names,
                  guess_max = guess_max,
                  complete_cases = complete_cases)

}

