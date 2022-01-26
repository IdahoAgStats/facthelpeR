#' Read multiple files (csv or excel) and/or multiple sheets (within an excel file)
#' and handle headers that span multiple rows.
#'
#' Read in multiple files and or multiple sheets and handle headers
#' that span multiple rows (using `read_header()`)
#' This function must be provided a data.frame with file information.
#' The data.frame can be initalized with the function `list_sheetnames()`
#'
#' @param data_folder A string denoting the folder that contains the
#' files to be read in
#' @param df A data.frame with the column "filename", which can be
#' inialized by the function `list_sheetnames()`
#' Required data.frame columns:
#'   - filename: name of the .xlsx or .csv
#' Optional columns:
#'   - sheets: name of the sheet (required for excel files with multiple sheets)
#'   - list_names: names to label the element in the list.  If list_names is not
#'  provided, the function will label the list elements using the
#'  concatenation of the file and sheet name
#'   - header_start: the row number corresponding to the start of the header
#'   - header_end: the row number corresponding to the last row of the header
#' header_end = 1 corresponds to the header being in only the first row and
#' corresponds to skipping 0 rows.  If header_end is not provided, the default
#' is set to 1, which will skip 0 rows.
#' Note: If header_end is NA, the file/sheet will be removed (not read in)
#' @inheritParams read_excelsheet
#' @import readr
#' @family readin functions
#' @export
read_multsheets <- function(data_folder,
                            df,
                            na = c("NA"),
                            col_names,
                            guess_max = 1000,
                            complete_cases = TRUE){

  if (col_names == FALSE){
    message("col_names is set to FALSE.  Data will be read-in without column names.")
  }

  if (!"header_end" %in% names(df)){
    df <- df %>% mutate(header_end = 1)
    message("Adding header_end")
  }

  if (!"header_start" %in% names(df)){
    df <- df %>% mutate(header_start = NA)
  }

  # Create names for list elements, if not provided
  if (!"list_names" %in% names(df)){
    if("sheets" %in% names(df)){
      df <- df %>% mutate(list_names = paste(filename, sheets))
    } else {
      df <- df %>% mutate(list_names = filename)
    }
  }

  # Remove files that shouldn't be read in
  df <- df %>% filter(!is.na(header_end))

  dat_ls <- df %>%
    pmap(function(...) {
      current <- tibble(...)
      # do cool stuff and access content from current row with
      path <- paste(data_folder, current$filename, sep = "/")

      if (str_detect(current$filename,".xls")){
        #print(current$filename)
        print("Using read_excelsheet")
        dat <- read_excelsheet(path = path,
                               sheet = current$sheets,
                               skip = current$header_end-1,
                               na = na,
                               col_names = col_names,
                               guess_max = guess_max,
                               complete_cases = complete_cases)


      } else if (str_detect(current$filename,".csv")){
        print("Using read_csv")
        dat <- read_csv(file = path,
                        skip = current$header_end-1,
                        na = na,
                        col_names = col_names)

      }

      # Replace the header if it is multiple lines
      if (!is.na(current$header_start) & (current$header_start < current$header_end)){
        header <- read_header(current$sheets,
                              path,
                              current$header_start,
                              current$header_end, unique_names = TRUE)
        length_newheader <- nrow(header)
        names(dat)[1:length_newheader] <- header$colnames

      }


      return(dat)
    }) %>%
    set_names(df$list_names)

  return(dat_ls)
}

#' A wrapper for read_multsheets to maintain backwards compatibility
#'
#' @inheritParams read_multsheets
#' @rdname read_multsheets
#' @export
read.multsheets <- function(data_folder,
                            df,
                            na = c("NA"),
                            col_names,
                            guess_max = 1000,
                            complete_cases = TRUE){

  message("This function name is being retained for backwards compatibility.
          Please use read_multsheets()")

  read_multsheets(data_folder = data_folder,
                  df = df,
                  na = na,
                  col_names = col_names,
                  guess_max = guess_max,
                  complete_cases = complete_cases)


}

