#' Read multiple sheets listed in a data.frame
#' created by list.sheetnames
#' @param data_folder A string denoting the folder that contains the
#' files to be read in
#' @param df A data.frame created by the function list.sheetnames
#' The df can be modified to include a column "header_end," which denotes
#' the number of the row corresponding to the last row of the header.
#' header_end = 1 corresponds to the header being in only the first row and
#' corresponds to skipping 0 rows.
#' If header_end is not provided, the default
#' is set to 1, which will skip 0 rows.  Similarly, the df can also include a column of "list_names,"
#' which denote the name of the list object.  If list_names is not provided,
#' the default is the concatenation of the file and sheet name
#' Note: If header_end is NA, the file/sheet will be removed (not read in)
#' @inheritParams read_excelsheet
#' @import readr
#' @export
#' @family readin functions
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
    message("Appending header_end")
  }

  if (!"header_start" %in% names(df)){
    df <- df %>% mutate(header_start = NA)
  }

  if (!"list_names" %in% names(df)){
    if("sheets" %in% names(df)){
      df <- df %>% mutate(list_names = paste(filename, sheets))
    } else {
      df <- df %>% mutate(list_names = filename)
    }
  }

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

        if (!is.na(current$header_start) & (current$header_start < current$header_end)){
          header <- read_excelheader(current$sheets,
                                     path,
                                     current$header_start,
                                     current$header_end, unique_names = TRUE)
          length_newheader <- nrow(header)
          names(dat)[1:length_newheader] <- header$colnames

        }
      } else if (str_detect(current$filename,".csv")){
        print("Using read_csv")
        dat <- read_csv(file = path,
                        skip = current$header_end-1,
                        na = na,
                        col_names = col_names)

      }


      return(dat)
    }) %>%
    set_names(df$list_names)

  return(dat_ls)
}

