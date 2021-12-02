#' Read in sheets
#'
#' @param file_name A string of a filename
#' @param data_folder A string of the path of the folder that contains the file
#' @param skip_df A data.frame containing the following columns: filename, sheets, and skip,
#' which contain string of filename, string of sheet name, and integer of number of rows to skip, respectively
#' https://stackoverflow.com/questions/60898358/how-can-i-add-the-sheet-name-to-my-data-frame-as-a-variable
#' @import readxl
read.sheets <- function(file_name, data_folder, skip_df){
  print(file_name)
  skip_df_filt <- skip_df %>% filter(filename == file_name) %>% filter(!is.na(skip))
  print(skip_df_filt)
  path <- here(data_folder, file_name)

  sheet_dat = map2(skip_df_filt$sheets,
                   skip_df_filt$skip,
                   ~ mutate(
                     read_excel(.x,
                                path = path,
                                skip = .y,
                                na = c("NA", ".", "none", " ", "No data", "No Data")) %>%
                       filter_all(any_vars(complete.cases(.))) ,
                     sheet = .x,
                     filename = file_name))


}

#' Read one excel sheet with read_excel
#' This function can be incorporated into read.sheets/ read.sheets may become deprecated
#'
#' @inheritParams read_excel
read.excelsheet <- function(path, sheet, skip, na, col_names, guess_max,
                            complete_cases = TRUE){

  sheet_df <- read_excel(path = path,
                         sheet = sheet,
                         skip = skip,
                         na = na,
                         col_names = col_names,
                         guess_max = guess_max)

  if (complete_cases){
    sheet_df <- sheet_df %>%
      filter_all(any_vars(complete.cases(.))) # remove empty rows
  }

  sheet_df
}



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
#' @inheritParams read.excelsheet
read.multsheets <- function(data_folder,
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
      path <- here(data_folder, current$filename)

      if (str_detect(current$filename,".xls")){
        #print(current$filename)
        print("Using read.excelsheet")
        dat <- read.excelsheet(path = path,
                               sheet = current$sheets,
                               skip = current$header_end-1,
                               na = na,
                               col_names = col_names,
                               guess_max = guess_max,
                               complete_cases = complete_cases)

        if (!is.na(current$header_start) & (current$header_start < current$header_end)){
          header <- read.excelheader(current$sheets,
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
#

# Can use the below code to reproduce the structure of read.sheets
# b <- lapply(files, function(x){
#   df2 <- maps_sheets %>% filter(filename == x)
#   a <- read.multsheets(df2, na = na_values)
#
# }) %>% set_names(files)

#'
#' List the names of all sheets within given files
#' @param data_folder A string pointing to the parent path
#' @param files A vector of filenames that will be pasted to the parent path
#' @param reg_ex A regular expression to select sheets using
#' @export
list.sheetnames <- function(data_folder, files, reg_ex = NULL){

  sheet_info <- map(files, ~ data.frame(filename = .x,
                                        sheets = excel_sheets(here(data_folder, .x))))
  sheet_info <- bind_rows(sheet_info)

  if (!is.null(reg_ex)){
    sheet_info <- sheet_info %>%
      filter(str_detect(sheets, regex(reg_ex, ignore_case = T)))
  }
  return(sheet_info)
}

#' Read excel headers that span over multiple rows
#'
#' @param sheet_name
#' @param path
#' @param header_start A numeric denoting the row number of the start of the header
#' Note!: If there are any blank rows at the top of the sheet, start the row counting
#' at the first filled row
#' https://github.com/tidyverse/readxl/issues/194#issuecomment-266829259
#' @param header_end A numeric denoting the row number of the end of the header
#' @import tibble
#' @import readxl
read.excelheader <- function(sheet_name, path, header_start, header_end, unique_names){
  # Read in full dataset and slice the header
  # If only read in the header, the columns without header names may be dropped
  header1 = read_excel(sheet_name,
                       path = path,
                       col_names = FALSE,
                       na = c("NA", ".", "none", " ")) %>%
    slice(header_start:header_end)

  header2 <- data.frame(t(header1))

  header3 <- header2 %>%
    mutate(across(everything(), ~as.character(.x))) %>%
    unite(colnames , everything(), na.rm = TRUE) %>%
    rownames_to_column(., var = "rowname") %>%
    mutate(colnames = ifelse(colnames == "", rowname, colnames)) %>%
    mutate(colnames = gsub("\r?\n|\r", " ", colnames))

  if (unique_names){
    header3 <- header3 %>% mutate(colnames_dupcheck = make.unique(colnames))  %>%
      mutate(colnames = ifelse(colnames != colnames_dupcheck, paste0(colnames, rowname), colnames))

  }

  return(header3)
}


