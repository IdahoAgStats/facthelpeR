#' Read in sheets
#'
#' This function may be deprecated (read_multsheets() has many improvements
#' over read_sheets)
#' Note: this function was previously read.sheets()
#'
#' @param file_name A string of a filename
#' @param data_folder A string of the path of the folder that contains the file
#' @param skip_df A data.frame containing the following columns: filename, sheets, and skip,
#' which contain string of filename, string of sheet name, and integer of number of rows to skip, respectively
#' https://stackoverflow.com/questions/60898358/how-can-i-add-the-sheet-name-to-my-data-frame-as-a-variable
#' @import readxl
#' @family readin functions
read_sheets <- function(file_name, data_folder, skip_df){
  print(file_name)
  skip_df_filt <- skip_df %>% filter(filename == file_name) %>% filter(!is.na(skip))
  print(skip_df_filt)
  path <- paste(data_folder, file_name, sep = "/")

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
#' Note: this function was previously read.excelsheet()
#' @inheritParams readxl::read_excel
#' @importFrom stats complete.cases
#' @param complete_cases A logical. The default TRUE will remove empty rows
#' @family readin functions
read_excelsheet <- function(path, sheet, skip, na, col_names, guess_max,
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

#' Read excel headers that span over multiple rows
#'
#' Note: this function was previously read.excelheader()
#'
#' @param sheet_name A string denoting the sheetname
#' @param path A path to the file that contains the sheet
#' @param header_start A numeric denoting the row number of the start of the header
#' Note!: If there are any blank rows at the top of the sheet, start the row counting
#' at the first filled row
#' https://github.com/tidyverse/readxl/issues/194#issuecomment-266829259
#' @param header_end A numeric denoting the row number of the end of the header
#' @param unique_names A logical denoting whether make.unique() for the column names
#' @import tibble
#' @import readxl
#' @import tidyr
#' @family readin functions
read_excelheader <- function(sheet_name, path, header_start, header_end, unique_names){
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

