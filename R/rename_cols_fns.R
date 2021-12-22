#' Rename columns using a .csv containing old and new column names
#' e.g. var_unique_rename.csv 
#' 
#' @param ls A list of data.frames whose columns will be renamed
#' @param rename_df A data.frame that has a column containing old column names and new column names
#' @param rename_col A variable name that corresponds to the column containing new column names
#' @param old_col A variable name that corresponds to the column to be renamed
#' @param element_col A variable name that corresponds to the name of the list element to be renamed.
#' The default value is NULL, meaning that the rename will be applied to all elements in ls
#' @param rm_col A logical specifying whether to remove columns that have been renamed to "rm_col" in rename_df
rename.col <- function(ls, rename_df, rename_col, old_col, element_col = NULL, rm_col = FALSE){
  
  rename_col_enquo <- enquo(rename_col)
  old_col_enquo <- enquo(old_col)
  element_col_enquo <- enquo(element_col)

  rename_df <- rename_df %>% ungroup() %>%
    mutate(!!rename_col_enquo := 
             ifelse(is.na(!!rename_col_enquo), 
                    !!old_col_enquo, 
                    !!rename_col_enquo)) 

  ls_rename <- imap(ls, function(x, y){
    print(y)
    
    if (!rlang::quo_is_null(element_col_enquo)){
      rename_df <- rename_df %>% filter(!!element_col_enquo == y)
    } else {
      message("Renaming all data.frames with the same rename values")
    }
    
    rename_df_temp <- rename_df %>%
      select(!!old_col_enquo, !!rename_col_enquo) %>% 
      filter(!!old_col_enquo %in% names(x)) %>%
      mutate(!!rename_col_enquo := make.unique(!!rename_col_enquo)) 
    
    #print(head(rename_df_temp))
    rename_vec <- tibble::deframe(rename_df_temp)
    
    #print(rename_vec)
    df2 <- rename_with(.data = x, 
                       .cols = tidyselect::one_of(names(rename_vec)), 
                       .fn = function(z){rename_vec[z]})
    if (rm_col){
      df2 <- df2 %>% select(-contains("rm_col"))
      message("Columns renamed 'rm_col' will be removed")
    }
    
    return(df2)
    })

}

#' Rename columns for a list of data.frames based on numeric type (integer versus double)
#' Related to select.colsfromlist
#' Note: this function does not rename based on column type (int versus num).  This function
#' renames based on whether the numeric is a decimal or round number
#' @inheritParams select.colsfromlist
#' @param rename_int A string to rename the column if the column values are all integers
#' @param rename_int A string to rename the column if the columns values are doubles
rename.intdouble <- function(data_list, reg_ex, rename_int, rename_double){
  
  cols_only <- imap(data_list, function(dat, y){
    print(y)
    dat_reg_ex <- dat %>% select(matches(reg_ex))
    print(paste(dat$filename[1],dat$sheet[1]))
    n_double <- dat_reg_ex %>%
      summarise(across(everything(), ~ sum(.x%%1 != 0, na.rm = TRUE))) %>% # sum of 0 means that all values are integers
      mutate(across(everything(), ~ ifelse(.x == 0, rename_int, rename_double))) # if 0, then corresponds to Plot No and if not 0, then corresponds to lbs/plot
    
    rename_temp <- structure(as.character(n_double[1,]), names = names(n_double))
    
    ## rename columns
    
    df <- rename_with(.data = dat, .cols = matches(reg_ex), .fn = function(x){rename_temp[x]})
    attr(df, paste0("rename.", reg_ex)) <- rename_temp 
    # want to print out which columns are renamed to what
    return(df)
  })
}

