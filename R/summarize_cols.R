# The jobs of these functions can be clarified and standardized
# They were written on the fly to perform specific tasks
# However, their functions can be look at more carefully at a
# later time

#' Find column information in a data.frame
#'
#' For a data.frame, loop through the columns provided (cols_check)
#' and summarise the number of non-NA entries and the groups that
#' contain non-NA entries (for example if want to search trial, year, or filename),
#' and return the first non-NA entry as an example
#'
#' @param df A data frame
#' @param cols_check A character vector of column names to check
#' @param by_col A bare variable to summarise
#' @family summarize column functions
#' @export
find_col_info <- function(df, cols_check, by_col){
  # added functionality to return a message instead of an error if cols_check
  # is an empty vector (no invalid column names left to check)
  out <- if(length(cols_check) > 0) {

  df2 <- df %>%
    mutate(across(.cols = tidyselect::everything(),
                  .fns = ~ ifelse(.x == -9, NA, .x)))

  ans <- map(cols_check, function(x){
    x_sym <- rlang::sym(x)
    ans <- df2 %>%
      filter(!is.na(!!x_sym)) %>%
      dplyr::summarise(n = n(),  # count the total number of entries
                       contained_in = paste(unique({{by_col}}), collapse = ";")) %>%
      unique(.) %>%
      mutate(variable = x) %>%
      mutate(example = as.character(first(na.omit(df2[[x]]))))
  })

  bind_rows(ans) %>% arrange(variable)
  } else {
    print("No column names left to check. Great job!")
  }
  out
}

#' Summarize information on variables that match a given regex
#'
#' Summarize information on variables that match a given regex.
#' This function loops through a list of data.frames and pulls out the columns
#' that match the regex and returns a joined data.frame of the name in the list,
#' the variable, variable type, and the first instance of the variable as an example.
#' This function is to help look at raw data in order to check which columns across files
#' contain the same information.
#' @param data_list A list of data.frames
#' @param reg_ex A regular expression to select column names
#' @param reg_ex_exclude A regular expression to exclude columns
#' Use NULL to return all column names
#' It is possible to edit this function to use the function select_colsfromlist())
#' @family summarize column functions
#' @export
summarize_variables <- function(data_list, reg_ex, reg_ex_exclude = NULL){
  cols_only <- imap(data_list %>% discard(is.null), function(x, y){

    if (is.null(reg_ex)){
      dat_cols1 <- x
    } else {
      dat_cols1 <- x %>% select(tidyselect::matches(reg_ex))
    }

    if (!is.null(reg_ex_exclude)){
      dat_cols1 <- dat_cols1 %>% select(!tidyselect::matches(reg_ex_exclude))
    }

    if (ncol(dat_cols1) == 0){
      ans2 <- NULL
    } else {
      col_type <- dat_cols1 %>%
        summarise(across(tidyselect::everything(), class)) %>%
        slice(1) %>%
        gather(., colname, type)

      # Pull an example row of data, pull a row that has the most filled columns
      col_ex <- dat_cols1 %>%
        mutate(count_na = rowSums(is.na(.))) %>%
        filter(count_na == min(.$count_na)) %>%
        select(-count_na) %>%
        slice(1) %>%
        mutate(across(tidyselect::everything(), as.character)) %>%
        gather(., colname, example)

      ans2 <- left_join(col_type, col_ex, by = "colname") %>%
        mutate(file_sheet = y) %>%
        group_by(file_sheet) %>%
        mutate(instance = dplyr::row_number()) %>%
        ungroup()
    }
  })

  temp <- bind_rows(cols_only)
  return(temp)
  #return(cols_only)
}

#' Select columns from data.frames stored within a list
#'
#' @param data_list A list with each element containing a data.frame
#' @param reg_ex A string containing a regular expression that will match the columns to be returned
#' @param multnames_only A logical expressing whether to only return data
#' with more than one column corresponding to a given regular expression
#' @family summarize column functions
#' @export
select_colsfromlist <- function(data_list, reg_ex, multnames_only = FALSE){

  data_list <- data_list %>% discard(is.null)
  cols_only <- map(1:length(data_list), function(x){
    #print(x)
    dat <- data_list[[x]]
    name <- names(data_list[x])

    dat_cols1 <- dat %>% select(tidyselect::matches(reg_ex))

    if (ncol(dat_cols1) > 0){
      names(dat_cols1) <- paste(names(dat_cols1), name, x)
    }

    dat_cols <- head(dat_cols1)

    return(dat_cols)
  })

  if (multnames_only){
    cols_only <- cols_only %>% keep(~ncol(.x)>1)
  }

  cols_only_bind <- bind_cols(cols_only)
  return(cols_only_bind)
  #return(cols_only)
}


#' Summarise the number of columns per data.frame that match a given regex
#' over a list of data.frames
#' This function detects columns with similar names within a dataset
#'
#' @param ls A list of data.frames
#' @param col_vec A vector containing string regex.
#' Each string will correspond to the columns to pull from each dataset
#' in order to summarize the number of columns associated with that regex string
#' @family summarize column functions
#' @export
summarize_ncol <- function(ls, col_vec){

  col_vars_n1 <- map(col_vec, function(x){
    #print(x)
    cols_summary <- summarize_variables(ls, x)

    if (nrow(cols_summary) == 0){
      warning(paste("There are no instances of variable:", x))
      cols_n <- NULL
    } else {
    cols_n <- cols_summary %>%
      group_by(file_sheet) %>%
      summarise(n = n()) %>%
      mutate(col_var = x)
    }

  })

  col_vars_n2 <- bind_rows(col_vars_n1) %>%
    pivot_wider(names_from = col_var, values_from = n)
}

#' Create a list of column names that match a given regex for all data.frames in given list
#' The function returns the proportion of the data.frames that contain the column name
#'
#'
#' @param ls A list of data.frames
#' @param col_vec A vector containing string regex.
#' Each string will correspond to the columns to pull from each dataset
#' in order to summarize the number of columns associated with that regex string
#' @family summarize column functions
#' @export
summarize_colnames <- function(ls, col_vec){

  col_var_name1 <- map(col_vec, function(x){
    #print(x)
    cols_summary <- summarize_variables(ls, x)

    if (nrow(cols_summary) == 0){
      warning(paste("There are no instances of variable:", x))
      col_names <- NULL
    } else {
      col_names <- cols_summary %>%
        group_by(colname) %>%
        summarise(prop_files = n()/length(ls)) %>%
        mutate(col_var = x)
    }

  })

  col_var_name2 <- bind_rows(col_var_name1)
}

#' A wrapper for summarize_variables to maintain backwards compatibility
#'
#' @rdname summarize_variables
#' @usage summarize.variables
#' @export summarize.variables
summarize.variables <- function(data_list, reg_ex, reg_ex_exclude = NULL){
  message("This function name is being retained for backwards compatibility.
          Please use summarize_variables()")

  summarize_variables(data_list = data_list,
                      reg_ex = reg_ex,
                      reg_ex_exclude = reg_ex_exclude)
}


#' A wrapper for select_colsfromlist to maintain backwards compatibility
#'
#' @rdname select_colsfromlist
#' @usage select.colsfromlist
#' @export select.colsfromlist
select.colsfromlist <- function(data_list, reg_ex, multnames_only = FALSE){
  message("This function name is being retained for backwards compatibility.
          Please use select_colsfromlist()")

  select_colsfromlist(data_list = data_list,
                      reg_ex = reg_ex,
                      multnames_only = multnames_only)
}


#' A wrapper for summarize_ncol to maintain backwards compatibility
#'
#' @rdname summarize_ncol
#' @usage summarize.ncol
#' @export summarize.ncol
summarize.ncol <- function(ls, col_vec){
  message("This function name is being retained for backwards compatibility.
          Please use summarize_ncol()")

  summarize_ncol(ls = ls, col_vec = col_vec)
}

#' A wrapper for summarize_colnames to maintain backwards compatibility
#'
#' @rdname summarize_colnames
#' @usage summarize.colnames
#' @export summarize.colnames
summarize.colnames <- function(ls, col_vec){
  message("This function name is being retained for backwards compatibility.
          Please use summarize_colnames()")

  summarize_colnames(ls = ls, col_vec = col_vec)
}
