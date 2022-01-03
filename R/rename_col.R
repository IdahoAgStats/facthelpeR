
#' Rename columns with a data.frame containing old and new column names
#'
#' @param ls A list of data.frames whose columns will be renamed
#' @param rename_df A data.frame that has a column containing
#' old column names and new column names
#' @param rename_col A variable name that corresponds to the column
#' containing new column names
#' @param old_col A variable name that corresponds to the column to be renamed
#' @param element_col A variable name that corresponds to the name of the list
#' element to be renamed.
#' The default value is NULL, meaning that the rename will be applied to
#' all elements in ls
#' @param rm_col A logical specifying whether to remove columns that have been
#' renamed to "rm_col" in rename_df
#' @family clean columns functions
#' @export
rename_col <- function(ls,
                       rename_df,
                       rename_col,
                       old_col,
                       element_col = NULL,
                       rm_col = FALSE){

  element_col_enquo <- enquo(element_col)

  rename_df <- rename_df %>% ungroup() %>%
    mutate({{rename_col}} :=
             ifelse(is.na({{rename_col}}),
                    {{old_col}},
                    {{rename_col}}))

  ls_rename <- imap(ls, function(x, y){
    print(y)

    if (!rlang::quo_is_null(element_col_enquo)){
      rename_df <- rename_df %>% filter({{element_col}} == y)
    } else {
      message("Renaming all data.frames with the same rename values")
    }

    rename_df_temp <- rename_df %>%
      select({{old_col}}, {{rename_col}}) %>%
      filter({{old_col}} %in% names(x)) %>%
      mutate({{rename_col}} := make.unique({{rename_col}}))

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

#' A wrapper for rename_col to maintain backwards compatibility
#'
#' @rdname rename_col
#' @export
rename.col <- function(ls,
                       rename_df,
                       rename_col,
                       old_col,
                       element_col = NULL,
                       rm_col = FALSE){
  message("This function name is being retained for backwards compatibility.
          Please use rename_col()")

  rename_col <- enexpr(rename_col)
  old_col <- enexpr(old_col)
  element_col <- enexpr(element_col)

  eval(expr(rename_col(ls = ls,
             rename_df = rename_df,
             rename_col = !!rename_col,
             old_col = !!old_col,
             element_col = !!element_col,
             rm_col = rm_col)))


}

