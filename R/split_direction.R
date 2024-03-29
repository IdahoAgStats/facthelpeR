#' Split a large table in one direction if there are blank columns or rows
#'
#' These functions were first written by @yusuzech
#' See stackoverflow: https://stackoverflow.com/questions/58251748/how-to-efficiently-import-multiple-excel-tables-located-in-one-sheet-into-an-r-l/58252193#58252193
#' and GitHub: https://github.com/yusuzech/tidyverse_notes/blob/master/utility/read_excel_tables.md
#' @param df A data.frame to be split
#' @param direction A string denoting whether to split on a
#' row or column ("col" or "row")
#' @family split tables functions
#' @export
split_direction <- function(df, direction){

  if(direction == "col"){

    col_has_data <- unname(purrr::map_lgl(df,~!all(is.na(.x))))
    df_mapping <- make_df_index(col_has_data)
    out <- purrr::map(df_mapping, ~df[,.x, drop = FALSE])

  } else if(direction == "row"){

    row_has_data <- df %>%
      mutate(across(tidyselect::everything(), ~!is.na(.x))) %>%
      as.matrix() %>%
      apply(1,any)

    df_mapping <- make_df_index(row_has_data)
    out <- purrr::map(df_mapping,~df[.x, , drop = FALSE])
  }
  return(out)
}


#' Split a large table into smaller tables if there are blank columns or rows
#'
#' Split a large table into smaller tables if there are blank columns or rows.
#' This function can detect several tables even if they are not neatly placed
#' (e.g. not placed in a grid)
#'
#' If you still see entire rows or columns missing. Please increase complexity
#' @param showWarning A logical denoting whether to show warning message
#' @param complexity An integer. Starting at 1, increase complexity if tables
#' don't split correctly.
#' @inheritParams split_direction
#' @family split tables functions
#' @export
split_df <- function(df, showWarning = TRUE, complexity = 1){
  if(showWarning){
    warning("Please don't use first row as column names.")
  }

  out <- split_direction(df,"col")

  for(i in 1 :complexity){
    out <- out %>%
      purrr::map(~split_direction(.x,"row")) %>%
      purrr::flatten() %>%
      purrr::map(~split_direction(.x,"col")) %>%
      purrr::flatten()
  }
  return(out)

}

#' Utility function to get rle as a named vector
#'
#' @param v A vector
#' @keywords internal
vec_rle <- function(v){
  temp <- rle(v)
  out <- temp$values
  names(out) <- temp$lengths
  return(out)
}

#' Utility function to separate a table into list elements based on a blank row or column
#'
#' Utility function to separate a table into list elements based on a blank row or column
#' @inheritParams vec_rle
#' @keywords internal
make_df_index <- function(v){
  table_rle <- vec_rle(v)
  divide_points <- c(0,cumsum(names(table_rle)))
  table_index <- purrr::map2((divide_points + 1)[1:length(divide_points)-1],
                      divide_points[2:length(divide_points)],
                      ~.x:.y)
  return(table_index[table_rle])
}


