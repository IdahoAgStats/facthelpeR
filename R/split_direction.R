#' https://stackoverflow.com/questions/58251748/how-to-efficiently-import-multiple-excel-tables-located-in-one-sheet-into-an-r-l/58252193#58252193

#' utility function to get rle as a named vector
#' 
#' @param v A vector
vec_rle <- function(v){
  temp <- rle(v)
  out <- temp$values
  names(out) <- temp$lengths
  return(out)
}

#' utility function to map table with their columns/rows in a bigger table
#' 
#' Utility function to separate a table into list elements based on a blank row or column
#' @inheritParams vec_rle
make_df_index <- function(v){
  table_rle <- vec_rle(v)
  divide_points <- c(0,cumsum(names(table_rle)))
  table_index <- map2((divide_points + 1)[1:length(divide_points)-1],
                      divide_points[2:length(divide_points)],
                      ~.x:.y)
  return(table_index[table_rle])
}

#' Split a large table in one direction if there are blank columns or rows
#' 
#' @param df A data.frame to be split
#' @param direction A string denoting whether to split on a row or column ("col" or "row")
split_direction <- function(df,direction = "col"){
  if(direction == "col"){
    col_has_data <- unname(map_lgl(df,~!all(is.na(.x))))
    df_mapping <- make_df_index(col_has_data)
    out <- map(df_mapping,~df[,.x])
  } else if(direction == "row"){
    row_has_data <- df %>% 
      mutate_all(~!is.na(.x)) %>%
      as.matrix() %>% 
      apply(1,any)
    df_mapping <- make_df_index(row_has_data)
    out <- map(df_mapping,~df[.x,])
  }
  return(out)
}

#' split a large table into smaller tables if there are blank columns or rows
#' if you still see entire rows or columns missing. Please increase complexity
split_df <- function(df,showWarnig = TRUE,complexity = 1){
  if(showWarnig){
    warning("Please don't use first row as column names.")
  }
  
  out <- split_direction(df,"col")
  
  for(i in 1 :complexity){
    out <- out %>%
      map(~split_direction(.x,"row")) %>%
      flatten() %>%
      map(~split_direction(.x,"col")) %>%
      flatten()
  }
  return(out)
  
}
