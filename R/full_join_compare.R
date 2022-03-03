#' Full join data.frames to compare columns
#' add a suffix before the merge to track where the
#' column originated
#' @param df1 A data frame
#' @param df2 A second data frame to join and compare
#' @param by Column(s) to join by
#' @param suffix1 A string denoting the suffix to attach to the columns
#' associated with the first data.frame.  If NA, no suffix attached
#' @param suffix2 A string denoting the suffix to attach to the columns
#' associated with the second data.frame.  If NA, no suffix attached
#' @family join functions
#' @export
full_join_compare <- function(df1, df2, by, suffix1, suffix2){

  if (is.null(names(by))){ names(by) = by }

  join_names <- names(by)

  if (!is.na(suffix1)){
    names(df1) <- paste(names(df1), suffix1, sep = "_")
    join_names <- paste(join_names, suffix1, sep = "_")
  }

  if (!is.na(suffix2)){
    names(df2) <- paste(names(df2), suffix2, sep = "_")
    by <- paste(by, suffix2, sep = "_")
  }


  names(by) <- join_names

  test <- full_join(df1, df2, by = by) %>%
    select(sort(names(.)))

}



