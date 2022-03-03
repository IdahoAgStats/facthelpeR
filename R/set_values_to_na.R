#' Set -9 and 0s to NA for given columns
#'
#' @param df A data.frame
#' @param cols_neg9_to_na A vector of the column names for which -9 values
#' will be set to NA
#' @param cols_zeros_to_na A vector of the column names for which 0 will be
#' set to NA
#' @param cols_neg_to_na A vector of the column names for which any negative values
#' will be set to NA
#' @family clean column contents functions
#' @export
set_values_to_na <- function(df,
                             cols_neg9_to_na = NULL,
                             cols_zeros_to_na = NULL,
                             cols_neg_to_na = NULL){

  df_na <- df %>%
    mutate(across(.cols = all_of(cols_neg9_to_na),
                  .fns = ~ ifelse(.x %in% c(-9, -.9), NA, .x))) %>%
    mutate(across(.cols = all_of(cols_zeros_to_na),
                  .fns = ~ ifelse(.x == 0, NA, .x))) %>%
    mutate(across(.cols = all_of(cols_neg_to_na),
                  .fns = ~ ifelse(.x < 0, NA, .x)))

}
