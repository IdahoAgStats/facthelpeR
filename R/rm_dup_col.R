#' Remove duplicate columns that may have different column names
#'
#' Remove duplicate columns.
#' If the columns are duplicates, except one column contains NAs, then
#' the column with NAs is removed
#' #' test_dup <- duplicated(as.list(df)) This method detects duplicates
#' but does not take into account NAs
#' @param df A data.frame
#' @param concat_names A logical denoting how to rename the columns
#' The default is TRUE, which renames the column by concatenating the names of
#' all the duplicate columns
#' FALSE will retain the first
#' @family clean columns functions
#' @export
rm_dup_col <- function(df, concat_names = TRUE){
  df <- as.data.frame(df) # cannot be a tibble otherwise identical() will fail if column names differ

  cols <- list(first = 1:ncol(df), second = 1:ncol(df))
  cols_compare <- cols %>% tidyr::expand_grid(first = .$first, second = .$second) %>%
    arrange(first) %>% filter(second > first)

  col_matches <- map2(cols_compare$first, cols_compare$second, function(x, y){
    df_2col <- data.frame(first = df[,x], second = df[,y])
    df_2col_rmna <- df_2col %>% drop_na()
    col_identical <- identical(df_2col$first, df_2col$second)

    # if after removing all NA, there are no rows, then they cannot match
    if (nrow(df_2col_rmna)>0){
      colna_identical <- identical(df_2col_rmna$first, df_2col_rmna$second)
    } else {
      colna_identical <- FALSE
    }

    # Test if column has NA
    # Need to remove rows with NA in both because R doesn't detect these as matches
    df_2col_rmmatchingna <- df_2col %>% filter(!(is.na(first) & is.na(second)))
    cols_wna_rmmatchingna <- df_2col_rmmatchingna %>%
      summarise(across(everything(), ~ any(is.na(.))))

    return(data.frame(x,
                      y,
                      col_identical,
                      colna_identical,
                      x_isna = cols_wna_rmmatchingna$first,
                      y_isna = cols_wna_rmmatchingna$second))
  })
  cols_rmna <- bind_rows(col_matches)
  c2 <- cols_rmna %>% filter(x!= y) %>% filter(colna_identical == TRUE)

  c3 <-
    c2 %>%
    mutate(coln_rm = ifelse(
      col_identical == TRUE &
        (x_isna == TRUE & y_isna == TRUE), y,
      ifelse(col_identical == FALSE & (x_isna == TRUE & y_isna == TRUE), NA,
             ifelse(col_identical == FALSE & (x_isna == TRUE & y_isna == FALSE), x, y)))) %>%
    mutate(coln_keep = ifelse(coln_rm == x, y, x))


  if (any(is.na(c3$coln_rm))){
    almost_dup <- unlist(c3 %>% filter(is.na(coln_rm)) %>% select(x,y))
    print(paste(paste(almost_dup, collapse = ", "), "- Both columns contain NAs, so both columns are retained"))
    c3 <- c3 %>% drop_na()
  }

  # Remove duplicate columns
  c4 <- c3 %>%
    mutate(name_keep = names(df)[coln_keep]) %>%
    mutate(name_rm = names(df)[coln_rm]) %>%
    select(name_keep, name_rm) %>%
    group_by(name_keep) %>%
    mutate(rename_keep_mult = paste(unique(name_rm), collapse="..")) %>%
    mutate(rename_keep = paste(name_keep, rename_keep_mult, sep = ".."))

  names2rm <- unique(c4$name_rm)
  z_clean <- df %>% select(-all_of(names2rm))

  if (length(names2rm) > 0){ # rename.col() only works if there are columns remove in c4
    print(paste("Removing:", paste(names2rm, collapse = ", ")))

    if (concat_names){ # Rename columns based on original and the duplicate(s)
      rename_df <- c4 %>%
        filter(! name_keep %in% c4$name_rm) %>%
        select(name_keep, rename_keep) %>%
        unique(.)

      # This is a bit hacky because rename.col() was developed for lists
      z_clean <- rename_col(list(z_clean),
                            rename_df = rename_df,
                            rename_col = rename_keep,
                            old_col = name_keep,
                            rm_col = FALSE)[[1]]
    }


  } else {
    print("No columns removed")
  }

  return(z_clean)
}

#' Test whether a column has identical elements
#' @keywords internal
#' @description
#' Test whether a column has identical elements.
#' The function will return TRUE if:
#' all values are equal,
#' all values are equal and there are NAs,
#' all values are NA.
#' @details
#' https://stackoverflow.com/questions/31968623/how-to-check-whether-a-column-contains-only-identical-elements-in-r#:~:text=You%20can%20use%20the%20duplicated,column%20contains%20all%20identical%20values.&text=If%20x%20contains%20NAs%20this,mixed%20columns%20will%20return%20FALSE%20.
#' @param df A data frame
#' @param colname A string. Name of column to test

test.col_id_elements <- function(df, colname){

  dim(table(df[[colname]], useNA = "no")) <= 1

}


#' A wrapper for rm_dup_col to maintain backwards compatibility
#'
#' @rdname rm_dup_col
#' @export
rm.dup_col <- function(df, concat_names = TRUE){
  message("This function name is being retained for backwards compatibility.
          Please use rm_dup_col()")
  rm_dup_col(df = df, concat_names = concat_names)

}


