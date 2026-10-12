# Rename columns with a data.frame containing old and new column names

This wrapper required using enexpr() because of the bare variables being
passed to rename_col. See tests/understand_wrappers.R for more details

## Usage

``` r
rename_col(
  ls,
  rename_df,
  rename_col,
  old_col,
  element_col = NULL,
  rm_col = FALSE
)

rename.col
```

## Arguments

- ls:

  A list of data.frames whose columns will be renamed

- rename_df:

  A data.frame that has a column containing old column names and new
  column names

- rename_col:

  A variable name that corresponds to the column containing new column
  names

- old_col:

  A variable name that corresponds to the column to be renamed

- element_col:

  A variable name that corresponds to the name of the list element to be
  renamed. The default value is NULL, meaning that the rename will be
  applied to all elements in ls

- rm_col:

  A logical specifying whether to remove columns that have been renamed
  to "rm_col" in rename_df

## See also

Other clean columns functions:
[`rm_dup_col()`](https://idahoagstats.github.io/facthelpeR/reference/rm_dup_col.md)
