# Create a list of column names that match a given regex for all data.frames in given list The function returns the proportion of the data.frames that contain the column name

Create a list of column names that match a given regex for all
data.frames in given list The function returns the proportion of the
data.frames that contain the column name

A wrapper for summarize_colnames to maintain backwards compatibility

## Usage

``` r
summarize_colnames(ls, col_vec)

summarize.colnames
```

## Arguments

- ls:

  A list of data.frames

- col_vec:

  A vector containing string regex. Each string will correspond to the
  columns to pull from each dataset in order to summarize the number of
  columns associated with that regex string

## See also

Other summarize column functions:
[`find_col_info()`](https://idahoagstats.github.io/facthelpeR/reference/find_col_info.md),
[`select_colsfromlist()`](https://idahoagstats.github.io/facthelpeR/reference/select_colsfromlist.md),
[`summarize_ncol()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md),
[`summarize_variables()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)
