# Find column information in a data.frame

For a data.frame, loop through the columns provided (cols_check) and
summarise the number of non-NA entries and the groups that contain
non-NA entries (for example if want to search trial, year, or filename),
and return the first non-NA entry as an example

## Usage

``` r
find_col_info(df, cols_check, by_col)
```

## Arguments

- df:

  A data frame

- cols_check:

  A character vector of column names to check

- by_col:

  A bare variable to summarise

## See also

Other summarize column functions:
[`select_colsfromlist()`](https://idahoagstats.github.io/facthelpeR/reference/select_colsfromlist.md),
[`summarize_colnames()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_colnames.md),
[`summarize_ncol()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md),
[`summarize_variables()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)
