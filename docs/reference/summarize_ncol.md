# Summarise the number of columns per data.frame that match a given regex over a list of data.frames This function detects columns with similar names within a dataset

Summarise the number of columns per data.frame that match a given regex
over a list of data.frames This function detects columns with similar
names within a dataset

A wrapper for summarize_ncol to maintain backwards compatibility

## Usage

``` r
summarize_ncol(ls, col_vec)

summarize.ncol
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
[`summarize_colnames()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_colnames.md),
[`summarize_variables()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)
