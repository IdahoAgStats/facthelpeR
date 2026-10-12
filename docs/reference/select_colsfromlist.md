# Select columns from data.frames stored within a list

Select columns from data.frames stored within a list

A wrapper for select_colsfromlist to maintain backwards compatibility

## Usage

``` r
select_colsfromlist(data_list, reg_ex, multnames_only = FALSE)

select.colsfromlist
```

## Arguments

- data_list:

  A list with each element containing a data.frame

- reg_ex:

  A string containing a regular expression that will match the columns
  to be returned

- multnames_only:

  A logical expressing whether to only return data with more than one
  column corresponding to a given regular expression

## See also

Other summarize column functions:
[`find_col_info()`](https://idahoagstats.github.io/facthelpeR/reference/find_col_info.md),
[`summarize_colnames()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_colnames.md),
[`summarize_ncol()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md),
[`summarize_variables()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)
