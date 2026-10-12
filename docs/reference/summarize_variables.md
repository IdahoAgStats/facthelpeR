# Summarize information on variables that match a given regex

Summarize information on variables that match a given regex. This
function loops through a list of data.frames and pulls out the columns
that match the regex and returns a joined data.frame of the name in the
list, the variable, variable type, and the first instance of the
variable as an example. This function is to help look at raw data in
order to check which columns across files contain the same information.

## Usage

``` r
summarize_variables(data_list, reg_ex, reg_ex_exclude = NULL)

summarize.variables
```

## Arguments

- data_list:

  A list of data.frames

- reg_ex:

  A regular expression to select column names

- reg_ex_exclude:

  A regular expression to exclude columns Use NULL to return all column
  names It is possible to edit this function to use the function
  [`select_colsfromlist()`](https://idahoagstats.github.io/facthelpeR/reference/select_colsfromlist.md)

## See also

Other summarize column functions:
[`find_col_info()`](https://idahoagstats.github.io/facthelpeR/reference/find_col_info.md),
[`select_colsfromlist()`](https://idahoagstats.github.io/facthelpeR/reference/select_colsfromlist.md),
[`summarize_colnames()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_colnames.md),
[`summarize_ncol()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md)
