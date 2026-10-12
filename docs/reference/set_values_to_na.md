# Set -9 and 0s to NA for given columns

Set -9 and 0s to NA for given columns

## Usage

``` r
set_values_to_na(
  df,
  cols_neg9_to_na = NULL,
  cols_zeros_to_na = NULL,
  cols_neg_to_na = NULL
)
```

## Arguments

- df:

  A data.frame

- cols_neg9_to_na:

  A vector of the column names for which -9 values will be set to NA

- cols_zeros_to_na:

  A vector of the column names for which 0 will be set to NA

- cols_neg_to_na:

  A vector of the column names for which any negative values will be set
  to NA

## See also

Other clean column contents functions:
[`plot_map()`](https://idahoagstats.github.io/facthelpeR/reference/plot_map.md)
