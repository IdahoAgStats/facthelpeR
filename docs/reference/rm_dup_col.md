# Remove duplicate columns that may have different column names

Remove duplicate columns. If the columns are duplicates, except one
column contains NAs, then the column with NAs is removed \#' test_dup
\<- duplicated(as.list(df)) This method detects duplicates but does not
take into account NAs

## Usage

``` r
rm_dup_col(df, concat_names = TRUE)

rm.dup_col(df, concat_names = TRUE)
```

## Arguments

- df:

  A data.frame

- concat_names:

  A logical denoting how to rename the columns The default is TRUE,
  which renames the column by concatenating the names of all the
  duplicate columns FALSE will retain the first

## See also

Other clean columns functions:
[`rename_col()`](https://idahoagstats.github.io/facthelpeR/reference/rename_col.md)
