# Split a large table in one direction if there are blank columns or rows

These functions were first written by @yusuzech See stackoverflow:
https://stackoverflow.com/questions/58251748/how-to-efficiently-import-multiple-excel-tables-located-in-one-sheet-into-an-r-l/58252193#58252193
and GitHub:
https://github.com/yusuzech/tidyverse_notes/blob/master/utility/read_excel_tables.md

## Usage

``` r
split_direction(df, direction)
```

## Arguments

- df:

  A data.frame to be split

- direction:

  A string denoting whether to split on a row or column ("col" or "row")

## See also

Other split tables functions:
[`split_df()`](https://idahoagstats.github.io/facthelpeR/reference/split_df.md)
