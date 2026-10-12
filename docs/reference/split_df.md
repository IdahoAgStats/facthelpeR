# Split a large table into smaller tables if there are blank columns or rows

Split a large table into smaller tables if there are blank columns or
rows. This function can detect several tables even if they are not
neatly placed (e.g. not placed in a grid)

## Usage

``` r
split_df(df, showWarning = TRUE, complexity = 1)
```

## Arguments

- df:

  A data.frame to be split

- showWarning:

  A logical denoting whether to show warning message

- complexity:

  An integer. Starting at 1, increase complexity if tables don't split
  correctly.

## Details

If you still see entire rows or columns missing. Please increase
complexity

## See also

Other split tables functions:
[`split_direction()`](https://idahoagstats.github.io/facthelpeR/reference/split_direction.md)
