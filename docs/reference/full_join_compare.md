# Full join data.frames to compare columns add a suffix before the merge to track where the column originated

Full join data.frames to compare columns add a suffix before the merge
to track where the column originated

## Usage

``` r
full_join_compare(df1, df2, by, suffix1, suffix2)
```

## Arguments

- df1:

  A data frame

- df2:

  A second data frame to join and compare

- by:

  Column(s) to join by

- suffix1:

  A string denoting the suffix to attach to the columns associated with
  the first data.frame. If NA, no suffix attached

- suffix2:

  A string denoting the suffix to attach to the columns associated with
  the second data.frame. If NA, no suffix attached
