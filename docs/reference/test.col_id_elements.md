# Test whether a column has identical elements

Test whether a column has identical elements. The function will return
TRUE if: all values are equal, all values are equal and there are NAs,
all values are NA.

## Usage

``` r
test.col_id_elements(df, colname)
```

## Arguments

- df:

  A data frame

- colname:

  A string. Name of column to test

## Details

https://stackoverflow.com/questions/31968623/how-to-check-whether-a-column-contains-only-identical-elements-in-r#:~:text=You%20can%20use%20the%20duplicated,column%20contains%20all%20identical%20values.&text=If%20x%20contains%20NAs%20this,mixed%20columns%20will%20return%20FALSE%20.
