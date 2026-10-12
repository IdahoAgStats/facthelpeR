# Check field map layout for errors and duplicate range/row locations.

This function can map either a single trial or all trials. When mapping
all trials, best practice is to first filter out trials without
range/row. If you do this and still receive a warning that the output
will contain list cols, you have duplicates in range/row.

## Usage

``` r
plot_map(
  df,
  id_col = "trial",
  trial_id,
  plot_id = NULL,
  range = range,
  row = row
)
```

## Arguments

- df:

  A data frame.

- id_col:

  A string. Column containing a unique identifier for the trial. The
  default is 'trial'.

- trial_id:

  A string. Use "all" to map all data by trial; for single trial, trial
  id string to locate in id_col, i.e. "OregonCereals_SWS_ION_2023". The
  string to match should be exact; the function is not currently set up
  to locate partial strings.

- plot_id:

  A string or vector; column name(s) to use as plot identifier.
  Currently can take up to two inputs, i.e. c("plot", "ibloc"). This is
  useful for finding missing plot numbers, ibloc, entry, variety, etc.

- range:

  Column containing range values; the default is 'range'

- row:

  Column containing row locations; the default is 'row'

## See also

Other clean column contents functions:
[`set_values_to_na()`](https://idahoagstats.github.io/facthelpeR/reference/set_values_to_na.md)
