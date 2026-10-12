# Changelog

## facthelpeR 0.4.0

- Fixed an issue in
  [`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md)
  where the guess_max argument was not used if the file type was csv.

- Added the
  [`plot_map()`](https://idahoagstats.github.io/facthelpeR/reference/plot_map.md)
  function to map the field layout of a single trial or all trials.

## facthelpeR 0.3.0

- Added the
  [`read_ods_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_ods_multsheets.md)
  function

- Added a `NEWS.md` file to track changes to the package.

- Edited
  [`find_col_info()`](https://idahoagstats.github.io/facthelpeR/reference/find_col_info.md)
  to return a nice message instead of an error when there are no more
  invalid columns left to check.
