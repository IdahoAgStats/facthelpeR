# Package index

## Read-in functions

- [`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md)
  [`list.sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md)
  : List the names of all sheets within given files
- [`read_excelsheet()`](https://idahoagstats.github.io/facthelpeR/reference/read_excelsheet.md)
  [`read.excelsheet()`](https://idahoagstats.github.io/facthelpeR/reference/read_excelsheet.md)
  : Read one excel sheet with read_excel
- [`read_header()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md)
  [`read.excelheader()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md)
  : Read headers that span over multiple rows
- [`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md)
  [`read.multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md)
  : Read multiple files (csv or excel) and/or multiple sheets (within an
  excel file) and handle headers that span multiple rows.
- [`read_ods_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_ods_multsheets.md)
  : A wrapper around read_ods to read multiple sheets in a .ods file
- [`read_sheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_sheets.md)
  [`read.sheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_sheets.md)
  : Read in sheets

## Split tables functions

- [`split_df()`](https://idahoagstats.github.io/facthelpeR/reference/split_df.md)
  : Split a large table into smaller tables if there are blank columns
  or rows
- [`split_direction()`](https://idahoagstats.github.io/facthelpeR/reference/split_direction.md)
  : Split a large table in one direction if there are blank columns or
  rows

## Clean columns functions

- [`rename_col()`](https://idahoagstats.github.io/facthelpeR/reference/rename_col.md)
  [`rename.col`](https://idahoagstats.github.io/facthelpeR/reference/rename_col.md)
  : Rename columns with a data.frame containing old and new column names
- [`rm_dup_col()`](https://idahoagstats.github.io/facthelpeR/reference/rm_dup_col.md)
  [`rm.dup_col()`](https://idahoagstats.github.io/facthelpeR/reference/rm_dup_col.md)
  : Remove duplicate columns that may have different column names

## Join functions

- [`full_join_compare()`](https://idahoagstats.github.io/facthelpeR/reference/full_join_compare.md)
  : Full join data.frames to compare columns add a suffix before the
  merge to track where the column originated

## Clean column contents

- [`plot_map()`](https://idahoagstats.github.io/facthelpeR/reference/plot_map.md)
  : Check field map layout for errors and duplicate range/row locations.
- [`set_values_to_na()`](https://idahoagstats.github.io/facthelpeR/reference/set_values_to_na.md)
  : Set -9 and 0s to NA for given columns

## Summarize column functions

- [`find_col_info()`](https://idahoagstats.github.io/facthelpeR/reference/find_col_info.md)
  : Find column information in a data.frame
- [`select_colsfromlist()`](https://idahoagstats.github.io/facthelpeR/reference/select_colsfromlist.md)
  [`select.colsfromlist`](https://idahoagstats.github.io/facthelpeR/reference/select_colsfromlist.md)
  : Select columns from data.frames stored within a list
- [`summarize_colnames()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_colnames.md)
  [`summarize.colnames`](https://idahoagstats.github.io/facthelpeR/reference/summarize_colnames.md)
  : Create a list of column names that match a given regex for all
  data.frames in given list The function returns the proportion of the
  data.frames that contain the column name
- [`summarize_ncol()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md)
  [`summarize.ncol`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md)
  : Summarise the number of columns per data.frame that match a given
  regex over a list of data.frames This function detects columns with
  similar names within a dataset
- [`summarize_variables()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)
  [`summarize.variables`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)
  : Summarize information on variables that match a given regex
