# Read one excel sheet with read_excel

This function can be incorporated into read.sheets/ read.sheets may
become deprecated

## Usage

``` r
read_excelsheet(
  path,
  sheet,
  skip,
  na,
  col_names,
  guess_max,
  complete_cases = TRUE
)

read.excelsheet(
  path,
  sheet,
  skip,
  na,
  col_names,
  guess_max,
  complete_cases = TRUE
)
```

## Arguments

- path:

  Path to the xls/xlsx file.

- sheet:

  Sheet to read. Either a string (the name of a sheet), or an integer
  (the position of the sheet). Ignored if the sheet is specified via
  `range`. If neither argument specifies the sheet, defaults to the
  first sheet.

- skip:

  Minimum number of rows to skip before reading anything, be it column
  names or data. Leading empty rows are automatically skipped, so this
  is a lower bound. Ignored if `range` is given.

- na:

  Character vector of strings to interpret as missing values. By
  default, readxl treats blank cells as missing data.

- col_names:

  `TRUE` to use the first row as column names, `FALSE` to get default
  names, or a character vector giving a name for each column. If user
  provides `col_types` as a vector, `col_names` can have one entry per
  column, i.e. have the same length as `col_types`, or one entry per
  unskipped column.

- guess_max:

  Maximum number of data rows to use for guessing column types.

- complete_cases:

  A logical. The default TRUE will remove empty rows

## Details

Note: this function was previously read.excelsheet()

## See also

Other readin functions:
[`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md),
[`read_header()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md),
[`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md),
[`read_ods_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_ods_multsheets.md),
[`read_sheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_sheets.md)
