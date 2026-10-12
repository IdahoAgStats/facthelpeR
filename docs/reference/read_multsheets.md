# Read multiple files (csv or excel) and/or multiple sheets (within an excel file) and handle headers that span multiple rows.

Read in multiple files and or multiple sheets and handle headers that
span multiple rows (using
[`read_header()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md))
This function must be provided a data.frame with file information. The
data.frame can be initalized with the function
[`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md)

## Usage

``` r
read_multsheets(
  data_folder,
  df,
  na = c("NA"),
  col_names,
  guess_max = 1000,
  complete_cases = TRUE
)

read.multsheets(
  data_folder,
  df,
  na = c("NA"),
  col_names,
  guess_max = 1000,
  complete_cases = TRUE
)
```

## Arguments

- data_folder:

  A string denoting the folder that contains the files to be read in

- df:

  A data.frame with the column "filename", which can be inialized by the
  function
  [`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md)
  Required data.frame columns:

  - filename: name of the .xlsx or .csv Optional columns:

  - sheets: name of the sheet (required for excel files with multiple
    sheets)

  - list_names: names to label the element in the list. If list_names is
    not provided, the function will label the list elements using the
    concatenation of the file and sheet name

  - header_start: the row number corresponding to the start of the
    header

  - header_end: the row number corresponding to the last row of the
    header header_end = 1 corresponds to the header being in only the
    first row and corresponds to skipping 0 rows. If header_end is not
    provided, the default is set to 1, which will skip 0 rows. Note: If
    header_end is NA, the file/sheet will be removed (not read in)

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

## See also

Other readin functions:
[`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md),
[`read_excelsheet()`](https://idahoagstats.github.io/facthelpeR/reference/read_excelsheet.md),
[`read_header()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md),
[`read_ods_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_ods_multsheets.md),
[`read_sheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_sheets.md)
