# Read headers that span over multiple rows

Note: this function was previously read.excelheader()/read_excelheader()

## Usage

``` r
read_header(sheet_name, path, header_start, header_end, unique_names)

read.excelheader(sheet_name, path, header_start, header_end, unique_names)
```

## Arguments

- sheet_name:

  A string denoting the sheetname

- path:

  A path to the file that contains the sheet

- header_start:

  A numeric denoting the row number of the start of the header Note!: If
  there are any blank rows at the top of the sheet, start the row
  counting at the first filled row
  https://github.com/tidyverse/readxl/issues/194#issuecomment-266829259

- header_end:

  A numeric denoting the row number of the end of the header

- unique_names:

  A logical denoting whether make.unique() for the column names

## See also

Other readin functions:
[`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md),
[`read_excelsheet()`](https://idahoagstats.github.io/facthelpeR/reference/read_excelsheet.md),
[`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md),
[`read_ods_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_ods_multsheets.md),
[`read_sheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_sheets.md)
