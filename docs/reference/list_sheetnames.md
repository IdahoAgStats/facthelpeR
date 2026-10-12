# List the names of all sheets within given files

Note: this function was previously list.sheetnames()

## Usage

``` r
list_sheetnames(data_folder, files, reg_ex = NULL)

list.sheetnames(data_folder, files, reg_ex = NULL)
```

## Arguments

- data_folder:

  A string pointing to the parent path

- files:

  A vector of filenames that will be pasted to the parent path

- reg_ex:

  A regular expression to select sheets using

## See also

Other readin functions:
[`read_excelsheet()`](https://idahoagstats.github.io/facthelpeR/reference/read_excelsheet.md),
[`read_header()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md),
[`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md),
[`read_ods_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_ods_multsheets.md),
[`read_sheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_sheets.md)
