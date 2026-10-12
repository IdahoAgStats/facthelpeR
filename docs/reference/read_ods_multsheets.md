# A wrapper around read_ods to read multiple sheets in a .ods file

Read in multiple sheets in a .ods file.

## Usage

``` r
read_ods_multsheets(path)
```

## Arguments

- path:

  Path to the .ods file to import.

## Value

A list of data frames–one data frame for each sheet.

## Details

Currently, read_ods will only read one sheet at a time.
read_ods_multsheets can handle multiple sheets in one .ods workbook, or,
when used with list.files and lapply, multiple workbooks with multiple
sheets.

The result is a named list of data frames, with the sheet name appended
to the basename of the filename with the file extension removed.

## See also

Other readin functions:
[`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md),
[`read_excelsheet()`](https://idahoagstats.github.io/facthelpeR/reference/read_excelsheet.md),
[`read_header()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md),
[`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md),
[`read_sheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_sheets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
write_ods(iris[iris$Species=="setosa",], "iris.ods", sheet = "setosa")

write_ods(iris[iris$Species=="versicolor",], "iris.ods",
          sheet = "versicolor", append = TRUE)

write_ods(iris[iris$Species=="virginica",],
          "iris.ods", sheet = "virginica", append = TRUE)

read_ods_multsheets("iris.ods")

# Read multiple files with multiple sheets
files <- list.files(data_folder, recursive = TRUE, pattern = "*.ods")

# Use lapply() with the result of list.files
lapply(files, read_ods_multsheets)
} # }
```
