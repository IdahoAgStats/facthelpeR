# Read in sheets

This function may be deprecated (read_multsheets() has many improvements
over read_sheets) Note: this function was previously read.sheets()

## Usage

``` r
read_sheets(file_name, data_folder, skip_df)

read.sheets(file_name, data_folder, skip_df)
```

## Arguments

- file_name:

  A string of a filename

- data_folder:

  A string of the path of the folder that contains the file

- skip_df:

  A data.frame containing the following columns: filename, sheets, and
  skip, which contain string of filename, string of sheet name, and
  integer of number of rows to skip, respectively
  https://stackoverflow.com/questions/60898358/how-can-i-add-the-sheet-name-to-my-data-frame-as-a-variable

## See also

Other readin functions:
[`list_sheetnames()`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md),
[`read_excelsheet()`](https://idahoagstats.github.io/facthelpeR/reference/read_excelsheet.md),
[`read_header()`](https://idahoagstats.github.io/facthelpeR/reference/read_header.md),
[`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md),
[`read_ods_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_ods_multsheets.md)
