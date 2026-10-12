# Introduction to facthelpeR

## Introduction

**facthelpeR** is a set of functions that are used to read in and do a
first pass at cleaning up data.

Load **facthelpeR**

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`facthelpeR`](https://idahoagstats.github.io/facthelpeR)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`here`](https://here.r-lib.org/)`)`

## List the file structure of the data

auxiliary_files is a folder that contains helper files that are used in
the curation process. Generally, the \*\_aux.csvs are manually edited to
match controlled vocabularies. They live in a separate folder than the
output so that they don’t get written over accidentally.

\
`main_folder`` ``<-`` ``"inst/extdata/OSU_2019_example"`\
`data_folder`` ``<-`` ``here``::`[`here`](https://here.r-lib.org/reference/here.html)`(``main_folder``, ``"data-raw"``)`\
`auxiliary_files`` ``<-`` ``here``::`[`here`](https://here.r-lib.org/reference/here.html)`(``main_folder``, ``"auxiliary_files"``)`` `\
`knitroutput_folder`` ``<-`` ``here``::`[`here`](https://here.r-lib.org/reference/here.html)`(``main_folder``, ``"output"``)`

## Manual edits

If there are any mistakes in numeric values or one-off typos, sometimes
it is faster to fix it in the raw files. Note any manual changes here in
the .Rmd for reproducibility.

## Read in raw data

### Generate a list of all of the files and sheets

If the header is not in the first row (or spans multiple rows),
`write.csv(sheet_info, ...)` and manually fill in the number of rows to
skip in each sheet. This data.frame will be read into
[`read_multsheets()`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md)

\
`files_csv`` ``<-`` `[`list.files`](https://rdrr.io/r/base/list.files.html)`(``data_folder``, recursive ``=`` ``TRUE``, pattern ``=`` ``".csv"``)`\
\
`sheet_info_csv`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``filename ``=`` ``files_csv``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``!`[`str_detect`](https://stringr.tidyverse.org/reference/str_detect.html)`(``filename``, ``"curation_files"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``list_names ``=`` `[`basename`](https://rdrr.io/r/base/basename.html)`(``filename``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``list_names ``=`` `[`str_remove`](https://stringr.tidyverse.org/reference/str_remove.html)`(``list_names``, ``".csv"``)``)`` `\
\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``sheet_info_csv``)`

| filename         | list_names   |
|:-----------------|:-------------|
| HWEYT_Condon.csv | HWEYT_Condon |
| HWEYT_Dufur.csv  | HWEYT_Dufur  |

#### How to handle sheets in .xls files

This section is just to demonstrate how to handle excel files with
multiple sheets. The data won’t be processed in the rest of this
vignette.

\
`files_xls`` ``<-`` `[`list.files`](https://rdrr.io/r/base/list.files.html)`(``data_folder``, recursive ``=`` ``TRUE``, pattern ``=`` ``".xls"``)`\
\
`sheet_info_xls`` ``<-`` `[`list_sheetnames`](https://idahoagstats.github.io/facthelpeR/reference/list_sheetnames.md)`(``data_folder``, ``files_xls``)`` `\
\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``sheet_info_xls``)`

| filename              | sheets      |
|:----------------------|:------------|
| HWEYT_Dufur_copy.xlsx | HWEYT_Dufur |
| HWEYT_Dufur_copy.xlsx | extra_sheet |

### Use read_multsheets()

\
`data_raw1`` ``<-`` `[`read_multsheets`](https://idahoagstats.github.io/facthelpeR/reference/read_multsheets.md)`(``data_folder``, ``sheet_info_csv``, na ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"NA"``)``, col_names ``=`` ``TRUE``)`\
`#> Adding header_end`\
`#> [1] "Using read_csv"`\
`#> ``Rows: ``100`` ``Columns: ``11`\
`#> ``──`` ``Column specification`` ``────────────────────────────────────────────────────────`\
`#> ``Delimiter:`` ","`\
`#> ``chr``  (1): Program ID`\
`#> ``dbl`` (10): Plot, Rep, Length_Feet, Length_Inches, Height, Test, Protein, Weig...`\
`#> `\
`#> ``ℹ```  Use `spec()` to retrieve the full column specification for this data. ``\
`#> ``ℹ```  Specify the column types or set `show_col_types = FALSE` to quiet this message. ``\
`#> [1] "Using read_csv"`\
`` #> Warning: One or more parsing issues, call `problems()` on your data frame for details, ``\
`#> e.g.:`\
`#>   dat <- vroom(...)`\
`#>   problems(dat)`\
`#> ``Rows: ``100`` ``Columns: ``11`\
`#> ``──`` ``Column specification`` ``────────────────────────────────────────────────────────`\
`#> ``Delimiter:`` ","`\
`#> ``chr``  (1): Program ID`\
`#> ``dbl`` (10): Plot, Length_Feet, Length_Inches, Height, Test, Protein, Weight, M...`\
`#> `\
`#> ``ℹ```  Use `spec()` to retrieve the full column specification for this data. ``\
`#> ``ℹ```  Specify the column types or set `show_col_types = FALSE` to quiet this message. ``\
`data_raw2`` ``<-`` `[`imap`](https://purrr.tidyverse.org/reference/imap.html)`(``data_raw1``, ``function``(``x``, ``y``)``{`` ``x`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``sourcefile ``=`` ``y``)``}``)`\
\
\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`[`head`](https://rdrr.io/r/utils/head.html)`(``data_raw2``[[``1``]``]``)``)`

| Plot | Rep | Program ID | Length_Feet | Length_Inches | Height | Test | Protein | Weight | Moisture | rep_dup | sourcefile |
|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| 1 | 1 | HRW20077 | 15 | 9 | 38 | 61.1 | 9.2 | 3.067 | 15.4 | 1 | HWEYT_Condon |
| 2 | 1 | HRW20023 | 15 | 10 | 36 | 63.2 | 12.5 | 2.240 | 14.0 | 1 | HWEYT_Condon |
| 3 | 1 | HRW20020 | 15 | 10 | 35 | 62.2 | 9.5 | 2.762 | 15.1 | 1 | HWEYT_Condon |
| 4 | 1 | HRW20021 | 15 | 5 | 34 | 61.9 | 8.9 | 3.222 | 16.0 | 1 | HWEYT_Condon |
| 5 | 1 | HRW20019 | 15 | 5 | 33 | 62.9 | 11.2 | 2.489 | 15.1 | 1 | HWEYT_Condon |
| 6 | 1 | HRW20001 | 15 | 4 | 37 | 61.9 | 10.4 | 2.138 | 14.0 | 1 | HWEYT_Condon |

\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`[`head`](https://rdrr.io/r/utils/head.html)`(``data_raw2``[[``2``]``]``)``)`

| Plot | Length_Feet | Length_Inches | Height | Test | Protein | Weight | Moisture | Entry | Rep | Program ID | sourcefile |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|
| 1 | 15 | 0 | 36 | 62.7 | 10.4 | 3.081 | 14.3 | 1 | 1 | HRW20077 | HWEYT_Dufur |
| 2 | 14 | 1 | 36 | 64.3 | 11.7 | 2.325 | 12.3 | 2 | 1 | HRW20023 | HWEYT_Dufur |
| 3 | 14 | 11 | 30 | 63.9 | 11.5 | 2.633 | 14.8 | 3 | 1 | HRW20020 | HWEYT_Dufur |
| 4 | 13 | 10 | 32 | 63.6 | 10.5 | 3.387 | 15.7 | 4 | 1 | HRW20021 | HWEYT_Dufur |
| 5 | 14 | 11 | 31 | 64.2 | 12.1 | 2.525 | 13.8 | 5 | 1 | HRW20019 | HWEYT_Dufur |
| 6 | 15 | 0 | 37 | 63.6 | 12.3 | 2.678 | 14.3 | 6 | 1 | HRW20001 | HWEYT_Dufur |

\
`#save(data_raw2,`\
` ``#    file = here::here(knitroutput_folder, "data_raw2.RData"))`

### Remove duplicate columns

Use
[`facthelpeR::rm_dup_col()`](https://idahoagstats.github.io/facthelpeR/reference/rm_dup_col.md)
because it can detect and remove duplicates when: - column names are not
exact, but column contents are - if the only difference between two
column contents are that one has some NAs, then that column is removed
and the other is retained

\
`data_raw3`` ``<-`` `[`imap`](https://purrr.tidyverse.org/reference/imap.html)`(``data_raw2``, ``function``(``x``,``y``)``{`\
`    `[`print`](https://rdrr.io/r/base/print.html)`(``y``)`\
`    `[`rm_dup_col`](https://idahoagstats.github.io/facthelpeR/reference/rm_dup_col.md)`(``x``, ``FALSE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(`[`where`](https://tidyselect.r-lib.org/reference/where.html)`(``~``!`[`all`](https://rdrr.io/r/base/all.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``.``)``)``)``)`\
`}``)`\
`#> [1] "HWEYT_Condon"`\
`#> [1] "Removing: rep_dup"`\
`#> [1] "HWEYT_Dufur"`\
`#> [1] "No columns removed"`

## Curate column names:

### Get column names

\
`colnames`` ``<-`` `[`summarize_variables`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)`(``data_raw3``, reg_ex ``=`` ``NULL``)`\
`colnames_summary`` ``<-`` ``colnames`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``colname``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`summarize`](https://dplyr.tidyverse.org/reference/summarise.html)`(`[`across`](https://dplyr.tidyverse.org/reference/across.html)`(`[`everything`](https://tidyselect.r-lib.org/reference/everything.html)`(``)``, ``~`` `[`paste`](https://rdrr.io/r/base/paste.html)`(`[`na.omit`](https://rdrr.io/r/stats/na.fail.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``.x``)``)``, collapse ``=`` ``"; "``)``)``)`\
\
[`write.csv`](https://rdrr.io/r/utils/write.table.html)`(``colnames_summary``, ``here``::`[`here`](https://here.r-lib.org/reference/here.html)`(``knitroutput_folder``, ``"colnames_summary.csv"``)``, row.names ``=`` ``FALSE``)`\
\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``colnames_summary``)`

| colname | type | example | file_sheet | instance |
|:---|:---|:---|:---|:---|
| Entry | numeric | 1 | HWEYT_Dufur | 9 |
| Height | numeric | 38; 36 | HWEYT_Condon; HWEYT_Dufur | 6; 4 |
| Length_Feet | numeric | 15 | HWEYT_Condon; HWEYT_Dufur | 4; 2 |
| Length_Inches | numeric | 9; 0 | HWEYT_Condon; HWEYT_Dufur | 5; 3 |
| Moisture | numeric | 15.4; 14.3 | HWEYT_Condon; HWEYT_Dufur | 10; 8 |
| Plot | numeric | 1 | HWEYT_Condon; HWEYT_Dufur | 1 |
| Program ID | character | HRW20077 | HWEYT_Condon; HWEYT_Dufur | 3; 11 |
| Protein | numeric | 9.2; 10.4 | HWEYT_Condon; HWEYT_Dufur | 8; 6 |
| Rep | numeric | 1 | HWEYT_Condon; HWEYT_Dufur | 2; 10 |
| Test | numeric | 61.1; 62.7 | HWEYT_Condon; HWEYT_Dufur | 7; 5 |
| Weight | numeric | 3.067; 3.081 | HWEYT_Condon; HWEYT_Dufur | 9; 7 |
| sourcefile | character | HWEYT_Condon; HWEYT_Dufur | HWEYT_Condon; HWEYT_Dufur | 11; 12 |

### Rename columns using auxiliary files

First, understand what each column means (description and units) by:

1.  checking through annual reports

2.  checking with Julia

3.  lastly, asking the PI

Then, fill out the auxiliary file with the appropriate controlled
vocabulary.

If there isn’t a corresponding controlled vocabulary, then need to:

1.  decide if the column can be captured by current controlled
    vocabulary. If so, standardize to the controlled vocabulary

2.  decide whether we need to keep the variable at all (can fill in
    auxiliary file with “rm_col” in the controlled_vocab column and
    rename.col() will remove them if rm_col argument = TRUE)

3.  decide whether a new controlled vocabulary should be added. If so,
    add to controlled vocabulary using the correct formatting, which is
    described in the README (I usually get Julia’s opinion on this,
    especially before adding new vocabularies)

\
`colnames_curation`` ``<-`` `\
`  `[`read.csv`](https://rdrr.io/r/utils/read.table.html)`(``here``::`[`here`](https://here.r-lib.org/reference/here.html)`(``auxiliary_files``, ``"colnames_summary_aux.csv"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``# If a column name still needs to be checked/confirmed, leave the controlled_vocab `\
`  ``# column blank in the "colnames_summary_aux.csv" file.  `\
`  ``# Before the renaming step, just set the unknowns to the `\
`  ``# original name.`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``controlled_vocab ``=`` `\
`           `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``controlled_vocab`` ``==`` ``""``, ``colname``, ``controlled_vocab``)``)`` `\
\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``colnames_curation``)`

| controlled_vocab | colname | type | example | file_sheet | instance |
|:---|:---|:---|:---|:---|:---|
| entry | Entry | numeric | 1 | HWEYT_Dufur | 9 |
| height | Height | numeric | 38; 36 | HWEYT_Condon; HWEYT_Dufur | 6; 4 |
| Length_Feet | Length_Feet | numeric | 15 | HWEYT_Condon; HWEYT_Dufur | 4; 2 |
| Length_Inches | Length_Inches | numeric | 9; 0 | HWEYT_Condon; HWEYT_Dufur | 5; 3 |
| moisture | Moisture | numeric | 15.4; 14.3 | HWEYT_Condon; HWEYT_Dufur | 10; 8 |
| plot | Plot | numeric | 1 | HWEYT_Condon; HWEYT_Dufur | 1 |
| Program ID | Program ID | character | HRW20077 | HWEYT_Condon; HWEYT_Dufur | 3; 11 |
| Protein | Protein | numeric | 9.2; 10.4 | HWEYT_Condon; HWEYT_Dufur | 8; 6 |
| rep | Rep | numeric | 1 | HWEYT_Condon; HWEYT_Dufur | 2; 10 |
| sourcefile | sourcefile | character | HWEYT_Condon; HWEYT_Dufur | HWEYT_Condon; HWEYT_Dufur | 11; 12 |
| test | Test | numeric | 61.1; 62.7 | HWEYT_Condon; HWEYT_Dufur | 7; 5 |
| Weight | Weight | numeric | 3.067; 3.081 | HWEYT_Condon; HWEYT_Dufur | 9; 7 |

\
\
`data_raw_rename1`` ``<-`` `[`rename_col`](https://idahoagstats.github.io/facthelpeR/reference/rename_col.md)`(``data_raw3``, `\
`                              rename_df ``=`` ``colnames_curation``, `\
`                              rename_col ``=`` ``controlled_vocab``, `\
`                              old_col ``=`` ``colname``, `\
`                              rm_col ``=`` ``TRUE``)`\
`#> [1] "HWEYT_Condon"`\
`#> Renaming all data.frames with the same rename values`\
`#> Columns renamed 'rm_col' will be removed`\
`#> [1] "HWEYT_Dufur"`\
`#> Renaming all data.frames with the same rename values`\
`#> Columns renamed 'rm_col' will be removed`\
\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`[`head`](https://rdrr.io/r/utils/head.html)`(``data_raw_rename1``[[``1``]``]``)``)`

| plot | rep | Program ID | Length_Feet | Length_Inches | height | test | Protein | Weight | moisture | sourcefile |
|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|:---|
| 1 | 1 | HRW20077 | 15 | 9 | 38 | 61.1 | 9.2 | 3.067 | 15.4 | HWEYT_Condon |
| 2 | 1 | HRW20023 | 15 | 10 | 36 | 63.2 | 12.5 | 2.240 | 14.0 | HWEYT_Condon |
| 3 | 1 | HRW20020 | 15 | 10 | 35 | 62.2 | 9.5 | 2.762 | 15.1 | HWEYT_Condon |
| 4 | 1 | HRW20021 | 15 | 5 | 34 | 61.9 | 8.9 | 3.222 | 16.0 | HWEYT_Condon |
| 5 | 1 | HRW20019 | 15 | 5 | 33 | 62.9 | 11.2 | 2.489 | 15.1 | HWEYT_Condon |
| 6 | 1 | HRW20001 | 15 | 4 | 37 | 61.9 | 10.4 | 2.138 | 14.0 | HWEYT_Condon |

\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`[`head`](https://rdrr.io/r/utils/head.html)`(``data_raw_rename1``[[``2``]``]``)``)`

| plot | Length_Feet | Length_Inches | height | test | Protein | Weight | moisture | entry | rep | Program ID | sourcefile |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|
| 1 | 15 | 0 | 36 | 62.7 | 10.4 | 3.081 | 14.3 | 1 | 1 | HRW20077 | HWEYT_Dufur |
| 2 | 14 | 1 | 36 | 64.3 | 11.7 | 2.325 | 12.3 | 2 | 1 | HRW20023 | HWEYT_Dufur |
| 3 | 14 | 11 | 30 | 63.9 | 11.5 | 2.633 | 14.8 | 3 | 1 | HRW20020 | HWEYT_Dufur |
| 4 | 13 | 10 | 32 | 63.6 | 10.5 | 3.387 | 15.7 | 4 | 1 | HRW20021 | HWEYT_Dufur |
| 5 | 14 | 11 | 31 | 64.2 | 12.1 | 2.525 | 13.8 | 5 | 1 | HRW20019 | HWEYT_Dufur |
| 6 | 15 | 0 | 37 | 63.6 | 12.3 | 2.678 | 14.3 | 6 | 1 | HRW20001 | HWEYT_Dufur |

\
\
[`save`](https://rdrr.io/r/base/save.html)`(``data_raw_rename1``,`\
`     file ``=`` ``here``::`[`here`](https://here.r-lib.org/reference/here.html)`(``knitroutput_folder``, ``"data_raw_rename1.RData"``)``)`

### Some functions that may help explore the columns within the list of data

There are often instances where column names are not used consistently
between files or there are similar column names within one dataset (test
vs. testwt). The following functions may help to summarize and explore
the data.

- See
  [`summarize_variables()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)
  used above.
- [`summarize_ncol()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md)
  and `summarize_colnames` summarizes the information collected from
  [`summarize_variables()`](https://idahoagstats.github.io/facthelpeR/reference/summarize_variables.md)

List the number of columns that match a given regex per dataset.

\
`cols_summary`` ``<-`` `[`summarize_ncol`](https://idahoagstats.github.io/facthelpeR/reference/summarize_ncol.md)`(``data_raw_rename1``, `[`c`](https://rdrr.io/r/base/c.html)`(``"entry"``, ``"length"``)``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``cols_summary``)`

| file_sheet   | entry | length |
|:-------------|------:|-------:|
| HWEYT_Dufur  |     1 |      2 |
| HWEYT_Condon |    NA |      2 |

List the proportion of files that contain a given

\
`cols_prop`` ``<-`` `[`summarize_colnames`](https://idahoagstats.github.io/facthelpeR/reference/summarize_colnames.md)`(``data_raw_rename1``, `[`c`](https://rdrr.io/r/base/c.html)`(``"entry"``, ``"length"``)``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``cols_prop``)`

| colname       | prop_files | col_var |
|:--------------|-----------:|:--------|
| entry         |        0.5 | entry   |
| Length_Feet   |        1.0 | length  |
| Length_Inches |        1.0 | length  |

Pull all the columns that match a given regex. This is useful for
checking to see whether column names have been used consistently within
and across datasets.

\
`cols`` ``<-`` `[`select_colsfromlist`](https://idahoagstats.github.io/facthelpeR/reference/select_colsfromlist.md)`(``data_raw_rename1``,  `[`c`](https://rdrr.io/r/base/c.html)`(``"entry"``, ``"length"``)``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``cols``)`

| Length_Feet HWEYT_Condon 1 | Length_Inches HWEYT_Condon 1 | entry HWEYT_Dufur 2 | Length_Feet HWEYT_Dufur 2 | Length_Inches HWEYT_Dufur 2 |
|---:|---:|---:|---:|---:|
| 15 | 9 | 1 | 15 | 0 |
| 15 | 10 | 2 | 14 | 1 |
| 15 | 10 | 3 | 14 | 11 |
| 15 | 5 | 4 | 13 | 10 |
| 15 | 5 | 5 | 14 | 11 |
| 15 | 4 | 6 | 15 | 0 |

## Bind all data.frames

\
`data_all1`` ``<-`` `[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)`(``data_raw_rename1``)`\
[`save`](https://rdrr.io/r/base/save.html)`(``data_all1``,`\
`     file ``=`` ``here``::`[`here`](https://here.r-lib.org/reference/here.html)`(``knitroutput_folder``,``"data_all1.RData"``)``)`

## Curate column contents

See the [**factcuratoR**](https://idahoagstats.github.io/factcuratoR/)
package for validating columns and column contents
