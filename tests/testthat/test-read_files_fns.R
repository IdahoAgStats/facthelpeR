data_folder <- testthat::test_path("test_example_files")
library(magrittr)

test_that("read.excelheader reads in header correctly", {
  sheets <- list_sheetnames(data_folder, "test_skiprows.xls") %>%
    dplyr::mutate(skip = 3) %>%
    dplyr::mutate(header_start = 2) %>%
    dplyr::mutate(header_end = 4)
  path <- paste(data_folder, sheets$filename, sep = "/")
  ans <- data.frame(colnames = paste0("col_name_name", 1:3))
  rownames(ans) <- paste0("...",1:3)
  header_test <- read.excelheader(sheets$sheet_name,
                                  path,
                                  sheets$header_start,
                                  sheets$header_end,
                                  unique_names = TRUE)
  expect_equal(ans$colnames, header_test$colnames)


})

test_that("read.excelheader reads in header correctly (NAs and duplicate names)", {
  sheets <- list_sheetnames(data_folder, "test_header.xlsx") %>%
    mutate(header_start = 1) %>%
    mutate(header_end = 2)
  path <- paste(data_folder, sheets$filename, sep = "/")
  ans <- c("col", "col...2", "col_3", "...4")
  header_test <- read.excelheader(sheets$sheets, path, sheets$header_start, sheets$header_end, unique_names = TRUE)
  expect_equal(ans, header_test$colnames)


})

test_that("read.excelheader reads in header correctly (NAs and duplicate names)", {
  sheets <- list_sheetnames(data_folder, "test_header2.xlsx") %>%
    mutate(header_start = 1) %>%
    mutate(header_end = 2)
  path <- paste(data_folder, sheets$filename, sep = "/")
  ans <- c("col", "...2", "col_3", "col...4")
  header_test <- read.excelheader(sheets$sheet_name, path, sheets$header_start, sheets$header_end, unique_names = TRUE)
  expect_equal(ans, header_test$colnames)


})


test_that("read.excelheader reads in header correctly (NAs and duplicate names)", {
  sheets <- list_sheetnames(data_folder, "test_header3.xlsx") %>%
    mutate(header_start = 1) %>%
    mutate(header_end = 2)
  path <- paste(data_folder, sheets$filename, sep = "/")
  ans <- c("...1", "col", "col name", "col_3", "col...5")
  header_test <- read.excelheader(sheets$sheet_name, path, sheets$header_start, sheets$header_end, unique_names = TRUE)
  expect_equal(ans, header_test$colnames)


})
