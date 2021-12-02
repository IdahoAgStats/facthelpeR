source(here::here("R", "dependencies.R"))
data_folder <- here::here("tests", "test_example_files/")


test_that("list.sheetnames lists correct sheets per file", {

  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).xlsx")

  maps_sheets <- list.sheetnames(data_folder, files) #%>% mutate(list_names = sheet)

  ans <- data.frame(filename =
                      rep(c("test_file1.xlsx", "test_file2.xlsx"), each = 3),
                    sheets =
                      rep(c("Sheet1", "Sheet2", "Sheet3"), 2))

  expect_equal(maps_sheets, ans)

})

test_that("read.multsheets reads in the correct number of elements", {
  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).xlsx")

  maps_sheets <- list.sheetnames(data_folder, files) #%>% mutate(list_names = sheet)

  maps_list <- read.multsheets(data_folder, maps_sheets, col_names = TRUE)

  expect_equal(length(maps_list), 6)
})

##
test_that("read.multsheets reads in the correct header", {
  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).xlsx")

  maps_sheets <- list.sheetnames(data_folder, files) #%>% mutate(list_names = sheet)

  maps_list <- read.multsheets(data_folder, maps_sheets, col_names = TRUE)

  expect_equal(names(maps_list[[1]]), c("a", "b"))
})


test_that("read.multsheets reads in the correct header for .csv files", {
  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).csv")
  sheet_info <- data.frame(filename = files)

  maps_list <- read.multsheets(data_folder, sheet_info, col_names = TRUE)

  expect_equal(names(maps_list[[1]]), c("a", "b"))
})

##
test_that("read.multsheets skips the correct rows and reads in the data as expected", {
  #files <- list.files(data_folder, recursive = TRUE, pattern = ".csv")

  # the current functions don't work for .csv
  sheets <- list.sheetnames(data_folder, "test_skiprows.xls") %>%
    mutate(header_end = 4) %>% mutate(header_start = NA)

  df_list <- read.multsheets(data_folder, sheets, col_names = TRUE)
  ans <- tibble(name1 = c(1,2,3), name2 = c("a", "b", "c"), name3 = c("a", "b", "c"))

  expect_equal(df_list[[1]], ans)

})

test_that("read.excelheader reads in header correctly", {
  sheets <- list.sheetnames(data_folder, "test_skiprows.xls") %>%
    mutate(skip = 3) %>%
    mutate(header_start = 2) %>%
    mutate(header_end = 4)
  path <- paste0(data_folder, sheets$filename)
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
  sheets <- list.sheetnames(data_folder, "test_header.xlsx") %>%
    mutate(header_start = 1) %>%
    mutate(header_end = 2)
  path <- paste0(data_folder, sheets$filename)
  ans <- c("col", "col...2", "col_3", "...4")
  header_test <- read.excelheader(sheets$sheets, path, sheets$header_start, sheets$header_end, unique_names = TRUE)
  expect_equal(ans, header_test$colnames)


})

test_that("read.excelheader reads in header correctly (NAs and duplicate names)", {
  sheets <- list.sheetnames(data_folder, "test_header2.xlsx") %>%
    mutate(header_start = 1) %>%
    mutate(header_end = 2)
  path <- paste0(data_folder, sheets$filename)
  ans <- c("col", "...2", "col_3", "col...4")
  header_test <- read.excelheader(sheets$sheet_name, path, sheets$header_start, sheets$header_end, unique_names = TRUE)
  expect_equal(ans, header_test$colnames)


})


test_that("read.excelheader reads in header correctly (NAs and duplicate names)", {
  sheets <- list.sheetnames(data_folder, "test_header3.xlsx") %>%
    mutate(header_start = 1) %>%
    mutate(header_end = 2)
  path <- paste0(data_folder, sheets$filename)
  ans <- c("...1", "col", "col name", "col_3", "col...5")
  header_test <- read.excelheader(sheets$sheet_name, path, sheets$header_start, sheets$header_end, unique_names = TRUE)
  expect_equal(ans, header_test$colnames)


})

test_that("read.multsheets reads in multiple header rows correctly", {
  sheets <- list.sheetnames(data_folder, "test_skiprows.xls") %>%
    mutate(header_start = 2) %>%
    mutate(header_end = 4)

  df_list <- read.multsheets(data_folder, sheets, col_names = TRUE)
  ans <- tibble(name1 = c(1,2,3), name2 = c("a", "b", "c"), name3 = c("a", "b", "c"))
  names(ans) <- paste0("col_name_name", 1:3)
  expect_equal(df_list[[1]], ans)

})


test_that("read.multsheets reads in multiple header rows correctly", {
  sheets <- list.sheetnames(data_folder, "test_header.xlsx") %>%
    mutate(header_start = 1) %>%
    mutate(header_end = 2)

  df_list <- read.multsheets(data_folder, sheets, col_names = TRUE)
  ans <- tibble(col = 1:3, col...2 = 1:3, col_3 = 1:3, ...4 = 1:3)
  expect_equal(df_list[[1]], ans)

})

