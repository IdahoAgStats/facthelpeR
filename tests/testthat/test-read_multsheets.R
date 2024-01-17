data_folder <- testthat::test_path("test_example_files")


test_that("read_multsheets reads in the correct number of elements", {
  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).xlsx")

  maps_sheets <- list_sheetnames(data_folder, files) #%>% dplyr::mutate(list_names = sheet)

  maps_list <- read_multsheets(data_folder, maps_sheets, col_names = TRUE)

  expect_equal(length(maps_list), 6)
})

test_that("read_multsheets reads in the correct header", {
  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).xlsx")

  maps_sheets <- list_sheetnames(data_folder, files) #%>% dplyr::mutate(list_names = sheet)

  maps_list <- read_multsheets(data_folder, maps_sheets, col_names = TRUE)

  expect_equal(names(maps_list[[1]]), c("a", "b"))
})


test_that("read_multsheets reads in the correct header for .csv files", {
  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).csv")
  sheet_info <- data.frame(filename = files)

  maps_list <- read_multsheets(data_folder, sheet_info, col_names = TRUE)

  expect_equal(names(maps_list[[1]]), c("a", "b"))
})

##
test_that("read_multsheets skips the correct rows and reads in the data as expected", {
  sheets <- list_sheetnames(data_folder, "test_skiprows.xls") %>%
    dplyr::mutate(header_end = 4) %>% dplyr::mutate(header_start = NA)

  df_list <- read_multsheets(data_folder, sheets, col_names = TRUE)
  ans <- tibble(name1 = c(1,2,3), name2 = c("a", "b", "c"), name3 = c("a", "b", "c"))

  expect_equal(df_list[[1]], ans)

})

test_that("read_multsheets skips the correct rows and reads in the data as expected
          for .csv", {
  files <- list.files(data_folder, recursive = TRUE, pattern = "test_skiprows.csv")
  sheet_info <- data.frame(filename = files, header_end = 4, header_start = NA)

  df_list <- read_multsheets(data_folder, sheet_info, col_names = TRUE)
  ans <- tibble(name1 = c(1,2,3), name2 = c("a", "b", "c"), name3 = c("a", "b", "c"))

  expect_equal(df_list[[1]], ans)

})


test_that("read_multsheets reads in multiple header rows correctly", {
  sheets <- list_sheetnames(data_folder, "test_skiprows.xls") %>%
    dplyr::mutate(header_start = 2) %>%
    dplyr::mutate(header_end = 4)

  df_list <- read_multsheets(data_folder, sheets, col_names = TRUE)
  ans <- tibble(name1 = c(1,2,3), name2 = c("a", "b", "c"), name3 = c("a", "b", "c"))
  names(ans) <- paste0("col_name_name", 1:3)
  expect_equal(df_list[[1]], ans)

})

test_that("read_multsheets reads in multiple header rows correctly for .csv", {
  file_df <- data.frame(filename = "test_skiprows.csv", header_start = 2, header_end = 4)

  df_list <- read_multsheets(data_folder, file_df, col_names = TRUE)
  ans <- tibble(name1 = c(1,2,3), name2 = c("a", "b", "c"), name3 = c("a", "b", "c"))
  names(ans) <- paste0("col_name_name", 1:3)
  expect_equal(df_list[[1]], ans, ignore_attr = TRUE)

})


test_that("read_multsheets uses guess_max input and guesses column types correctly for csv files", {
files <- list.files(data_folder, recursive = TRUE, pattern = "test_guess_max.csv")
sheet_info <- data.frame(filename = files, header_end = 1)

df_list <- read_multsheets(data_folder, sheet_info, col_names = TRUE, na = "", guess_max = 2000)
expect_type(df_list[[1]][["col2"]], "double")
})
