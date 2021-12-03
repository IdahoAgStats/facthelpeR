data_folder <- testthat::test_path("test_example_files")
test_that("list_sheetnames lists correct sheets per file", {

  files <- list.files(data_folder, recursive = TRUE, pattern = "test_file(1|2).xlsx")

  maps_sheets <- list_sheetnames(data_folder, files) #%>% mutate(list_names = sheet)

  ans <- data.frame(filename =
                      rep(c("test_file1.xlsx", "test_file2.xlsx"), each = 3),
                    sheets =
                      rep(c("Sheet1", "Sheet2", "Sheet3"), 2))

  expect_equal(maps_sheets, ans)

})
