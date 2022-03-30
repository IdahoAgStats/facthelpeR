data_folder <- testthat::test_path("test_example_files")


test_that("read_ods_multsheets reads in the correct number of elements", {

  files <- list.files(data_folder, recursive = TRUE, pattern = "test_read_ods_multsheets1.ods")

    ods_list <- read_ods_multsheets(paste(data_folder, files, sep = "/"))

  expect_equal(length(ods_list), 3)
})

test_that("read_ods_multsheets reads in the correct number of files", {

  files <- list.files(data_folder, pattern = "*.ods", recursive = TRUE, full.names = TRUE)
  ods_list <- lapply(files, read_ods_multsheets)

  expect_equal(length(ods_list), length(files))
})

test_that("read_ods_multsheets names sheets as expected", {

    ods_list <- read_ods_multsheets(paste(data_folder, "test_read_ods_multsheets1.ods", sep = "/"))

  expect_equal(names(ods_list), c("test_read_ods_multsheets1_setosa",
                                  "test_read_ods_multsheets1_versicolor",
                                  "test_read_ods_multsheets1_virginica"))

})

test_that("read_ods_multsheets reads in the expected number of rows", {

  setosa <- iris[iris$Species=="setosa",]
  virginica <- iris[iris$Species=="virginica",]
  versicolor <- iris[iris$Species=="versicolor",]
  iris_list <- list(setosa, versicolor, virginica)

  ods_list <- read_ods_multsheets(paste0(data_folder, "/", "test_read_ods_multsheets1.ods"))

  expect_equal(nrow(do.call(cbind, do.call(c, ods_list))), nrow(do.call(cbind, do.call(c, iris_list))))

})
