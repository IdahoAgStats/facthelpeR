blank <- rep(NA, 3)
a <- c(1, 2, blank, 4)
b <- c(1, 2, blank, 4, blank, 6, blank, 8, 9)
d <- c(1:3)
df1 <- data.frame(a, a)
df2 <- data.frame(b, b)
df3 <- data.frame(d, d, blank, d)

test_that("split_direction() returns expected entries in the list", {
  test <- split_direction(df1, direction = "row")
  expect_equal(length(test), 2)
})

test_that("split_df() returns returns expected entries in the list", {
  test <- split_df(df1, showWarning = FALSE)
  expect_equal(length(test), 2)
})

test_that("split_direction() returns expected entries in the list", {
  test <- split_direction(df2, direction = "row")
  expect_equal(length(test), 4)
})

test_that("split_df() returns returns expected entries in the list", {
  test <- split_df(df2, showWarning = FALSE)
  expect_equal(length(test), 4)
})

test_that("split_direction() returns expected entries in the list", {
  test <- split_direction(df3, direction = "col")
  expect_equal(length(test), 2)
})

test_that("split_direction() returns data.frames for each object in the list", {
  test <- split_direction(df3, direction = "col")
  expect_equal(unlist(lapply(test, function(x) typeof(x))), c("list", "list"))
})

test_that("split_df() returns returns expected entries in the list", {
  test <- split_df(df3, showWarning = FALSE)
  expect_equal(length(test), 2)
})


