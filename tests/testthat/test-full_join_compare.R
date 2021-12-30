
test_that("full_join_compare() returns expected join", {
  df1 <- data.frame(a = c(1,2) , b = c("a", "b"))
  df2 <- data.frame(a = c(1,3), c = c("y", "z"))
  test <- full_join_compare(df1, df2, by = "a", "df1", "df2")
  ans <- data.frame(a_df1 = c(1,2,3), b_df1 = c("a", "b", NA), c_df2 = c("y", NA, "z"))
  
  expect_equal(test,ans)
})

test_that("full_join_compare() returns expected join", {
  df1 <- data.frame(a = c(1,2) , b = c("a", "b"))
  df2 <- data.frame(a = c(1,3), c = c("y", "z"))
  test <- full_join_compare(df1, df2, by = "a", NA, "df2")
  ans <- data.frame(a = c(1,2,3), b = c("a", "b", NA), c_df2 = c("y", NA, "z"))
  
  expect_equal(test,ans)
})


test_that("full_join_compare() returns expected join", {
  df1 <- data.frame(a = c(1,2) , b = c("a", "b"))
  df2 <- data.frame(a = c(1,3), c = c("y", "z"))
  test <- full_join_compare(df1, df2, by = "a", "df1", NA)
  ans <- data.frame(a_df1 = c(1,2,3), b_df1 = c("a", "b", NA), c = c("y", NA, "z"))
  
  expect_equal(test,ans)
})

test_that("full_join_compare() returns expected join", {
  df1 <- data.frame(a = c(1,2) , b = c("a", "b"))
  df2 <- data.frame(a = c(1,3), c = c("y", "z"))
  test <- full_join_compare(df1, df2, by = "a", NA, NA)
  ans <- data.frame(a = c(1,2,3), b = c("a", "b", NA), c = c("y", NA, "z"))
  
  expect_equal(test,ans)
})

test_that("full_join_compare() returns expected join using 2 join variables", {
  df1 <- data.frame(a = c(1,2) , b = c("a", "b"), c = c("y", "z") )
  df2 <- data.frame(a = c(1,3), b = c("a", "a") , d = c("y", "z"))
  test <- full_join_compare(df1, df2, by = c("a", "b"), "df1", "df2")
  ans <- data.frame(a_df1 = c(1,2,3), b_df1 = c("a", "b", "a"), c_df1 = c("y", "z", NA), d_df2 = c("y", NA, "z"))
  
  expect_equal(test,ans)
})

