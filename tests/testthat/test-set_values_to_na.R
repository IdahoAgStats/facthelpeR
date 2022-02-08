vec <- c(NA, -9, 0, 1)
df <- data.frame(a  = vec,
                 b = vec)

test_that("set_values_to_na() returns expected entries in the list", {
  test <- set_values_to_na(df, cols_neg9_to_na = "a")
  expect_equal(sum(is.na(test)), 3)

  test2 <- set_values_to_na(df, cols_neg9_to_na = c("a", "b"))
  expect_equal(sum(is.na(test2)), 4)

  test3 <- set_values_to_na(df,
                            cols_neg9_to_na = c("a", "b"),
                            cols_zeros_to_na = "b")
  expect_equal(sum(is.na(test3)), 5)

  test3 <- set_values_to_na(df,
                            cols_neg_to_na = "a")
  expect_equal(sum(is.na(test3)), 3)

})
