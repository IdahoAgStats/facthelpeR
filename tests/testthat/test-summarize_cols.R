test_that("summarize.variables returns the correct summary", {
  df1 <- data.frame(y0 = c(1, 2, 3), y2 = c(4, 5, 6))
  df2 <- data.frame(y1 = c(3, 2, 1), y2 = c(6, 5, 4))
  ls1 <- list(df1, df2)
  names(ls1) <- c("df1", "df2")
  ans <- tibble(colname = c("y0", "y2", "y1", "y2"),
                type = rep("numeric", 4),
                example = c("1","4","3","6"),
                file_sheet = c(rep("df1",2),
                               rep("df2", 2)),
                instance = rep(c(1L,2L), 2))
  expect_equal(summarize.variables(ls1, NULL), ans)
})

test_that("find_col_info()", {
  cols <-  c("a","b", "c")
  df <- data.frame(year = c(91, 92,92), a = 1:3, b = c(-9, -9, 2), c = c(NA, "a", "b") )
  ans <- data.frame(n = c(3, 1, 2), contained_in = c("91;92", "92", "92"), variable = cols, example = c("1", "2", "a"))
  test <- find_col_info(df, cols, year)
  expect_equal(test,ans)

})
