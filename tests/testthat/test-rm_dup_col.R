test_that("rm.dup_col removes duplicate columns of letters", {
  df1 <- data.frame(a = letters[1:2], b = letters[1:2], c = letters[3:4])
  ans <- df1 %>% select(a..b = a, c)
  expect_equal(rm.dup_col(df1), ans)
})

test_that("rm.dup_col removes duplicate columns and retains original names", {
  df1 <- data.frame(a = letters[1:2], b = letters[1:2], c = letters[3:4])
  ans <- df1 %>% select(a, c)
  expect_equal(rm.dup_col(df1, FALSE), ans)
})

test_that("rm.dup_col retains columns with different contents", {
  df1 <- data.frame(a = letters[1:2], c = letters[3:4])
  expect_equal(rm.dup_col(df1), df1)
})

test_that("rm.dup_col removes duplicate columns of letters (with more than one duplicate column)", {
  df1 <- data.frame(a = letters[1:2], b = letters[1:2], c = letters[3:4], d = letters[1:2])
  ans <- df1 %>% select(a..b..d = a, c)
  expect_equal(rm.dup_col(df1), ans)
})

test_that("rm.dup_col removes duplicate columns of numerics",{
  df1 <- data.frame(a = c(1,2,3), b = c(NA,2,3), c = c(NA,4,5), d = c(3, 4,5))
  ans <- df1 %>% select(a..b = a, d..c = d)
  expect_equal(rm.dup_col(df1), ans)
})

test_that("rm.dup_col detects if duplicate columns both contain NAs", {
  df1 <- data.frame(a = c(1,NA,3), b = c(NA,2,3), c = c(NA,4,5), d = c(3, 4,5))
  ans <- df1 %>% select(a, b, d..c = d)
  expect_equal(rm.dup_col(df1), ans)
})


# rm.dup_col() can handle the NAs in both cols c and d and removes column c
test_that("rm.dup_col detects if duplicate columns both contain NAs", {
  df1 <- data.frame(a = c(1,NA,3), b = c(NA,2,3), c = c(NA,NA,5), d = c(NA, 4,5))
  ans <- df1 %>% select(a, b, d..c = d)
  expect_equal(rm.dup_col(df1), ans)
})

test_that("rm.dup_col detects if duplicate columns both contain NAs", {
  df1 <- data.frame(a = c(1,2,NA),
                    b = c(1,2,NA),
                    c = c(1,NA,3),
                    d = c(3, 4,5),
                    e = c(3, NA, 5))

  ans <- df1 %>% select(a..b = a, c, d..e = d)
  expect_equal(rm.dup_col(df1), ans)
})

# test if both columns are NA
test_that("rm.dup_col detects if duplicate columns both contain NAs", {
  df1 <- data.frame(a = c(1,NA,NA),
                    b = c(NA,2,3))
  df2 <- data.frame(a = rep(NA,3), b = rep(NA, 3))
  expect_equal(rm.dup_col(df1), df1)
  expect_equal(rm.dup_col(df2), df2)
})


test_that("test.col_id_elements detects identical elements", {
  df1 <- data.frame(a = c(rep(1,3)))
  df2 <- data.frame(a = c(rep(NA,3)))
  df3 <- data.frame(a = c(rep(1,3), NA))
  df4 <- data.frame(a = c(1,2,3))

  expect_equal(test.col_id_elements(df1, "a"), TRUE)
  expect_equal(test.col_id_elements(df2, "a"), TRUE)
  expect_equal(test.col_id_elements(df3, "a"), TRUE)
  expect_equal(test.col_id_elements(df4, "a"), FALSE)


})
