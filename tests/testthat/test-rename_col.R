test_that("rename.col correctly renames column names", {

  ls <- list(df1 = data.frame(a = 1:3, b = 1:3),
             df2 = data.frame(a = 1:3, b = 1:3))

  rename_df <- data.frame(new_name = c("a_new", "b_new"),
                          old_name = c("a", "b"))

  ans <- list(df1 = data.frame(a_new = 1:3, b_new = 1:3),
              df2 = data.frame(a_new = 1:3, b_new = 1:3))

  ls_rename <- rename.col(ls, rename_df, rename_col = new_name, old_col = old_name)

  expect_equal(ls_rename, ans)

})

test_that("rename.col can handle duplicate names in a data.frame", {

  ls <- list(df1 = data.frame(a = 1:3, b = 1:3), df2 = data.frame(a = 1:3, b = 1:3))
  rename_df <- data.frame(new_name = c("new1", "new1"), old_name = c("a", "b"))
  ans <- list(df1 = data.frame(new1 = 1:3, new1.1 = 1:3), df2 = data.frame(new1 = 1:3, new1.1 = 1:3))

  ls_rename <- rename.col(ls, rename_df, new_name, old_name)

  expect_equal(ls_rename, ans)

})

test_that("rename.col mathces names exactly", {

  ls <- list(df1 = data.frame(a = 1:3, ab = 1:3), df2 = data.frame(a = 1:3, b = 1:3))
  rename_df <- data.frame(new_name = c("a_new", "b_new"), old_name = c("a", "b"))
  ans <- list(df1 = data.frame(a_new = 1:3, ab = 1:3), df2 = data.frame(a_new = 1:3, b_new = 1:3))

  ls_rename <- rename.col(ls, rename_df, new_name, old_name)

  expect_equal(ls_rename, ans)

})


test_that("rename.col correctly renames column names (if no new name is given)", {

  ls <- list(df1 = data.frame(a = 1:3, b = 1:3), df2 = data.frame(a = 1:3, b = 1:3))
  rename_df <- data.frame(new_name = c("a_new", NA), old_name = c("a", "b"))
  ans <- list(df1 = data.frame(a_new = 1:3, b = 1:3), df2 = data.frame(a_new = 1:3, b = 1:3))

  ls_rename <- rename.col(ls, rename_df, new_name, old_name)

  expect_equal(ls_rename, ans)

})

test_that("rename.col can handle renaming each element in ls independently", {

  ls <- list(df1 = data.frame(a = 1:3, b = 1:3), df2 = data.frame(a = 1:3, b = 1:3))
  rename_df <- data.frame(new_name = c("a_df1", "b_df1", "a_df2", "b_df2"),
                          old_name = rep(c("a", "b")),
                          element = rep(c("df1", "df2"), each = 2))

  ans <- list(df1 = data.frame(a_df1 = 1:3, b_df1 = 1:3), df2 = data.frame(a_df2 = 1:3, b_df2 = 1:3))

  ls_rename <- rename.col(ls, rename_df, new_name, old_name, element)

  expect_equal(ls_rename, ans)

})

test_that("rename.col removes cols that have the label 'rm_col'", {

  ls <- list(df1 = data.frame(a = 1:3, b = 1:3), df2 = data.frame(a = 1:3, b = 1:3))
  rename_df <- data.frame(new_name = c("a_df1", "b_df1", "a_df2", "rm_col"),
                          old_name = rep(c("a", "b")),
                          element = rep(c("df1", "df2"), each = 2))

  ans <- list(df1 = data.frame(a_df1 = 1:3, b_df1 = 1:3), df2 = data.frame(a_df2 = 1:3))

  ls_rename <- rename.col(ls, rename_df, new_name, old_name, element, rm_col = TRUE)

  expect_equal(ls_rename, ans)

})

