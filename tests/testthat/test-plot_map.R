test_that("plot_map() returns expected output", {
  df <- data.frame(
    trial = "trial1",
    variety = rep(c("test1", "test2", "test3"), 3),
    range = rep(1:3, each = 3),
    row = c(1:3, 2, 3, 1, 3:1)
  )

  test <- plot_map(df, "trial", "trial1", "variety")
  expect_equal(nrow(test), 3)
})

test_that("plot_map() returns a warning for duplicate range/row locations", {
  df <- data.frame(
    trial = "trial1",
    variety = rep(c("test1", "test2", "test3"), 3),
    range = rep(1:3, each = 3),
    row = c(1:3, 2, 2, 1, 3:1)
  )

  test <- expect_warning(plot_map(df, "trial", "trial1", "variety"), "Values from")
})

test_that("plot_map() correctly maps multiple trials", {
  df <- data.frame(
    trial = "trial1",
    variety = rep(c("test1", "test2", "test3"), 3),
    range = rep(1:3, each = 3),
    row = c(1:3, 2, 3, 1, 3:1)
  ) %>%
    bind_rows(
      df <- data.frame(
        trial = "trial2",
        variety = rep(c("test1", "test2", "test3"), 3),
        range = rep(1:3, each = 3),
        row = c(1:3, 2, 3, 1, 3:1)
      )
    )

  test <- plot_map(df, "trial", "all", "variety")
  expect_equal(nrow(test), 6)
})


