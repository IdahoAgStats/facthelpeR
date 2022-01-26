# This file is to test the behavior of wrappers, especially with quosures/tidyeval
#https://community.rstudio.com/t/programming-with-a-function-that-accepts-bare-arguments/6756/4
# what does enexpr() do versus enquo()? -> enquo() looks like it also provides an environment
# https://stackoverflow.com/questions/53093630/r-bare-to-quosure-in-function-invalid-argument-type

# w = wrapper
library(dplyr)
df <- data.frame(a = 1:5, b = 5:1)

f1 <- function(x, data) {
  enquo_x <- enquo(x)
  data %>% select(enquo_x)
}

w1 <- function(x, data) {
  f1(x = x, data = data)
}

w1(df, x = a)

w1_2 <- function(x, data) {
  .x <- enexpr(x)
  print(.x)
  eval(expr(f1(data = data, !!.x)))
}

w1_2(x = a, df)

# The function must use tidyeval {{}}, not enquo and !!, unsure why
f2 <- function(x, data) {
  data %>% select({{x}})
}

w2 <- function(x, data) {
  f2(x = x, data = data)
}

w2(df, x = a)

# This wrapper works
w2_2 <- function(x, data) {
  .x <- enexpr(x) # enquo doesn't work
  print(.x)
  eval(expr(f2(data = data, !!.x)))
}

w2_2(x = a, df)
