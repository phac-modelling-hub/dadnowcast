test_that("linear models work", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_simple_model(y ~ x, model = "lm", df)$coef[[1]], 1712.976, tolerance = 0.001)
})

test_that("linear models work 2", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_simple_model(y ~ x, model = "lm", df)$coef[[2]], -0.1818, tolerance = 0.001)
})

test_that("AR models work", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_simple_model(y~x, model = "ar", df, order = 1)$coef[[1]], 0.7590775, tolerance = 0.001)
})

test_that("AR models work 2", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_simple_model(y~x, model = "ar", df, order = 1)$coef[[2]], 1706.807, tolerance = 0.001)
})

test_that("Incorrect model specification works", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_simple_model(y~x, model = "None", df), "Model not recognized")
})