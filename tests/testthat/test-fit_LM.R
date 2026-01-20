test_that("fit simple model works", {
  testY <- seq(1,10,1)
  testX <- rnorm(10)
  testNow <- rnorm(10)
  test_mod <- fit_LM(testY,testX,testNow)
  expect_equal(test_mod$coefficients[[1]], lm(testY ~ testX)$coefficients[[1]])
  expect_equal(test_mod$coefficients[[2]], lm(testY ~ testX)$coefficients[[2]])
})
