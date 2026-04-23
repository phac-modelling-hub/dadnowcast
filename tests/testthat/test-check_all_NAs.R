test_that("check_all_NAs works", {
  n = 20
  df.good = data.frame(date = as.Date('2026-01-01')+1:n,
                       foo1 = runif(n),
                       foo2 = rpois(n, lambda = 5))
  df.good$foo1[c(2,5,7)] <- NA
  df.bad = df.good
  df.bad$foo2 <- NA
  expect_no_error(check_all_NAs(df.good))
  expect_error(check_all_NAs(df.bad))
})
