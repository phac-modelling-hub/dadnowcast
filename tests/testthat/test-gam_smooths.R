test_that("Gam smooths works for arbitrary inputs", {
  expect_no_error({
    df <- simulate_mechanistic()
  
    foo = nowcast(
      model = 'gam', 
      formula = dad ~ cnisp + ptsos, 
      data = df, 
      date_col = "date",
      params = list(smooths = list('cnisp', 'ptsos' = 10))
    ) 
  })
})
