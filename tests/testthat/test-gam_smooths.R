test_that(desc = "Gam smooths works for arbitrary inputs", {
  
  df <- simulate_mechanistic()
  
  expect_no_error({
    foo1 = nowcast(
      model = 'gam', 
      formula = dad ~ cnisp + ptsos, 
      data = df, 
      date_col = "date",
      params = list(smooths = list('cnisp')),
      quiet = TRUE
    ) 
  })
  
  expect_no_error({
    foo2 = nowcast(
      model = 'gam', 
      formula = dad ~ cnisp + ptsos, 
      data = df, 
      date_col = "date",
      params = list(smooths = list('cnisp', 'ptsos' = 10)),
      quiet = TRUE
    )
  })
  
})
