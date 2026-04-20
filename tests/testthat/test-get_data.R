test_that("get_data() works", {
  
  # Create synthetic data
  n = 100
  dates = as.Date('2026-01-01', ) + 1:n
  rvdss = rnorm(n=n, mean = c(1:n)/3/n, sd = 0.05 )
  dad = round(50 * rvdss + 20 + rnorm(n=n, mean = 0, sd = 8))
  rvdss[rvdss < 0] = 0
  dad[dad < 0] = 0
  dad[(n-12):n] <- NA
  # plot(rvdss, dad)
  
  data = data.frame(date = dates, rvdss = rvdss, dad = dad)
  
  # Create dadnow object
  nc = nowcast(formula  = dad ~ rvdss , 
               data     = data, 
               model    = 'lm',
               date_col = 'date', 
               se       = 'theoretical') |>
    add_nowcast(model = "arx", 
                formula = dad~ rvdss, 
                params = list(p = 1)) 

  # Tests on `get_data()`
  
  thedata  = get_data(dadnow = nc, include_training = TRUE)
  thedata2 = get_data(dadnow = nc, include_training = FALSE)
  
  testthat::expect_s3_class(thedata, 'data.frame')
  testthat::expect_s3_class(thedata2, 'data.frame')
  
})
