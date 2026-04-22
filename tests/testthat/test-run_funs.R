test_that("No errors in main functions", {
  expect_no_error({
    df <- simulate_mechanistic()
    dadnow2 <- nowcast(
      formula = dad ~ lag(cnisp, 3) + lag(ptsos, 3),
      data = df, 
      model = "ar", 
      date_col = "date", 
      params = list(p = 1)
    )
    
    dadnow2 <- add_nowcast(
      dadnow2, model = "ar", params = list(p = 2)
    ) |>
      add_nowcast("lm")|>
      add_nowcast("mechanistic",
        formula = dad ~ cnisp + ptsos + npos,
        params = list(sc = 0.2, sp = 0.3, method = "normal")
      )

    evals <- get_evals(dadnow2)

    train_preds <- get_data(dadnow2, include_training = TRUE)

    resids <- get_residuals(dadnow2)

    model_lm <- get_model(dadnow2, "lm_f1")

    new_data <- prep_newdata(
      formula = get_formula(dadnow2, "lm_f1"),
      data = df,
      date_col = "date",
      interpolate = TRUE
    )
  })
})
