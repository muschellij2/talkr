testthat::context("Trying to arrange")

df = tibble::rownames_to_column(mtcars, var = "car")


testthat::test_that("Testing commands from examples", {
  cmds = c(
    "Sort df by  mpg",
    "Sort df by  column    mpg  ",
    "arrange by column 5",
    "arrange by columns 4 and 5",
    "arrange by columns 4 and 5, mpg decreasing",
    # duplciate
    "arrange by columns 2 and 5, mpg decreasing",
    "arrange by columns 4, 5, and 6",
    "sort by mpg descending",
    "sort by mpg ascending",
    "sort by mpg ascending",
    "sort by mpg low to high")
  testthat::expect_warning({
    res = lapply(cmds, talk_arrange, .data = df)
  })
  testthat::expect_is(res, "list")
  testthat::expect_is(res[[1]], "data.frame")

  # is this relevant?
  testthat::expect_warning({
    df %>%
      talk_arrange("arrange by columns 2 and 5, mpg decreasing")
  }, regexp = "uplicate")

  testthat::expect_silent({
    df %>%
      talk_arrange("arrange by columns 2 and 5")
  })

})
