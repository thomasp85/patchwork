test_that("`wrap_plots()` allows a trailing comma", {
  expect_equal(
    as.list(wrap_plots(p1, p2,)),
    as.list(wrap_plots(p1, p2))
  )
})
