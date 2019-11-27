test_that("The grid can be controlled", {
  expect_doppelganger('Setting ncol: p1 + p2 + p3 + p4 + plot_layout(ncol = 3)', {
    p1 + p2 + p3 + p4 + plot_layout(ncol = 3)
  })

  expect_doppelganger('Setting nrow: p1 + p2 + p3 + p4 + plot_layout(nrow = 3)', {
    p1 + p2 + p3 + p4 + plot_layout(nrow = 3)
  })

  expect_doppelganger('Setting widths: p1 + p2 + p3 + p4 + plot_layout(widths = c(1, 2))', {
    p1 + p2 + p3 + p4 + plot_layout(widths = c(1, 2))
  })

  expect_doppelganger('Setting heights: p1 + p2 + p3 + p4 + plot_layout(heights = c(1, 2))', {
    p1 + p2 + p3 + p4 + plot_layout(heights = c(1, 2))
  })

  expect_doppelganger('Setting widths as units: p1 + p2 + p3 + p4 + plot_layout(widths = unit(3, "cm"))', {
    p1 + p2 + p3 + p4 + plot_layout(widths = grid::unit(3, "cm"))
  })

  expect_doppelganger('Setting heights as units: p1 + p2 + p3 + p4 + plot_layout(heights = unit(3, "cm"))', {
    p1 + p2 + p3 + p4 + plot_layout(heights = grid::unit(3, "cm"))
  })
})
