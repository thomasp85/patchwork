test_that("`+` works", {
  expect_doppelganger('Standard addition: p1 + p2 + p3', {
    p1 + p2 + p3
  })

  expect_doppelganger('Adding to patchwork: (p1 + p2) + p3', {
    (p1 + p2) + p3
  })

  expect_doppelganger('Add patchwork to plot: p1 + (p2 + p3)', {
    p1 + (p2 + p3)
  })

  expect_doppelganger('Add grob: p1 + textGrob("test")', {
    p1 + grid::textGrob("test")
  })

  expect_doppelganger('Add ggplot elements: p1 + p2 + theme_bw()', {
    p1 + p2 + theme_bw()
  })

  skip_if_not_installed("gridGraphics")
  expect_doppelganger('Add base graphics: p1 + ~plot(1:10, 1:10)', {
    p1 + ~plot(1:10, 1:10)
  })
})

test_that("`-` works", {
  expect_doppelganger('Nest left-hand side: (p1 + p2) - p3', {
    (p1 + p2) - p3
  })

  expect_doppelganger('Nest right-hand side: p1 - (p2 + p3)', {
    p1 - (p2 + p3)
  })
})

test_that("`|` and `/` works", {
  expect_doppelganger('Stack 3 plots: p1 / p2 / p3', {
    p1 / p2 / p3
  })

  expect_doppelganger('Pack 4 plots: p1 | p2 | p3 | p4', {
    p1 | p2 | p3 | p4
  })

  expect_doppelganger('Complex composition: ((p1 / p2) | p3) / p4', {
    ((p1 / p2) | p3) / p4
  })
})

test_that("`&` and `*` works", {
  patchwork <- ((p1 / p2) | p3) / p4
  expect_doppelganger('Adding to all subplots: patchwork & theme_bw()', {
    patchwork & theme_bw()
  })

  expect_doppelganger('Adding to all on level: patchwork * theme_bw()', {
    patchwork * theme_bw()
  })
})
