test_that("axes and titles are collected correctly for multi-cell plots", {
  plots  <- wrap_plots(rep(list(p1), 8))
  layout <- plot_layout(
    design = "12345\n62378",
    axes = "collect",
    axis_titles = "collect"
  )

  # columns 1, 2 and 4 should have titles and y-axes
  # only row 2 should have titles and x-axes
  expect_doppelganger(
    "multi-cell title and axis collection",
    plots + layout
  )
})

test_that("axis columns are properly resized", {

  p5 <- p1 + scale_y_continuous(
    labels = function(x) paste0("a long axis label signifying ", x)
  )
  p6 <- p1 + theme(axis.text = element_text(colour = "red")) +
    ggtitle("Interrupting plot")

  layout <- plot_layout(2, 2, axes = "collect", axis_titles = "collect")

  # only column 1 should have long axis labels
  # upper left plot should not have x-axis, there rest should
  # there should be no excess space between column 1 and 2
  expect_doppelganger(
    "corrected spacing for long axis labels",
    p5 + p5 + p5 + p6 + layout
  )
})
