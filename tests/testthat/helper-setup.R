# vdiffr ignores failures when
#   - VDIFFR_RUN_TESTS is "false" (on Travis CI with older versions and dev version of R)
#   - CI is not set (on CRAN)

expect_doppelganger <- vdiffr::expect_doppelganger

# Predefined plots

library(ggplot2)
theme_set(theme_test() + theme(panel.grid = element_blank()))

p1 <- ggplot(mtcars) +
  geom_point(aes(mpg, disp)) +
  ggtitle('Plot 1')

p2 <- ggplot(mtcars) +
  geom_boxplot(aes(gear, disp, group = gear)) +
  ggtitle('Plot 2')

p3 <- ggplot(mtcars) +
  geom_point(aes(hp, wt, colour = mpg)) +
  ggtitle('Plot 3')

p4 <- ggplot(mtcars) +
  geom_bar(aes(gear)) +
  facet_wrap(~cyl) +
  ggtitle('Plot 4')


print_plot.patchwork <- function(p, title = '') {
  if (is.null(p$patches$annotation$title)) {
    p <- p + plot_annotation(title = title)
  }
  print(p)
}
