
<!-- README.md is generated from README.Rmd. Please edit that file -->
patchwork
=========

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/patchwork.svg?branch=master)](https://travis-ci.org/thomasp85/patchwork) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/patchwork?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/patchwork) [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/patchwork)](https://CRAN.R-project.org/package=patchwork) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/patchwork)](https://CRAN.R-project.org/package=patchwork)

The goal of `patchwork` is to make it ridiculously simple to combine separate ggplots into the same graphic. As such it tries to solve the same problem as `gridExtra::grid.arrange()` but using an API that incites exploration and iteration.

Installation
------------

You can install patchwork from github with:

``` r
# install.packages("devtools")
devtools::install_github("thomasp85/patchwork")
```

Example
-------

The usage of `patchwork` is simple: just add plots together!

``` r
library(ggplot2)
library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 + p2
```

![](man/figures/README-example-1.png)

you are of course free to also add the plots together as part of the same plotting operation:

``` r
ggplot(mtcars) +
  geom_point(aes(mpg, disp)) +
  ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, group = gear))
```

![](man/figures/README-unnamed-chunk-2-1.png)

layouts can be specified by adding a `plot_layout()` call to the assemble. This lets you define the dimensions of the grid and how much space to allocate to the different rows and columns

``` r
p1 + p2 + plot_layout(ncol = 1, heights = c(3, 1))
```

![](man/figures/README-unnamed-chunk-3-1.png)

If you need to add a bit of space between your plots you can use `plot_spacer()` to fill a cell in the grid with nothing

``` r
p1 + plot_spacer() + p2
```

![](man/figures/README-unnamed-chunk-4-1.png)

This is all it does for now, but stay tuned as more functionality is added, such as collapsing guides, and nesting plots...