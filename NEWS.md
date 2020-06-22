# patchwork 1.0.1

* Renaming of `align_plots()` to `align_patches()` to avoid namespace clash
  with cowplot (#130)
* Renaming of `as_grob()` (unexported) to `as_patch()` to avoid potential 
  future namespace clash with cowplot (#131)
* Fix bug in plot simplification with `theme(strip.placement = 'outside')` 
  (#132)
* Fix a bug in guide collection in R >= 4.0 due to the new unit implementation
  in grid (#170)
* Collected guides now behave as ggplot2 guides when position is top or bottom
  (#137)
* Fix a bug in base graphic support where the environment of the plot was not
  captured (#138)
* Fix a bug when combining plots having guides placed manually in combination 
  with faceting (#144)
* Fix a bug where having negative margins around the legend would result in an
  unintelligeble error (#148)
* Fix a bug when trying to combine faceted plots with fixed aspect ratio (#156)
* Fix alignments of strips when only a single strip is present (#163)
* Fix a bug that caused theme void to result in errors (#180)
* Make aligning multiple fixed aspect plots more consistent (#175)
* Correct alignment of guides when ssembling fixed aspect plots (#140, 
  @ilia-kats)

# patchwork 1.0.0

* First CRAN release. Provide utility and operators for assembling and nesting
  plots into a composition, tag subplots, collect guides and remove duplicates,
  and align plots across pages.
