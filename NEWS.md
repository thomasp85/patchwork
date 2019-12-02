# patchwork (development version)

* Renaming of `align_plots()` to `align_patches()` to avoid namespace clash
  with cowplot (#130)
* Renaming of `as_grob()` (unexported) to `as_patch()` to avoid potential 
  future namespace clash with cowplot (#131)
* Fix bug in plot simplification with `theme(strip.placement = 'outside')` 
  (#132)

# patchwork 1.0.0

* First CRAN release. Provide utility and operators for assembling and nesting
  plots into a composition, tag subplots, collect guides and remove duplicates,
  and align plots across pages.
