
# aplot 0.0.5

+ remove margin in all plots (2020-07-22, Wed)
+ bug fixed of using `insert_*` with `scale_*_discret(limits="")`. (2020-07-03, Fri)
  - <https://github.com/YuLab-SMU/aplot/issues/2>
  - <https://github.com/YuLab-SMU/aplot/pull/3>
+ use `ggplot() + theme_void()` instead of `patchwork::plot_spacer()` for empty plot (2020-04-15, Wed)

# aplot 0.0.4

+ export `xrange` and `yrange` (2020-04-07, Tue)
+ `plot_list` (2020-03-31, Tue)

# aplot 0.0.3

+ CRAN release
  + xlim2()
  + ylim2()
  + insert_left()
  + insert_right()
  + insert_top()
  + insert_bottom()
  + user can use `ggsave` to export the `aplot` object to a figure (e.g. png, pdf)

