test_that("gglist works with cowplot grob conversion", {
    skip_if_not_installed("cowplot")

    plots <- list(
        ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point(),
        ggplot2::ggplot(mtcars, ggplot2::aes(disp, qsec)) + ggplot2::geom_point()
    )
    x <- aplot::gglist(plots, ncol = 1)

    expect_s3_class(cowplot::as_grob(x), "gtable")

    combined <- cowplot::plot_grid(x, x, ncol = 1)
    expect_s3_class(combined, "ggplot")
    expect_no_error(ggplot2::ggplotGrob(combined))
})
