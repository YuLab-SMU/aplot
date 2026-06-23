test_that("set_panel_spacing stores validated spacing on aplot objects", {
    p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()

    ap <- insert_right(p1, p2)
    ap2 <- set_panel_spacing(ap, 2)

    expect_identical(ap2$spacing, 2)
    expect_error(set_panel_spacing(ap, -1), "non-negative numeric value or 'asis'")
    expect_error(set_panel_spacing(ap, "bad"), "non-negative numeric value or 'asis'")
    expect_error(set_panel_spacing(p1, 2), "'x' should be an 'aplot' object.")
})

test_that("panel margins are split across shared edges", {
    layout <- matrix(c(1, 2), nrow = 1)
    margins <- aplot:::.compute_panel_margins(layout, 2)

    expect_equal(as.numeric(margins[[1]]), c(0, 1, 0, 0))
    expect_equal(as.numeric(margins[[2]]), c(0, 0, 0, 1))
})

test_that("object spacing preserves plot margins when using 'asis'", {
    p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point() +
        ggplot2::theme(plot.margin = ggplot2::margin(1, 2, 3, 4))
    p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point() +
        ggplot2::theme(plot.margin = ggplot2::margin(4, 3, 2, 1))

    plotlist <- list(p1, p2)
    updated <- aplot:::.apply_panel_spacing(
        plotlist = plotlist,
        layout = matrix(c(1, 2), nrow = 1),
        spacing = "asis"
    )

    expect_identical(
        as.numeric(updated[[1]]$theme$plot.margin),
        as.numeric(p1$theme$plot.margin)
    )
    expect_identical(
        as.numeric(updated[[2]]$theme$plot.margin),
        as.numeric(p2$theme$plot.margin)
    )
})

test_that("object spacing takes precedence over legacy option", {
    old_spacing <- getOption("space.between.plots")
    on.exit(options(space.between.plots = old_spacing), add = TRUE)
    options(space.between.plots = 0)

    p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()

    ap <- insert_right(p1, p2)
    resolved <- aplot:::.resolve_panel_spacing(set_panel_spacing(ap, 2))

    expect_identical(resolved$mode, "object")
    expect_identical(resolved$spacing, 2)
})
