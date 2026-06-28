test_that("set_guide_layout stores validated legend and guides layout", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()

    ap <- aplot::insert_right(p_main, p_right)
    ap2 <- aplot::set_guide_layout(
        ap,
        legend_ncol = 2,
        legend_byrow = TRUE,
        legend_title_position = "top",
        legend_direction = "horizontal",
        legend_keywidth = 4,
        legend_keyheight = 3,
        legend_spacing_x = 2,
        legend_spacing_y = 1,
        guides_ncol = 2,
        guides_byrow = TRUE,
        guides_direction = "horizontal"
    )
    resolved <- aplot:::.resolve_guide_layout(ap2)

    expect_identical(resolved$legend$ncol, 2)
    expect_identical(resolved$legend$byrow, TRUE)
    expect_identical(resolved$legend$title.position, "top")
    expect_identical(resolved$legend$direction, "horizontal")
    expect_identical(resolved$legend$keywidth, 4)
    expect_identical(resolved$legend$keyheight, 3)
    expect_identical(resolved$legend$spacing.x, 2)
    expect_identical(resolved$legend$spacing.y, 1)
    expect_identical(resolved$guides$ncol, 2)
    expect_identical(resolved$guides$byrow, TRUE)
    expect_identical(resolved$guides$direction, "horizontal")
})

test_that("set_guide_layout with no options clears stored layout", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()

    ap <- aplot::insert_right(p_main, p_right) |>
        aplot::set_guide_layout(legend_ncol = 2, guides_ncol = 2)
    cleared <- aplot::set_guide_layout(ap)

    expect_null(cleared$guide_layout)
    expect_null(aplot:::.resolve_guide_layout(cleared))
})

test_that("set_guide_layout rejects invalid inputs", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()

    ap <- aplot::insert_right(p_main, p_right)

    expect_error(aplot::set_guide_layout(p_main, legend_ncol = 2), "'x' should be an 'aplot' object.")
    expect_error(aplot::set_guide_layout(ap, legend_ncol = 0), "'legend_ncol' should be a positive integer value.")
    expect_error(aplot::set_guide_layout(ap, guides_ncol = 1.5), "'guides_ncol' should be a positive integer value.")
    expect_error(aplot::set_guide_layout(ap, legend_byrow = "yes"), "'legend_byrow' should be a single logical value.")
    expect_error(aplot::set_guide_layout(ap, guides_byrow = "yes"), "'guides_byrow' should be a single logical value.")
    expect_error(aplot::set_guide_layout(ap, legend_title_position = "middle"), "'legend_title_position' should be one of")
    expect_error(aplot::set_guide_layout(ap, guides_direction = "diagonal"), "'guides_direction' should be one of")
    expect_error(aplot::set_guide_layout(ap, legend_spacing_x = -1), "'legend_spacing_x' should be a non-negative numeric value.")
})

test_that("guide layout helper is a no-op when layout is NULL", {
    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()

    updated <- aplot:::.apply_guide_layout(
        plotlist = list(p_main),
        guide_layout = NULL
    )

    expect_identical(updated[[1]], p_main)
})

test_that("legend layout helper applies compact legend settings", {
    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()

    updated <- aplot:::.apply_legend_layout(
        plotlist = list(p_main),
        legend_layout = list(
            ncol = 2,
            byrow = TRUE,
            title.position = "top",
            direction = "horizontal",
            keywidth = 4,
            keyheight = 3,
            spacing.x = 2,
            spacing.y = 1
        )
    )

    expect_identical(aplot:::.guide_layout_targets(updated[[1]]), "colour")
    expect_equal(as.numeric(updated[[1]]$theme$legend.spacing.x), 2)
    expect_equal(as.numeric(updated[[1]]$theme$legend.spacing.y), 1)
    expect_no_error(ggplot2::ggplotGrob(updated[[1]]))
})

test_that("guide layout helper identifies multiple guideable aesthetics", {
    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl), shape = factor(gear))
    ) +
        ggplot2::geom_point()

    targets <- aplot:::.guide_layout_targets(p_main)

    expect_identical(targets, c("colour", "shape"))
})

test_that("guides layout helper wraps multiple collected guide boxes", {
    p_colour <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()
    p_fill <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, fill = factor(vs))
    ) +
        ggplot2::geom_density(alpha = 0.4)

    guides <- aplot:::.collect_plot_guides(list(p_colour, p_fill))
    guide_box <- aplot:::.apply_guides_layout(
        collected_guides = aplot:::.collapse_plot_guides(guides),
        guides_layout = list(
            ncol = 2,
            byrow = FALSE,
            direction = "horizontal"
        ),
        guide_position = "right",
        theme = ggplot2::theme_get()
    )

    layout <- guide_box[["layout"]]
    guide_rows <- layout[layout$name == "guides", ]
    expect_equal(nrow(guide_rows), 2)
    expect_equal(length(unique(guide_rows$t)), 1)
    expect_equal(length(unique(guide_rows$l)), 2)
})

test_that("guide area rendering supports collected guides in guide area", {
    old_guides <- getOption("aplot_guides")
    on.exit(options(aplot_guides = old_guides), add = TRUE)
    options(aplot_guides = "collect")

    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()
    p_top <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, wt, shape = factor(gear))
    ) +
        ggplot2::geom_point()
    p_right <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(x = factor(vs), y = disp, fill = factor(vs))
    ) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.3) |>
        aplot::insert_right(p_right, width = 0.25) |>
        aplot::set_guide_area("top-right") |>
        aplot::set_guide_layout(
            legend_title_position = "top",
            legend_spacing_x = 2,
            legend_spacing_y = 1,
            guides_ncol = 2,
            guides_direction = "horizontal"
        )

    expect_s3_class(aplot::as.patchwork(ap), "patchwork")
    expect_no_error(patchwork::patchworkGrob(aplot::as.patchwork(ap)))
})

test_that("guide area rendering supports collected guides in right-side region", {
    old_guides <- getOption("aplot_guides")
    on.exit(options(aplot_guides = old_guides), add = TRUE)
    options(aplot_guides = "collect")

    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()
    p_right <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(x = factor(cyl), y = disp, fill = factor(cyl))
    ) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_right(p_right, width = 0.25) |>
        aplot::set_guide_area("right")

    rendered <- aplot::as.patchwork(ap)

    expect_s3_class(rendered, "patchwork")
    expect_no_error(patchwork::patchworkGrob(rendered))
})

test_that("guide area rendering supports collected guides in left-side region", {
    old_guides <- getOption("aplot_guides")
    on.exit(options(aplot_guides = old_guides), add = TRUE)
    options(aplot_guides = "collect")

    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()
    p_left <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(y = disp, x = factor(cyl), fill = factor(cyl))
    ) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_left(p_left, width = 0.25) |>
        aplot::set_guide_area("left")

    rendered <- aplot::as.patchwork(ap)

    expect_s3_class(rendered, "patchwork")
    expect_no_error(patchwork::patchworkGrob(rendered))
})

test_that("guide area rendering supports collected guides in bottom region", {
    old_guides <- getOption("aplot_guides")
    on.exit(options(aplot_guides = old_guides), add = TRUE)
    options(aplot_guides = "collect")

    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()
    p_top <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, fill = factor(cyl))
    ) +
        ggplot2::geom_density(alpha = 0.4)

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.25) |>
        aplot::set_guide_area("bottom")

    rendered <- aplot::as.patchwork(ap)

    expect_s3_class(rendered, "patchwork")
    expect_no_error(patchwork::patchworkGrob(rendered))
})

test_that("guide area rendering supports collected guides in top region", {
    old_guides <- getOption("aplot_guides")
    on.exit(options(aplot_guides = old_guides), add = TRUE)
    options(aplot_guides = "collect")

    p_main <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, disp, colour = factor(cyl))
    ) +
        ggplot2::geom_point()
    p_bottom <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(mpg, fill = factor(cyl))
    ) +
        ggplot2::geom_density(alpha = 0.4)

    ap <- p_main |>
        aplot::insert_bottom(p_bottom, height = 0.25) |>
        aplot::set_guide_area("top")

    rendered <- aplot::as.patchwork(ap)

    expect_s3_class(rendered, "patchwork")
    expect_no_error(patchwork::patchworkGrob(rendered))
})
