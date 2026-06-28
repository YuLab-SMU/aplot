test_that("set_guide_area stores a guide region on aplot objects", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_top <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
        ggplot2::geom_density()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(x = 1, y = disp)) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.3) |>
        aplot::insert_right(p_right, width = 0.2)

    ap2 <- aplot::set_guide_area(ap, "top-right")
    resolved <- aplot:::.resolve_guide_area(ap2)

    expect_identical(ap2$guide_area$position, "top-right")
    expect_identical(resolved$mode, "corner")
    expect_identical(resolved$position, "top-right")
    expect_equal(resolved$row, 1L)
    expect_equal(resolved$col, 2L)
    expect_equal(resolved$rows, 1)
    expect_equal(resolved$cols, 2)
    expect_equal(resolved$guide_indices, 3)
    expect_equal(resolved$region_indices, 3)
})

test_that("set_guide_area rejects invalid inputs", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_top <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
        ggplot2::geom_density()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(x = 1, y = disp)) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.3) |>
        aplot::insert_right(p_right, width = 0.2)

    expect_error(aplot::set_guide_area(p_main, "top-right"), "'x' should be an 'aplot' object.")
    expect_error(aplot::set_guide_area(ap, "middle"), "'position' should be one of")
    expect_error(aplot::set_guide_area(ap, NA_character_), "'position' should be one of")
})

test_that("set_guide_area requires an empty corner region", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_top <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
        ggplot2::geom_density()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(x = 1, y = disp)) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.3) |>
        aplot::insert_right(p_right, width = 0.2)

    expect_error(aplot:::.resolve_guide_area(aplot::set_guide_area(ap, "top-left")),
                 "The selected corner does not contain an empty guide region.")
})

test_that("set_guide_area supports automatic right-side guide region", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(x = 1, y = disp)) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_right(p_right, width = 0.2)

    ap2 <- aplot::set_guide_area(ap, "right")
    resolved <- aplot:::.resolve_guide_area(ap2)

    expect_identical(ap2$guide_area$position, "right")
    expect_identical(resolved$mode, "side")
    expect_identical(resolved$position, "right")
    expect_identical(resolved$width, 0.2)
    expect_equal(dim(resolved$layout), c(1, 3))
    expect_equal(resolved$guide_indices, 3)
    expect_equal(resolved$region_indices, 3)
})

test_that("set_guide_area anchors right-side guide region on the main row", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_top <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
        ggplot2::geom_density()
    p_right <- ggplot2::ggplot(mtcars, ggplot2::aes(x = 1, y = disp)) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.3) |>
        aplot::insert_right(p_right, width = 0.2)

    resolved <- aplot:::.resolve_guide_area(
        aplot::set_guide_area(ap, "right")
    )

    expect_equal(dim(resolved$layout), c(2, 3))
    expect_equal(resolved$guide_indices, 6)
    expect_equal(resolved$region_indices, c(5, 6))
})

test_that("set_guide_area supports automatic left-side guide region", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_left <- ggplot2::ggplot(mtcars, ggplot2::aes(y = disp, x = factor(cyl))) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_left(p_left, width = 0.2)

    ap2 <- aplot::set_guide_area(ap, "left")
    resolved <- aplot:::.resolve_guide_area(ap2)

    expect_identical(ap2$guide_area$position, "left")
    expect_identical(resolved$mode, "side")
    expect_identical(resolved$position, "left")
    expect_identical(resolved$width, 0.2)
    expect_equal(dim(resolved$layout), c(1, 3))
    expect_equal(resolved$guide_indices, 1)
})

test_that("set_guide_area supports automatic bottom guide region", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_top <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
        ggplot2::geom_density()

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.2)

    ap2 <- aplot::set_guide_area(ap, "bottom")
    resolved <- aplot:::.resolve_guide_area(ap2)

    expect_identical(ap2$guide_area$position, "bottom")
    expect_identical(resolved$mode, "side")
    expect_identical(resolved$position, "bottom")
    expect_identical(resolved$height, 0.2)
    expect_equal(dim(resolved$layout), c(3, 1))
    expect_equal(resolved$guide_indices, 3)
})

test_that("set_guide_area supports automatic top guide region", {
    p_main <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    p_bottom <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
        ggplot2::geom_density()

    ap <- p_main |>
        aplot::insert_bottom(p_bottom, height = 0.2)

    ap2 <- aplot::set_guide_area(ap, "top")
    resolved <- aplot:::.resolve_guide_area(ap2)

    expect_identical(ap2$guide_area$position, "top")
    expect_identical(resolved$mode, "side")
    expect_identical(resolved$position, "top")
    expect_identical(resolved$height, 0.2)
    expect_equal(dim(resolved$layout), c(3, 1))
    expect_equal(resolved$guide_indices, 1)
})

test_that("guide area rendering supports collected guides", {
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
    p_right <- ggplot2::ggplot(
        mtcars,
        ggplot2::aes(x = factor(cyl), y = disp, fill = factor(cyl))
    ) +
        ggplot2::geom_boxplot()

    ap <- p_main |>
        aplot::insert_top(p_top, height = 0.3) |>
        aplot::insert_right(p_right, width = 0.25) |>
        aplot::set_guide_area("top-right")

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
