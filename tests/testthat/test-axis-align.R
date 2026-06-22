test_that("xlim2 preserves discrete panel range with nudged geoms", {
    skip_if_not_installed("ggplot2")

    expr <- c(
        0.10, 0.20, 0.30, 0.40, 0.50,
        0.15, 0.25, 0.35, 0.45, 0.55,
        0.12, 0.22, 0.32, 0.42, 0.52,
        0.18, 0.28, 0.38, 0.48, 0.58,
        0.14, 0.24, 0.34, 0.44, 0.54
    )
    heat_data <- expand.grid(
        condition = paste0("t", 1:5),
        gene = paste0("g", 1:5)
    )
    heat_data$expr <- expr

    fig_heat <- ggplot2::ggplot() +
        ggplot2::geom_tile(
            data = heat_data,
            ggplot2::aes(condition, gene, fill = expr),
            position = ggplot2::position_nudge(x = -1)
        ) +
        ggplot2::scale_y_discrete(position = "right") +
        ggplot2::theme_minimal() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL)

    point_data <- data.frame(
        condition = paste0("t", 1:5),
        value = 2:6
    )
    fig_point <- ggplot2::ggplot() +
        ggplot2::geom_point(
            data = point_data,
            ggplot2::aes(condition, value, size = value, color = condition),
            position = ggplot2::position_nudge(x = -1)
        ) +
        ggplot2::theme_minimal() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL)

    heat_range <- ggplot2::ggplot_build(fig_heat)$layout$panel_params[[1]]$x$continuous_range
    aligned_range <- ggplot2::ggplot_build(fig_point + xlim2(fig_heat))$layout$panel_params[[1]]$x$continuous_range

    expect_identical(unname(aligned_range), unname(heat_range))
})
