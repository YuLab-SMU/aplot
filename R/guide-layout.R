##' Set collected guide layout on an 'aplot' object
##'
##' Control collected guides at two semantic levels when an 'aplot' object is
##' rendered. Use [set_guide_area()] to decide where guides should go, and use
##' `set_guide_layout()` to decide how they should be arranged once placed.
##' @title set_guide_layout
##' @param x an 'aplot' object
##' @param legend_ncol number of columns used inside each individual legend.
##'   Must be a positive integer. If `NULL`, the internal column layout of each
##'   legend is left unchanged.
##' @param legend_byrow logical; whether keys inside each individual legend
##'   should be filled row-wise when a multi-column legend is created. The
##'   default is `FALSE`.
##' @param legend_title_position title position used inside each individual
##'   legend. Must be one of `"top"`, `"bottom"`, `"left"`, or `"right"`.
##'   If `NULL`, the current title position is left unchanged.
##' @param legend_direction direction used inside each individual legend. Must
##'   be one of `"horizontal"` or `"vertical"`. If `NULL`, the current legend
##'   direction is left unchanged.
##' @param legend_keywidth,legend_keyheight legend key size in millimeters for
##'   each individual legend. These values are converted to
##'   `grid::unit(..., "mm")` and passed to [ggplot2::guide_legend()]. If
##'   `NULL`, the corresponding key dimension is left unchanged.
##' @param legend_spacing_x,legend_spacing_y spacing between legend items in
##'   millimeters inside each individual legend. These values are converted to
##'   `grid::unit(..., "mm")` and applied through [ggplot2::theme()] as
##'   `legend.spacing.x` and `legend.spacing.y`. If `NULL`, the corresponding
##'   spacing is left unchanged.
##' @param guides_ncol number of columns used to arrange multiple collected
##'   legend boxes as a group. Must be a positive integer. If `NULL`, the group
##'   layout stays in a single strip determined by `guides_direction`.
##' @param guides_byrow logical; whether collected legend boxes should be filled
##'   row-wise when a wrapped guide group is created. The default is `FALSE`.
##' @param guides_direction overall direction used to arrange multiple collected
##'   legend boxes. Must be one of `"horizontal"` or `"vertical"`. If `NULL`,
##'   the default follows the guide placement side used by [set_guide_area()].
##' @importFrom utils getFromNamespace
##' @return an 'aplot' object
##' @export
set_guide_layout <- function(x,
                             legend_ncol = NULL,
                             legend_byrow = FALSE,
                             legend_title_position = NULL,
                             legend_direction = NULL,
                             legend_keywidth = NULL,
                             legend_keyheight = NULL,
                             legend_spacing_x = NULL,
                             legend_spacing_y = NULL,
                             guides_ncol = NULL,
                             guides_byrow = FALSE,
                             guides_direction = NULL) {
    if (!inherits(x, "aplot")) {
        stop("'x' should be an 'aplot' object.")
    }

    x$guide_layout <- .validate_guide_layout(
        legend = list(
            ncol = legend_ncol,
            byrow = legend_byrow,
            title.position = legend_title_position,
            direction = legend_direction,
            keywidth = legend_keywidth,
            keyheight = legend_keyheight,
            spacing.x = legend_spacing_x,
            spacing.y = legend_spacing_y
        ),
        guides = list(
            ncol = guides_ncol,
            byrow = guides_byrow,
            direction = guides_direction
        )
    )
    x
}

.validate_guide_layout <- function(legend = NULL, guides = NULL) {
    legend <- .validate_legend_layout(legend)
    guides <- .validate_guides_layout(guides)

    if (is.null(legend) && is.null(guides)) {
        return(NULL)
    }

    list(legend = legend, guides = guides)
}

.validate_legend_layout <- function(legend = NULL) {
    if (is.null(legend)) {
        return(NULL)
    }

    legend$byrow <- .validate_guide_layout_logical("legend_byrow", legend$byrow)
    legend$ncol <- .validate_guide_layout_numeric(
        name = "legend_ncol",
        value = legend$ncol,
        integer = TRUE
    )
    legend$title.position <- .validate_guide_layout_choice(
        name = "legend_title_position",
        value = legend$title.position,
        choices = c("top", "bottom", "left", "right")
    )
    legend$direction <- .validate_guide_layout_choice(
        name = "legend_direction",
        value = legend$direction,
        choices = c("horizontal", "vertical")
    )
    legend$keywidth <- .validate_guide_layout_numeric(
        "legend_keywidth",
        legend$keywidth
    )
    legend$keyheight <- .validate_guide_layout_numeric(
        "legend_keyheight",
        legend$keyheight
    )
    legend$spacing.x <- .validate_guide_layout_numeric(
        "legend_spacing_x",
        legend$spacing.x
    )
    legend$spacing.y <- .validate_guide_layout_numeric(
        "legend_spacing_y",
        legend$spacing.y
    )

    if (.is_empty_legend_layout(legend)) {
        return(NULL)
    }

    legend
}

.validate_guides_layout <- function(guides = NULL) {
    if (is.null(guides)) {
        return(NULL)
    }

    guides$byrow <- .validate_guide_layout_logical("guides_byrow", guides$byrow)
    guides$ncol <- .validate_guide_layout_numeric(
        name = "guides_ncol",
        value = guides$ncol,
        integer = TRUE
    )
    guides$direction <- .validate_guide_layout_choice(
        name = "guides_direction",
        value = guides$direction,
        choices = c("horizontal", "vertical")
    )

    if (.is_empty_guides_layout(guides)) {
        return(NULL)
    }

    guides
}

.validate_guide_layout_choice <- function(name, value, choices) {
    if (is.null(value)) {
        return(NULL)
    }

    if (!is.character(value) || length(value) != 1 || is.na(value) ||
            !value %in% choices) {
        stop(sprintf(
            "'%s' should be one of %s.",
            name,
            paste(sprintf("'%s'", choices), collapse = ", ")
        ))
    }

    value
}

.validate_guide_layout_numeric <- function(name, value, integer = FALSE) {
    if (is.null(value)) {
        return(NULL)
    }

    if (!is.numeric(value) || length(value) != 1 || is.na(value) || value < 0) {
        stop(sprintf("'%s' should be a non-negative numeric value.", name))
    }

    if (integer && value != as.integer(value)) {
        stop(sprintf("'%s' should be a positive integer value.", name))
    }

    if (integer && value < 1) {
        stop(sprintf("'%s' should be a positive integer value.", name))
    }

    value
}

.validate_guide_layout_logical <- function(name, value) {
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
        stop(sprintf("'%s' should be a single logical value.", name))
    }

    value
}

.is_empty_legend_layout <- function(legend) {
    is.null(legend$ncol) &&
        !legend$byrow &&
        is.null(legend$title.position) &&
        is.null(legend$direction) &&
        is.null(legend$keywidth) &&
        is.null(legend$keyheight) &&
        is.null(legend$spacing.x) &&
        is.null(legend$spacing.y)
}

.is_empty_guides_layout <- function(guides) {
    is.null(guides$ncol) &&
        !guides$byrow &&
        is.null(guides$direction)
}

.resolve_guide_layout <- function(x) {
    if (is.null(x$guide_layout)) {
        return(NULL)
    }

    .validate_guide_layout(
        legend = x$guide_layout$legend,
        guides = x$guide_layout$guides
    )
}

.apply_guide_layout <- function(plotlist, guide_layout) {
    if (is.null(guide_layout)) {
        return(plotlist)
    }

    .apply_legend_layout(plotlist, guide_layout$legend)
}

.apply_legend_layout <- function(plotlist, legend_layout) {
    if (is.null(legend_layout)) {
        return(plotlist)
    }

    theme_args <- list()
    if (!is.null(legend_layout$spacing.x)) {
        theme_args$legend.spacing.x <- grid::unit(legend_layout$spacing.x, "mm")
    }
    if (!is.null(legend_layout$spacing.y)) {
        theme_args$legend.spacing.y <- grid::unit(legend_layout$spacing.y, "mm")
    }

    guide_args <- list(byrow = legend_layout$byrow)
    if (!is.null(legend_layout$ncol)) {
        guide_args$ncol <- legend_layout$ncol
    }
    if (!is.null(legend_layout$title.position)) {
        guide_args$title.position <- legend_layout$title.position
    }
    if (!is.null(legend_layout$direction)) {
        guide_args$direction <- legend_layout$direction
    }
    if (!is.null(legend_layout$keywidth)) {
        guide_args$keywidth <- grid::unit(legend_layout$keywidth, "mm")
    }
    if (!is.null(legend_layout$keyheight)) {
        guide_args$keyheight <- grid::unit(legend_layout$keyheight, "mm")
    }

    for (i in seq_along(plotlist)) {
        targets <- .guide_layout_targets(plotlist[[i]])

        if (length(targets) > 0) {
            guide <- do.call(ggplot2::guide_legend, guide_args)
            plotlist[[i]] <- plotlist[[i]] + do.call(
                ggplot2::guides,
                stats::setNames(rep(list(guide), length(targets)), targets)
            )
        }

        if (length(theme_args) > 0) {
            plotlist[[i]] <- plotlist[[i]] + do.call(ggplot2::theme, theme_args)
        }
    }

    plotlist
}

.has_guides_layout <- function(guide_layout) {
    !is.null(guide_layout) && !is.null(guide_layout$guides)
}

.guide_layout_box_position <- function(guide_area_spec) {
    if (is.null(guide_area_spec)) {
        return("right")
    }

    position <- guide_area_spec$position
    if (position %in% c("top-right", "bottom-right", "right")) {
        return("right")
    }
    if (position %in% c("top-left", "bottom-left", "left")) {
        return("left")
    }
    if (identical(position, "top")) {
        return("top")
    }
    if (identical(position, "bottom")) {
        return("bottom")
    }

    "right"
}

.collect_plot_guides <- function(plotlist) {
    guide_list <- list()

    for (plot in plotlist) {
        gt <- ggplot2::ggplotGrob(plot)
        guide_idx <- grepl("^guide-box", gt$layout$name)
        if (!any(guide_idx)) {
            next
        }

        guide_list <- c(
            guide_list,
            unlist(lapply(gt$grobs[guide_idx], .extract_plot_guides), recursive = FALSE)
        )
    }

    guide_list
}

.extract_plot_guides <- function(grob) {
    if (inherits(grob, "zeroGrob")) {
        return(list())
    }

    if (!inherits(grob, "gtable")) {
        return(list(grob))
    }

    child_idx <- grepl("^guides", grob$layout$name)
    if (any(child_idx)) {
        return(unlist(
            lapply(grob$grobs[child_idx], .extract_plot_guides),
            recursive = FALSE
        ))
    }

    list(grob)
}

.suppress_plot_guides <- function(plotlist) {
    lapply(plotlist, function(plot) {
        plot + ggplot2::theme(legend.position = "none")
    })
}

.collapse_plot_guides <- function(guides) {
    if (length(guides) < 2) {
        return(guides)
    }

    collapse_guides <- getFromNamespace("collapse_guides", "patchwork")
    collapse_guides(guides)
}

.apply_guides_layout <- function(collected_guides,
                                 guides_layout,
                                 guide_position = "right",
                                 theme = ggplot2::theme_get()) {
    if (length(collected_guides) == 0) {
        return(NULL)
    }

    if (is.null(guides_layout)) {
        assemble_guides <- getFromNamespace("assemble_guides", "patchwork")
        return(assemble_guides(collected_guides, guide_position, theme))
    }

    theme <- .complete_guides_theme(
        guide_position = guide_position,
        theme = theme,
        guides_layout = guides_layout
    )
    layout <- .guides_layout_matrix(length(collected_guides), guides_layout)
    .build_guides_gtable(collected_guides, layout, theme)
}

.complete_guides_theme <- function(guide_position, theme, guides_layout) {
    if (is.null(theme$legend.box)) {
        if (!is.null(guides_layout$direction)) {
            theme$legend.box <- guides_layout$direction
        } else if (guide_position %in% c("top", "bottom")) {
            theme$legend.box <- "horizontal"
        } else {
            theme$legend.box <- "vertical"
        }
    }

    theme
}

.guides_layout_matrix <- function(n, guides_layout) {
    direction <- guides_layout$direction
    if (is.null(direction)) {
        direction <- "vertical"
    }

    ncol <- guides_layout$ncol
    if (is.null(ncol)) {
        ncol <- if (identical(direction, "horizontal")) n else 1
    }
    ncol <- min(ncol, n)
    nrow <- ceiling(n / ncol)

    fill_byrow <- guides_layout$byrow || identical(direction, "horizontal")
    layout <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol, byrow = fill_byrow)
    positions <- lapply(seq_len(n), function(i) which(layout == i, arr.ind = TRUE)[1, ])

    list(
        nrow = nrow,
        ncol = ncol,
        row = vapply(positions, `[[`, integer(1), 1),
        col = vapply(positions, `[[`, integer(1), 2)
    )
}

.build_guides_gtable <- function(guides, layout, theme) {
    spacing_x <- ggplot2::calc_element("legend.spacing.x", theme)
    spacing_y <- ggplot2::calc_element("legend.spacing.y", theme)
    box_margin <- ggplot2::calc_element("legend.box.margin", theme)
    if (is.null(box_margin)) {
        box_margin <- ggplot2::margin(0, 0, 0, 0)
    }

    col_widths <- vector("list", layout$ncol)
    row_heights <- vector("list", layout$nrow)
    for (i in seq_along(guides)) {
        col <- layout$col[[i]]
        row <- layout$row[[i]]

        width <- gtable::gtable_width(guides[[i]])
        height <- gtable::gtable_height(guides[[i]])

        if (is.null(col_widths[[col]])) {
            col_widths[[col]] <- width
        } else {
            col_widths[[col]] <- grid::unit.pmax(col_widths[[col]], width)
        }

        if (is.null(row_heights[[row]])) {
            row_heights[[row]] <- height
        } else {
            row_heights[[row]] <- grid::unit.pmax(row_heights[[row]], height)
        }
    }

    col_widths <- do.call(grid::unit.c, lapply(col_widths, function(x) {
        if (is.null(x)) {
            return(grid::unit(0, "mm"))
        }
        x
    }))
    row_heights <- do.call(grid::unit.c, lapply(row_heights, function(x) {
        if (is.null(x)) {
            return(grid::unit(0, "mm"))
        }
        x
    }))

    widths <- .interleave_guide_units(col_widths, spacing_x)
    heights <- .interleave_guide_units(row_heights, spacing_y)
    widths <- grid::unit.c(box_margin[4], widths, box_margin[2])
    heights <- grid::unit.c(box_margin[1], heights, box_margin[3])

    guide_box <- gtable::gtable(widths, heights, name = "guide-box")
    guide_box <- gtable::gtable_add_grob(
        guide_box,
        guides,
        t = 1 + (layout$row * 2 - 1),
        l = 1 + (layout$col * 2 - 1),
        name = rep("guides", length(guides))
    )
    guide_box <- gtable::gtable_add_grob(
        guide_box,
        ggplot2::element_render(theme, "legend.box.background"),
        t = 1,
        l = 1,
        b = -1,
        r = -1,
        z = -Inf,
        clip = "off",
        name = "legend.box.background"
    )

    guide_box
}

.interleave_guide_units <- function(values, spacing) {
    if (length(values) <= 1) {
        return(values)
    }

    out <- grid::unit(rep_len(0, length(values) * 2 - 1), "mm")
    out[seq(1, length(out), by = 2)] <- values
    out[seq(2, length(out), by = 2)] <- rep(spacing, length(values) - 1)
    out
}

.guide_layout_targets <- function(plot) {
    built <- suppressMessages(ggplot2::ggplot_build(plot))
    scales <- built$plot$scales$scales

    if (length(scales) == 0) {
        return(character())
    }

    aesthetics <- unique(unlist(lapply(scales, function(scale) {
        if (.is_legend_guide(scale$guide)) {
            return(scale$aesthetics)
        }
        NULL
    })))

    guideable <- c(
        "alpha", "colour", "color", "fill", "linetype",
        "linewidth", "shape", "size", "stroke"
    )
    aesthetics[aesthetics %in% guideable]
}

.is_legend_guide <- function(guide) {
    if (inherits(guide, "waiver")) {
        return(TRUE)
    }

    if (is.character(guide)) {
        return(identical(guide, "legend"))
    }

    any(grepl("legend", class(guide), ignore.case = TRUE))
}
