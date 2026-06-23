##' Set panel spacing for an 'aplot' object
##'
##' Control the spacing between aligned panels when an 'aplot' object is
##' rendered. Numeric values are interpreted in millimeters and represent the
##' gap between neighboring panels. Use `"asis"` to preserve the subplot
##' margins already defined in each plot.
##' @title set_panel_spacing
##' @param x an 'aplot' object
##' @param spacing a non-negative numeric value in millimeters, or `"asis"`
##' @return an 'aplot' object
##' @export
set_panel_spacing <- function(x, spacing = 0) {
    if (!inherits(x, "aplot")) {
        stop("'x' should be an 'aplot' object.")
    }
    x$spacing <- .validate_panel_spacing(spacing)
    x
}

.validate_panel_spacing <- function(spacing, allow_null = FALSE) {
    if (is.null(spacing)) {
        if (allow_null) {
            return(NULL)
        }
        stop("'spacing' should be a non-negative numeric value or 'asis'.")
    }

    if (is.character(spacing)) {
        if (length(spacing) == 1 && identical(spacing, "asis")) {
            return(spacing)
        }
        stop("'spacing' should be a non-negative numeric value or 'asis'.")
    }

    if (!is.numeric(spacing) || length(spacing) != 1 || is.na(spacing) ||
            spacing < 0) {
        stop("'spacing' should be a non-negative numeric value or 'asis'.")
    }

    spacing
}

.resolve_panel_spacing <- function(x) {
    if (!is.null(x$spacing)) {
        return(list(
            mode = "object",
            spacing = .validate_panel_spacing(x$spacing)
        ))
    }

    list(
        mode = "legacy",
        spacing = .validate_panel_spacing(
            getOption("space.between.plots", default = 0),
            allow_null = TRUE
        )
    )
}

.compute_panel_margins <- function(layout, spacing) {
    ids <- sort(unique(stats::na.omit(as.vector(layout))))
    margins <- vector("list", max(ids))

    if (identical(spacing, "asis")) {
        return(margins)
    }

    half_spacing <- spacing / 2
    nrows <- nrow(layout)
    ncols <- ncol(layout)

    for (id in ids) {
        pos <- which(layout == id, arr.ind = TRUE)
        row <- pos[1, 1]
        col <- pos[1, 2]

        top <- if (row > 1 && !is.na(layout[row - 1, col])) half_spacing else 0
        right <- if (col < ncols && !is.na(layout[row, col + 1])) half_spacing else 0
        bottom <- if (row < nrows && !is.na(layout[row + 1, col])) half_spacing else 0
        left <- if (col > 1 && !is.na(layout[row, col - 1])) half_spacing else 0

        margins[[id]] <- ggplot2::margin(top, right, bottom, left, unit = "mm")
    }

    margins
}

.apply_panel_spacing <- function(plotlist, layout, spacing) {
    if (identical(spacing, "asis")) {
        return(plotlist)
    }

    margins <- .compute_panel_margins(layout, spacing)
    ids <- sort(unique(stats::na.omit(as.vector(layout))))

    for (id in ids) {
        plotlist[[id]] <- plotlist[[id]] +
            ggplot2::theme(plot.margin = margins[[id]])
    }

    plotlist
}

.compose_plotlist <- function(plotlist) {
    pp <- plotlist[[1]]
    for (i in 2:length(plotlist)) {
        pp <- pp + plotlist[[i]]
    }
    pp
}

.compose_plotlist_legacy <- function(plotlist, spacing) {
    if (inherits(spacing, "character")) {
        if (spacing == "asis") {
            theme_margin <- ggplot2::theme()
        } else {
            stop("invalid 'space.between.plots' setting.")
        }
    } else {
        theme_margin <- theme_no_margin()
        if (spacing != 0) {
            theme_margin$plot.margin <- do.call(
                ggplot2::margin,
                c(rep(list(spacing), 4), unit = "mm")
            )
        }
    }

    pp <- plotlist[[1]] + theme_margin
    for (i in 2:length(plotlist)) {
        pp <- pp + (plotlist[[i]] + theme_no_margin())
    }
    pp
}
