##' Set a guide area on an 'aplot' object
##'
##' Designate a guide placement region in an 'aplot' layout for collected
##' guides. Corner positions use the continuous empty region anchored at the
##' requested corner. Side positions create a guide region on the requested side.
##' The selected guide area remains empty when guides are not collected.
##' @title set_guide_area
##' @param x an 'aplot' object
##' @param position placement of collected guides. Supported values are
##'   `"top-right"`, `"top-left"`, `"bottom-right"`, `"bottom-left"`, `"right"`,
##'   `"left"`, `"bottom"`, and `"top"`.
##'   Corner positions use the largest continuous empty rectangular region
##'   anchored at the requested corner. Side positions create a single guide
##'   region on the requested side, where the guide box is placed as one object
##'   rather than as repeated guide cells.
##' @return an 'aplot' object
##' @export
set_guide_area <- function(x, position = c(
    "top-right",
    "top-left",
    "bottom-right",
    "bottom-left",
    "right",
    "left",
    "bottom",
    "top"
)) {
    if (!inherits(x, "aplot")) {
        stop("'x' should be an 'aplot' object.")
    }

    x$guide_area <- list(position = .validate_guide_area_position(position))
    .resolve_guide_area(x)
    x
}

.validate_guide_area_position <- function(position) {
    choices <- c("top-right", "top-left", "bottom-right", "bottom-left", "right", "left", "bottom", "top")

    if (!is.character(position) || length(position) != 1 || is.na(position)) {
        stop("'position' should be one of 'top-right', 'top-left', 'bottom-right', 'bottom-left', 'right', 'left', 'bottom', or 'top'.")
    }

    if (!position %in% choices) {
        stop("'position' should be one of 'top-right', 'top-left', 'bottom-right', 'bottom-left', 'right', 'left', 'bottom', or 'top'.")
    }

    position
}

.guide_area_strip_width <- function() {
    0.2
}

.guide_area_strip_height <- function() {
    0.2
}

.resolve_guide_area <- function(x) {
    if (is.null(x$guide_area)) {
        return(NULL)
    }

    layout <- x$layout

    if (!is.matrix(layout) || length(layout) == 0) {
        stop("The selected guide area cannot be resolved from an empty layout.")
    }

    position <- .validate_guide_area_position(x$guide_area$position)

    if (identical(position, "right")) {
        return(.resolve_guide_area_right_strip(layout, x$main_row))
    }

    if (identical(position, "left")) {
        return(.resolve_guide_area_left_strip(layout, x$main_row))
    }

    if (identical(position, "bottom")) {
        return(.resolve_guide_area_bottom_strip(layout, x$main_col))
    }

    if (identical(position, "top")) {
        return(.resolve_guide_area_top_strip(layout, x$main_col))
    }

    .resolve_guide_area_corner_region(layout, position)
}

.resolve_guide_area_corner_region <- function(layout, position) {
    region <- .corner_empty_region(layout, position)

    if (is.null(region)) {
        stop("The selected corner does not contain an empty guide region.")
    }

    list(
        mode = "corner",
        position = position,
        row = region$rows[[1]],
        col = region$cols[[1]],
        rows = region$rows,
        cols = region$cols,
        layout = layout,
        guide_indices = .guide_area_indices(
            layout,
            .guide_area_anchor(region$rows),
            .guide_area_anchor(region$cols)
        ),
        region_indices = .guide_area_indices(layout, region$rows, region$cols)
    )
}

.corner_empty_region <- function(layout, position) {
    nrows <- nrow(layout)
    ncols <- ncol(layout)

    row_order <- if (grepl("^top", position)) seq_len(nrows) else nrows:1
    col_order <- if (grepl("left$", position)) seq_len(ncols) else ncols:1

    if (!is.na(layout[row_order[[1]], col_order[[1]]])) {
        return(NULL)
    }

    row_extent <- .corner_empty_extent(layout, row_order, col_order[[1]], "row")
    col_extent <- .corner_empty_extent(layout, col_order, row_order[[1]], "col")

    if (row_extent == 0 || col_extent == 0) {
        return(NULL)
    }

    rows <- row_order[seq_len(row_extent)]
    cols <- col_order[seq_len(col_extent)]

    if (!all(is.na(layout[rows, cols, drop = FALSE]))) {
        return(NULL)
    }

    list(rows = sort(rows), cols = sort(cols))
}

.corner_empty_extent <- function(layout, extent_order, fixed, margin) {
    count <- 0
    for (i in extent_order) {
        value <- if (identical(margin, "row")) layout[i, fixed] else layout[fixed, i]
        if (!is.na(value)) {
            break
        }
        count <- count + 1
    }

    count
}

.guide_area_indices <- function(layout, rows, cols) {
    as.vector(outer(rows, cols, function(row, col) {
        row + (col - 1) * nrow(layout)
    }))
}

.guide_area_anchor <- function(x) {
    x[[ceiling(length(x) / 2)]]
}

.resolve_guide_area_right_strip <- function(layout, anchor_row) {
    nrows <- nrow(layout)
    extended_layout <- cbind(layout, rep(NA_real_, nrows))
    cols <- ncol(extended_layout)
    rows <- seq_len(nrows)

    list(
        mode = "side",
        position = "right",
        width = .guide_area_strip_width(),
        rows = rows,
        cols = cols,
        layout = extended_layout,
        guide_indices = .guide_area_indices(extended_layout, anchor_row, cols),
        region_indices = .guide_area_indices(extended_layout, rows, cols)
    )
}

.resolve_guide_area_left_strip <- function(layout, anchor_row) {
    nrows <- nrow(layout)
    extended_layout <- cbind(rep(NA_real_, nrows), layout)
    rows <- seq_len(nrows)
    cols <- 1

    list(
        mode = "side",
        position = "left",
        width = .guide_area_strip_width(),
        rows = rows,
        cols = cols,
        layout = extended_layout,
        guide_indices = .guide_area_indices(extended_layout, anchor_row, cols),
        region_indices = .guide_area_indices(extended_layout, rows, cols)
    )
}

.resolve_guide_area_bottom_strip <- function(layout, anchor_col) {
    ncols <- ncol(layout)
    extended_layout <- rbind(layout, rep(NA_real_, ncols))
    rows <- nrow(extended_layout)
    cols <- seq_len(ncols)

    list(
        mode = "side",
        position = "bottom",
        height = .guide_area_strip_height(),
        rows = rows,
        cols = cols,
        layout = extended_layout,
        guide_indices = .guide_area_indices(extended_layout, rows, anchor_col),
        region_indices = .guide_area_indices(extended_layout, rows, cols)
    )
}

.resolve_guide_area_top_strip <- function(layout, anchor_col) {
    ncols <- ncol(layout)
    extended_layout <- rbind(rep(NA_real_, ncols), layout)
    rows <- 1
    cols <- seq_len(ncols)

    list(
        mode = "side",
        position = "top",
        height = .guide_area_strip_height(),
        rows = rows,
        cols = cols,
        layout = extended_layout,
        guide_indices = .guide_area_indices(extended_layout, rows, anchor_col),
        region_indices = .guide_area_indices(extended_layout, rows, cols)
    )
}
