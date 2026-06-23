
as.aplot <- function(plot) {
    if (inherits(plot, "aplot"))
        return(plot)
    
    if (!inherits(plot, 'gg')) {
        stop("input should be a 'gg' object.")  
    }
    structure(list(plotlist = list(plot),
                   width = 1,
                   height = 1,
                   layout = matrix(1),
                   n = 1,
                   main_col = 1,
                   main_row = 1,
                   spacing = NULL),
              class = c("aplot", "ggplot"))
    
}


##' @method print aplot
##' @importFrom patchwork plot_layout
##' @importFrom patchwork plot_spacer
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 theme_void
##' @export
print.aplot <- function(x, ...) {
    grid.draw(x)
}

##' as.patchwork
##' 
##' @param x object
##' @param align "x","y","xy","none", align the axis of x/y or not.
##' @importFrom patchwork plot_layout
##' @importFrom ggplot2 ggplotGrob
##' @export
as.patchwork <- function(x,
                         align = getOption("aplot_align", default = "xy")
                        ) {
    
    align <- match.arg(align, c("x","y","xy","none"))
    
    if (!inherits(x, 'aplot') && !inherits(x, "gglist")) {
        stop("only aplot or gglist object supported")
    }
    if (inherits(x, "gglist")) {
        y <- c(list(gglist=x), attr(x, 'params'))
        res <- do.call(plot_list2, y)
        return(res)
    }
    
    mp <- x[[1]]
    if ( length(x$plotlist) == 1) {
        return(ggplotGrob(mp))
    }
    width <- x$width
    height <- x$height
    if(align == "x" || align == "xy"){
        for (ind in seq(length(x$layout[, x$main_col]))) {
            i <- x$layout[,x$main_col][ind]
            if (is.na(i)) next
            if (i == 1) next
            x[[i]] <- suppressMessages(x[[i]] + xlim2(mp))
            x <- adjust_coord(x, i, ind, type = "height")
        }
    }
    
    if(align == "y" || align == "xy"){
        for (ind in seq(length(x$layout[x$main_row,]))) {
            i <- x$layout[x$main_row,][ind]
            if(is.na(i)) next
            if (i == 1) next
            x[[i]] <- suppressMessages(x[[i]] + ylim2(mp))
            x <- adjust_coord(x, i, ind, type = "width")
        }
    }

    if (is.coord_fixed(mp)){
        width <- height <- NULL
    }
    
    spacing_spec <- .resolve_panel_spacing(x)
    idx <- as.vector(x$layout)
    plotlist <- x$plotlist
    plotlist[[x$n+1]] <- ggplot() + theme_void() # plot_spacer()
    if (spacing_spec$mode == "object") {
        plotlist <- .apply_panel_spacing(
            plotlist = plotlist,
            layout = x$layout,
            spacing = spacing_spec$spacing
        )
    }
    idx[is.na(idx)] <- x$n + 1 
    plotlist <- .process_plotlist(plotlist[idx])

    if (spacing_spec$mode == "legacy") {
        pp <- .compose_plotlist_legacy(plotlist, spacing_spec$spacing)
    } else {
        pp <- .compose_plotlist(plotlist)
    }

    guides <- getOption('aplot_guides', default="collect")

    pp + plot_layout(byrow=F,
                     ncol=ncol(x$layout),
                     widths = width,
                     heights= height,
                     guides = guides)
}


##' @importFrom ggplot2 ggplotGrob
##' @importFrom patchwork patchworkGrob
aplotGrob <- function(x) {
    res <- as.patchwork(x)
    patchworkGrob(res)
}

oncoplotGrob <- function(x) {
    guides <- getOption('aplot_guides', default="collect")
    on.exit(options(aplot_guides = guides))
    options(aplot_guides = "keep")
    aplotGrob(x)
}

##' @importFrom grid grid.draw
##' @method grid.draw aplot
##' @export
grid.draw.aplot <- function(x, recording = TRUE) {
    grid::grid.draw(as.patchwork(x))
}

##' @method grid.draw oncoplot
##' @export
grid.draw.oncoplot <- function(x, recording = TRUE) {
    guides <- getOption('aplot_guides', default="collect")
    on.exit(options(aplot_guides = guides))
    options(aplot_guides = "keep")
    grid.draw.aplot(x)
}
