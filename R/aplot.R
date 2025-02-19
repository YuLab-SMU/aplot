
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
                   main_row = 1),
              class = "aplot")
    
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
    
    idx <- as.vector(x$layout)
    idx[is.na(idx)] <- x$n + 1 
    x$plotlist[[x$n+1]] <- ggplot() + theme_void() # plot_spacer()
    plotlist <- x$plotlist[idx]
    space <- getOption(x="space.between.plots", default=0)
    # check space, either 'asis' or numeric value
    if (!inherits(space, c("character", "numeric"))) {
        stop("'space.between.plots' should be a numeric value or 'asis'.")
    }
    if (inherits(space, 'character')) {
        if (space == "asis") {
            theme_margin <- theme() # do nothing
        } else {
            stop("invalid 'space.between.plots' setting.")
        }
    } else {
        theme_margin <- theme_no_margin()
        if (space != 0) {
            theme_margin$plot.margin <- do.call(ggplot2::margin, c(rep(list(space), 4), unit='mm'))
        } 
    }

    plotlist <- .process_plotlist(plotlist)

    pp <- plotlist[[1]] + theme_margin
    for (i in 2:length(plotlist)) {
        pp <- pp + (plotlist[[i]] + theme_no_margin())
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
