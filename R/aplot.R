
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

##' @importFrom ggplot2 ggplotGrob
##' @importFrom patchwork patchworkGrob
aplotGrob <- function(x) {
    mp <- x$plotlist[[1]]
    if ( length(x$plotlist) == 1) {
        return(ggplotGrob(mp))
    }

    for (i in x$layout[, x$main_col]) {
        if (is.na(i)) next
        if (i == 1) next
        x$plotlist[[i]] <- suppressMessages(x$plotlist[[i]] + xlim2(mp))
    }
    for (i in x$layout[x$main_row,]) {
        if(is.na(i)) next
        if (i == 1) next
        x$plotlist[[i]] <- suppressMessages(x$plotlist[[i]] + ylim2(mp))
    }

    idx <- as.vector(x$layout)
    idx[is.na(idx)] <- x$n + 1 
    x$plotlist[[x$n+1]] <- ggplot() + theme_void() # plot_spacer()
    plotlist <- x$plotlist[idx]
    
    pp <- plotlist[[1]] + theme_no_margin()
    for (i in 2:length(plotlist)) {
        pp <- pp + (plotlist[[i]] + theme_no_margin())
    }
    
    res <- pp + plot_layout(byrow=F, ncol=ncol(x$layout), 
                            widths = x$width,
                            heights= x$height,
                            guides = 'collect')
    patchworkGrob(res)
}

##' @importFrom grid grid.draw
##' @method grid.draw aplot
##' @export
grid.draw.aplot <- function(x, recoding = TRUE) {
    grid::grid.draw(aplotGrob(x))
}


