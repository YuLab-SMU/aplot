
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
##' @export
print.aplot <- function(x, ...) {
    idx <- as.vector(x$layout)
    idx[is.na(idx)] <- x$n + 1 
    x$plotlist[[x$n+1]] <- plot_spacer()
    plotlist <- x$plotlist[idx]
    
    pp <- plotlist[[1]]
    np <- length(plotlist)
    if (np == 1) {
        return(pp)
    }
    
    for (i in 2:length(plotlist)) {
        pp <- pp + plotlist[[i]]
    }
    
    res <- pp + plot_layout(byrow=F, ncol=ncol(x$layout), 
                            widths = x$width,
                            heights= x$height,
                            guides = 'collect')
    print(res)
}
