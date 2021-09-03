##' plot a list of ggplot objects using patchwork, similar to `cowplot::plot_grid(plotlist)`
##'
##' 
##' @title plot a list of ggplot objects
##' @param ... list of plots to be arranged
##' @param gglist (optional) list of plots
##' @param ncol number of columns
##' @param nrow number of rows
##' @param byrow If "FALSE" the plots will be filled in in column-major order
##' @param widths relative widths
##' @param heights relative heights
##' @param tag_levels format to label plots
##' @param tag_size size of tags
##' @return composite plot
##' @importFrom patchwork plot_layout
##' @importFrom patchwork plot_annotation
##' @importFrom ggplotify as.ggplot
##' @importFrom ggplot2 theme
##' @importFrom utils modifyList
##' @export
##' @author Guangchuang Yu
plot_list <- function(..., gglist = NULL,
                      ncol = NULL, nrow = NULL, byrow = NULL,
                      widths = NULL, heights = NULL,
                      tag_levels = NULL,
                      tag_size = 14) {

    gglist <- c(list(...), gglist)
    name <- names(gglist)

    for (i in seq_along(gglist)) {
        if (!inherits(gglist[[i]], 'gg') || inherits(gglist[[i]], 'patchwork')) {
            gglist[[i]] <- ggplotify::as.ggplot(gglist[[i]])
        }
        if (!is.null(name)) {
            gglist[[i]] <- ggfun::add_facet(gglist[[i]], name[i])
        }
    }

    p <- Reduce(`+`, gglist) +
        plot_layout(ncol = ncol,
                    nrow = nrow,
                    byrow = byrow,
                    widths = widths,
                    heights = heights
                    )

    if (!is.null(tag_levels)) {
        pt <- modifyList(p$theme$plot.tag, list(size = tag_size))
        p <- p + plot_annotation(tag_levels=tag_levels) &
            theme(plot.tag = pt)
    }
    return(p)
}



