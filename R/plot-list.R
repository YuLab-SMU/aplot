##' plot a list of ggplot objects using patchwork, similar to `cowplot::plot_grid(plotlist)`
##'
##' 
##' @title plot a list of ggplot objects
##' @param gglist list of ggplot objects
##' @param ncol number of columns
##' @param nrow number of rows
##' @param widths relative widths
##' @param heights relative heights
##' @param ... additional parameters that passed to plot_layout
##' @return composite plot
##' @importFrom patchwork plot_layout
##' @export
##' @author Guangchuang Yu
plot_list <- function(gglist,
                      ncol = NULL, nrow = NULL,
                      widths = NULL, heights = NULL,
                      ...) {

    Reduce(`+`, gglist) +
        plot_layout(ncol = ncol,
                    nrow = nrow,
                    widths = widths,
                    heights = heights,
                    ...)
}

