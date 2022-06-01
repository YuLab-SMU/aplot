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
##' @param guides A string specifying how guides should be treated in the layout.
##' @param labels manual specified labels to label plots
##' @param tag_levels format to label plots, will be disable if 'labels' is not NULL
##' @param tag_size size of tags
##' @param design specification of the location of areas in the layout
##' @param output one of 'gglist' or 'patchwork'
##' @return composite plot
##' @importFrom patchwork plot_layout
##' @importFrom patchwork plot_annotation
##' @importFrom ggplotify as.ggplot
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 labs
##' @importFrom utils modifyList
##' @importFrom ggfun ggbreak2ggplot
##' @export
##' @author Guangchuang Yu
plot_list <- function(..., gglist = NULL,
                      ncol = NULL, 
                      nrow = NULL, 
                      byrow = NULL,
                      widths = NULL, 
                      heights = NULL,
                      guides = NULL,
                      labels = NULL,        
                      tag_levels = NULL,
                      tag_size = 14,
                      design = NULL, 
                      output = "gglist") {
    
    output <- match.arg(output, c("gglist", "patchwork"))
    
    gglist <- c(list(...), gglist)
    name <- names(gglist)
    
    if (!all_ggplot(gglist)) {
        for (i in seq_along(gglist)) {
            if (is.ggbreak(gglist[[i]])) {
                gglist[[i]] <- ggbreak2ggplot(gglist[[i]])
            } else {
                gglist[[i]] <- ggplotify::as.ggplot(gglist[[i]])
            }
            
        }
    }
    
    if (!is.null(name)) {
        for (i in seq_along(gglist)) {
            if (name[i] != "") {
                if (inherits(gglist[[i]], 'patchwork')) {
                    gglist[[i]] <- ggplotify::as.ggplot(gglist[[i]])
                }

                gglist[[i]] <- gglist[[i]] + ggfun::facet_set(label = name[i])                
            }
        }
    }
    
    if (!is.null(labels)) {
        tag_levels <- NULL
        n <- min(length(labels), length(gglist))
        for (i in seq_len(n)) {
            if (labels[i] == "") next
            gglist[[i]] <- gglist[[i]] + labs(tag = labels[i])
        }
    }

    res <- gglist(gglist = gglist,
          ncol = ncol, 
          nrow = nrow, 
          byrow = byrow,
          widths = widths, 
          heights = heights,
          guides = guides,
          labels = labels,        
          tag_levels = tag_levels,
          tag_size = tag_size,
          design = design)        
    if (output == 'gglist') return(res)
    as.patchwork(res)
}


as.patchwork <- function(x) {
    y = c(list(gglist=x), attr(x, 'params'))
    do.call(plot_list2, y)
}

plot_list2 <- function(gglist = NULL,
                      ncol = NULL, 
                      nrow = NULL, 
                      byrow = NULL,
                      widths = NULL, 
                      heights = NULL,
                      guides = NULL,
                      labels = NULL,        
                      tag_levels = NULL,
                      tag_size = 14,
                      design = NULL) {

    p <- Reduce(`+`, gglist, init=plot_filler()) +
        plot_layout(ncol = ncol,
                    nrow = nrow,
                    byrow = byrow,
                    widths = widths,
                    heights = heights,
                    guides = guides,
                    design = design
                    )

    if (!is.null(tag_levels) || !is.null(labels)) {
        pt <- p$theme$plot.tag
        if (is.null(pt)){
            pt <- ggplot2::element_text()
        }
        pt <- modifyList(pt, list(size = tag_size))
        p <- p + plot_annotation(tag_levels=tag_levels) &
            theme(plot.tag = pt)
    }
    return(p)
}

##' @importFrom ggplot2 is.ggplot
##' @importFrom ggfun is.ggbreak
all_ggplot <- function(gglist) {
    for (i in seq_along(gglist)) {
        if (!is.ggplot(gglist[[i]])) {
            return(FALSE)
        #} else if (inherits(gglist[[i]], 'patchwork')) {
        #    return(FALSE)
        } else if (is.ggbreak(gglist[[i]])) {
            return(FALSE)
        }
    }
    return(TRUE)
}



plot_filler <- yulab.utils::get_fun_from_pkg("patchwork", "plot_filler")


##' constructure a `gglist` object that contains a list of plots (`gglist`) and 
##' parameters (via `...`), the object can be displayed via the `plot_list()` function.
##' 
##' @title construct a `gglist` object
##' @param gglist a list of plots
##' @param ... parameters for plotting the `gglist`
##' @return gglist object
##' @export
##' @author Guangchuang Yu
gglist <- function(gglist, ...) {
    res <- gglist
    attr(res, 'params') <- list(...)
    structure(res,
        class = c("gglist", "list")
    )
}

##' @method print gglist
##' @export
print.gglist <- function(x, ...) {
    as.patchwork(x)
}

