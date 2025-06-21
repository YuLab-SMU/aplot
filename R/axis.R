##' display x or y axis label as an ordinary text, so that the label will not be aligned with axis label of another plot
##'
##'
##' @rdname aplot_label
##' @param label axis label
##' @param fontsize fontsize of the label
##' @param ... additional parameter passed to `gpar` 
##' @return gg object with new label
##' @export
##' @author Guangchuang Yu
xlab2 <- function(label, fontsize = 11, ...) {
    alab_(label = label, axis = 'x', 
        fontsize = fontsize, ...)
}

##' @rdname aplot_label
##' @export
ylab2 <- function(label, fontsize = 11, ...) {
    alab_(label = label, axis = 'y', 
        fontsize = fontsize, ...)
}

alab_ <- function(label, axis, fontsize=11, ...) {
    structure(
            list(label = label,
                axis = axis,
                fontsize = fontsize,
                ...),
            class = "alab"
    )
}

##' @method ggplot_add alab
##' @importFrom grid textGrob
##' @importFrom ggplot2 annotation_custom
##' @importFrom ggplot2 coord_cartesian
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 margin
##' @export
ggplot_add.alab <- function(object, plot, object_name, ...) {
    label <- object$label
    object$label <- NULL
    axis <- object$axis
    object$axis <- NULL
    gp <- do.call(grid::gpar, object)

    th <- plot$theme

    if (axis == 'x') {    
        grob <- textGrob(label = label, gp = gp, y=-30, default.units = 'pt')
        th <- theme(axis.title.x = element_blank(),
                plot.margin = margin(b=40))
    } else {
        grob <- textGrob(label = label, gp = gp, rot = 90, x=-30, default.units = 'pt')
        th <- theme(axis.title.y = element_blank(),
                plot.margin = margin(l=40))            
    }

    layer <- annotation_custom(grob = grob)

    plot + layer + coord_cartesian(clip="off") + th
}



##' set axis limits (x or y) of a `ggplot` object (left hand side of `+`)
##' based on the x (`xlim2`) or y (`ylim2`) limits of another `ggplot` object (right hand side of `+`).
##' This is useful for using `cowplot` or `patchwork` to align `ggplot` objects.
##'
##'
##' @title xlim2
##' @rdname align_axis
##' @param gg ggplot object
##' @param limits vector of limits. If NULL, determine from `gg`. 
##' @return ggplot2 object with new limits
##' @export
##' @examples 
##' library(ggplot2)
##' library(aplot)
##' p1 <- ggplot(mtcars, aes(cyl)) + geom_bar()
##' p2 <- ggplot(subset(mtcars, cyl != 4), aes(cyl)) + geom_bar()
##' p2 + xlim2(p1)
##' @author Guangchuang Yu
xlim2 <- function(gg, limits = NULL) {
    axis_align(gg = gg, limits = limits, axis = 'x')
}

##' @rdname align_axis
##' @title ylim2
##' @export
ylim2 <- function(gg, limits = NULL) {
    axis_align(gg = gg, limits = limits, axis = 'y')
}

axis_align <- function(gg, limits = NULL, axis) {
    if (is.null(limits)) {
        if (axis == "x") {
            limits <- xrange(gg, region = 'plot')
        } else {
            limits <- yrange(gg, region = 'plot')
        }
    }
    structure(list(limits = limits, axis = axis),
              class = "axisAlign")
}


##' @method ggplot_add axisAlign
##' @importFrom ggplot2 ggplot_add
##' @importFrom ggplot2 scale_x_discrete
##' @importFrom ggplot2 scale_y_discrete
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom methods is
##' @export
ggplot_add.axisAlign <- function(object, plot, object_name, ...) {
    limits <- object$limits

    ## expand_limits <- object$expand_limits
    ## limits[1] <- limits[1] + (limits[1] * expand_limits[1]) - expand_limits[2]
    ## limits[2] <- limits[2] + (limits[2] * expand_limits[3]) + expand_limits[4]
    gb <- ggplot2::ggplot_build(plot)
    if (is.numeric(limits)) {
        if (inherits(plot, 'ggtree')){
            lim_x <- scale_x_continuous(limits=limits, expand=c(0, 0))
            lim_y <- scale_y_continuous(limits = limits, expand = c(0, 0))
        }else{
            lim_x <- set_scale_limits(gb$layout$panel_scales_x[[1]], limits=limits, 
                                      expand = gb$layout$panel_scales_x[[1]]$expand)
            lim_y <- set_scale_limits(gb$layout$panel_scales_y[[1]], limits=limits, 
                                      expand = gb$layout$panel_scales_y[[1]]$expand)
        }
    } else {
        if (inherits(plot, 'ggtree')){
            lim_x <- scale_x_discrete(limits=limits, expand = c(0, 0.6))
            lim_y <- scale_y_discrete(limits = limits, expand = c(0, 0.6))
        }else{
            lim_x <- set_scale_limits(gb$layout$panel_scales_x[[1]], limits=limits, 
                                      expand = gb$layout$panel_scales_x[[1]]$expand)
            lim_y <- set_scale_limits(gb$layout$panel_scales_y[[1]], limits=limits, 
                                      expand = gb$layout$panel_scales_y[[1]]$expand)
        }
    }

    if (object$axis == 'x') {
        ## if (object$by == "x") {
        if (is(plot$coordinates, "CoordFlip")) {
            message("the plot was flipped and the x limits will be applied to y-axis")
            scale_lim <- lim_y
            if (!inherits(plot, 'ggtree')){
                scale_lim <- switch_position(scale_lim)
            }
        } else {
            scale_lim <- lim_x
        }
        ## } else {
        ##     if (is(plot$coordinates, "CoordFlip")) {
        ##         message("the plot was flipped and the x limits will be applied to x-axis")
        ##         scale_lim <- scale_x_continuous(limits=limits, expand=c(0,0))
        ##     } else {
        ##         scale_lim <- scale_y_continuous(limits=limits, expand=c(0,0))
        ##     }
        ## }
    } else { ## axis == 'y'
        ## if (object$by == "x") {
        ##     if (is(plot$coordinates, "CoordFlip")) {
        ##         message("the plot was flipped and the y limits will be applied to y-axis")
        ##         scale_lim <- scale_y_continuous(limits = limits, expand = c(0, 0))
        ##     } else {
        ##         scale_lim <- scale_x_continuous(limits = limits, expand = c(0, 0))
        ##     }
        ## } else {
        if (is(plot$coordinates, "CoordFlip")) {
            message("the plot was flipped and the y limits will be applied to x-axis")
            scale_lim <- lim_x
            if (!inherits(plot, 'ggtree')){
                scale_lim <- switch_position(scale_lim)
            }
        } else {
            scale_lim <- lim_y
        }
        ## }
    }
    ggplot_add(scale_lim, plot, object_name, ...)
}

set_scale_limits <- function(scales, limits, expand){
    scales$limits <- limits
    scales$expand <- expand
    return(scales)
}

switch_position <- function(scales){
    scales$position <- switch(scales$position, 
                              bottom='left', 
                              top='right', 
                              left='bottom', 
                              right='top')
    return(scales)
}
