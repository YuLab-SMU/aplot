##' insert an associated plot to left, right, top and bottom of a main plot
##'
##' The first input serve as a main plot, and other plots can be progressively inserted to
##' different sides on left, right, top and bottom.
##' @title plot-insertion
##' @rdname plot-insertion
##' @param .data an 'aplot' or 'gg' object
##' @param plot a 'gg' plot to be inserted
##' @param width relative width to the main plot
##' @param height relative height to the main plot
##' @return an 'aplot' object
##' @export
##' @examples 
##' library(ggplot2)
##' library(aplot)
##' 
##' p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
##' p2 <- ggplot(mtcars, aes(mpg)) + 
##'     geom_density(fill='steelblue', alpha=.5) + 
##'         ggfun::theme_noxaxis()
##' p3 <- ggplot(mtcars, aes(x=1, y=disp)) + 
##'     geom_boxplot(fill='firebrick', alpha=.5) + 
##'     theme_void()
##' ap <- p %>% 
##'     insert_top(p2, height=.3) %>% 
##'     insert_right(p3, width=.1)
##' 
##' ap
##' ap[2, 1] <- ap[2, 1] + theme_bw()
##' ap[2, 1] <- ap[2, 1] + 
##'             aes(color = as.factor(am)) + 
##'             scale_color_manual(values = c('steelblue', 'darkgreen'))
##' ap[1, 1] <- ap[1, 1] + theme(axis.line.x.bottom=element_line())
##' ap
##' @author Guangchuang Yu
insert_left <- function(.data, plot, width=1) {
    insert_lr(.data = .data, plot = plot,
              width = width, side = "left")
}


##' @rdname plot-insertion
##' @export
insert_right <- function(.data, plot, width=1) {
    insert_lr(.data = .data, plot = plot,
              width = width, side = "right")
}

##' @importFrom ggplot2 ylab
##' @importFrom ggfun is.ggtree
insert_lr <- function(.data, plot, width,  side) {
    side <- match.arg(side, c("left", "right"))
    .data <- as.aplot(.data)
    .data$n <- .data$n + 1

    new_col <- matrix(nrow=nrow(.data$layout), ncol=1)
    new_col[.data$main_row] <- .data$n

    if (side == "left") {
        .data$width <- c(width, .data$width)
        .data$layout <- cbind(new_col, .data$layout)
        .data$main_col <- .data$main_col + 1
    } else {
        .data$width <- c(.data$width, width)
        .data$layout <- cbind(.data$layout, new_col)
    }
    
    if (is.ggtree(plot)) { ## re-order based on the tree
        selected <- .data$layout[.data$main_row,]
        selected <- selected[!is.na(selected)]
        selected <- selected[selected != .data$n]
        for (i in selected) {
            if (is.coord_flip(.data$plotlist[[i]])) {
                xvar <- ggfun::get_aes_var(.data$plotlist[[i]]$mapping, 'x')
                lvs <- rev(get_taxa_order(plot))

                axis_trans <- list(
                    aes(x = factor(.data[[xvar]], 
                                   levels = lvs)), ## c(.data[[xvar]][!.data[[xvar]] %in% lvs], lvs))),
                    xlab(.data$plotlist[[i]]$labels$x)
                )
               
            } else {
                yvar <- ggfun::get_aes_var(.data$plotlist[[i]]$mapping, 'y')
                lvs = rev(get_taxa_order(plot))

                axis_trans <- list(
                    aes(y = factor(.data[[yvar]], 
                                   levels = lvs)), ## c(.data[[yvar]][!.data[[yvar]] %in% lvs], lvs))),
                    ylab(.data$plotlist[[i]]$labels$y)
                )
            }
            .data$plotlist[[i]] <- .data$plotlist[[i]] + axis_trans
        }
    }
    
    .data$plotlist[[.data$n]] = plot 
    .data

}

##' @rdname plot-insertion
##' @export
insert_top <- function(.data, plot, height=1) {
    insert_tb(.data = .data, plot = plot,
              height = height, side = "top")
}


##' @rdname plot-insertion
##' @export
insert_bottom <- function(.data, plot, height=1) {
    insert_tb(.data = .data, plot = plot,
              height = height, side = "bottom")
}

##' @importFrom ggplot2 aes
##' @importFrom ggplot2 xlab
insert_tb <- function(.data, plot, height, side) {
    side <- match.arg(side, c("top", "bottom"))
    .data <- as.aplot(.data)
    .data$n <- .data$n + 1

    new_row <- matrix(nrow=1, ncol=ncol(.data$layout))
    new_row[.data$main_col] <- .data$n

    if (side == "top") {
        .data$height <- c(height, .data$height)
        .data$layout <- rbind(new_row, .data$layout)
        .data$main_row <- .data$main_row + 1
    } else {
        .data$height <- c(.data$height, height)
        .data$layout <- rbind(.data$layout, new_row)
    }

    
    if (is.ggtree(plot)) { ## re-order based on the tree
        selected <- .data$layout[,.data$main_col]
        selected <- selected[!is.na(selected)]
        selected <- selected[selected != .data$n]

        for (i in selected) {
            if (is.coord_flip(.data$plotlist[[i]])) {
                yvar <- ggfun::get_aes_var(.data$plotlist[[i]]$mapping, 'y')
                lvs = rev(get_taxa_order(plot))

                axis_trans <- list(
                    aes(y = factor(.data[[yvar]], 
                                   levels = lvs)), ## c(.data[[yvar]][!.data[[yvar]] %in% lvs], lvs))),
                    ylab(.data$plotlist[[i]]$labels$y)
                )
            } else {
                xvar <- ggfun::get_aes_var(.data$plotlist[[i]]$mapping, 'x')
                lvs <- rev(get_taxa_order(plot))

                axis_trans <- list(
                    aes(x = factor(.data[[xvar]], 
                                   levels = lvs)), ## c(.data[[xvar]][!.data[[xvar]] %in% lvs], lvs))),
                    xlab(.data$plotlist[[i]]$labels$x)
                )
            }
            .data$plotlist[[i]] <- .data$plotlist[[i]] + axis_trans
        }

        ## for (i in selected) {
        ##     xvar <- rvcheck::get_aes_var(.data$plotlist[[i]]$mapping, 'x')
        ##     .data$plotlist[[i]] <- .data$plotlist[[i]] + 
        ##         aes(x = factor(.data[[xvar]], 
        ##                        levels = rev(get_taxa_order(plot)))) +
        ##         xlab(.data$plotlist[[i]]$labels$x)
        ## }
    }
    
    .data$plotlist[[.data$n]] <- plot 
    .data
}
