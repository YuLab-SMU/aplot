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
##'         ggtree::theme_dendrogram()
##' p3 <- ggplot(mtcars, aes(x=1, y=disp)) + 
##'     geom_boxplot(fill='firebrick', alpha=.5) + 
##'     theme_void()
##' ap <- p %>% 
##'     insert_top(p2, height=.3) %>% 
##'     insert_right(p3, width=.1)
##'     
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
    
    if (inherits(plot, "ggtree")) { ## re-order based on the tree
        yvar <- rvcheck::get_aes_var(.data$plotlist[[1]]$mapping, 'y')
        selected <- .data$layout[.data$main_row,]
        selected <- selected[!is.na(selected)]
        selected <- selected[selected != .data$n]
        for (i in selected) {
            .data$plotlist[[i]] <- .data$plotlist[[i]] + 
                aes(y = factor(.data[[yvar]], 
                               levels = rev(get_taxa_order(plot)))) +
                ylab(.data$plotlist[[i]]$labels$y)
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

    
    if (inherits(plot, "ggtree")) { ## re-order based on the tree
        xvar <- rvcheck::get_aes_var(.data$plotlist[[1]]$mapping, 'x')
        selected <- .data$layout[,.data$main_col]
        selected <- selected[!is.na(selected)]
        selected <- selected[selected != .data$n]

        for (i in selected) {
            .data$plotlist[[i]] <- .data$plotlist[[i]] + 
                aes(x = factor(.data[[xvar]], 
                               levels = rev(get_taxa_order(plot)))) +
                xlab(.data$plotlist[[i]]$labels$x)
        }
    }
    
    .data$plotlist[[.data$n]] <- plot 
    .data
}
