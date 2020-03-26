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
##' @author Guangchuang Yu
insert_left <- function(.data, plot, width=1) {
    .data <- as.aplot(.data)
    .data$n <- .data$n + 1
    .data$width <- c(width, .data$width)
    new_col <- matrix(nrow=nrow(.data$layout), ncol=1)
    new_col[.data$main_row] <- .data$n
    .data$layout <- cbind(new_col, .data$layout)
    .data$main_col <- .data$main_col + 1
    
    plot <- plot + ggtree::ylim2(.data$plotlist[[1]])
    
    if (inherits(plot, "ggtree")) { ## re-order based on the tree
        yvar <- rvcheck::get_aes_var(.data$plotlist[[1]]$mapping, 'y')
        for (i in 1:length(.data$plotlist)) {
            .data$plotlist[[i]] <- .data$plotlist[[i]] + 
                aes(y = factor(.data[[yvar]], 
                               levels = rev(ggtree::get_taxa_name(plot)))) +
                ylab(NULL)
        }
    }
    
    .data$plotlist[[.data$n]] = plot 
    .data
}

##' @rdname plot-insertion
##' @export
insert_right <- function(.data, plot, width=1) {
    .data <- as.aplot(.data)
    .data$n <- .data$n + 1
    .data$width <- c(.data$width, width)
    new_col <- matrix(nrow=nrow(.data$layout), ncol=1)
    new_col[.data$main_row] <- .data$n
    .data$layout <- cbind(.data$layout, new_col)
    
    plot <- plot + ggtree::ylim2(.data$plotlist[[1]])
    
    if (inherits(plot, "ggtree")) { ## re-order based on the tree
        yvar <- rvcheck::get_aes_var(.data$plotlist[[1]]$mapping, 'y')
        for (i in 1:length(.data$plotlist)) {
            .data$plotlist[[i]] <- .data$plotlist[[i]] + 
                aes(y = factor(.data[[yvar]], 
                               levels = rev(ggtree::get_taxa_name(plot)))) +
                ylab(NULL)
        }
    }
    
    .data$plotlist[[.data$n]] = plot 
    .data
}

##' @rdname plot-insertion
##' @export
insert_top <- function(.data, plot, height=1) {
    .data <- as.aplot(.data)
    .data$n <- .data$n + 1
    .data$height <- c(height, .data$height)
    new_row <- matrix(nrow=1, ncol=ncol(.data$layout))
    new_row[.data$main_col] <- .data$n
    .data$layout <- rbind(new_row, .data$layout)
    .data$main_row <- .data$main_row + 1
    
    plot <- plot + ggtree::xlim2(.data$plotlist[[1]])
    
    if (inherits(plot, "ggtree")) { ## re-order based on the tree
        xvar <- rvcheck::get_aes_var(.data$plotlist[[1]]$mapping, 'x')
        for (i in 1:length(.data$plotlist)) {
            .data$plotlist[[i]] <- .data$plotlist[[i]] + 
                aes(x = factor(.data[[xvar]], 
                       levels = rev(ggtree::get_taxa_name(plot)))) +
                xlab(NULL)
        }
    }
    
    .data$plotlist[[.data$n]] = plot 
    .data
}


##' @rdname plot-insertion
##' @export
insert_bottom <- function(.data, plot, height=1) {
    .data <- as.aplot(.data)
    .data$n <- .data$n + 1
    .data$height <- c(.data$height, height)
    new_row <- matrix(nrow=1, ncol=ncol(.data$layout))
    new_row[.data$main_col] <- .data$n
    .data$layout <- rbind(.data$layout, new_row)
    
    plot <- plot + ggtree::xlim2(.data$plotlist[[1]])
  
    if (inherits(plot, "ggtree")) { ## re-order based on the tree
        xvar <- rvcheck::get_aes_var(.data$plotlist[[1]]$mapping, 'x')
        for (i in 1:length(.data$plotlist)) {
            .data$plotlist[[i]] <- .data$plotlist[[i]] + 
                aes(x = factor(.data[[xvar]], 
                               levels = rev(ggtree::get_taxa_name(plot)))) +
                xlab(NULL)
        }
    }
    
    .data$plotlist[[.data$n]] = plot 
    .data
}
