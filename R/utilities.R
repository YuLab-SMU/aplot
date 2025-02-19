



## derive from ggtree::get_taxa_name
get_taxa_order <- function (tree_view) {
    df <- tree_view$data
    with(df, {
        i = order(y, decreasing = T)
        label[i][isTip[i]]
    })
}


theme_no_margin <- getFromNamespace("theme_no_margin", "ggfun")


is.coord_flip <- function(p) {
    inherits(p, "gg") && inherits(p$coordinates, "CoordFlip")
}

is.coord_fixed <- function(p){
    inherits(p, "gg") && inherits(p$coordinates, "CoordFixed")
}

#' @importFrom ggplot2 coord_fixed
adjust_coord <- function(x, i, ind, type='width'){
    coordfixed <- is.coord_fixed(x[[1]])
    coordfixed2 <- !is.coord_fixed(x[[i]])
    ratio <- 1
    if (coordfixed && coordfixed2){
        ajustcoord <- getOption("ajust_coord", default = TRUE)
        if (ajustcoord) ratio <- .cal_ratio(x[[i]], x[[type]][ind], type)
        x[[i]] <- suppressMessages(x[[i]] + coord_fixed(ratio = ratio))

    }
    return(x)
}

.cal_ratio <- function(x, size, type='width'){
    xr <- .cal_limit_range(xlim2(x)$limits)
    yr <- .cal_limit_range(ylim2(x)$limits)
    val <- xr / yr
    if (type == 'width'){
        val <- val / size
    }else{
        val <- val * size
    }
    return(val)
}

.cal_limit_range <- function(x){
    if (inherits(x, "character")){
        res <- length(x)
    }else if(inherits(x, "numeric")){
        res <- diff(x)
    }
    return(res)
}
