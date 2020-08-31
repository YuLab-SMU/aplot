
##' test whether input object is produced by ggtree function
##'
##'
##' @title is.ggtree
##' @param x object
##' @return TRUE or FALSE
##' @export
##' @author Guangchuang Yu
## copy from treeio
is.ggtree <- function(x) {
    if (inherits(x, 'ggtree')) return(TRUE)

    if (!inherits(x, 'gg')) return(FALSE)

    ## to compatible with user using `ggplot(tree) + geom_tree()`

    tree_layer <- vapply(x$layers,
                         function(y) {
                             any(grepl("StatTree", class(y$stat)))
                         },
                         logical(1)
                         )
    return(any(tree_layer))
}



## derive from ggtree::get_taxa_name
get_taxa_order <- function (tree_view) {
    df <- tree_view$data
    with(df, {
        i = order(y, decreasing = T)
        label[i][isTip[i]]
    })
}


theme_no_margin <- function(...) {
    ggplot2::theme(plot.margin = ggplot2::margin(), ...)
}


is.coord_flip <- function(p) {
    inherits(p, "gg") && inherits(p$coordinates, "CoordFlip")
}


