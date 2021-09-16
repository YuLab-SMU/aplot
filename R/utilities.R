



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



