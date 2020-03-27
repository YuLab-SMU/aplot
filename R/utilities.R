get_taxa_order <- function(tree_view) {
    df <- tree_view$data
    with(df, {
        i = order(y, decreasing = T)
        label[i][isTip[i]]
    })
}

