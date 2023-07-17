oncoplot <- function(maf, genes) {
    p_main <- oncoplot_main(maf, genes)
    p_top <- oncoplot_sample(maf, genes)
    p_right <- oncoplot_gene(maf, genes, ylab = 'percentage')

    pp <- insert_top(p_main, p_top, height=.2) |>
        insert_right(p_right, width=.2)
    class(pp) <- c("oncoplot", class(pp))

    return(pp)
}

oncoplot_main <- function(maf, genes = 20) {
    d <- oncoplot_tidy_onco_matrix(maf, genes)

    ggplot(d, aes(x=Sample, y=Gene, fill=Type)) +
        geom_tile(colour="white", linewidth=.05) + 
        oncoplot_setting(continuous = FALSE) +
        theme(legend.position = "bottom") 
}


oncoplot_sample <- function(maf, genes = 20, sort = FALSE) {    
    numMat <- get_oncoplot_numericMatrix(maf, genes)
    samp_sum <- getSampleSummary(x = maf) %>%
        as.data.frame() %>%
        dplyr::select(!total) 

    i <- match(colnames(numMat), samp_sum$Tumor_Sample_Barcode)
    samp_sum <- samp_sum[i, , drop=FALSE]
    d <- tidyr::pivot_longer(samp_sum, -1) |>
        dplyr::rename(Sample=Tumor_Sample_Barcode, Type=name, Freq=value)
    d$Sample <- factor(d$Sample, levels = unique(colnames(numMat)))
    if (sort) {
        td <- group_by(d, Type) |> 
            summarize(total = sum(Freq)) |> 
            arrange(total) |> 
            pull(Type)
        d$Type <- factor(d$Type, levels = td)
    }

    ggplot(d, aes(x=Sample,y=Freq,fill=Type)) +
        geom_col(position="stack") +
        oncoplot_setting() +
        ylab("TMB") 
}

oncoplot_gene <- function(maf, genes = 20, ylab = 'gene') {
    ylab <- match.arg(ylab, c("gene", "percentage"))

    d <- oncoplot_tidy_onco_matrix(maf, genes)
    d <- d[!is.na(d$Type), ]

    p <- ggplot(d, aes(Gene, fill = Type)) + 
        geom_bar(position='stack') + 
        coord_flip() + 
        oncoplot_setting(noxaxis = FALSE) +
        ylab("No. of samples") #+ 
        # guides(y= guide_axis_label_trans(~str_pad(rev(percent_alt), 5)))

    if (ylab == 'percentage') {
        numMat <- get_oncoplot_numericMatrix(maf, genes)
        totSamps = as.numeric(maf@summary[3, 'summary'])
        percent <- apply(numMat, 1, function(x) sum(x >0))/totSamps
        percent_alt <- paste0(round(percent * 100), '%')
        p <- p + scale_x_discrete(breaks = rownames(numMat),
                                labels = percent_alt)
    }
    return(p)
}


oncoplot_setting <- function(noxaxis = TRUE, continuous = TRUE) {  
    if (continuous) {
        scale_setting <- scale_y_continuous(expand = c(0, 0))
    } else {
        scale_setting <- scale_y_discrete(expand = c(0, 0))
    }

    list(
        theme_minimal(),
        if (noxaxis) ggfun::theme_noxaxis(),
        theme(legend.position = "none", panel.grid.major = element_blank()),
        scale_setting,
        oncoplot_fill(),
        xlab(NULL),
        ylab(NULL)
    )
}

oncoplot_fill <- function(breaks=NULL, values=NULL, name = NULL, na.value = "#bdbdbd") {
    vc_col <- maftools:::get_vcColors(websafe = FALSE)  # maftools color setting
    if (is.null(values)) {
        values <- vc_col
    } 
    if (is.null(breaks)) {
        vc_lev <- names(vc_col)
        breaks <- factor(vc_lev, levels = rev(vc_lev))
    }
    scale_fill_manual(name = NULL, 
            breaks = breaks, 
            values = values, 
            na.value = na.value)
}


get_oncoplot_genes <- function(maf, genes = 20) {
    if (is.numeric(genes)) {
        if (length(genes) == 1) {
            # Top N genes
            genes <- getGeneSummary(x = maf)[1:genes, "Hugo_Symbol"]  
        } else {
            # as index
            genes <- getGeneSummary(x = maf)[genes, "Hugo_Symbol"]
        }
    }
    # if character, return as it is.

    return(genes)
}

get_oncoplot_numericMatrix <- function(maf, genes = 20) {
    genes <- get_oncoplot_genes(maf, genes = )
    om <- maftools:::createOncoMatrix(m = maf, g = genes)
    numMat = om$numericMatrix  # gene/sample ~ frequency
    return(numMat)
}


oncoplot_tidy_onco_matrix <- function(maf, genes = 20) {
    genes <- get_oncoplot_genes(maf, genes)
    om <- maftools:::createOncoMatrix(m = maf, g = genes)
    mat_origin = om$oncoMatrix # gene/sample ~ variant type

    d = mat_origin[rev(rownames(mat_origin)),] |>
        as.data.frame() |>
        tibble::rownames_to_column('Gene') |>
        tidyr::pivot_longer(-1) |>
        dplyr::rename(Sample = name, Type = value)

    d$Type[d$Type == ""] <- NA #"Non_Mut"
    #d$Type = factor(d$Type, levels = type_levels_2)
    d$Gene <- factor(d$Gene, levels = rev(rownames(mat_origin)))
    d$Sample <- factor(d$Sample, levels = colnames(mat_origin))
    return(d)
}


# To add annotation on right y axis: https://github.com/tidyverse/ggplot2/issues/3171
# guide_axis_label_trans <- function(label_trans = identity, ...) {
#   axis_guide <- guide_axis(...)
#   axis_guide$label_trans <- rlang::as_function(label_trans)
#   class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
#   axis_guide
# }

# guide_train.guide_axis_trans <- function(x, ...) {
#   trained <- NextMethod()
#   trained$key$.label <- x$label_trans(trained$key$.label)
#   trained
# }

