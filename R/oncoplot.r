#' ploting oncoplot with aplot
#' @param maf MAF object. 
#' @param genes the gene names or the number, default is 20.
#' @return \code{oncoplot} object, which is also a \code{aplot} object
#' @export
#' @examples
#' laml.maf <- system.file("extdata", "tcga_laml.maf.gz", package = "maftools")
#' laml.clin <- system.file('extdata', 'tcga_laml_annot.tsv', package = 'maftools')
#' laml <- maftools::read.maf(maf = laml.maf, clinicalData = laml.clin)
#' p <- oncoplot(maf = laml, genes = 20)
#' p
oncoplot <- function(maf, genes = 20) {
    p_main <- oncoplot_main(maf, genes)
    p_top <- oncoplot_sample(maf, genes)
    p_right <- oncoplot_gene(maf, genes, ylab = 'percentage')

    pp <- insert_top(p_main, p_top, height=.2) |>
        insert_right(p_right, width=.2)
    class(pp) <- c("oncoplot", class(pp))

    return(pp)
}

#' @importFrom ggplot2 geom_tile
oncoplot_main <- function(maf, genes = 20) {
    d <- oncoplot_tidy_onco_matrix(maf, genes)

    ggplot(d, aes(x=.data$Sample, y=.data$Gene, fill=.data$Type)) +
        geom_tile(colour="white", linewidth=.05) + 
        oncoplot_setting(continuous = FALSE) +
        theme(legend.position = "bottom", axis.text.y.left=element_text(face='italic')) 
}


oncoplot_sample <- function(maf, genes = 20, sort = FALSE) {    
    numMat <- get_oncoplot_numericMatrix(maf, genes)
    samp_sum <- obtain.sample.summary.MAF(x = maf) %>%
        as.data.frame() %>%
        dplyr::select(!.data$total) 

    i <- match(colnames(numMat), samp_sum$Tumor_Sample_Barcode)
    samp_sum <- samp_sum[i, , drop=FALSE]
    d <- tidyr::pivot_longer(samp_sum, -1) |>
        dplyr::rename(Sample=.data$Tumor_Sample_Barcode, Type=.data$name, Freq=.data$value)
    d$Sample <- factor(d$Sample, levels = unique(colnames(numMat)))
    if (sort) {
        td <- dplyr::group_by(.data$d, .data$Type) |> 
            dplyr::summarize(total = sum(.data$Freq)) |> 
            dplyr::arrange(.data$total) |> 
            dplyr::pull(.data$Type)
        d$Type <- factor(d$Type, levels = td)
    }

    ggplot(d, aes(x=.data$Sample,y=.data$Freq,fill=.data$Type)) +
        geom_col(position="stack") +
        oncoplot_setting() +
        ylab("TMB") 
}

#' @importFrom rlang .data
#' @importFrom ggplot2 coord_flip geom_bar
oncoplot_gene <- function(maf, genes = 20, ylab = 'gene') {
    ylab <- match.arg(ylab, c("gene", "percentage"))

    d <- oncoplot_tidy_onco_matrix(maf, genes)
    d <- d[!is.na(d$Type), ]

    p <- ggplot(d, aes(y = .data$Gene, fill = .data$Type)) + 
        geom_bar(position='stack', orientation = 'y') + 
        oncoplot_setting(noxaxis = FALSE, scale='none') +
        xlab("No. of samples") #+ 
        # guides(y= guide_axis_label_trans(~str_pad(rev(percent_alt), 5)))

    if (ylab == 'percentage') {
        numMat <- get_oncoplot_numericMatrix(maf, genes)
        totSamps = as.numeric(maf@summary[3, 'summary'])
        percent <- apply(numMat, 1, function(x) sum(x >0))/totSamps
        percent_alt <- paste0(round(percent * 100), '%')
        p <- p + scale_y_discrete(breaks = rownames(numMat),
                                labels = percent_alt,
                                expand = c(0, 0))
    }
    return(p)
}

#' @importFrom ggplot2 theme_minimal
oncoplot_scale <- function(continuous = TRUE, scale = 'y') {
    scale <- match.arg(scale, c('x', 'y', 'none'))
    if (scale == 'none') {
        return(NULL)
    } 
    
    if (scale == 'x') {
        if (continuous) {
            scale_fun <- ggplot2::scale_x_continuous
        } else {
            scale_fun <- ggplot2::scale_x_discrete
        }
    } else {
        if (continuous) {
            scale_fun <- ggplot2::scale_y_continuous
        } else{
            scale_fun <- ggplot2::scale_y_discrete
        }
    }
    
    scale_fun(expand = c(0, 0))

}

oncoplot_setting <- function(noxaxis = TRUE, continuous = TRUE, scale='y') {  
    list(
        theme_minimal(),
        if (noxaxis) ggfun::theme_noxaxis(),
        theme(legend.position = "none", panel.grid.major = element_blank()),
        oncoplot_scale(continuous = continuous, scale = scale),
        oncoplot_fill(),
        xlab(NULL),
        ylab(NULL)
    )
}

#' @importFrom ggplot2 scale_fill_manual
oncoplot_fill <- function(breaks=NULL, values=NULL, name = NULL, na.value = "#bdbdbd") {
    vc_col <- get_vcColors(websafe = FALSE)  # maftools color setting
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
            genes <- obtain.gene.summary.MAF(x = maf)[1:genes, "Hugo_Symbol"]  
        } else {
            # as index
            genes <- obtain.gene.summary.MAF(x = maf)[genes, "Hugo_Symbol"]
        }
    }
    # if character, return as it is.

    return(genes)
}

get_oncoplot_numericMatrix <- function(maf, genes = 20) {
    genes <- get_oncoplot_genes(maf, genes = genes)
    om <- createOncoMatrix(m = maf, g = genes)
    numMat = om$numericMatrix  # gene/sample ~ frequency
    return(numMat)
}


oncoplot_tidy_onco_matrix <- function(maf, genes = 20) {
    genes <- get_oncoplot_genes(maf, genes)
    om <- createOncoMatrix(m = maf, g = genes)
    mat_origin = om$oncoMatrix # gene/sample ~ variant type

    d = mat_origin[rev(rownames(mat_origin)),] |>
        as.data.frame() |>
        tibble::rownames_to_column('Gene') |>
        tidyr::pivot_longer(-1) |>
        dplyr::rename(Sample = "name", Type = "value")

    d$Type[d$Type == ""] <- NA #"Non_Mut"
    d$Gene <- factor(d$Gene, levels = rev(rownames(mat_origin)))
    d$Sample <- factor(d$Sample, levels = colnames(mat_origin))
    return(d)
}

obtain.sample.summary.MAF <- function(x){
    x@variant.classification.summary
}

obtain.gene.summary.MAF <- function(x){
    x@gene.summary
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

#' @importFrom yulab.utils get_fun_from_pkg
get_vcColors <- yulab.utils::get_fun_from_pkg('maftools', 'get_vcColors')

#' @import survival
createOncoMatrix <- yulab.utils::get_fun_from_pkg('maftools', 'createOncoMatrix')

if(getRversion() >= "2.15.1")  {
    utils::globalVariables(c(".", "Hugo_Symbol", 
       "Tumor_Sample_Barcode", "Variant_Classification","Variant_Classification_temp","Variant_Type")
    )
}
