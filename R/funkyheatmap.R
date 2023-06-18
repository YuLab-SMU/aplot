funky_setting <- function(gglist, options) {
#  gglist <- lapply(gglist, function(p) p + ggfun::theme_stamp())
  if (!is.null(options)) {
      gglist <- lapply(gglist, function(p) p + options)
  }

  return(gglist)
}

##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 margin
##' @importFrom grid unit
funky_theme <- function() {
    theme(axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = -30, hjust = 0, size=13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.length.y = unit(0,"pt"),
        plot.margin = margin(0,0,0,0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) 
}

##' @importFrom tibble rownames_to_column
funky_id <- function(data) {
  if (!'id' %in% colnames(data)) {
    data <- tibble::rownames_to_column(data, "id")
  }
  return(data)
}

##' @importFrom dplyr mutate
##' @importFrom tidyr pivot_longer
funky_data <- function(data, cols) {
    data <- funky_id(data)
    data[, c(1, cols)] |>
      funky_id() |>
      tidyr::pivot_longer(-1) |>
      dplyr::mutate(id=factor(.data$id, levels = rev(data$id)))
}

##' create text plot (i.e., rownames) for funkyheatmap
##'
##' 
##' @title funky_text
##' @param data data frame
##' @param hjust text alignment adjustment
##' @return ggplot object
##' @importFrom ggplot2 xlim
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 element_blank
##' @importFrom rlang .data
##' @export
##' @author Guangchuang Yu
funky_text <- function(data, hjust=0) {  
  ggplot(funky_id(data), aes(x=1, y=.data$id, label=.data$id)) + 
    xlim(1,2) +
    geom_text(hjust=hjust) + 
    funky_theme() +
    theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
}

##' create dot plot for funkyheatmap
##'
##' 
##' @title funky_point
##' @param data data frame
##' @param cols selected columns
##' @return ggplot object
##' @importFrom ggplot2 geom_point
##' @export
##' @author Guangchuang Yu
funky_point <- function(data, cols) {
  d2 <- funky_data(data, cols)

  ggplot(d2, aes(.data$name, .data$id)) + 
    geom_point(aes(size=.data$value, fill=.data$value), 
              stroke=0.3, shape=21) + 
    funky_theme()
}

##' create bar plot for funkyheatmap
##'
##' 
##' @title funky_bar
##' @param data data frame
##' @param cols selected columns
##' @return ggplot object
##' @importFrom ggplot2 geom_col
##' @importFrom ggplot2 geom_vline
##' @importFrom ggplot2 scale_x_continuous
##' @export
##' @author Guangchuang Yu
funky_bar <- function(data, cols) {
  d2 <- funky_data(data, cols)
  if (length(cols) == 1) {
    label = names(data)[cols]
  } else {
    label = ""
  }

  ggplot(d2, aes(.data$value, .data$id)) + 
    geom_col(aes(fill=.data$value), color='black', linewidth=0.3) + 
    funky_theme() +
    #geom_vline(xintercept = 0, linetype="dashed", linewidth=0.8) +
    geom_vline(xintercept = 1, linetype="dashed", linewidth=0.8) +
    #scale_fill_gradient(low = "#CC4C02", high = "#FFFFE5") +
    scale_x_continuous(breaks = 0.5, labels=label, expand=c(0,0)) 
}

##' create a funkyheatmap
##'
##' 
##' @title funky_heatmap
##' @param ... funky plots (e.g., outputs of `funky_point`, `funky_bar`, etc.)
##' @param data If data is provided, create a funkyheatmap from it. Otherwise, create composite plot from `...`
##' @param widths relative widths of the plots
##' @param options any ggplot component that can be added to the plots 
##' @return gglist object
##' @export
##' @author Guangchuang Yu
funky_heatmap <- function(..., data=NULL, widths = NULL, options=NULL) {

    nrow = 1
    guides = 'collect'
    output = "gglist"
    
    if (!is.null(data)) {
        pg <- list(funky_text(data), 
                funky_point(data, (1:ncol(data))+1)
                )
        if (is.null(widths)) widths <- c(1,3)
    } else {
      pg <- list(...)
    }
    
    pg <- funky_setting(pg, options)

    plot_list(gglist=pg, 
        nrow=nrow, 
        widths=widths, 
        guides=guides, 
        output=output
        )
}
