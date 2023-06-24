##' @importFrom ggfun theme_blinds
funky_setting <- function(gglist, options) {
  gglist <- lapply(gglist, function(p) p + ggfun::theme_blinds(colour = c('grey90', 'white')))
  if (!is.null(options)) {
      gglist <- lapply(gglist, function(p) p + options)
  }

  return(gglist)
}


funky_fill_label <- function(data, cols) {
  fill_label <- NULL

  if (length(cols) == 1) {
    label <- names(data)[cols]
    fill_label <- labs(fill = label)
  } 

  return(fill_label)
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
funky_data <- function(data, cols, cols2 = NULL) {
    data <- funky_id(data)
    data <- data[, c(1, cols, cols2)] |>
      funky_id() 
    cols2 <- setdiff(seq_len(ncol(data)), seq_len(length(cols))+1)
    data |>
      tidyr::pivot_longer(-c(1, cols2)) |>
      dplyr::mutate(id=factor(.data$id, levels = rev(data$id)))
}

##' create text plot (i.e., rownames) for funkyheatmap
##'
##' 
##' @title funky_text
##' @param data data frame
##' @param cols selected column
##' @param hjust text alignment adjustment
##' @return ggplot object
##' @importFrom ggplot2 xlim
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 element_blank
##' @importFrom rlang .data
##' @export
##' @author Guangchuang Yu
funky_text <- function(data, cols = 1, hjust=0) {
  d <- funky_data(data, cols)  
  ggplot(d, aes(x=1, y=.data$id, label=.data$id)) + 
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
##' @param cols2 selected columns to keep names 
##' @param ... additional parameters, passing to \code{\link[ggstar]{geom_star}}
##' @return ggplot object
##' @importFrom ggplot2 geom_point
##' @importFrom ggstar geom_star
##' @export
##' @author Guangchuang Yu
funky_point <- function(data, cols, cols2 = NULL, ...) {
  d2 <- funky_data(data, cols, cols2)

  ggstar <- "ggstar"
  require(ggstar, character.only=TRUE, quietly=TRUE) 

  p <- ggplot(d2, aes(.data$name, .data$id)) #+ 
  mapping <- aes(size = .data$value, fill = .data$value)
  dots <- list(...)
  if (length(dots) >= 1){
    for (i in seq_len(length(dots))){
      if (inherits(dots[[i]], "uneval")){
        mapping <- modifyList(mapping, dots[[i]]) 
        dots[[i]] <- NULL
      }
    }
  }

  if ('stroke' %in% names(dots)){
    names(dots)[names(dots) == "stroke"] <- 'starstroke'
  }else if (!any(names(dots) %in% c('stroke', 'starstroke'))){
    dots[["starstroke"]] <- .3
  }
  if ('shape' %in% names(dots)){
    names(dots)[names(dots) == 'shape'] <- 'starshape'
  }else if ('shape' %in% names(mapping)){
    names(mapping)[names(mapping) == 'shape'] <- 'starshape'
  }else if (!any(names(dots) %in% c('shape', 'starshape')) && !any(names(dots) %in% names(mapping))){
    dots[['starshape']] <- 15
  }
  dots$mapping <- mapping
  point_layers <- do.call('geom_star', dots)
  p <- p + point_layers + funky_theme()
  # p <- p + funky_fill_label(data, cols)
  return(p)
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
    mapping <- aes(fill = .data$value)
    position <- 'stack'
  } else {
    label = "name"
    mapping <- aes(fill = .data$name)
    name.levels <- names(data)[cols]
    d2 <- d2 |> dplyr::mutate(name = factor(.data$name, levels = name.levels))
    position <- 'fill'
  }


  p <- ggplot(d2, aes(.data$value, .data$id)) + 
    #geom_col(aes(fill=.data$value), color='black', linewidth=0.3) + 
    geom_col(mapping=mapping, position=position, color='black', linewidth=0.3) +
    funky_theme() +
    #geom_vline(xintercept = 0, linetype="dashed", linewidth=0.8) +
    geom_vline(xintercept = 1, linetype="dashed", linewidth=0.8) 
    #scale_fill_gradient(low = "#CC4C02", high = "#FFFFE5") +

  if (label == "") {
    p <- p + scale_x_continuous(expand=c(0,0)) 
  } else {
    p <- p + scale_x_continuous(breaks = 0.5, labels=label, expand=c(0,0)) 
  }

  # p <- p + funky_fill_label(data, cols)
  return(p)
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
    
    if (!is.null(names(pg))) {
      options <- list(theme(plot.margin = margin(r=2)), options)
    }
    
    pg <- funky_setting(pg, options)

    plot_list(gglist=pg, 
        nrow=nrow, 
        widths=widths, 
        guides=guides, 
        output=output
        )
}
