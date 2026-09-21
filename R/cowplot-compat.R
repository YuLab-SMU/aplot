#' Convert `gglist` objects for cowplot layouts
#'
#' `cowplot::plot_grid()` and downstream helpers dispatch through
#' `cowplot::as_grob()`. Registering the method on the `gglist` class keeps the
#' compatibility with `cowplot` close to the `gglist` implementation itself.
#'
#' @param plot A `gglist` object.
#' @param device Unused graphics device argument required by `cowplot`.
#' @return A grob converted with [gglistGrob()].
#' @exportS3Method cowplot::as_grob gglist
as_grob.gglist <- function(plot, device = NULL) {
    gglistGrob(plot)
}
