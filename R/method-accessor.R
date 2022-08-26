##' @method [[ aplot
##' @export
`[[.aplot` <- function(x, i){
    x$plotlist[[i]]
}

##' @method [ aplot
##' @export
`[.aplot` <- function(x, i, j, ...){
    x[[x$layout[i, j]]]
}

##' @method [[<- aplot
##' @export
`[[<-.aplot` <- function(x, i, value){
    if(!inherits(value, 'ggplot')){
        stop('The value should be a ggplot object.')
    }
    x$plotlist[[i]] <- value
    return(x)
}


##' @method [<- aplot
##' @export
`[<-.aplot` <- function(x, i, j, value){
    if (!inherits(value, 'ggplot')){
        stop('The value should be a ggplot object.')
    }else if (is.na(x$layout[i, j])){
        stop(paste0('The subplot which local in row ', i, 
                    ' and col ', j, 
                    ' is NULL, it can not be replaced.'))
    }
    x[[x$layout[i, j]]] <- value
    return(x)
}



