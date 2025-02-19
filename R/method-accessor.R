##' @method [[ aplot
##' @export
`[[.aplot` <- function(x, i){
    if(inherits(i, "numeric")){
       return(x$plotlist[[i]])
    }
    NextMethod()
}

##' @method [ aplot
##' @export
`[.aplot` <- function(x, i, j, ...){
    if (inherits(i, "numeric") && inherits(i, "numeric")){
       return(x[[x$layout[i, j]]])
    }
    NextMethod()
}

##' @method [[<- aplot
##' @export
`[[<-.aplot` <- function(x, i, value){
    if (inherits(i, "numeric")){
       if(!inherits(value, 'ggplot')){
           stop('The value should be a ggplot object.')
       }
       
       x$plotlist[[i]] <- value
       return(x)
    }
    x <- NextMethod(value)
    return(x)
}


##' @method [<- aplot
##' @export
`[<-.aplot` <- function(x, i, j, value){
    if (inherits(i, "numeric") && inherits(i, "numeric")){
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
    x <- NextMethod(value)
    return(x)
}



