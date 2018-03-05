##' magnitude of number
##'
##' returns the magnitude of the input. Why? No reason. Oh, ok, it is actually a
##'     helper function for \code{trunc_digits}.
##' @param x a numeric
##' @export
##' @author Henrik Renlund
magnitude <- function(x){
    x <- abs(x)
    if(x == 0) return(0)
    e <- 1
    if(x>=1){
        func <- `/`
        if(x < 10) return(1)
    } else {
        func <- `*`
    }
    e <- e * 10
    y <- func(x, e)
    while(y < 1 | y > 10){
        e <- e*10
        y <- func(x, e)
    }
    1 / func(1, e)
}

##' truncation with digits
##'
##' like \code{trunc}, but with digits specification
##' @param x a numeric
##' @param digits an integer
##' @export
##' @author Henrik Renlund
trunc_digits <- function(x, digits = 1){
    if(x == 0) return(0)
    s <- if(x>0) +1L else -1L
    x <- abs(x)
    m <- magnitude(x)
    M <- signif(m * (1/10)^(digits-1), 1)
    y <- x / M
    s * floor(y) * M
}
