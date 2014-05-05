#' Look for a pattern in a character vector and return the hits
#' 
#' This is a modification of \code{\link{grep}} to return not only the indexes
#' of the hits (optional), but the hits themselves. 
#' 
#' @author Henrik Renlund
#' @param pattern pattern to look for
#' @param x character string to inspect
#' @param index logical; whether to also return indexes of hits (defaul FALSE)
#' @param ... arguments to be passed to \code{\link{grep}}
#' @examples
#' grepRet(pattern="a", x=names(datasets::mtcars))  
#' @seealso \code{\link{grep}}, \code{\link{grepl}}
#' @export

grepRet <- function(pattern, x, index=FALSE, ...) {
   m <- x[grep(pattern, x, ...)]
   if(index) names(m) <- grep(pattern, x, ...)
   m
}
