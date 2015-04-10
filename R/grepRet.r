#' @title Look for a pattern in a character vector and return the hits
#'
#' @description This is a wrapper for \code{\link{grep}} to return not only the indexes
#' of the hits (optional), but the hits themselves. (This is mainly a helper function
#' for \code{varFind} but can at times be useful.)
#'
#'
#'
#' @author Henrik Renlund
#' @param pattern pattern to look for
#' @param x character string to inspect
#' @param index logical; whether to also return indexes of hits (defaul FALSE)
#' @param ... arguments to be passed to \code{\link{grep}}
#' @examples
#' grepRet(pattern="a", x=names(datasets::mtcars))
#' @seealso \code{\link{grep}}, \code{\link{grepl}}
#' @note This function will be \code{grepr} in the 'dataman' package.
#' (It will be eventually be removed so as to not clutter the 'ucR' package too
#' much, but I don't want to break to much code in current use!)
#' @export

grepRet <- function(pattern, x, index=FALSE, ...) {
   m <- x[grep(pattern, x, ...)]
   if(index) names(m) <- grep(pattern, x, ...)
   if(length(m)==0) NULL else m
}
