#' Look for a pattern in the \code{names} of R objects (typically data frames) and 
#' return the hits
#' 
#' This function applies \code{\link{grepRet}} to \code{\link{names}} of a given set
#' of R objects and returns the results as a list
#' 
#' @author Henrik Renlund
#' @param pattern pattern to look for
#' @param dfs character string to of R objects
#' @param index logical; whether to also return indexes of hits (defaul FALSE)
#' @param ... arguments to be passed to \code{\link{grep}}
#' @examples
#' require(datasets)
#' varFind(pattern="a", dfs=c("mtcars", "esoph"), index=TRUE)  
#' @seealso \code{\link{grepRet}}, \code{\link{grep}}
#' @export

varFind <- function(pattern, dfs, index=FALSE, ...) {
   L <- NULL
   class(L) <- "list"
   for(df in dfs){
      if(!is.null(x <- names(get(df)))){
         L[[df]] <- grepRet(pattern, x, index, ...)
      } else {
         warning(paste0("[varFind] Object", df, " does not have any names."))
      }
   }
   L
}
