#' @title Look for a pattern in the \code{names} of R objects (typically data frames) and 
#' return the hits
#' 
#' @description This function applies \code{\link{grepRet}} to \code{\link{names}} of a set
#' of R objects and returns the results as a list
#' 
#' @author Henrik Renlund
#' @param pattern pattern to look for
#' @param dfs logical or character string. If a character vector of the names of 
#' R objects is given, the \code{names} of these will be exmined with \code{grepRet}
#' ; if \code{TRUE} all data frames in the global environment will be examined and if
#' \code{FALSE} all objects with a 'names' attribute will be examined. 
#' @param index logical; whether to also return indexes of hits (defaul FALSE)
#' @param ... arguments to be passed to \code{\link{grep}}
#' @examples
#' require(datasets)
#' varFind(pattern="a", dfs=c("mtcars", "esoph"), index=TRUE)  
#' @seealso \code{\link{grepRet}}, \code{\link{grep}}
#' @export

varFind <- function(pattern, dfs=TRUE,  index=FALSE, ...) {
   if(is.logical(dfs)){
      tmp <- c()
      if(dfs) {
         for(K in ls(.GlobalEnv)) 
            if("data.frame" %in% class(get(K))) tmp <- c(K, tmp)
      } else {
         for(K in ls(.GlobalEnv)) 
            if(!is.null(dummy <- attr(get(K), "names"))) tmp <- c(K, tmp)
      }
      dfs <- tmp
   }
   L <- NULL
   class(L) <- "list"
   for(df in dfs){
      if(!is.null(var.names <- names(get(df)))){
         L[[df]] <- grepRet(pattern, var.names, index, ...)
      } else {
         warning(paste0("[varFind] Object ", df, " does not have any names."))
      }
   }
   L
}
