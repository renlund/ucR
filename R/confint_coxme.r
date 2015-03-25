#' @title \code{confint} method for class \code{coxme}
#' @description This is a outstandingly crappy solution to getting the
#'   confidence intervals for the coefficients from a \code{coxme} object. It
#'   captures the output from \code{print(object)}... Oh, well.
#' @param object an object of class \code{coxme}
#' @param parm (only for method compatability)
#' @param level (only for method compatability)
#' @param ... (only for method compatability)
#' @param more set to TRUE if you also want the coefficients and p-values.
#' @author Henrik Renlund - but don't hold it against me
#' @method confint coxme
#' @export

confint.coxme <- function(object, parm=NULL, level=0.95, ..., more=FALSE){
   if(!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this methos")
   if(level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
   old_width <- options("width"=1000)
   foo <- capture.output(object)
   options("width" = old_width$width)
   a <- which(foo == "Fixed coefficients") + 1
   b <- which(foo == "Random effects") - 2
   raw <- foo[a:b]
   l <- strsplit(x = raw, split = " +")
   indx_co <- which(l[[1]] == "coef")
   co <- as.numeric(unlist(lapply(X = l, FUN = "[", i=indx_co))[-1])
   indx_se <- which(l[[1]] == "se(coef)")
   se <- as.numeric(unlist(lapply(X = l, FUN = "[", i=indx_se))[-1])
   if(more){
      indx_p <-  which(l[[1]] == "p")
      ps <- as.numeric(unlist(lapply(X = l, FUN = "[", i=indx_p))[-1])
      m <- matrix(c(co - 2*se, co + 2*se, co, ps), ncol=4)
      colnames(m) <- c("2.5 %", "97.5 %", "coef", "p")
      rownames(m) <- unlist(lapply(X = l, FUN = "[", i=1))[-1]
   } else {
      m <- matrix(c(co - 2*se, co + 2*se), ncol=2)
      colnames(m) <- c("2.5 %", "97.5 %")
      rownames(m) <- unlist(lapply(X = l, FUN = "[", i=1))[-1]
   }
   m
}
