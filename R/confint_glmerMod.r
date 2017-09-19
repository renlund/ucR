#' @title \code{confint} method for class \code{glmerMod}
#' @description This is a highly crappy solution to getting the confidence
#'   intervals for the coefficients from a \code{coxme} object. It captures the
#'   output from \code{print(object)}... Oh, well.
#' @param object an object of class \code{coxme}
#' @param parm (only for method compatability)
#' @param level (only for method compatability)
#' @param ... (only for method compatability)
#' @param more set to TRUE if you also want the coefficients and p-values.
#' @author Henrik Renlund  - but don't hold it against me
#' @method confint glmerMod
#' @export

confint.glmerMod <- function(object, parm=NULL, level=0.95, ...,  more=FALSE){
   if(!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this methos")
   if(level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
   old_width <- options("width"=1000)
   foo <- utils::capture.output(summary(object))
   options("width" = old_width$width)
   a <- which(foo == "Fixed effects:") + 1
   b <- grep(pattern = "^Signif. codes:", x = foo)
   raw <- foo[a:b]
   borty <- function(x){
      y <- c()
      for(k in x) if(!k %in% c(".", "*", "**", "***", "<")) y <- c(y, k)
      y
   }
   l0 <- strsplit(x = raw, split = " +")
   l0[[1]] <- c("", "Estimate", "Std. Error", "z value", "Pr(>|z|)")
   l <- lapply(X = l0, FUN = borty)
   indx_co <- which(l[[1]] == "Estimate")
   co <- as.numeric(unlist(lapply(X = l, FUN = "[", i=indx_co))[-1])
   indx_se <- which(l[[1]] == "Std. Error")
   se <- as.numeric(unlist(lapply(X = l, FUN = "[", i=indx_se))[-1])
   if(more){
      indx_p <-  which(l[[1]] == "Pr(>|z|)")
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
