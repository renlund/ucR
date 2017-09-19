#' @title \code{confint} method for class \code{coxme}
#' @description This is a now only semi-crappy solution to getting the
#'   confidence intervals for the coefficients from a \code{coxme} object. It
#'   extracts information from \code{print(object)} in the usual was - cool!
#' @param object an object of class \code{coxme}
#' @param parm (only for method compatability)
#' @param level (only for method compatability)
#' @param ... (only for method compatability)
#' @param more set to TRUE if you also want the coefficients and p-values.
#' @author Henrik Renlund/Lars Lindhagen - but don't hold it against us!
#' @method confint coxme
#' @export

confint.coxme <- function(object, parm=NULL, level=0.95, ..., more=FALSE){
  if(!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this method")
  if(level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
  co <- object$coef
  se <- sqrt(diag(stats::vcov(object)))
  m <- matrix(c(co - 2*se, co + 2*se), ncol=2)
  colnames(m) <- c("2.5 %", "97.5 %")
  rownames(m) <- names(co)
  if(more){
    p <- 2*stats::pnorm(abs(co/se), lower.tail=F)
    m <- cbind(m, co, p)
    rownames(m)[3:4] <- c("coef", "p")
  }
  return (m)
}
