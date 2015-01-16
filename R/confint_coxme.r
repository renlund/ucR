#' @title \code{confint} method for class \code{coxme}
#' @description This is a crappy solution to getting the confidence intervals
#' for the coefficients from a \code{coxme} object. It captures the output from
#' \code{print(object)}... Oh, well.
#' @param object an onbject of class \code{coxme}
#' @author Henrik Renlund
#' @export

confint.coxme <- function(object){
   # CRAPPY SOLUTION!
   foo <- capture.output(object)
   a <- which(foo == "Fixed coefficients") + 1
   b <- which(foo == "Random effects") - 2
   raw <- foo[a:b]
   l <- strsplit(x = raw, split = " +")
   indx_co <- which(l[[1]] == "coef")
   co <- as.numeric(unlist(lapply(X = l, FUN = "[", i=indx_co))[-1])
   indx_se <- which(l[[1]] == "se(coef)")
   se <- as.numeric(unlist(lapply(X = l, FUN = "[", i=indx_se))[-1])
   m <- matrix(c(se - 2*se, se + 2*se), ncol=2)
   colnames(m) <- c("2.5 %", "97.5 %")
   rownames(m) <- unlist(lapply(X = l, FUN = "[", i=1))[-1]
   m
}