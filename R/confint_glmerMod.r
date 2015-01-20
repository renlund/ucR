#' @title \code{confint} method for class \code{glmerMod}
#' @description This is a highly crappy solution to getting the confidence intervals
#' for the coefficients from a \code{coxme} object. It captures the output from
#' \code{print(object)}... Oh, well.
#' @param object an onbject of class \code{coxme}
#' @param more set to TRUE if you also want the coefficients and p --values.
#' @author Henrik Renlund
#' @export

confint.glmerMod <- function(object, more=FALSE){
   foo <- capture.output(summary(object))
   a <- which(foo == "Fixed effects:") + 1
   b <- which(foo == "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1") - 2
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
