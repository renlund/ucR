##' @title Print a ucr.base.tab object
##' @description Format the object with base_tab_exp before printing
##' @param object A ucr.base.tab object or the 'tab' component of that list.
##' @param replacer Replace the LaTeX in first column with this
##' @author Henrik Renlund
##' @export

print.ucr.base.tab <- function(x, replacer = " -:- ", ...){
   y <- base_tab_exp(x, replacer = replacer)
   print(y, ...)
}