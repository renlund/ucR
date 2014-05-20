#' @title An identity function that returns TRUE for confirmable equalities
#' 
#' @description This function will return true if the comparison \code{'=='} 
#' is TRUE. It will return false if \code{'=='} returns \code{FALSE} or \code{NA}.
#' Optionally it can return \code{TRUE} if both things compared are \code{NA}.
#' 
#' @author Henrik Renlund
#' @param x Numeric (or boolean) vector to be compared with \code{y}
#' @param y Numeric (or boolean) vector to be compared with \code{x}
#' @param na.equal if \code{TRUE} the comparison of \code{NA} with  \code{NA}
#' will return TRUE
#' @examples
#' x <- c(0,1,NA,2); y <- c(0,2,NA,NA)
#' ceq(x,y)
#' ceq(x,y,na.equal=TRUE)
#' @export

ceq <- function(x, y, na.equal=FALSE){
   if(na.equal){
      ifelse( 
         is.na(x) & is.na(y), TRUE,
         ifelse( 
            is.na(x) | is.na(y), FALSE,
            ifelse(
               x==y, TRUE, 
               FALSE)
         ) 
      )
   } else { 
      ifelse( 
         is.na(x) | is.na(y), FALSE,
         ifelse(
            x==y, TRUE, 
            FALSE)
      )   
   }
}
