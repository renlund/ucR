#' @title Create \code{cr_group} object
#' 
#' @description Sets the 'rgroup' and 'cgroup' attributes and returns a 
#' 'cr_group' object. These can also be set with \code{attr}, but this 
#' functions checks that the dimensionality is OK.
#' @note This functions is mainly used because of its \code{latex} method. 
#' There is also an indexing method. However, In RStudio 
#' (version Version 0.98.507) \code{'[.cr_group'} causes 
#' an unexpected message "<simpleError in ..." at times. It is unclear
#' why, as this does not appear in RConsole or RTerm.  
#' @author Henrik Renlund
#' @param x the object (typically a matrix och data frame)
#' @param rgroup the 'rgroup' attribute with length equal to \code{dim(object)[1]}
#' @param cgroup the 'cgroup' attribute with length equal to \code{dim(object)[2]}
#' @param colnames use this to set colnames (class \code{cr_group} can keep track of these under permutations (if object is 'data.frame' colnames are dropped under permuation)
#' @export


cr_group <- function(x,rgroup,cgroup,colnames){
    classy <- class(x)
    if( "cr_group" %in% classy) warning("[cr_group] 'x' is already of class 'cr_group")
    if(!missing(rgroup)){
        if(length(rgroup)==nrow(x))
            attr(x, "rgroup") <- as.character(rgroup)
        else
            warning("[cr_group] 'rgroup' is wrong length and has not been set")
    }
    if(!missing(cgroup)){
        if(length(cgroup)==ncol(x))
            attr(x, "cgroup") <- as.character(cgroup)
        else
            warning("[cr_group] 'cgroup' is wrong length and has not been set")
    }
    if(!missing(colnames)){
        if(length(colnames)==ncol(x))
            attr(x, "colnames") <- as.character(colnames)
        else
            warning("[cr_group] 'colnames' is wrong length and has not been set")
    }
    class(x) <- unique(c("cr_group",classy))
    x
}


# An unexported function that can create cr_group objects from a n*m matrix
# This is only used in 'test_cr_group.r'

CReator <- function(n=3, m=2, rg=TRUE, cg=TRUE, cn=TRUE, df=FALSE){
        M <- matrix(rpois(n*m,15), nrow=n, ncol=m)
        if(df) M <- as.data.frame(M)
        rg.code <- if(rg) "rgroup=letters[1:n]" else ""
        cr.code <- if(cg) "cgroup=LETTERS[1:m]" else ""
        cn.code <- if(cn) "colnames=sprintf('GR-%d', 1:m)" else ""
        tmp <- c(rg.code, cr.code, cn.code)
        opts_code <- tmp[tmp!=""]
        code <- paste0("y <- cr_group(M,", paste(opts_code, collapse=", ") , ")")
        eval(parse(text=code))
        y
    }



#' @title Index a 'cr_group' object
#' 
#' @description This method makes sure that attributes 'rgroup', 'cgroup' and 
#' 'colnames' are intact after permutation.
#' 
#' @note In RStudio (version Version 0.98.507) \code{'[.cr_group'} causes 
#' an unexpected message "<simpleError in ..." at times. It is unclear
#' why, as this does not appear in RConsole or RTerm.  
#' 
#'  @author Henrik Renlund
#'  @param x an 'cr_group' object
#'  @param i first index
#'  @param j second index
#'  @param ... arguments to be passed to \code{'['}
#'  @export 

'[.cr_group' <- function(x,i,j){
    class(x) <- setdiff(class(x), "cr_group")
    y <- '['(x,i,j,drop=FALSE)
    if(!is.null(rg <- attr(x, "rgroup"))){
        attr(y, "rgroup") <- rg[i]
    }
    if(!is.null(cg <- attr(x, "cgroup"))){
        attr(y, "cgroup") <- cg[j]
    }
    if(!is.null(cn <- attr(x, "colnames"))){
        attr(y, "colnames") <- cn[j]
    }
    class(y) <- c("cr_group", class(y))
    y
} 


#' @title Latex method for objects of class \code{cr_group}
#' 
#' @description Objects of class \code{cr_group} have attributes 
#' 'rgroup' and/or 'cgroup'. These are extracted for use in the 
#' \code{latex} function of package \code{Hmics}
#' 
#' @author Henrik Renlund
#' @param object an 'cr_group' object
#' @param r.perm is either (interpreted as) 'as.is', 'alphabetical' or a permutation of 
#' \code{sort(unique(attr(object, "rgroup")))}. If 'as.is' row groups are ordered
#' as they appear in 'rgroups', if 'alphabetical' they appear in alphabetical
#' order, otherwise in the permutation given.
#' @param c.perm is analogous to \code{r.perm}
#' @param colheads is an argument that if 'character' is passed to \code{latex} 
#' but if TRUE will set 'colheads' (in \code{latex}) to the objects attribute 
#' 'colnames' (if possibly, else \code{dimnames(object)[[2]]})
#' @param file is an argument passed to \code{latex} (default "")
#' @param title is an argument passed to \code{latex} (default "")
#' @param ... additional arguments passed to \code{latex}
#' @seealso \code{\link{latex}}, \code{\link{cr_group}}
#' @examples
#' nr <- 7; nc <- 5; M <- matrix(1:(nr*nc), nrow=nr, byrow=TRUE)
#' rownames(M) <- letters[1:nr]
#' colnames(M) <- LETTERS[1:nc]
#' attr(M, "rgroup") <- rep(c("foo", "bar", "baz"), length.out=nr)
#' attr(M, "cgroup") <- rep(c("Fuzzy", "Busy"), length.out=nc)
#' class(M) <- "cr_group"
#' dummy <- latex(M, r.perm='as.is', c.perm='alpha')
#' # see vignette 
#' @export

latex.cr_group <- function(object, r.perm="as.is", c.perm="as.is", colheads=TRUE, file="", title="", ...){
   if(!"cr_group" %in% class(object)){
      stop("[?] 'object' is not of class 'cr_group'.")
   }
   if(is.null(rg <- attr(object, "rgroup")) & 
         is.null(cg <- attr(object, "cgroup"))){
      stop("[?] the relevant attributes 'rgroup' and 'cgroup' are both missing.")
   }
   if(missing(colheads)) colheads <- dimnames(object)[[2]]
   if(!is.character(colheads)){
      if(is.logical(colheads)){
         if(colheads){
            if(!is.null(attr(object, "colnames"))){
               colheads <- attr(object, "colnames")
            } else {
               colheads <- dimnames(object)[[2]]
            }
         }
      }
   }
   class(object) <- setdiff(class(object), "cr_group")
   new_object <- object
   ENV <- environment()
   char_vec <- function(x, mellan="'") {
      y <- x
      for(k in seq_along(x)) y[k] <- paste0(mellan,x[k],mellan)
      paste0("c(", paste(y, collapse=", "), ")")
   }
   foo <- function(name, R.perm=r.perm, C.perm=c.perm, milieu=ENV){ 
      curr_attr <- get(name)
      if(is.null(curr_attr)){
         ""
      } else {
         if(length(curr_attr) != if(name=="rg") nrow(object) else ncol(object)){
            stop("[?] attributes need to be of the same lengths as the objects dimension")
         }
         RG <- levels(factor(curr_attr))
         perm <- if( name=="rg" ) R.perm else C.perm
         if(is.character(perm)){
            if( grepl(perm, "as.is", ignore.case=TRUE)) {
               RG <- unique(curr_attr) 
               if(perm!="as.is"){
                  perm <- "as.is"
                  message("[?] 'rperm' interpreted as 'as.is'")
               }
            } 
            if( grepl(perm, "alphabetical", ignore.case=TRUE)) {
               if(perm!="alphabetical"){
                  perm <- "alphabetical"
                  message("[?] 'perm' interpreted as 'alphabetical'")
               }
            } 
            if( !perm %in% c("alphabetical", "as.is") ){
               stop("'perm' must be (interpretable as) 'alphabetical', 'as.is' or a permuation.")
            }
         }
         if(is.numeric(perm)){ 
            if(!setequal(perm,1:length(RG))){
               stop("[?] 'perm' needs to be a permuation of the indexes of 'levels(rgroup)'")
            }
            RG <- RG[perm] 
         }
         frg <- factor(curr_attr, levels=RG)
         X <- get(x="new_object", envir=milieu)
         assign(
            x="new_object", 
            value = if(name=="rg") X[order(frg),] else X[,order(frg)], 
            envir=milieu
            )
         pref <- if(name=="rg") "r" else "c"
         paste0(pref,"group = ", char_vec(RG),", 
      n.",pref,"group = ",char_vec(rle(as.character(sort(frg)))$lengths, ""),",
      ")
      }
   }
   row_code <- foo(name="rg")
   col_code <- foo(name="cg")
   code <- paste0("latex(
      object=new_object, 
      ", 
      row_code, 
      col_code,
      "title=title,
      file=file,
      colheads=colheads,
      ...
   )")
   eval(parse(text=code))
}
