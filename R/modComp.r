#' @title Compare covariate influence
#' @description  Compare different subsets of covariates within a model
#' @param resp the response variable
#' @param vars character vector containing the names of all variables of interest
#' @param model name (as character) of the model
#' @param covars a list containing index vectors on \code{vars}. Each entry corresponds to an analysis.
#' @param data location of the variables in \code{vars}
#' @param uni should univariate analyses be performed (if integer, this is used as an index on \code{vars} to determine which univariate analyses)
#' @param ci if TRUE confidence intervals are included
#' @param ... arguments passed to \code{transFormat}
#' @author Henrik Renlund
#' @export

modComp <- function(resp, vars, model, covars, data=NULL, uni=TRUE, ci=TRUE, ...){
   n <- length(vars)
   if(is.logical(uni)) if(uni) {
      uni_filt <- 1:n
   }
   if(is.numeric(uni)){
      uni_filt <- uni
      uni <- TRUE
   }
   kol <- length(covars) + if(uni) 1 else 0
   DF <- if(!is.null(data)) data else .GlobalEnv
   v_n <- rep(1, n)
   rnames <- c()
   for(k in 1:n){
      tmp <- get(x=vars[k], DF)
      if(is.numeric(tmp)) {
         rnames <- c(rnames, vars[k])
         next}
      if(is.factor(tmp))    lev <- levels(tmp)
      if(is.character(tmp)) lev <- sort(unique(tmp))
      v_n[k] <- length(lev) - 1
      tmp1 <- gsub(vars[k],"", lev[-1])
      tmp2a <- paste(vars[k], tmp1[1])
      tmp2b <- if(v_n[k]>1) paste( paste(rep(" ", nchar(vars[k])),collapse="" ), tmp1[-1])
      tmp2 <- c(tmp2a, tmp2b)
      rnames <- c(rnames, tmp2)
   }
   v_indx <- c(1,cumsum(v_n)[-n]+1)
   rad <- sum(v_n)
   M <- matrix(NA, nrow=rad, ncol=kol)
   rownames(M) <- rnames
   colnames(M) <- c(if(uni) "Univariate" else NULL , sprintf("Model %d", 1:length(covars)))
   for(k in 1:kol){ # k = 1
      indx <- if(uni & k==1) 1 else if(uni) k-1 else k 
      if(uni & k==1){
         for(i in uni_filt){
            vs <- vars[i]
            form <- formula(paste0(resp ," ~ ", paste(vs, collapse=" + ")))
            MOD <- if(is.data.frame(DF)) model(form, data=DF) else model(form)
            look <- paste0("(", vs, ")", collapse="|")
            coef_mod <- coef(MOD)
            coef_indx <- grepl(look, names(coef_mod))
            COEF <- gsub(" ", "", transFormat(coef_mod[coef_indx], ...))
            if(ci){
               ci_mod <- confint(MOD)
               ci_indx <- grepl(look, rownames(ci_mod))
               CI <- confint(MOD)[ci_indx,]
               COEF <- paste(COEF, klister(CI, ...))
            }
            filt <- which(vars %in% vs)
            M[spann(v_indx[filt], v_n[filt]),indx] <- COEF
         }
      } else {
         vs <- vars[ sort(unique(covars[[indx]])) ]
         form <- formula(paste0(resp ," ~ ", paste(vs, collapse=" + ")))
         MOD <- if(is.data.frame(DF)) model(form, data=DF) else model(form)
         look <- paste0("(", vs, ")", collapse="|")
         coef_mod <- coef(MOD)
         coef_indx <- grepl(look, names(coef_mod))
         COEF <- gsub(" ", "", transFormat(coef_mod[coef_indx], ...))
         if(ci){
            ci_mod <- confint(MOD)
            ci_indx <- grepl(look, rownames(ci_mod))
            CI <- confint(MOD)[ci_indx,]
            COEF <- paste(COEF, klister(CI, ...))
         }
         filt <- which(vars %in% vs)
         M[spann(v_indx[filt], v_n[filt]),k] <- COEF
      }
   }
   M
}

#' @title Transform and format###############################################
#' @description Apply a function to x, then \code{round} or \code{signif} before formatting
#' @param x the object
#' @param fun the function
#' @param signif if non-NULL this is the 'digits' argument for \code{signif}
#' @param round if non-NULL this is the 'digits' argument for \code{round}
#' @param skip if TRUE \code{format} is skipped
#' @param ... arguments passed to \code{format}

transFormat <- function(x, fun=NULL, signif=NULL, round=NULL, skip=FALSE, ...){
   if(is.null(skip)) skip <- FALSE # so that transFormat can be called with skip = empty list entry
   if(is.null(fun)) fun <- identity
   if(is.null(y <- tryCatch(fun(x), error=function(e) NULL))){
      warning("[transFormat] 'fun' not applicable to 'x' and is set to 'identity'")
   } else {
      x <- y
   }
   if(!is.null(signif) & !is.null(round)) stop("[transFormat] 'signif' and 'round' should not be non-null simultaneous.")
   if(!is.null(signif)) x <- signif(x, signif)
   if(!is.null(round)) x <- round(x, round)
   if(!skip) x <- format(x, ...)
   x
}

# - # @title klister  #########################################################
# - # @description Glue (collapse) the rows of a matrix into a character vector
# - # @param M matrix to be collapsed
# - # @param start start of character entry
# - # @param stopp end of character entry
# - # @param sep separator of matrix entries when collapsed
# - # @param ... arguments passed to \code{transFormat}

klister <- function(M, start="(", stopp=")", sep=",", ...){
   if(is.null(nrow(M))){
      kli <- paste0(start, paste0(transFormat(M,...), collapse=sep), stopp)
   } else {
      kli <- rep(NA,nrow(M))
      for(i in 1:nrow(M)){
         kli[i] <- paste0(start, paste0(transFormat(M[i,],...), collapse=sep), stopp)
      }
   }
   gsub(" ","",kli)
}

# - # @title Spann ###########################################################
# - # @description From each point in \code{start}, extend by the corresponding point in \code{length}
# - # @param start integer vector
# - # @param length integer vector

spann <- function(start, length) {
   s <- c()
   for(k in seq_along(start)){
      s <- c(s, start[k]:(start[k]+length[k]-1))
   }
   if(!setequal(s,unique(s))) warning("[modComp\\spann] spann contains multiplicities")
   s
}

# tests
if(FALSE){
   foo <- function(x=0, ...){
      x + bar(...) + baz(...)
   }
   bar <- function(y=0, ...) if(y==0) 3 else -3
   baz <- function(z=0, ...) if(z==0) 10 else -10
   foo()
   foo(1)
   foo(y=1)
   foo(z=1)
   foo(y=1,z=1)
   foo(z=1,y=1)
   foo(z=1,y=1,100)
   foo(100,1,1)
   foo(100,1)
}

if(FALSE){
   bar <- function(x) mean(x)
   bar(1:2)
   bar(1:2, 1) # Error
   
   baz <- function(y,z=0) '+'(y,z)
   baz(1)
   baz(1,2)
   baz(1,2,3) # Error
   
   foo <- function(...){
      ret <- rep(NA,2)
      L <- list(...)
      ret[1] <- bar(x=L[['x']])
      ret[2] <- baz(y=L[['y']])
      ret
   }
   foo(x=1:2,y=0) 
   
}