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
#' @examples 
#' # Comparing two set of covariates in model 'lm'
#' DF <- data.frame(x1=c(1,2,3,4),x2=c(3,4,0,1))
#' DF$y <- 2*DF$x1 + DF$x2 + c(0.1, -0.2, 0.05,0.05)
#' modComp(
#'    resp = "y", 
#'    vars = c("x1", "x2"), 
#'    model = lm, 
#'    covars = list(1, 1:2), 
#'    data = DF, 
#'    uni = FALSE, 
#'    ci = TRUE, 
#'    round=2
#'    )
#' # Comparing different covariates in model 'coxph'
#' library(survival)
#' DF <- data.frame(
#'    x = c(3,1,2,3,2,4,5,6,4,5,3,2,4,1,1,2,3,4,6,7,8,1,1,2,6,4,2,1,1,3,4),
#'    y = c(0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,0),
#'    z =  rep(letters[1:2], length.out=31),
#'    u = rep(c(1:5), length.out=31)
#' )
#' cox_endp <- with(DF, Surv(x,y))
#' modComp(resp = "cox_endp", 
#'    vars = c("z", "u"), 
#'    model=coxph,
#'    covars=list(1:2), 
#'    data = DF, 
#'    uni=TRUE,
#'    ci=FALSE,
#'    round=1,
#'    fun=exp
#' )
#' # Comparing different covariates in model 'glm'
#' # NOTE: must incorporate the argument "family='binomial'" by defining a function such that this is true
#' Model <- function(formula, data) glm(formula=formula, family="binomial",data=data)
#' modComp(resp = "y", 
#'    vars = c("x", "z"), 
#'    model=Model,
#'    covars=list(1:2, 2), 
#'    data = DF, 
#'    uni=TRUE,
#'    ci=FALSE,
#'    signif=3,
#'    fun=exp
#' )
#' rm(Model, cox_endp, DF)
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

#' @title Transform and format
#' @description Apply a function to x, then \code{round} or \code{signif} before formatting
#' @param x the object
#' @param fun the function
#' @param signif if non-NULL this is the 'digits' argument for \code{signif}
#' @param round if non-NULL this is the 'digits' argument for \code{round}
#' @param skip if TRUE \code{format} is skipped
#' @param ... arguments passed to \code{format}

transFormat <- function(x, fun=NULL, signif=NULL, round=NULL, skip=FALSE, ...){
   if(is.null(skip)) skip <- FALSE # ? consistent with the other arguments?
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
