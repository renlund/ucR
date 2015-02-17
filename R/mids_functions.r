#' @title Extract imputed data sets
#' @description Extract imputed data sets from a \code{mids} object
#' @param object a mids object
#' @param imp.only do you want only the rows with
#' @author Henrik Renlund
#' @export

mids_get <- function(object, m){
   if(!"mids" %in% class(object)) warning("[mids_get] object is not of class 'mids'")
   DF <- object$data
   for(K in names(object$imp)){
      indx <- is.na(DF[[K]])
      if(!any(indx)) next
      DF[[K]][indx] <- object$imp[[K]][[m]]
   }
   DF
}

#' @title Subset imputed data sets
#' @description Subset imputed data sets from a \code{mids} object
#' @param object a mids object
#' @param index the rows you want (can be a logical expression)
#' @author Henrik Renlund
#' @export

mids_subset <- function(object, index){
   if(!"mids" %in% class(object)) warning("[mids_subset] object is not of class 'mids'")
   if(is.logical(index)) index <- which(index)
   new_object <- object
   new_object$data <- object$data[index, ]
   for(K in names(object$imp)){ # K = names(object$imp)[2]
      tmp <- object$imp[[K]]
      if(is.null(tmp)) next
      replacer <- tmp[rownames(tmp) %in% as.character(index),]
      if(nrow(replacer)==0){
         new_object$imp[[K]] <- NULL
      } else {
         new_object$imp[[K]] <- replacer
      }
   }
   class(new_object) <- c("mids_subset", class(object))
   new_object
}

#' @title Predict on imputed data
#' @description Predict from an object (model) on a \code{mids} object.
#' @note It would be nice to include the equivalent of parameter \code{se.fit}
#' from other prediction methods.
#' @param object prediction object (=model, typically)
#' @param mids mids object to be predicted on.
#' @param ... arguments passed to predict
#' @author Henrik Renlund
#' @export

mids_predict_on <- function(object, mids, ...){
    if(!"mids" %in% class(mids)) warning("[mids_predict_on] object class not 'mids'")
    m <- mids$m
    temp <- matrix(NA_real_, nrow=nrow(mids$data), ncol=m)
    for(index in 1:m){
        temp[,index] <- predict(object = object, newdata = mids_get(object = mids, m = index), ...)
    }
    rowMeans(temp)
}

#' @title Predict from imputed data
#' @description Predict from imputed data sets from a \code{mids} object and a
#' logistic regerssion model
#' @note It would be nice to include the equivalent of parameter \code{se.fit}
#' from other prediction methods.
#' @param object a mids object
#' @param formula formula for the logistic regression
#' @param newdata data for which predictions are wanted
#' @author Henrik Renlund
#' @export

mids_predict_logreg <- function(object, formula, newdata){
   if(!"mids" %in% class(object)) warning("[mids_predict_logreg] object class not 'mids'")
   prob_function <- function(x) exp(x)/(1+exp(x))
   code <- paste0("glm(", formula, ", family='binomial')")
   eval(parse(text=paste0("mira <- with(object, ",code,")")))
   mipo <- pool(object = mira)
   coef <- mipo$qbar
   mod_mat <- model.matrix(object = as.formula(formula), data = newdata) # ???
   fail <- "[mids_predict_logreg] something is wrong ... "
   tryCatch(expr = sum(mod_mat[1,] * coef), warning = function(w) stop(fail), error = function(e) stop(fail))
   S <- rep(0, nrow(mod_mat))
   for(k in 1:ncol(mod_mat[,,drop=FALSE])){ # k = 1
      S <- S + coef[k] * mod_mat[, k]
   }
   ret <- data.frame(
      "linear_pred" = rep(NA_real_, nrow(newdata)),
      "prob_pred" = rep(NA_real_, nrow(newdata))
   )
   rownames(ret) <- rownames(newdata)
   index1 <- rownames(newdata)
   index2 <- rownames(mod_mat)

   ret$linear_pred[index1 %in% index2] <- S
   ret$prob_pred[index1 %in% index2] <- prob_function(S)
   ret
}

#' @title Predict from imputed data on imputed data
#' @description Predict from imputed data sets from a \code{mids} object and a
#' logistic regerssion model
#' @param object a mids object
#' @param formula formula for the logistic regression
#' @param new_data data for which predictions are wanted
#' @note It would be very nice to be able to include \code{se.fit}...
#' @author Henrik Renlund
#' @export

mids_predict_logreg_2 <- function(object, formula, newmids){
   if(!"mids" %in% class(object)) warning("[mids_predict_logreg_2] object class not 'mids'")
   if(!"mids" %in% class(newmids)) warning("[mids_predict_logreg_2] object class not 'mids'")
   prob_function <- function(x) exp(x)/(1+exp(x))
   m <- newmids$m
   r <- nrow(newmids$data)
   linear <- matrix(NA_real_, nrow=r, ncol=m)
   probs <- matrix(NA_real_, nrow=r, ncol=m)
   for(k in 1:m){
      tmp <- mids_predict_logreg(object = object,
                                formula = formula,
                                newdata = mids_get(object = newmids, m=k))
      linear[,k] <- tmp$linear_pred
   }
   linear_pred <- rowMeans(linear)
   data.frame(
      linear_pred = linear_pred,
      prob_pred = prob_function(linear_pred)
      )
}

if(FALSE){
   df <- gimme_some_data(1000)
   mdf <- mice(df)
   df2 <- gimme_some_data(100)
   mdf2 <- mice(df2)
   cbind(
      mids_predict_logreg_2(mdf, "y~x+z", mdf2),
      mids_predict_logreg(mdf, "y~x+z", df2)
   )

}

#' @title Describe an imputed data set
#' @description Describe complete case, missing and imputed via ucr.base.tab
#' @param object a mids object
#' @param file file to write latex output to (empty string by default)
#' @param ... arguments passed to \code{Hmisc::latex}
#' @author Henrik Renlund
#' @export

mids_describe <- function(object, file="", ...){
   if(!"mids" %in% class(object)) warning("[mids_describe] object class not 'mids'")
   raw <- object$data
   while((miss_ind <- paste0(sample(c(letters,LETTERS), 10), collapse="")) %in% names(raw)){
      "Foo" #
   }
   raw[[miss_ind]] <- factor(ifelse(complete.cases(raw), "Complete", "Missing"))
   bt <- ucr.base.tab(data = raw, group.name = miss_ind, include.p = FALSE, include.combined = FALSE, show.missing = "in.row", include.n = FALSE)
   bt_var <- bt$tab[,1]
   Imputation <- rep(NA_character_, length(bt_var))
   place <- function(s, x = bt$tab){
      indx <- which(x %in% s)
      if(length(indx) == 1) return(indx)
      indx <- which(substr(x, 1, length(s)) %in% s)
      if(length(indx) == 1) return(indx)
      stop("[mids_describe] cannot find variable location... (ish)")
   }
   for(K in names(object$imp)){ # K = names(object$imp)[3]
      temp_var <- object$imp[[K]]
      if(is.null(temp_var)) next
      if(is.numeric(raw[[K]])){
         indx <- place(K)
         tmp <- as.numeric(as.matrix(object$imp[[K]]))
         Imputation[indx] <- paste0(median(tmp), " (", paste(quantile(tmp, probs=c(0.27,0.75)), collapse=" - "), ")")
      }
      if(is.factor(raw[[K]]) | is.character(raw[[K]])){
         indx <- place(K)
         tmp <- as.character(as.matrix(object$imp[[K]]))
         if(is.factor(raw[[K]])) tmp <- factor(tmp, levels=levels(raw[[K]]))
         tmp_tab <- table(tmp) / length(tmp)
         Imputation[indx:(indx+length(tmp_tab)-1)] <- paste0(100*tmp_tab, "%")
      }
   }
   bt$tab <- cbind(bt$tab, "Imputed values" = Imputation)
   bt$extra.col.heads <- c(bt$extra.col.heads, " ")
   latex(bt, file = file, ...)
}

# # For test purposes

gimme_some_data <- function(n=1000){
   z <- factor(sample(LETTERS[1:5], n, TRUE), levels=LETTERS[1:5])
   x <- round(as.numeric(z) / (1+rbinom(n, 2, 0.5)) + runif(n, 0, 3.7), 1)
   foo <- exp(1-ifelse(z %in% LETTERS[1:2], 0, ifelse(z %in% LETTERS[c(3,5)], 2, 5)) + 0.4*x)
   y <- rbinom(n, 1, prob =  (1-foo/(1+foo))/2)
   df <- data.frame(y=y, x=x, z=z)
   df$x[sample(1:n, size = max(0.09*n, 3), FALSE)] <- NA
   df$z[sample(1:n, size = max(0.04*n, 4), FALSE)] <- NA
   df
}

if(FALSE){
   if(!require(mice)) stop("! need 'mice'")
   df <- gimme_some_data(50)
   object <- mice(df)
   mids_get(object, 1)
   index <- 1:25
   sobject <- mids_subset(object, index)
   class(sobject)
   str(sobject$data)
   rownames(sobject$imp$x)
   rownames(object$imp$x)
   rownames(sobject$imp$z)
   rownames(object$imp$z)
}
