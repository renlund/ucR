##' sensitivity and specificity
##'
##' calculate the sensitivity and specificity from a grouping and a measurement
##' @param status the grouping of diagnosis
##' @param measure a predictive value
##' @param data a data frame or similar
##' @param disease.value if status is not 0/1 specify the value indicating a
##'     positive diagnosis
##' @param direction if 1 then a higher value is more indicative of a positive
##'     diagnosis, anything the other way around
##' @export
sens_spec <- function(status, measure, data = NULL,
                      disease.value = 1, direction = 1){
    the_status <- status
    if(stat_char <- is.character(status) & length(status) == 1){
        if(is.null(data)) stop("need 'data'")
        the_status <- data[[status]]
    }
    m <- measure
    if(meas_char <- is.character(measure) & length(measure) == 1){
        if(is.null(data)) stop("need 'data'")
        m <- data[[measure]]
    }
    y <- ifelse(the_status == disease.value, 1, 0)
    n_sick <- sum(y==1)
    n_unsick <- sum(y==0)
    n <- length(m)
    sens <- rep(NA_real_, n)
    spec <- rep(NA_real_, n)
    if(direction == 1){
        for(k in 1:n){
            sens[k] <- sum(y==1 & m >= m[k]) / n_sick
            spec[k] <- sum(y==0 & m <  m[k]) / n_unsick
        }
    } else {
        for(k in 1:n){
        sens[k] <- sum(y==1 & m <= m[k]) / n_sick
        spec[k] <- sum(y==0 & m >  m[k]) / n_unsick
        }
    }
    R <- data.frame(
        status = the_status,
        measure = m,
        sens = sens,
        spec = spec
    )
    if(stat_char){
       if(!stat_char %in% names(R)) names(R)[1] <- status
    }
    if(meas_char){
       if(!meas_char %in% names(R)) names(R)[2] <- measure
    }
    R
}


if(FALSE){

    n <- 500
    test <- data.frame(
        foo = rbinom(n = n, size = 1, prob = 0.4)
    )
    test$bar <- rexp(n = n, rate = 1/(test$foo+1))
    boxplot(bar~foo, data = test)
    ss <- sens_spec("foo", "bar", test)
    plot(x = 1-ss$spec, y = ss$sens,
         xlab = "1-Specificity", ylab = "Sensitivity")
    abline(a=0, b=1)

    sens_spec(test$foo, test$bar)

    test$baz <- rexp(n = n, rate = 1/(ifelse(test$foo == 0, 2, 1)))
    boxplot(baz~foo, data = test)
    ss2 <- sens_spec("foo", "baz", test, direction = 0)
    plot(x = 1-ss2$spec, y = ss2$sens,
         xlab = "1-Specificity", ylab = "Sensitivity")
    abline(a=0, b=1)

}
