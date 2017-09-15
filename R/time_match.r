##' find matches on variables in a given time period
##'
##' in a dataset with one or more variables (typically containing text or codes)
##'     associated with a date, find matches on those variables within specifed
##'     time frames
##' @param data a data frame
##' @param s a search string (regular expression)
##' @param search names of variables to search in (given in order of importance)
##' @param id name of id variable
##' @param date name of associated date variable
##' @param begin date (or vector of dates) or the name of a date variable
##' @param end date (or vector of dates) or the name of a date variable
##' @param matches specification of what to return; 'all' for all matches,
##'     'first.date' to filter on the first occurence for any individual and
##'     date, and 'first.id' to filter on the first occurence of any individual
##' @param non.matches if \code{TRUE} also return data on the individuals with
##'     no matches
##' @return a data frame with \itemize{
##'  \item id id variable
##'  \item event 1 of match found (else 0 if any and \code{non.matches = TRUE})
##'  \item t days from 'begin' to 'date'
##'  \item match the match found
##'  \item variable match found in this variable
##'  \item t2 days from 'date' to end
##'  \item first.id indicator for first occurence of associted id
##'  \item first.date indicator for first occurence of associated id and date
##'  \item begin the begin variable
##'  \item date the date variable
##'  \item end the end variable
##' }
##' @author Henrik Renlund
##' @export
time_match <- function(data, s, search,
                id = 'lopnr', date = 'INDATUMA',
                begin = NULL, end = NULL,
                matches = "all", non.matches = TRUE){
    ## -- some sanity checks --
    ok.matches <- c("first.id", "first.date", "all")
    if(!matches %in% ok.matches){
        stop("'matches' must be one of: ", paste(ok.matches, collapse = ", "))
    }
    nm <- c(id, date, search)
    badname.indx <- which(!nm %in% names(data))
    if(length(badname.indx) > 0){
        stop("some variable names (", paste(nm[badname.indx], collapse = ", "),
                ") does not exist in the data set")
    }
    ## -- get relevant data and fix 'begin' and 'end' if not provided
    if(!is.null(begin) & is.character(begin)){
        begin <- data[[begin]]
        if(is.null(begin)){
            warning("is 'begin' really a variable in data?\n",
                    "it will be ignored!")
        }
    }
    if(!is.null(end) & is.character(end)){
        end <- data[[end]]
        if(is.null(end)){
            warning("is 'end' really a variable in data?\n",
                    "it will be ignored!")
        }
    }
    data <- data[, nm]
    names(data) <- c("id", "date", search)
    if(is.null(end)) end <- max(data$date, na.rm = TRUE)
    if(is.null(begin)) begin <- min(data$date, na.rm = TRUE)
    data$begin <- begin
    data$end <- end
    ## -- hard to deal with missing in 'date', so if so throw
    ## -- error and let user fix this
    na.indx <- which(is.null(data$date))
    if(length(na.indx) > 0){
        warning("Missing 'date' at rows:", paste0(na.indx, collapse = ", "),
                "\nThese will be removed\n")
        data <- data[!na.indx, ]
    }
    ## -- calculate 't', time from beginning, and 't2', time to end
    data$t  <- as.numeric(difftime(data$date, data$begin, units = "days"))
    data$t2 <- as.numeric(difftime(data$end, data$date, units = "days"))
    ## -- filter down to relevant time period but keep copy of data for later
    data.copy <- subset(data, TRUE, select = c("id", "begin", "end"))
    data <- data[data$date >= data$begin & data$date <= end, ]
    ## -- look for patters 's' in each search variable
    R <- NULL
    for(K in search){ ##  K <- search[1] ## for testing
        g <- grepl(pattern = s, x = data[[K]])
        if(sum(g) == 0) next
        tmp <- data[g, ]
        tmp$variable <- factor(K, levels = search)
        tmp$match <- tmp[[K]]
        tmp$event <- 1L
        R <- if(is.null(R)) tmp else rbind(R, tmp)
    }
    ## -- order matches and create indicators for first id and first date
    ## -- if there are no matches, object R is still NULL (treat separately)
    vars <- c("id", "event", "t", "match", "variable", "t2",
              "first.id", "first.date", "begin", "date", "end")
    if(is.null(R)){
        S <- as.data.frame(matrix(vector(), nrow = 0, ncol = length(vars),
                           dimnames = list(c(), vars)))
    } else {
        S <- R[order(R$id, R$date, R$variable), ]
        n <- nrow(S)
        ## -- create indicators for first instance of each id and first instance
        ## -- of each id + date combination. This code must treat the case of
        ## -- length 1 vectors separately
        if(n > 1){
            S$first.id <- as.integer(c(TRUE, !(S$id[2:n] == S$id[1:(n-1)])))
            S$first.date <- as.integer(
                c(TRUE, !(S$id[2:n] == S$id[1:(n-1)] &
                          S$date[2:n] == S$date[1:(n-1)]))
            )
        } else {
            S$first.id <- 1L
            S$first.date <- 1L
        }
    }
    ## -- look at individuals without any matches
    TMP <- subset(data.copy, !id %in% S$id)
    ## -- create the object to return based on arguments given
    RET <- if(non.matches & nrow(TMP) > 0){
               n <- nrow(TMP)
               TMP <- TMP[order(TMP$id), ]
               ## -- keep only one line for each id
               if(n > 1){
                   B <- TMP[c(TRUE, !TMP$id[2:n] == TMP$id[1:(n-1)]), ]
               } else {
                   B <- TMP
               }
               B$event <- 0L
               B$t <- as.numeric(difftime(B$end, B$begin))
               B$match <- NA_character_
               B$variable <- NA_character_
               B$t2 <- 0
               B$first.id <- 1L
               B$first.date <- 1L
               B$date <- B$end
               rbind(S[, vars], B[, vars])
           } else {
               S[, vars]
           }
    if(matches == "all"){
        RET
    } else if(matches == "first.date"){
        RET[RET$first.date == 1,]
    } else if(matches == "first.id"){
        RET[RET$first.id == 1,]
    } else {
        message("one should never see this message")
        invisible(NULL)
    }
}

if(FALSE){ ## -- for testing ---

    df <- data.frame(
        foo = rep(1:5, c(4, 3, 1, 1, 1)),
        bar = as.Date("2001-01-01") + c(-371,1,1,2, 2,3,371, 0, 372, 4),
        baz =  c("b","a","a","b", "a","b","b", "a", "b", "b"),
        quuz = c("a","b","a","b", "b","a","a", "b", "a", "b")
    )
    df <- df[sample(1:nrow(df)), ]

    time_match(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2001-01-01"),
               end = as.Date("2001-12-31"), matches = "all", non.matches = TRUE)

    time_match(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2010-01-01"),
               end = as.Date("2011-12-31"), matches = "all", non.matches = TRUE)

    time_match(data = df, s = "c", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2001-01-01"),
               end = as.Date("2001-12-31"), matches = "all", non.matches = TRUE)

    time_match(data = df, s = ".*", search = c("quuz", "baz"), id = "foo",
               date = "bar", matches = "all", non.matches = TRUE)

    time_match(data = df, s = "a", search = c("baz", "quuz"), id = "foo",
               begin = as.Date("1990-01-01"), date = "bar",
               matches = "first.id", non.matches = FALSE)

    ## data = df
    ## s = "c"
    ## search = c("quuz", "baz")
    ## id = "foo"
    ## date = "bar"
    ## begin = as.Date("2001-01-01")
    ## end = as.Date("2001-12-31")
    ## matches = "all"
    ## non.matches = TRUE

    ## this function might be useful at some point, but maybe not
    df_first_instance <- function(data, ..., name = ".first_instance"){
        if((n <- nrow(data)) == 1) {
            data[[name]] <- 1L
            return(data)
        }
        nm <- as.character(eval(substitute(alist(...))))
        bad.names <- which(!nm %in% names(data))
        if(length(bad.names) > 0){
            stop("variable(s)", paste(nm[bad.names], collapse = ", "),
                 "is/are not in data")
        }
        code <- paste0("data[order(",
                       paste(paste0("data[['", nm, "']]"), collapse = ", "),
                       "), ]")
        data.ord <- eval(parse(text = code))
        a <- "2:n"
        b <- "1:(n-1)"
        code2 <- paste0("!(", paste0(paste0("data.ord[['", nm, "']][", a, "] == data.ord[['",
                                            nm, "']][", b, "]"),
                                     collapse = " & "), ")")
        f <- eval(parse(text = code2))
        data.ord[[name]] <- as.integer(c(TRUE, f))
        data.ord
    }

}

