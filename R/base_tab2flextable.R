##' turn ucr.base.tab object into a flextable
##'
##' this function cleans a ucr.base.tab of its LaTeX elements and produced a
##' flextable roughly corresponding to what you get from
##' latex.ucr.base.tab. Optionally, you can use a template to obtain an order
##' and row-grouping of the variables.
##' @param object an object of class "ucr.base.tab"
##' @param template optional; a data.frame with variables 'group' and 'label'
##'     representing the order in which to display the variables and (optional)
##'     the grouping of the rows
##' @param caption character; the table caption
##' @param use.groups logical; create table with grouped rows? (The groups must
##'     be specified with 'template')
##' @param indent number of whitespace indentations under rowgroups (if any),
##'     and under categorical data variables (displaying values)
##' @param gray logical; should every other variable be highlighted?
##' @param fontsize fontsize for header, body and footer of table. Choose
##'     smaller ones if fitting onto the page is a problem
##' @return a flextable
##' @import flextable
##' @examples
##' n <- 6000
##' D <- data.frame(
##'     ## id = sprintf("id%s", 1:n),
##'     gr2 = factor(rep(c("Group 1", "Group 2"), each = n / 2)),
##'     gr3 = factor(rep(c(sprintf("Group %s", LETTERS[1:3])), each = n / 3)),
##'     age = runif(n, 35, 95),
##'     sex = factor(sample(c("M", "F"), size = n, replace = TRUE)),
##'     NotInTemplate = runif(n),
##'     measx = 100*rbeta(n, 2, 1),
##'     measy = rexp(n, 1/100),
##'     catgx = factor(sample(0:1, size = n, replace = TRUE)),
##'     catgy = factor(sample(c("foo", "bar", "baz"), size = n, replace = TRUE))
##' )
##' D$age[97] <- NA
##' D$measx[c(501, 842)] <- NA
##' d <- data.frame(
##'     group = rep(c("Demographics", "A set of interesting covariates"), each = 4),
##'     variable = c("sex", "age", "gr2", "gr3",
##'                  "catgx", "catgy", "measx", "measy"),
##'     label = c("Sex", "Baseline age", "Foo group", "Bar group",
##'               "Zero one variable", "Programming names",
##'               "Higgs density", "Flogiston")
##' )
##' X <- D
##' for(v in names(X)){
##'     attr(X[[v]], "label") <- d$label[d$variable == v]
##' }
##'
##' x <- ucr.base.tab2flextable(
##'     object = ucR::ucr.base.tab(data = X,
##'                                group.name = "gr2",
##'                                include.p = FALSE,
##'                                include.n = FALSE),
##'     template = d
##' )
##' ## ## view results
##' ## library(flextable)
##' ## print(x, preview = "html")
##' ## print(x, preview = "docx")
##' ## print(x, preview = "pdf", latex_engine = "xelatex")
##' @export
ucr.base.tab2flextable <- function(object, template = NULL, caption = NULL,
                                   use.groups = NULL, indent = c(3,3),
                                   gray = TRUE, fontsize = c(11,11,9)){
    B <- object$tab
    labs <- V2v(B[,1])
    if(is.null(template)){
        template <- data.frame(group = "noGroup",
                               label = labs[!duplicated(labs)])
        use.groups <- FALSE
    } else {
        ## possibly some check on the sanity of template?
        if(is.null(use.groups)) use.groups <- TRUE
    }
    if(use.groups){
        indent1 <- paste(rep(" ", indent[1]), collapse = "")
        indent2 <- paste(rep(" ", indent[1] + indent[2]), collapse = "")
    } else {
        indent1 <- ""
        indent2 <- paste(rep(" ", indent[2]), collapse = "")
    }
    group.names <- unique(template$group)
    ## choose and order rows according to the template
    ## index <- order_as(labs, template$label, incl.unordered = FALSE)
    index <- align(labs, template$label, all = FALSE)$order
    DB <- as.data.frame(B[index,])

    ## deLaTeX DB
    names(DB)[grepl("$N$", names(DB), fixed = TRUE)] <- "N"
    names(DB)[grepl("$P$-value", names(DB), fixed = TRUE)] <- "p-value"
    DB$Variable <- gsub("\\hspace{1em}", ":", DB$Variable, fixed = TRUE)
    DB$Variable <- gsub("\\#", "", DB$Variable, fixed = TRUE) ## ??
    DB[] <- lapply(DB, function(X) gsub(" -- ", " - ", X))
    DB[] <- lapply(DB, function(X) gsub("$\\pm$", "\U00B1", X, fixed = TRUE))

    ## identify footnotes for p-value info
    test.names <- object$test.names
    tests <- any(!is.na(test.names))
    if(tests){
        fn <- unlist(lapply(strsplit(DB$`p-value`, split = "$^", fixed = TRUE),
               FUN = function(X){
                   n <- length(X)
                   if(n == 2){
                       gsub("$", "", X[[2]], fixed = TRUE)
                   } else 0
               }))
        DB$`p-value` <- unlist(lapply(strsplit(DB$`p-value`, split = "$^", fixed = TRUE),
               FUN = function(X){
                   n <- length(X)
                   if(n >= 1) X[[1]] else ""
               }))
    }

    ## add indentation white spaces (flextable will only display in pdf/word)
    tmp <- DB$Variable
    DB$Variable <- ifelse(grepl("^:", tmp),
                          yes = paste0(indent2, tmp),
                          no = paste0(indent1, tmp))

    ## get a version of template same order/nrow as DB
    g <- merge(x = data.frame(label = labs[index]), y = template,
               by = "label", all.x = TRUE, sort = FALSE)
    g$group <- factor(g$group, levels = group.names)
    m <- nrow(g)
    ## create indicator for which rows could be coloured/gray
    gl <- g$label
    graa <- cluster.by.incl.next(c(gl[1:(m-1)] == gl[2:m], FALSE)) %% 2

    ## this info will go in the header
    ech <- gsub("$", "", object$extra.col.heads, fixed = TRUE)
    ech[1] <- names(DB)[1]

    ## i.na will be the indices where row group labels will be inserted (if any)
    i.na <- NULL

    if(use.groups){
        DB <- do.call("rbind", lapply(split(DB, f = g$group), FUN = addNArow))
        ## update color indicator
        graa.update <- rep(2, nrow(DB))
        graa.update[!is.na(DB$Variable)] <- graa
        graa <- graa.update
        if(tests){ ## update footnote indicator
            fn.update <- rep(0, nrow(DB))
            fn.update[!is.na(DB$Variable)] <- fn
            fn <- fn.update
        }
        ## fill in rowgroup labels
        i.na <- which(is.na(DB$Variable))
        DB$Variable[is.na(DB$Variable)] <- levels(g$group)
        rownames(DB) <- NULL
    }

    ## its flextime:
    names(DB)[1] <- " "
    ft <- flextable(DB)
    ft <- set_table_properties(ft, layout = "autofit")

    ## this will fix the row group labels (if any)
    for(i in seq_along(i.na)){
        ft <- bold(ft, i = i.na[i], j = 1)
        ft <- italic(ft, i = i.na[i], j = 1)
        ft <- merge_h(ft, i = i.na[i])
    }

    ## header stuff
    ## ft <- add_header_row(ft, values = hr)
    ft <- add_header_row(ft, values = ech, top = FALSE)
    ft <- hline(ft, i = 1, border = fp_border_default(width = 0), part = "header")
    ft <- bold(ft, i = 1, part = "header")

    ## footer stuff
    fl <- ubt_text(object)
    if(tests) fl <- fl[!grepl("^Tests used", fl)]
    ft <- add_footer_lines(ft, values = as_paragraph(fl))
    ft <- padding(ft, padding = 0, part = "footer")

    ## fontsize stuff
    ft <- fontsize(ft, size = fontsize[1], part = "header")
    ft <- fontsize(ft, size = fontsize[2], part = "body")
    ft <- fontsize(ft, size = fontsize[3], part = "footer")

    ## p-value footnote stuff XK
    if(tests){
        ft <- add_footer_lines(ft, values = as_paragraph("Tests used: "))
        for(i in seq_along(test.names)){
            ft <- footnote(ft, i = which(fn == i),
                           j = which(names(DB) == 'p-value'),
                           value = as_paragraph(test.names[i]),
                           ref_symbols = as.character(i), inline = TRUE)
        }
    }

    ## mark missing value rows (if any)
    ft <- italic(ft, i = grepl(" *missing$",  DB[[1]]), j = 1)

    ## very other variable grey
    if(gray) ft <- bg(ft, i = graa == 1, bg = "#EFEFEF")

    ## add caption
    if(!is.null(caption)) ft <- set_caption(ft, caption = caption)

    ft
}

ubt_text <- function(object){
    bot <- ubt.bottom.text(object)
    bot <- gsub("$\\pm$", "\U00B1", bot, fixed = TRUE)
    bot <- gsub("$_1$", "\U2081", bot, fixed = TRUE)
    bot <- gsub("$_3$", "\U2083", bot, fixed = TRUE)
    bot <- gsub("$", "", bot, fixed = TRUE)
    bot <- gsub("^ *\\n\\n ", "", bot)
    bot <- gsub(" -- ", " - ", bot)
    bot <- unlist(strsplit(bot, split = " \n\n ", fixed = TRUE))
    bot
}

if(FALSE){ ## TEST START =======================================================

    n <- 6000
    D <- data.frame(
        ## id = sprintf("id%s", 1:n),
        gr2 = factor(rep(c("Group 1", "Group 2"), each = n / 2)),
        gr3 = factor(rep(c(sprintf("Group %s", LETTERS[1:3])), each = n / 3)),
        age = runif(n, 35, 95),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE)),
        NotInTemplate = runif(n),
        measx = 100*rbeta(n, 2, 1),
        measy = rexp(n, 1/100),
        catgx = factor(sample(0:1, size = n, replace = TRUE)),
        catgy = factor(sample(c("foo", "bar", "baz"), size = n, replace = TRUE))
    )
    D$age[97] <- NA
    D$measx[c(501, 842)] <- NA
    d <- data.frame(
        group = rep(c("Demographics", "A set of interesting covariates"), each = 4),
        variable = c("sex", "age", "gr2", "gr3",
                     "catgx", "catgy", "measx", "measy"),
        label = c("Sex", "Baseline age", "Foo group", "Bar group",
                  "Zero one variable", "Programming names",
                  "Higgs density", "Flogiston")
    )
    X <- D
    for(v in names(X)){
        attr(X[[v]], "label") <- d$label[d$variable == v]
    }

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   include.p = FALSE),
        template = NULL
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")


    test <- ucR::ucr.base.tab(data = X,
                              num.format = "mean",
                              mean.format = "pm",
                              include.p = FALSE)
    b <- ubt_text(test)

    x <- ucr.base.tab2flextable(
        object = test,
        template = NULL
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   include.p = FALSE,
                                   include.n = FALSE),
        template = NULL
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   include.p = FALSE,
                                   show.missing = "sep.row"),
        template = NULL
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   include.p = FALSE,
                                   include.n = FALSE),
        template = d
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   group.name = "gr2",
                                   include.p = FALSE,
                                   include.n = FALSE),
        template = NULL
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   group.name = "gr2",
                                   include.p = FALSE,
                                   include.n = FALSE),
        template = d
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   group.name = "gr2",
                                   include.p = TRUE,
                                   include.n = TRUE),
        template = d, fontsize = c(8,8,6)
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   group.name = "gr3",
                                   include.p = FALSE,
                                   include.n = FALSE),
        template = NULL
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

    x <- ucr.base.tab2flextable(
        object = ucR::ucr.base.tab(data = X,
                                   group.name = "gr3",
                                   include.p = TRUE,
                                   include.n = FALSE,
                                   show.missing = "sep.row"),
        template = d
    )
    x
    print(x, preview = "docx")
    print(x, preview = "pdf", latex_engine = "xelatex")

} ########### TEST END =========================================================

## HELPER FUNCTIONS:
## (some of these copied from other packages)

locf <- function(x){
    n <- length(x)
    if(n <= 1){
        ## warning("x has no non-missing elements")
        return(x)
    }
    if(is.na(x[1])) return(c(NA, locf(x[-1])))
    x[!is.na(x)][cumsum(!is.na(x))]
}

V2v <- function(v){
    x <- unlist(lapply(strsplit(v, split = ":"), FUN = function(z) z[1]))
    x[grepl("\\hspace{1em}", x, fixed = TRUE)] <- NA_character_
    x[grepl("\\# missing", x, fixed = TRUE)] <- NA_character_
    locf(x)
}

addNArow <- function(d){
    rbind(d[NA,][1,], d)
}

cluster.by.incl.next <- function(incl.next){
    n <- length(incl.next)
    c(1, 1 + cumsum(!incl.next[-n]))
}

align <- function(x, template = NULL, group = NULL,
                  all = TRUE, outgroup = ".Other"){
    if(length(x) == 0){
        warning("zero length input makes no sense")
        return(as.list(NULL))
    }
    if(!is.null(group) & !is.null(template)){
        if(length(group) != length(template)){
            stop("template and group of the same length, please")}
    }
    if(is.null(template)) template = sort(x)
    m <- match(x, template)
    distinct_m <- sort(unique(na.omit(m)))
    order <- rep(NA_integer_, length(x))
    dummy <- 0L
    for(d in distinct_m){
        i <- which(x %in% x[which(d==m)][1])
        n <- length(i)
        order[dummy + 1:n] <- i
        dummy <- dummy + n
    }
    if(any(is.na(m))){
        if(all){
            order[which(is.na(order))] <- which(is.na(m))
        } else {
            order <- order[!is.na(order)]
        }
    }
    z <- data.frame(x = x[order])
    if(is.null(group)){
        z$group <- outgroup
        list(order = order,
             sorted = z,
             group.rle = list(lengths = nrow(z),
                              values = outgroup))
    } else {
        tg <- data.frame(template = template, group = group)
        s <- merge(x = z, y = tg, all.x = TRUE,
                   by.x = "x", by.y = "template", sort = FALSE)
        if(all){
            s$group[is.na(s$group)] <- outgroup
        }
        Rle <- rle(s$group)
        class(Rle) <- "list"
        list(order = order,
             sorted = s,
             group.rle = Rle)
    }
}

## order_as <- function (given, wanted, incl.unordered = TRUE){
##     .s <- "_."
##     if (any(grepl(paste0("_\\.[0-9]_\\.$"), given))) {
##         mess <- paste0("'order_as' uses suffix '", .s, "<number>",
##             .s, "' ", "internally hoping noone would ever use such a ",
##             "strange variable name, but if so then this might ",
##             "cause the ordering to fail. Please check the results ",
##             "(or rename your variables)")
##         warning(mess)
##     }
##     want <- wanted[wanted %in% given]
##     if (any(duplicated(want))) {
##         warning("duplicated entries in 'wanted'")
##         want <- unique(want)
##     }
##     foo <- function(X) {
##         n <- nrow(X)
##         X$nr <- if (n == 1)
##             ""
##         else 1:n
##         NR <- if (n == 1)
##             ""
##         else paste0(.s, 1:n, .s)
##         X$attention <- if (n == 1)
##             0
##         else c(rep(0, n - 1), 1)
##         X$edited <- paste0(X$given, NR)
##         X
##     }
##     df <- data.frame(given = given, stringsAsFactors = FALSE)
##     spl <- lapply(split(df, f = df$given), foo)
##     dc <- unsplit(spl, f = df$given)
##     rownames(dc) <- NULL
##     sdc <- subset(dc, dc$attention == 1)
##     lw <- as.list(want)
##     names(lw) <- want
##     for (k in seq_along(sdc$given)) {
##         K <- as.character(sdc$given[k])
##         if (!K %in% names(lw))
##             next
##         n <- sdc$nr[k]
##         lw[[K]] <- sprintf(paste0(lw[[K]], .s, "%s", .s), 1:n)
##     }
##     W <- unlist(lw)
##     G <- dc$edited
##     indx <- match(W, G)
##     rest <- setdiff(1:length(given), indx)
##     if (incl.unordered) {
##         c(indx, rest)
##     }
##     else {
##         indx
##     }
## }
