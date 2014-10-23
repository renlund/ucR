#' @title Am I on the server?
#' @description Checks wether \code{Sys.info()} contains info pointing to the server
#' @export

server <- function() as.character(Sys.info()['nodename']) == "UCRANALYS"
