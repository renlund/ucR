#' @title The UCR palette
#' @description Returns the colors of the UCR palette
#' @param show if true the palette is shown in a new graphical window
#' @return The codes for the five colors in the UCR graphical profile
#' @author Henrik Renlund
#' @export

ucr_palette <- function(show=FALSE){
   colors <- c("#61A60E", "#3A96B4", "#DC6511", "#6E3784", "#005E85")
   if(show) {
      grDevices::dev.new()
      graphics::barplot(table(factor(colors, levels=colors)), col=colors, yaxt='n')
   }
   colors
}
