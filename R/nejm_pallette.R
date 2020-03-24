#' @title The NEJM palette
#' @description Returns the colors of the NEJM palette
#' @param show if true the palette is shown in a new graphical window
#' @return The codes for the eight colors in the NEJM graphical profile
#' @author Henrik Renlund
#' @export

nejm_palette <- function(show=FALSE){
   colors <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "#7876B1FF", "#6F99ADFF", "#FFDC91FF", "#EE4C97FF")
   if(show) {
      grDevices::dev.new()
      graphics::barplot(table(factor(colors, levels=colors)), col=colors, yaxt='n')
   }
   colors
}
