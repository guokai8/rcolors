#' light colors for making figures
#' @author Kai Guo
#' @export
lightcolor<-c('#E5D2DD', '#53A85F', '#F1BB72', '#F3B1A0', '#D6E7A3', '#57C3F3', '#476D87',
              '#E95C59', '#E59CC4', '#AB3282', '#23452F', '#BD956A', '#8C549C', '#585658',
              '#9FA3A8', '#E0D4CA', '#5F3D69', '#C5DEBA', '#58A4C3', '#E4C755', '#F7F398',
              '#AA9A59', '#E63863', '#E39A35', '#C1E6F3', '#6778AE', '#91D0BE', '#B53E2B',
              '#712820', '#DCC1DD', '#CCE0F5',  '#CCC9E6', '#625D9E', '#68A180', '#3A6963',
              '#968175','#e6194b', '#3cb44b', '#ffe119', '#4363d8','#f58231', '#911eb4',
              '#46f0f0', '#f032e6', '#bcf60c',
              '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8',
              '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075',
              '#808080'
)
#' distinguish colors for making figures
#' @author Kai Guo
#' @export
distcolor<-c("#A6761D","#D95F02","deepskyblue","#1B9E77","#E7298A","#7570B3","#E6AB02","#FC9D9A","#83AF9B","#FE4365","#F9CDAD","#C8C8A9",
             "#B6C29A", "#8A977B", "#F4D000", "#E58308", "#DC5712",
             "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C","slateblue1","darkgreen", "darkred" , "plum1","#FB9A99", "#E31A1C",
             "darkmagenta","hotpink2","slategray4","magenta2","yellow4",
    "#A6761D","#D95F02","#66A61E","#1B9E77","#E7298A","#7570B3","#E6AB02",'#e6194b', '#3cb44b', '#ffe119', '#4363d8',
             '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c',
             '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8',
             '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075',
             '#808080', '#ffffff', '#000000')
#' generate random colors without grey o not
#' @param grey include the grey color or not (default: FALSE)
#' @param alpha Transparent level
#' author Kai Guo
#' @export
gcolor<-function(ncolor, grey = FALSE, alpha = 1){
  color_all <- grDevices::colors() 
  if(isTRUE(grey)){
      colors <- sample(palette2_no_gray, ncolor) 
    }else{
      no_gray <- palette2_all[grep("gr(a|e)y",             # Remove gray colors
                                      grDevices::colors(),
                                      invert = T)]
      colors <- sample(no_gray, ncolor,replace=FALSE) 
    }
  colors <- Transparent(colors, alpha = alpha)
 return(colors)
}
 
### https://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
#' Transparent color
#' @param colors a vector of colors: "red","blue" 
#' @param alpha Transparent level
#' @author Kai Guo
#' @examples {
#' Transparent(2, 4)
#' Transparent("red", "blue")
#' Transparent(rgb(1,0,0), rgb(0,0,1))
#' }
#' @export

Transparent = function(colors, alpha=0.8) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(colors)), alpha=FALSE)
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}

