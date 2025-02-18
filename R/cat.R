#' colors from cell
#' @author Kai Guo
#' @export
cell_colors <- c(
  "#2670B8", "#B3D9CE", "#69D8B8", "#36BD9C", "#16A16E", "#206A9A", "#5499B1", "#6DBC87",
  "#876AAA", "#6DB3BD", "#4C8D85", "#A5CDA7", "#9E1B8E", "#62B8CA", "#AEF8F3", "#B85F81",
  "#69B9B7", "#405BA0", "#C0F5D7", "#223C4D", "#4758A0", "#3F578B", "#C2DF7E", "#71B9A3",
  "#BDFBDA", "#D7E7D1", "#F8C4C9", "#E79292", "#6C9D8E", "#58B390", "#B6DDCB", "#E0F0E5",
  "#16416E", "#5499B2", "#83BBB7", "#B5D6B9", "#DAE6D3", "#6FB1B9", "#A8D7D5", "#B7CDD1",
  "#82BCD8", "#AFE8F3", "#B5F8F1", "#494574", "#427794", "#4DA8A0", "#94CFB2", "#6D8C87",
  "#87B6AA", "#C9E1BE", "#4D85A0", "#82BED8", "#C2DFE7", "#91BE93", "#BFD8D4", "#D35400",
  "#E67E22", "#F1C40F", "#FBC02D", "#EC407A", "#F06292", "#00ACC1", "#26C6DA", "#C0392B"
)

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
#' @author Kai Guo
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
#' @export
palette2_all <- grDevices::colors()
#' color groups
#' @export
#' @author Kai Guo
colist<-list(
"col1"=c("#D9C8DF", "#C19CC4", "#FAE9BD","#F4D6CE","#E3E2E1","#C1BDBC","#666464"),
"col2"=c("#ABD494", "#EEE47E","#75C3AE","#898BB1","#7BA8B4","#8A6494"),
"col3"=c("#D4D4D4", "#D2AEAC","#B3D6AD","#8EA5C8","#A17DB4","#ADA579","#5F5F5F"),
"col4"=c("#F2CE6C","#839D44","#EBA133","#E1706E","#6F8CBD","#C06B81"),
"col5"=c("#D2EAF7","#BCDECF","#FADDA6","#9FA5CD","#72A3D0","#62BC9B","#F3B926"),
"col6"=c("#CAE8EC","#FBE3E2","#CCD6BD","#3ABABD","#1E908F","#EF8881","#F3B926"))




