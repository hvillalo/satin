# imageScale() function from package "sinkr" by Marc Taylor: (modified for log scale)
#  https://github.com/marchtaylor/sinkr/
# Make a color scale to accompany an image or other plot
imageScale <- 
function(z, zlim, col = heat.colors(12), breaks, axis.pos = 1,
         add.axis = TRUE, xlim = NULL, ylim = NULL, log = FALSE, ...)
{
  if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){
    stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm = TRUE)
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  if(axis.pos %in% c(1,3)){YLIM <- c(0,1); XLIM <- range(breaks)}
  if(axis.pos %in% c(2,4)){YLIM <- range(breaks); XLIM <- c(0,1)}
  if(!missing(ylim)){ YLIM <- ylim }
  if(!missing(xlim)){ XLIM <- xlim }
   
  plot(1, 1, t="n", ylim=YLIM, xlim=XLIM, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)
  for(i in seq(poly)){
    if(axis.pos %in% c(1,3)){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=col[i], lwd = 0.01)
    }
    if(axis.pos %in% c(2,4)){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=col[i], lwd = 0.01)
    }
  }
  box()
  if(add.axis & log == FALSE) {axis(axis.pos, las = 1, ...)}
  if(add.axis & log == TRUE) {
    labs <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20)
    atp <- log(labs)
    axis(axis.pos, las = 1, at = atp, labels = labs, ...)
  }
}