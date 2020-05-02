satinPalette <-
function(zmin, zmax, col.sep = 0.1, scheme = "default", visu = FALSE)
{
  if (missing(scheme)) scheme <- "default"
  if (length(scheme) > 1) {
    cols <- scheme 
  } else {
    if (scheme != "default") 
      stop ("scheme must be either 'default' or a vector of valid color names")	 
    if (scheme == "default")
      cols <- c("purple", "blue", "darkblue", "cyan", "green", "darkgreen", "yellow",
                "orange", "red", "darkred")
  }
 
  fpal <- colorRampPalette(colors = cols)
  breaks <- seq(zmin, zmax, by = col.sep)
  if ( breaks[length(breaks)] < zmax )
    breaks <- c(breaks, zmax)
  nbcols = length(breaks) - 1
  ans <- list(palette = fpal(nbcols), breaks = breaks)
  
  if (visu == TRUE){
    nb <- length(breaks)
    xl <- breaks[1:(nb-1)]
    xr <- breaks[2:nb]
    xlims <- range(breaks)
  
    plot(breaks, rep(0.5, nb), ylim = c(0, 1), bty = "n", xaxt = "n", yaxt = "n", 
         xlab = "", ylab = "", type = "n" )
    rect(xl, 0, xr, 1, col = fpal(nbcols), border = "white")
    axis(1, at = breaks, breaks, line = -0.7)
  }
  ans
}
