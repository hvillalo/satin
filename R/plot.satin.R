plot.satin <-
function(x, period = 1, depth = 1, xlim = NULL, ylim = NULL, zlim = NULL, 
   map = NULL, map.col = "grey", map.outline = "black",  
   scheme = "default", col.sep = 0.1, colbar = TRUE, main = NULL, 
   main.pos = "topright", log = FALSE, units = NULL, ...)
{
  X <- x
  x <- X@lon
  y <- X@lat
  z <- X@data
  if ( length(dim(z)) > 3 )
    z <- X@data[ , , period, depth]
  else 
    z <- X@data[ , , period]
  if ( missing(main) ) {
    dini <- X@period$tmStart[period]
    dend <- X@period$tmEnd[period] 
    if (dini == dend)
      date <- format(dini, "%Y-%m-%d")
    else date <- paste(format(dini, "%Y-%m-%d"), format(dend, "%Y-%m-%d"), sep = " : ")
    main <- paste(X@attribs$longname, "\n", date, sep="")  
  }  
  if ( missing(xlim) ) 
    xlim <- c(floor(min(x)), ceiling(max(x)))
  if ( missing(ylim) ) 
    ylim <- c(floor(min(y)), ceiling(max(y)))
  if ( missing(zlim) ) 
    zlim <- c(floor(min(z, na.rm = TRUE)), ceiling(max(z, na.rm = TRUE)))
  xlims <- xlim
  ylims <- ylim

  if ( log == TRUE ){
     z[z > 20] <- 20
     z <- log10(z)
     zlim <- log(c(0.01, 20))
  }
  
  cb <- satinPalette(zmin = zlim[1], zmax = zlim[2], col.sep = col.sep, scheme = scheme)
  cbp <- cb$palette
  cbb <- cb$breaks

  #op <- par(no.readonly = TRUE)  
  #on.exit(par(op))
  if (colbar == TRUE){
    if ( missing(units) )
     units <- X@attribs$units
    layout( matrix(c(2, 1), ncol = 2), widths = c(7/8, 1/8), heights = c(1, 1) )
    par(mar = c(5.1, 0, 4.1, 4))
    satin2::imageScale(z = t(z[nrow(z):1, ]), col = cbp, breaks = cbb, axis.pos = 4, las = 2, log = log)
    mtext(units, side = 4, line = -1.5, outer = TRUE)
    par(mar = c(5.1, 4.1, 4.1, 0.1)) 
  }
  if (colbar == FALSE)
    par(mar = c(5.1, 4.1, 4.1, 3.5))  
  image(x, y, t(z), xlim = xlims, ylim = ylims, zlim, asp = 1, xlab = "", ylab = "",
      col = cbp, breaks = cbb, xaxt = "n", yaxt = "n")
  pu <- par("usr")
  if ( missing(map) ) {  
   map("world", xlim = pu[1:2], ylim = pu[3:4], add = TRUE)
   box(); axis(1); axis(2)
  } else {
  plot(map, xlim = pu[1:2], ylim = pu[3:4], xaxs = "i", yaxs = "i", axes = TRUE, 
       lty = 1, col = map.col, border = map.outline, add = TRUE)
  box(); axis(1); axis(2, las=1)  
  }
  xyp <- fmainPos(pu, main.pos)
  text(xyp$cx, xyp$cy, label = main, pos = xyp$pos)
  #invisible(pu)
}
