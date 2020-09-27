quiver <-
function(u, v, period = 1, depth = 1, xlim = NULL, ylim = NULL, scale = 1, 
  length = 0.05, colarrow = NULL, scheme = "default", ra.pos = NULL, 
  ra.speed = NULL, map = NULL, map.col = "grey", map.outline = "black",  
  colbar = FALSE, main = NULL, main.pos = "topright", add2map = FALSE, ...)
{
  # ancillary functions
  par.uin <- function() {
    pu <- par("usr")
    pp <- par("pin")
    c(pp[1]/(pu[2] - pu[1]), pp[2]/(pu[4] - pu[3]))
  }

  colArrows <- function(speed, scheme = "default") {
    speed <- as.vector(speed)
    speed.rng <- range(speed, na.rm = TRUE)
    col.arrows <- speed
    speed[is.na(speed)] <- 999
    
    p <- satinPalette(zmin = 0, zmax = speed.rng[2], col.sep = 0.02, scheme)
    pal <- p$palette
    nbcols <- length(pal)
    breaks <- p$breaks
    
    for (i in 1:length(col.arrows)) {
      for(j in 1:nbcols) {
        if( speed[i] > breaks[j] & speed[i] <= breaks[j+1] ) 
        col.arrows[i] <- pal[j]
      }    
    }     
    ans <- list(col.arrows = col.arrows, pal = pal, breaks = breaks)
    ans
  }

  if ( !inherits(u, "satin") | !inherits(v, "satin") )
    stop ( "'u' and 'v' must be of class 'satin'" )
 
  # main title
  if ( missing(main) ) 
    main <- u@period$tmStart[period]
 
  # plot limits and arrows coordinates
  x <- u@lon
  y <- u@lat
  if ( missing(xlim) ) 
    xlim <- c(floor(min(x)), ceiling(max(x)))
  if ( missing(ylim) ) 
    ylim <- c(floor(min(y)), ceiling(max(y)))
  xcoor <- matrix(rep(x, length(y)), ncol = length(x), byrow = TRUE)
  ycoor <- matrix(rep(y, length(x)), ncol = length(x), byrow = FALSE)
  
  # velocity matrices
  u <- u@data[ , , period, depth]
  v <- v@data[ , , period, depth]

  # reference arrow
  if ( !missing(ra.pos) ) {
    if ( length(ra.pos) != 2) 
      stop("'ra.pos' must contain (lon, lat) coordinates for reference arrow")
    ru <- min(which(abs(y - ra.pos[2]) == min(abs(y - ra.pos[2]))))
    cu <- min(which(abs(x - ra.pos[1]) == min(abs(x - ra.pos[1]))))
    if ( missing(ra.speed) ) 
      ra.speed <- round(mean(abs(c(u, v)), na.rm=TRUE), 1)
    u[ru, cu] <- ra.speed
    v[ru, cu] <- 0
  }
   
  # scaling arrows
  speed <- sqrt(u^2 + v^2)
  maxspeed <- max(speed, na.rm = TRUE)
  u <- u * scale/maxspeed
  v <- v * scale/maxspeed
  if ( missing(colarrow) ) 
    colarrow = "black"
  if (colarrow == TRUE){ 
    ca <- colArrows(speed, scheme)
    colarrow <- ca$col.arrows
  }

  if ( add2map == TRUE ) {
    suppressWarnings(
      arrows( xcoor, ycoor, xcoor + u, ycoor + v, col = colarrow,
        length = length * min(par.uin()), ... ))
    pu <- par("usr")
  } else {
    if (colbar == TRUE){
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
      layout( matrix(c(2, 1), ncol = 2), widths = c(8/10, 1/5), heights = c(1, 1) )
      par(mar = c(5.1, 0, 4.1, 4))
      satin::imageScale(z = c(u,v), col = ca$pal, breaks = ca$breaks, axis.pos = 4, log = FALSE)
      mtext(expression(m%.%s^{-1}), side = 4, line = -1.5, outer = TRUE)
      par(mar = c(5.1, 4.1, 4.1, 0.5)) 
    }
    plot(xcoor, ycoor, type = "n", asp = 1, xlim = xlim, ylim = ylim, 
         xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    pu <- par("usr")
    if ( missing(map) ) {  
      map("world", xlim = pu[1:2], ylim = pu[3:4], add = TRUE)
      box(); axis(1); axis(2)
    } else {
      plot(map, xlim = pu[1:2], ylim = pu[3:4], xaxs = "i", yaxs = "i", axes = TRUE, 
           lty = 1, col = map.col, border = map.outline, add = TRUE)
      box(); axis(1); axis(2, las=1)  
    }
    suppressWarnings(
    arrows( xcoor, ycoor, xcoor + u, ycoor + v, col = colarrow,
            length = length * min(par.uin()), ... ))
  }
  if (length(ra.pos) == 2) {
    text(x = ra.pos[1], y = ra.pos[2], label = 
    substitute(paste(ra.speed, " ", m.s^{-1}),
    list(ra.speed = ra.speed)), adj = c(0, 1.5), cex = 0.7)
  }
  xyp <- fmainPos(pu, main.pos)
  text(xyp$cx, xyp$cy, label = main, pos = xyp$pos)
}
