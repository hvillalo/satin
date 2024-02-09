climatology <-
function(X, depth = NULL) 
{
  if (!inherits(X, "satin"))
    stop ("need object of class 'satin'")
  z <- X@data
  np <- dim(z)[3]
  if (length(dim(z)) > 3) {
    if (missing(depth))
      depth <- 1
    X@depth <- X@depth[depth]	  
    z <- z[, , , depth]   
    d <- paste(round(X@depth[depth], 2), "m")
  } else {
    d <- "0 m"
  }
  clim <- array(NA, dim=c(nrow(z), ncol(z), 5) )
  dimnames(clim)[[3]] <- list("coverage", "mean", "sd", "min", "max")
  sumpix <- apply(!is.na(z), MARGIN=c(1, 2), "sum")
  clim[, , 1] <- sumpix/np * 100 # percent coverage
  clim[, , 2] <- apply(z, MARGIN=c(1, 2), "mean", na.rm=TRUE )
  clim[, , 3] <- apply(z, MARGIN=c(1, 2), "sd", na.rm=TRUE )   
  clim[, , 4] <- suppressWarnings(apply(z, MARGIN=c(1, 2), "min", na.rm=TRUE))
  clim[, , 5] <- suppressWarnings(apply(z, MARGIN=c(1, 2), "max", na.rm=TRUE))
  for (i in 2:5)
    clim[, , i][clim[, , 1] == 0] <- NA
  pFr <- format(X@period$tmStart[1], "%Y-%m-%d")
  pTo <- format(X@period$tmEnd[np], "%Y-%m-%d")
  X@data <- clim
  X@period$tmStart <- rep(X@period$tmStart[1], np) 
  X@period$tmEnd <- rep(X@period$tmEnd[np], np) 
  X@attribs$labels <- paste(dimnames(clim)[[3]], " (at ", d, ") \n from ", pFr, " to: ", pTo, sep="")
  X
}