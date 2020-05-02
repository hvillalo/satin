isolines <-
function(X, levels, period = 1, depth = 1, plot = TRUE)
{
  if (!inherits(X, "satin"))
    stop ("need object of class 'satin'")
  dz <- dim(X@data)
  if (length(dz) > 3) {
    mz <- X@data[ , , period, depth]
  } else {
    mz <- X@data[ , , period]
  }	
  GRID <- list(x = X@lon, y = X@lat, z = t(mz))
  CL <- contourLines(GRID, levels = levels)
  ans <- convCP(CL)
  attr(ans$PolySet, "projection") <- "LL"
  if (plot == TRUE)
    plotMap(ans$PolySet)
  ans
}