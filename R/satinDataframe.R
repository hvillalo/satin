satinDataframe <-
function(X, reverse = FALSE)
{
  if (reverse == FALSE) {
    if (!inherits(X, "satin"))
      stop ("need object of class 'satin'")
    lon <- X@lon; nx <- length(lon)
    lat <- X@lat; ny <- length(lat)
    vlon <- rep(lon, each = ny)
    vlat <- rep(lat, nx)
    np <- length(X@period$tmStart)
    nd <- length(X@depth)
    z <- X@data
    if (length(dim(z)) > 3){
      ni <- nd * np
    } else {
      ni <- np
      nd <- 1
    }  
    Z <- array(z, dim = c(ny * nx, ni))
    ans <- data.frame(x = vlon, y = vlat, Z)
    names(ans)[3:(ni+2)] <- paste("p", 1:ni, sep = "")
    attribs <-  X@attribs
    attribs$period <- X@period
    attribs$depth <- X@depth
    attribs$labels <- paste("depth", rep(round(X@depth, 1), each = np), "_", 
                           "T", rep(format(X@period$tmStart, "%Y-%m-%d"), nd), sep = "")
    attr(ans, "attribs") <- attribs
    class(ans) <- c("satinDataframe", "data.frame")
  }
  if (reverse == TRUE) {
    if (!inherits(X, "satinDataframe"))
      stop ("I cannot transform 'X' into a 'satin' object") 
    attribs <- attr(X, "attribs")
    np <- length(attribs$period$tmStart)
    nd <- length(attribs$depth)
    lon <- unique(X[ , 'x'])
    lat <- sort(unique(X[ , 'y']))
    ni <- ncol(X) - 2
    Z <- as.vector(as.matrix(X[ , -c(1, 2)]))
    ans <- list()
    if (length(attribs$depth) == 0) {
      z <- array(Z, dim = c(length(lat), length(lon), np))
    } else {
      z <- array(Z, dim = c(length(lat), length(lon), np, nd))
    }
    ans <- new("satin", lon = lon, lat = lat, data = z, attribs = attribs[1:6], 
               period = attribs$period, depth = attribs$depth)
    }
  ans
}