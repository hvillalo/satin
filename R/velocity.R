velocity <-
function(u, v, depth = 1)
{
  if ( !inherits(u, "satin") | !inherits(v, "satin") )
    stop ( "'u' and 'v' must be of class 'satin'" )
  if ( length(dim(u@data)) != 4 | length(dim(v@data)) != 4)
    stop ( "function expects data arrays with 4 dimensions" )
  sp <- u
  dims <- dim(sp@data)[1:3]
  
  # origin coordinates
  x <- u@lon
  y <- u@lat
  xo <- matrix(rep(x, length(y)), ncol = length(x), byrow = TRUE)
  yo <- matrix(rep(y, length(x)), ncol = length(x), byrow = FALSE)
  
  # velocity matrices for chosen depth
  u <- u@data[ , , , depth]
  v <- v@data[ , , , depth]

  # speed calculation
  speed <- sqrt(u^2 + v^2)
  
  # destination coordinates: xo + u; yo + v
  xd <- sweep(x = u, MARGIN = c(1, 2), STATS = xo, FUN = "+")
  yd <- sweep(v, c(1, 2), yo, FUN = "+")
  
  # direction (rhumb)
  origin <- cbind(as.vector(xo), as.vector(yo))
  rhumb <- array(NA, dim = dims)
  np <- dims[3]
  
  for (p in 1:np){
    X <- xd[ , , p]
      X[is.na(X)] <- origin[is.na(X), 1]
    Y <- yd[ , , p]
      Y[is.na(Y)] <- origin[is.na(Y), 2]
    dest <- cbind(as.vector(X), as.vector(Y))
    rh <- bearingRhumb(p1 = origin, p2 = dest)
    rhumb[ , , p] <- rh
  }
  
  #results
  rh <- sp
    sp@data <- speed
    sp@attribs$title <- "speed"
    sp@attribs$longname <- "current speed"
    sp@attribs$name <- "speed"
    sp@depth <- sp@depth[depth]
  rh@data <- rhumb
    rh@attribs$title <- "rhumb"
    rh@attribs$longname <- "current direction"
    rh@attribs$name <- "rhumb"
    rh@attribs$units <- "degrees"
    rh@depth <- rh@depth[depth]
  ans <- list(speed = sp, rhumb = rh)
  ans
}

