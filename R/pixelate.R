pixelate <-
function(X, extent = 0.25, FUN = mean)
{
  if (!inherits(X, "satin"))
   stop ( "need object of class 'satin'" )
  X@attribs$spatial_resolution <- paste(extent, "degrees")
  lon <- X@lon
  lat <- X@lat
  z <- X@data
  nr <- dim(z)[1]; nc <- dim(z)[2]
  vz <- as.vector(z)

  xs <- seq(floor(min(lon)), ceiling(max(lon)), extent)
  if (max(lon) > xs[length(xs)]) 
    xs <- c(xs, xs[length(xs)] + extent)

  ys <- seq(floor(min(lat)), ceiling(max(lat)), extent)
  if (max(lat) > ys[length(ys)]) 
    ys <- c(ys, ys[length(ys)] + extent)

  pmx <- xs[-length(xs)] + extent / 2
  pmy <- ys[-length(ys)] + extent / 2

  nlon <- rep(NA, length(lon))
  for (i in 1:nc) {
    for(j in 2:length(xs)) {
      if(lon[i] > xs[j-1] & lon[i] <= xs[j]) 
        nlon[i] <- pmx[j-1]
    }
  }     

  nlat <- rep(NA, length(lat))
  for (i in 1:nr) {
    for(j in 2:length(ys)) {
      if(lat[i] > ys[j-1] & lat[i] <= ys[j]) 
        nlat[i] <- pmy[j-1]
    }    
  }     
   
  pmx <- pmx[pmx %in% unique(nlon)]
  pmy <- pmy[pmy %in% unique(nlat)]
   
  X@lon <- nlon
  X@lat <- nlat
  persf <- format(X@period$tmStart, "%Y-%m-%d")
  Xl <- satinDataframe(X)
  Xl <- reshape(Xl, direction="long", varying = names(Xl)[3:ncol(Xl)], v.names = attr(Xl, "attribs")$name)
  tfp <- tfd <- as.factor(Xl$time)

  if (length(dim(z)) > 3){
    levels(tfd) <- rep(X@depth, each=(length(persf)))
    levels(tfp) <- rep(persf, length(X@depth))
    sx <- tapply(Xl[, 4], list(Xl$y, Xl$x, tfp, tfd), FUN, na.rm=TRUE)
  } else {
    levels(tfp) <- persf
    sx <- tapply(Xl[, 4], list(Xl$y, Xl$x, tfp), FUN, na.rm=TRUE)
  }
  dimnames(sx) <- NULL
  ans <- satin(lon = pmx, lat = pmy, data = sx, attribs = X@attribs, 
             period = X@period, depth = X@depth)
  ans
}

