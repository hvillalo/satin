extractPts <-
function(X, points = NULL)
{
  if (!inherits(X, "satin"))
    stop ("need object of class 'satin'")
  sL <- satinDataframe(X)
  nc <- ncol(sL)
  np <- length(X@period$tmStart)
  depths <- X@depth
  if (length(dim(X@data)) > 3){
   nd <- length(depths)
   ni <- nd * np
  } else {
   ni <- np
   nd <- 1
  } 
	
  if (missing(points)) {
    pts <- locator(type = "p", col = "white", cex = 1.5)       
    points <- as.data.frame(pts)
  }
  
  nPts <- nrow(points)
  param <- matrix(rep(NA, ni * nPts), ncol = ni)
  ans <- data.frame(id = 1:nPts, points, d = rep(NA, nPts), 
                       lon = rep(NA, nPts), lat = rep(NA, nPts), 
                       data = param)
  names(ans)[(ncol(ans)-ni+1):ncol(ans)] <- names(sL)[3:nc]
  
  attribs <-  X@attribs
  attribs$period <- X@period
  attribs$depth <- X@depth
  attribs$labels <- paste("depth", rep(round(X@depth, 1), each = np), "_", "T", 
                        rep(format(X@period$tmStart, "%Y-%m-%d"), nd), sep = "")
  
  for (i in 1:nPts){
    p <- points
    d <- distRhumb(p1 = p[i, ], p2 = sL[, 1:2])/1000 
    ans[i, 'd'] <- round(min(d), digits = 1)
    ans[i, 5:(6+ni)] <- sL[which.min(d), ]
  }
  attr(ans, "attribs") <- attribs
  ans  
}