verticalProfiles <- 
function(X, point = NULL, xlim = NULL, ylim = NULL) 
{
  if (!inherits(X, "satin"))
    stop ( "need object of class 'satin'" )
  nd <- length(X@depth)
  if (nd < 5)
    stop("there's no point in drawing vertical profiles with so few depths")
  if (missing(point)) {
    xx <- extractPts(X)  
  } else {
    xx <- extractPts(X, point)
  }
  
  # construct data matrix
  ni <- ncol(xx) - 6
  np <- length(X@period$tmStart)
  ans <- as.data.frame(matrix(NA, ncol = np + 1, nrow = nd))
  ans[, 1] <- X@depth 
  for (k in 1:np){
    ans[, k + 1] <- as.vector(t(xx[1, seq(6 + k, ni + 6, np)]))
  }                    
  
  names(ans) <- c("depth", format(X@period$tmStart, "%Y-%m-%d"))
  row.names(ans) <- NULL
  attribs <-  X@attribs
  attribs$period <- X@period
  attribs$depth <- X@depth
  attr(ans, "attribs") <- attribs

  vn <- attribs$name    
  mt <- round(xx[, c('lon', 'lat')], 2)
  mt <- paste("(", mt[1], ", ", mt[2], ")", sep="")

  if (missing(xlim))
    xlim <- c(0, max(ans[, 2:(np+1)], na.rm = TRUE))
  if (missing(ylim))
    ylim <- c(0, max(ans$depth))

  op <- par(no.readonly = TRUE)  
  on.exit(par(op))
  par(mfrow = c(1, 1))
  plot(ans[, 2], ans$depth, type = "n", ylim = rev(ylim), xlim, main = mt,
       xlab = paste(vn, " (", attribs$units, ")", sep = ""), ylab = "Depth (m)")
  abline(h = ans$depth, lty = 3, col="grey")
  matlines(ans[, 2:(np + 1)], ans$depth, col = rgb(0, 0, 1, 0.3))
  return(ans)
  invisible()
}