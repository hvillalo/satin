anomaly <-
function(X, Y)
{
  if (!inherits(X, "satin") & !inherits(Y, "satin"))
    stop ("both 'X' and 'Y' must be objects of class 'satin'")
  dmX <- dim(X@data)[1:2]; dmY <- dim(Y@data)[1:2] 
  if (!all(dmX == dmY))
    stop ("first two dimensions of 'X' and 'Y' must be equal")
  labX <- X@attribs$labels
    ulX <- unique(labX)  
  labY <- Y@attribs$labels
  if (all(!ulX %in% labY) & all(!labY %in% ulX))
    stop ("incompatible periods found between 'X' and 'Y'")
  
  ans <- X
  dm <- dim(ans@data)
  x <- X@data
  y <- Y@data
  
  for (k in 1:length(labY)){
    mX <- x[, , labX == labY[k]]
    nm <- dim(mX)[3]
    if (is.na(nm)){
      nm <- 1  
      mX <- array(mX, dim=c(dm[1:2], nm))
    }  
    mY <- array(NA, dim=c(dm[1:2], nm))
    for (m in 1:nm)
     mY[, , m] <- y[, , labY == labY[k]]
    ans@data[, , labX == labY[k]] <- mX - mY
  }
  ans
}
