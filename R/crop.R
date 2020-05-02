crop <- 
function(X, polygon = NULL, return.poly = FALSE)
{
  if (!inherits(X, "satin"))
    stop ( "need object of class 'satin'" )
  if (missing(polygon))
    polygon <- getpoly()      
  if (!identical(polygon[1, ], polygon[nrow(polygon), ]))
    polygon <- rbind(polygon, polygon[1, ])
  polygon <- as.data.frame(polygon)
  names(polygon) <- c("x", "y")
  Xr <- range(polygon$x)
  Yr <- range(polygon$y)
  Xl <- satinDataframe(X)
  outPts <- !inout(Xl[, 1:2], polygon, bound = TRUE, quiet = TRUE)
  Xl[outPts, -c(1, 2)] <- NA
  Xl <- Xl[Xl$x >= Xr[1] & Xl$x <= Xr[2], ]
  Xl <- Xl[Xl$y >= Yr[1] & Xl$y <= Yr[2], ]
  ans <- satinDataframe(Xl, reverse = TRUE)
  if (return.poly == TRUE)
    ans <- list( aoi = ans, polygon = polygon )
  ans
}