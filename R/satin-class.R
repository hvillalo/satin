satin <- setClass("satin",
  slots = c(
    lon = "numeric", 
    lat = "numeric", 
    data = "array", 
    attribs = "list", 
    period  = "list",
    depth = "numeric"
  )
)

setValidity("satin", method = 
    function(object){
      lx <- length(object@lon)
      ly <- length(object@lat)
      lp <- length(object@period$tmStart)
      ld <- length(object@depth)
      ddata <- dim(object@data)
      # check data dimensions
      if (lx != ddata[2])
        return(paste("longitudes mismatch ", lx, " != ", ddata[2], sep = ""))
      if (ly != ddata[1])
        return(paste("latitudes mismatch ", ly, " != ", ddata[1], sep = ""))
      if (lp != ddata[3])
        return(paste("periods mismatch ", lp, " != ", ddata[3], sep = ""))
      # if different depths
      if (length(ddata) == 4){
        if (ld != ddata[4])
         return(paste("depths mismatch ", ld, " != ", ddata[4], sep = ""))
      }  
      # check class of periods
      if (!inherits(object@period$tmStart, "POSIXct"))
        return(paste("periods must be of class POSIXct"))
      if (length(object@period) == 2){
        if (!inherits(object@period$tmEnd, "POSIXct"))
          return(paste("periods must be of class POSIXct"))
      }
    }
)

"print.satin" <- function(x, ...){
  X <- list()
  X[["class"]] <- class(x)
  X[["attribs"]] <- x@attribs
  X[["dims"]] <- dim(x@data)
  vn <- x@attribs$name
  rng.lon <- range(x@lon)
  rng.lat <- range(x@lat)
  rng.data <- c(range(as.vector(x@data), na.rm=TRUE))
  rng.per <- c(format(min(x@period$tmStart), "%Y-%m-%d"), 
               format(max(x@period$tmEnd), "%Y-%m-%d"))
  ans <- data.frame(lon=rng.lon, lat=rng.lat, rng.data, period=rng.per)
  row.names(ans) <- c("min", "max")
  names(ans)[3] <- vn
  if (length(x@depth) > 0){
    rng.dep <- range(x@depth)
    ans$depth <- rng.dep
  }
  X[["ans"]] <- ans
  
  cat(paste("Object of class ", X[["class"]], "\n", sep = ""))
  cat("\n", "Title:", unlist(X[["attribs"]])[1], "\n", "Long name:", 
      unlist(X[["attribs"]])[2], "\n", "Name:", unlist(X[["attribs"]])[3], 
      "\n", "Units:", unlist(X[["attribs"]])[4], "\n",
      "Temporal range:", unlist(X[["attribs"]])[5], "\n",
      "Spatial resolution:", unlist(X[["attribs"]])[6], "\n")
  cat("\nData dimensions:\n", X[["dims"]], "\n")
  cat("\nData ranges:\n")
  print(X[["ans"]])
  invisible(X)
}

setMethod("show", "satin", function(object) print.satin(object))

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...)
    standardGeneric("plot"))

setMethod("plot", 
          signature(x = "satin", y = "missing"), 
          function(x, ...) plot.satin(x, ...))
