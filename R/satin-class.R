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


validSatin <- function(object){
  lx <- length(object@lon); ly <- length(object@lat)
  lp <- length(object@period$tmStart); ld <- length(object@depth)
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

setValidity("satin", validSatin)

setMethod("show", "satin", function(object){
  x <- list()
  x[["class"]] <- class(object)
  x[["attribs"]] <- object@attribs
  x[["dims"]] <- dim(object@data)
  vn <- object@attribs$name
  rng.lon <- range(object@lon)
  rng.lat <- range(object@lat)
  rng.data <- c(range(as.vector(object@data), na.rm=TRUE))
  rng.per <- c(format(min(object@period$tmStart), "%Y-%m-%d"), 
               format(max(object@period$tmEnd), "%Y-%m-%d"))
  ans <- data.frame(lon=rng.lon, lat=rng.lat, rng.data, period=rng.per)
  row.names(ans) <- c("min", "max")
  names(ans)[3] <- vn
  if (length(object@depth) > 0){
    rng.dep <- range(object@depth)
    ans$depth <- rng.dep
  }
  x[["ans"]] <- ans
  
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("\n", "Title:", unlist(x[["attribs"]])[1], "\n", "Long name:", 
      unlist(x[["attribs"]])[2], "\n", "Name:", unlist(x[["attribs"]])[3], 
      "\n", "Units:", unlist(x[["attribs"]])[4], "\n",
      "Temporal range:", unlist(x[["attribs"]])[5], "\n",
      "Spatial resolution:", unlist(x[["attribs"]])[6], "\n")
  cat("\nData dimensions:\n", x[["dims"]], "\n")
  cat("\nData ranges:\n")
  print(x[["ans"]])
  invisible(x)
})


if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...)
    standardGeneric("plot"))

setMethod("plot", 
          signature(x = "satin", y = "missing"), 
          function(x, ...) plot.satin(x, ...))
