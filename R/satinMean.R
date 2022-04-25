satinMean <-
function(X, by = "%m", depth = NULL) 
{
  if (!inherits(X, "satin"))
    stop ("need object of class 'satin'")
  # Implemented averaging periods in "by". This includes two non-standard 
  # conversion specifications and their combinations with year: 
  # "%qtr", and "%sem", for quarter and semester, respectively. 
  iby <- c("%Y", "%m", "%Y-%m", "%j", "%qtr", "%Y-%qtr", "%sem", "%Y-%sem",
           "%U", "%V", "%W", "%Y-%U", "%Y-%V", "%Y-%W")
  tr <- c("yearly", rep("monthly", 2), "dayly", rep("quarterly", 2),
             rep("semesterly", 2), rep("weekly", 6))
  if (!by %in% iby )
    stop (paste("by =", by, "not implemented"))
  z <- X@data
  if (length(dim(z)) > 3) {
    if (missing(depth))
      depth <- 1
    X@depth <- X@depth[depth]	  
    z <- z[, , , depth]   
    d <- paste(round(X@depth, 2), "m")
  } else {
    d <- "0 m"
  }
  
  # get time vectors
  tvs <- X@period$tmStart
  tve <- X@period$tmEnd
  tv <- tvs + (tve[1]-tvs[1])/2
  
  if (by %in%  iby[-c(5:8)])
    idx <- format(tv, by)
  if (by == "%qtr")
    idx <- quarters(tv)
  if (by == "%Y-%qtr"){
    q <- quarters(tv)
    y <- format(tv, "%Y")
    idx <- paste(y, q, sep = "-")
  }  
  if (by == "%sem"){
    m <- as.numeric(format(tv, "%m"))
    idx <- ifelse(m <= 6, "S1", "S2")
  }  
  if (by == "%Y-%sem"){
    m <- as.numeric(format(tv, "%m"))
    s <- ifelse(m <= 6, "S1", "S2")
    y <- format(tv, "%Y")
    idx <- paste(y, s, sep = "-")
  }
  uidx <- unique(idx)
  nidx <- length(uidx)
  tmS <- tmE <- numeric(nidx)
  
  # build labels
  ssp <- substr(uidx[1], 5, 5)
  if (ssp == '-'){
    suidx <- unlist(strsplit(uidx, split="-"))  
    suidx <- suidx[seq(2, length(suidx), 2)]
  }  
  if (ssp == ""){
    suidx <- uidx
    if (inherits(suidx, 'integer'))
      suidx <- paste("0", suidx, sep="")
  }  
  
  lab <- paste(substr(tr[which(iby == by)], 1, 1), suidx, sep="-")
  
  for (k in 1:nidx) tmS[k] <- min(tvs[idx == uidx[k]])
  for (k in 1:nidx) tmE[k] <- max(tve[idx == uidx[k]])
    
  satM <- array(NA, dim=c(nrow(z), ncol(z), nidx) )
  
  for (k in 1:nidx) {
   x <- z[, , idx == uidx[k]]
   sumpix <- apply(!is.na(x), MARGIN=c(1, 2), "sum")
   coverage <- sumpix/dim(x)[3]
   satM[, , k] <- apply(x, MARGIN=c(1, 2), "mean", na.rm=TRUE)
   satM[, , k][coverage == 0] <- NA
  }
  
  X@data <- satM
  X@attribs$temporal_range <- tr[which(iby == by)]
  X@period <- list(tmStart = as.POSIXct(tmS, origin = "1970-01-01", tz="UTC"),
                   tmEnd = as.POSIXct(tmE, origin = "1970-01-01", tz="UTC"))
  X@attribs$labels <- lab
  X
}