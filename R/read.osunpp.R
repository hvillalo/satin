read.osunpp <-
function(h5, lons, lats)
{
  ni <- length(h5)
  tmStart <- tmEnd <- numeric(ni)
 
  # open h5 file
  ncf <- nc_open(h5)
  
  # get attributes 
  vname <- names(ncf$var)[1]
  allAtt <- ncatt_get( ncf, varid=vname )
  lim <- allAtt$Limit
  vunits <- allAtt$Units
  hole <- allAtt$`Hole Value`
  vdim <- ncf$var[[vname]]$varsize
  
  # define missing attributes
  tempRng <- "8 day"
  vtitle <- "Ocean Productivity"
  vlongname <- "Ocean Net Primary Production"
  spatRes <- "9.26 km"
  if(all(vdim == c(2160, 1080)))
    spatRes <- "18.52 km"
  
  # process longitudes and latitudes 
  lon <- seq(lim[2], lim[4], len=vdim[1])
  lat <- seq(lim[1], lim[3], len=vdim[2])
  xlon <- lon >= min(lons) & lon <= max(lons)
  ylat <- lat >= min(-lats) & lat <= max(-lats)
  longit <- lon[xlon]
  latit <- sort(-lat[ylat])
  st <- c(min(which(xlon)), min(which(ylat)))
  cnt <- c(length(longit), length(latit))
  
  # create array for storing data
  D <- array(NA, dim = c(length(latit), length(longit), ni))
  nc_close(ncf)
  
  # process hdf5 file(s)
  for (h in 1:ni) {
    ncf <- nc_open(h5[h])
    vn <- names(ncf$var)[1]
    if (vname != vn)
      stop(paste("type of variable is different in at least one file:", vname, vn))

   # time coverage
   tst <- ncatt_get(ncf, varid=0)$'Start Time String_GLOSDS'
   ten <- ncatt_get(ncf, varid=0)$'Stop Time String_GLOSDS'
     tst <- as.POSIXct(tst, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
     ten <- as.POSIXct(ten, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
    tmStart[h] <- tst
    tmEnd[h] <- ten
    
    # read data
    data <- ncvar_get(ncf, varid=vn, start = st, count = cnt)
    data <- t(data)
    aoi <-  data[sort.list(nrow(data):1), ]
    aoi[aoi == hole] <- NA
    nc_close(ncf)
    D[ , , h] <- aoi
  }
  avps <- list(tmStart = as.POSIXct(tmStart, origin = "1970-01-01", tz="UTC"), 
                 tmEnd = as.POSIXct(tmEnd, origin = "1970-01-01", tz="UTC"))
  if (as.numeric((ten - tst)) > 10) 
    tempRng <- "monthly"
  
  ans <- new("satin", lon = longit, lat = latit, data = D, attribs = list(title = vtitle, 
             longname = vlongname, name = vname, units = vunits, temporal_range = tempRng, spatial_resolution = spatRes), period = avps, depth = numeric())
  ans
}
