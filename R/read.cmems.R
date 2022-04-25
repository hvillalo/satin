read.cmems <-
function(nc) 
{
  if (length(nc) > 1)
    stop("this function expects only one Copernicus nc file")
  
  # open nc file
  ncf <- nc_open(nc)
  
  # Processing dates
  # factors for converting time to seconds
  tf <- data.frame(ut = c("seconds", "hour", "hours", "days"), 
                   fc = c(1, 60*60, 60*60, 24*60*60))
  chut <- ncf$dim$time$units
    chut <-  unlist(strsplit(chut, " "))
    ut <- chut[1]
  ti <- ncf$dim$time$vals
  fc <- tf$fc[tf$ut == ut]
  ti <- ti * fc
  # origin
  if ( length(chut) > 3)
    or <- paste(chut[3], chut[4])
  if ( length(chut) == 3)
    or <- chut[3]
  
  np <- length(ti)
  tmStart <- as.POSIXct(ti, tz = "UTC", format = "%Y-%m-%d", origin = or)
  avps <- list(tmStart = tmStart, tmEnd = tmStart)
  
  # determine temporal range
  if ( np > 1){
    dt <- as.numeric(difftime(tmStart[2], tmStart[1], units = "days"))
    if(dt > 27){
      tempRng <- "monthly"
    } else {
      tempRng <- "daily"
    }
  } else {
    tempRng <- "unknown"
  }
  
  # read longitudes, latitudes, and depths
  lat <- as.vector(ncf$dim$lat$vals)
    if (lat[1] - lat[length(lat)] > 0){
      lat <- sort(lat)
      flip <- TRUE
    } else {
      flip <- FALSE
    }
  nlat <- length(lat)
  lon <- as.vector(ncf$dim$lon$vals)
  if (any(lon > 180))
    lon <- lon - 360
  nlon <- length(lon)
  depth <- as.vector(ncf$dim$depth$vals)
  if (is.null(depth))
    depth <- 0
    
  # determine spatial resolution
  spatRes <- round(distGeo(p1=c(lon[1], lat[1]), p2=c(lon[1], lat[2]))/1000, digits=1)
  spatRes <- paste(spatRes, "km")
  
  # variables in nc file
  vars <- names(ncf$var)
  nv <- length(vars)
  ans <- list()  
  
  # process variables and depths in nc file
  for (k in 1:nv) {
   nd <- length(depth)
   vn <- vars[k]
   VAo <- ncvar_get(ncf, vn)
   ndim <- length(dim(VAo))
   if (ndim == 2)
     nd <- 1
   if (ndim == 3){
     if (np == 1){
       nd <- dim(VAo)[3]
     } else {
       nd <- 1
     }
   }   
   dim(VAo) <- c(nlon, nlat, nd, np)
   VA <- array(NA, dim = c(nlat, nlon, np, nd)) 
   for (i in 1:np){
       for (d in 1:nd){
         mat <- t(VAo[ , , d, i])
         if ( flip )
           mat <- mat[nrow(mat):1, ]
         VA[ , , i, d] <- mat
        }     
    }
   units <- ncf$var[[vn]]$units
   longname <- ncf$var[[vn]]$longname
   na <- ncf$var[[vn]]$missval
   VA[VA == na] <- NA
   so <- new("satin", lon=lon, lat=lat, data=VA, attribs=list(title=vn, 
             longname=longname, name=vn, units=units, temporal_range=tempRng,
             spatial_resolution=spatRes), period=avps, depth=depth[1:nd]) 
   assign(vn, so)
   ans[[k]] <- get(vn)
  }  
  nc_close(ncf)
  names(ans) <- vars
  if (nv == 1)
    ans <- ans[[1]]
  ans
}
