read.cmems <-
function(nc) 
{
  if (length(nc) > 1)
    stop("this function expects only one Copernicus nc file")
  
  # open nc file
  ncf <- nc_open(nc)
  
  # Processing dates
  chut <- ncf$dim$time$units
  chut <-  unlist(strsplit(  chut, " "))
    ut <- chut[1]
    if ( ut %in% c("hour", "hours") ) {
      fac <- 60*60
    } else {
      fac <- 1
    }  
    if ( length(chut) > 3)
      or <- paste(chut[3], chut[4])
    if ( length(chut) == 3)
      or <- chut[3]
    
  ti <- ncf$dim$time$vals
  tmStart <- as.POSIXct(ti*fac, tz="UTC", format="%Y-%m-%d", origin=or)
  avps <- list(tmStart = tmStart, tmEnd = tmStart)
  
  # read longitudes, latitudes, and depths
  lat <- as.vector(ncf$dim$lat$vals)
    if (lat[1] - lat[length(lat)] > 0){
      lat <- sort(lat)
      flip <- TRUE
    } else {
      flip <- FALSE
    }
  lon <- as.vector(ncf$dim$lon$vals)
  depth <- as.vector(ncf$dim$depth$vals)

  dt <- as.numeric(difftime(tmStart[2], tmStart[1], units="days"))
  dt <- if(dt > 25){
    tempRng <- "monthly"
  } else {
    tempRng <- "daily"
  }  
  spatRes <- round(distGeo(p1=c(lon[1], lat[1]), p2=c(lon[1], lat[2]))/1000, digits=1)
  spatRes <- paste(spatRes, "km")
  
  # variables in nc file
  vars <- names(ncf$var)
  nv <- length(vars)
  ans <- list()  
  
  # process variables and depths in nc file
  for (k in 1:nv) {
   vn <- vars[k]
   VAo <- ncvar_get(ncf, vn)
   dimvar <- dim(VAo)
   if (length(dimvar) == 3){
     nd <- 1
     dim(VAo) <- c(dimvar[1:2], nd, dimvar[3])
   }
   ni <- dim(VAo)[4]
   nd <- dim(VAo)[3]
   VA <- array(NA, dim = c(length(lat), length(lon), ni, nd)) 
   for (i in 1:ni){
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
