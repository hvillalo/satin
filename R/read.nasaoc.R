read.nasaoc <-
  function(nc, lons, lats) 
  {
    ni <- length(nc)
    tmStart <- tmEnd <- numeric(ni)
    
    # open nc file
    ncf <- nc_open(nc[1])
    nv <- 1
    tempRng <- ncatt_get(ncf, varid=0)[['temporal_range']]
    if(is.null(tempRng))
      tempRng <- "daily" 
    nm <- names(ncatt_get(ncf, varid=0))
    srvn <- nm[grep("^spatial", nm)]
    spatRes <- ncatt_get( ncf, varid=0)[[srvn]]
    vtitle <- ncatt_get( ncf, varid=0)[['title']]
    type <- unlist(strsplit(vtitle, split = " "))[1]
    if ( type == "AVHRR" ) 
      nv <- grep("temperature", names(ncf$var), ignore.case = TRUE)
    
    # get variable name and units
    vname <- names(ncf$var)[nv]
    vlongname <- ncf$var[[vname]]$longname
    vunits <- ncf$var[[vname]]$units
    na <- ncf$var[[vname]]$missval

    # read longitudes and latitudes
    lon <- as.vector(ncf$dim$lon$vals)
    lat <- as.vector(ncf$dim$lat$vals)
    xlon <- lon >= min(lons) & lon <= max(lons)
    ylat <- lat >= min(lats) & lat <= max(lats)
    longit <- lon[xlon]
    latit <- sort(lat[ylat])
    st <- c(which(lon == min(longit)), which(lat == max(latit)))
    cnt <- c(length(longit), length(latit))
    if ( type == "AVHRR" ) {
      st <- c(st, 1)
      cnt <- c(cnt, 1) 
    }
    
    D <- array(NA, dim = c(length(latit), length(longit), ni))
    nc_close(ncf)

    # process nc file(s)
    for (h in 1:ni) {
      ncf <- nc_open(nc[h])
      type <- unlist(strsplit(vtitle, split = " "))[1]
      if ( type == "AVHRR" ) 
        nv <- grep("temperature", names(ncf$var), ignore.case = TRUE)
      vn <- names(ncf$var)[nv]
      if (vname != vn)
        stop(paste("type of variable is different in at least one nc file:", vname, vn))
      
      # time coverage	
      tst <- ncatt_get(ncf, varid=0)$time_coverage_start
      ten <- ncatt_get(ncf, varid=0)$time_coverage_end
      if (nchar(tst) < 19){
        tst <- as.POSIXct(tst, format="%Y%m%dT%H%M%S", tz="UTC")
        ten <- as.POSIXct(ten, format="%Y%m%dT%H%M%S", tz="UTC")
      } else {
        tst <- as.POSIXct(tst, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
        ten <- as.POSIXct(ten, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
      }
      tmStart[h] <- tst
      #if ( tst != ten )
      tmEnd[h] <- ten
      
      # read data
      data <- ncvar_get(ncf, varid=vn, start = st, count = cnt)
      data <- t(data)#[xlon, ylat])
	  # AVHRR quality level
	  if (type == "AVHRR") {
	    mask <- ncvar_get(ncf, varid="quality_level", start = st, count = cnt)
	    mask <- t(mask)
	    data[mask < 4] <- NA # keep only acceptable and best quality
	  }
      aoi <-  data[sort.list(nrow(data):1), ]
      aoi[aoi == na] <- NA
      nc_close(ncf)
      D[ , , h] <- aoi
    }  
    avps <- list(tmStart = as.POSIXct(tmStart, origin = "1970-01-01", tz="UTC"), 
                 tmEnd = as.POSIXct(tmEnd, origin = "1970-01-01", tz="UTC"))
		
    ans <- satin(lon = longit, lat = latit, data = D, 
               attribs = list(title = vtitle, longname = vlongname, name = vname, 
                              units = vunits, temporal_range = tempRng,
                              spatial_resolution = spatRes), 
               period = avps, depth = numeric())
    ans
  }
