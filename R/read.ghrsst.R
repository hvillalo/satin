read.ghrsst <-
  function(nc, lons, lats) 
  {
    ni <- length(nc)
    tmStart <- tmEnd <- numeric(ni)
    
    # open nc file
    ncf <- nc_open(nc[1])
    nv <- 1
    tempRng <- "daily" 
	nm <- names(ncatt_get(ncf, varid=0))
    srvn <- nm[grep("^spatial", nm)]
	spatRes <- ncatt_get( ncf, varid=0)[[srvn]]
    vtitle <- ncatt_get( ncf, varid=0)$title    
	
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
    st <- c(which(lon == min(longit)), which(lat == min(latit)), 1)
    cnt <- c(length(longit), length(latit), 1)

	D <- array(NA, dim = c(length(latit), length(longit), ni))
    nc_close(ncf)
    
    # process nc file(s)
    for (h in 1:ni) {
      ncf <- nc_open(nc[h])
      vn <- names(ncf$var)[nv]
      if (vname != vn)
        stop(paste("type of variable is different in at least one nc file:", vname, vn))
      
      # time coverage	
      tst <- ncatt_get(ncf, varid = 0)$time_coverage_start
      ten <- ncatt_get(ncf, varid = 0)$time_coverage_end
        tst <- as.POSIXct(tst, format = "%Y%m%dT%H%M%S", tz = "UTC")
        ten <- as.POSIXct(ten, format = "%Y%m%dT%H%M%S", tz = "UTC")
      tmStart[h] <- tst
      tmEnd[h] <- ten	  
      
      # read data
      data <- ncvar_get(ncf, varid = vn, start = st, count = cnt)
      data <- t(data)
      # read mask:  1=open-sea; 2=land; 3=coast/shore; 5=open-lake; 9=open-sea with ice in the grid;
      # 11=coast/shore with ice in the grid; 13=open-lake with ice in the grid
      mask <- ncvar_get(ncf, varid="mask", start = st, count = cnt)
      mask <- t(mask)
      data[mask != 1] <- NA # keep only open-sea
      aoi <-  data
      aoi[aoi == na] <- NA
      nc_close(ncf)
      D[ , , h] <- aoi
    }  
    avps <- list(tmStart = as.POSIXct(tmStart, origin = "1970-01-01", tz = "UTC"), 
                 tmEnd = as.POSIXct(tmEnd, origin = "1970-01-01", tz = "UTC"))
		
    ans <- satin(lon = longit, lat = latit, data = D, 
               attribs = list(title = vtitle, longname = vlongname, name = vname, 
                              units = vunits, temporal_range = tempRng,
                              spatial_resolution = spatRes), 
		       period = avps, depth = numeric())
    ans
  }
