#' Get names of variables in a netCDF file
#' 
#' This function returns names and long names of the variables in a netCDF file.
#'
#' @param nc name of a netCDF file.
#'
#' @details This function provides a practical way to know which variables are
#' contained in a netCDF file before importing the data.
#' 
#' @return This function only displays the number of variables, their names,
#' and corresponding long names.
#'
#' @author Héctor Villalobos.   
#'
#' @examples
#' if(interactive()){
#'   name_ncvars("global-reanalysis-phy-001-030-monthly_1553098099635.nc")
#' }
#'
name_ncvars <- function(nc) {
  ncf <- nc_open(nc)
  Vars <- names(ncf$var)
  nv <- length(Vars)
  longname <- rep("", nv)
  for(k in 1:nv){
    longname[k] <- ncf$var[[k]]$longname
  }
  nc_close(ncf) 
 
  cat("\nThere are", nv, "variables in nc file:\n\n", 
      paste(Vars, collapse = "; "), 
      "\n\nLong names:\n\n", 
      paste(Vars, "-", longname, collapse = "\n "))
}
