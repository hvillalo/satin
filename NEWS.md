# Changes to Version 1.2.0
* Added missing package anchors in Rd files.
* New argument, Pacific.centric = TRUE, in plot.satin() allows for maps centered in the Pacific.

# Changes to Version 1.1.0
* New argument (restore.par = FALSE) added to plot.satin() allowing to annotate plots, e.g. add isolines.
 
# Changes to Version 1.0.3

* velocity(), a new function for calculating current speed and direction have been added. 
* Improved date handling in read.cmems() for daily data. 
* satinMean() no longer depends on lubridate's functions.
 
# Changes to Version 1.0.2

* gshhg database's hyperlink changed from ftp:// to htpps://

# Changes to Version 1.0.1

* Fixed read.osunpp() function that was unable to import monthly datasets due to different names found in h5 files ('Start Time String_GLOSDS' in 8-day files, 'Start Time String' in monthly files).
 
 
 @