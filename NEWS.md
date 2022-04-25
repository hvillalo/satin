# Changes to Version 1.0.3

* velocity(), a new function for calculating current speed and direction have been added. 
* Improved date handling in read.cmems() for daily data. 
* satinMean() no longer depends on lubridate's functions.
 
# Changes to Version 1.0.2

* gshhg database's hyperlink changed from ftp:// to htpps://

# Changes to Version 1.0.1

* Fixed read.osunpp() function that was unable to import monthly datasets due to different names found in h5 files ('Start Time String_GLOSDS' in 8-day files, 'Start Time String' in monthly files).
 