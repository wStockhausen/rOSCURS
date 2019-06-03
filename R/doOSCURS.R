#'
#' @title Run OSCURS and plot results
#'
#' @description Function to run OSCURS and plot results.
#'
#' @param fnBase - base for output filenames (default = "OSCURS_")
#' @param path - path to folder in which to store output files
#' @param nDays - number of days to run model
#' @param stYrs - vector of years for particle releases
#' @param stMDs - list of release dates, by month
#' @param fnStartLocs - filename for csv file with initial particle lat/lon locations
#' @param wcsc - wind/current speed coefficient (default=1.0; see https://oceanview.pfeg.noaa.gov/oscurs/)
#' @param wad - wind angle deviation (default=0.0; see https://oceanview.pfeg.noaa.gov/oscurs/)
#' @param gsf - geostrophic speed factor (default=1.0; see https://oceanview.pfeg.noaa.gov/oscurs/)
#' @param link - url to run OSCURS (default = "https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?")
#' @param randNum - random number to add to server-side file name to ensure uniqueness
#' @param strCRS - character representation of coordinate reference system for final map (default is Alaska Albers [EPSG=3338])
#' @param basemap - a base map for plotting the tracks (default is the EBS using CRS defined by strCRS)
#' @param  alpha - transparency for track lines
#' @param showMap - flag to show the map
#' @param verbose - flag to print diagnostic info
#'
#' @return same list as \code{plotOSCURS} (see @details).
#'
#' @details Requires packages \code{readr}, \code{tmaptools}, \code{wtsGIS}.
#' The returned list has the following elements:
#'  * map       - a tmap object
#'  * tracks    - a sf tibble with tracks that span the IDL to "roll your own" map
#'  * startLocs - a sf tibble with starting track locations as points to "roll your own" map
#'  * basemap   - a basemap to "roll your own" map

#'
#' @export
#'
doOSCURS<-function(fnBase="OSCURS_",
                   path=".",
                   nDays=90,           #number of days to track
                   stYrs=2017,         #years for releases
                   stMDs=list(APR=15), #months and days for releases
                   fnStartLocs="OSCURS_StartLocations.csv",
                   wcsc=1.0,
                   wad=0.0,
                   gsf=1.0,
                   link="https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?",
                   randNum=round(runif(1,1,100000)),
                   strCRS=tmaptools::get_proj4(3338,output="character"),
                   basemap=wtsGIS::createBaseTMap(layer.land=wtsGIS::getPackagedLayer("Alaska"),
                                                  layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                                                  strCRS.finl=strCRS),
                   alpha=1,
                   showMap=TRUE,
                   verbose=FALSE){

  #--create a tibble from the csv file with starting particle locations
  startLocs<-readr::read_csv(fnStartLocs);

  #--run the OSCURS model for all years, months, days, and particle release locations
  res<-runOSCURS(fnBase=fnBase,
                 path=path,
                 nDays=nDays,         #number of days to track
                 stYrs=stYrs,         #years for releases
                 stMDs=stMDs,         #months and days for releases
                 stLLs=startLocs,
                 wcsc=wcsc,
                 wad=wad,
                 gsf=gsf,
                 link=link,
                 randNum=randNum,
                 test=FALSE,
                 verbose=verbose);

  #convert OSCURS output to list with data.frame and sf tibble with a WGS84 lat/lon crs.
  lst<-convertOSCURStoTbl(fnBase=file.path(path,fnBase),
                          stYrs=stYrs,         #years for releases
                          stMDs=stMDs,         #months and days for releases
                          stLLs=startLocs,
                          verbose=verbose);

  map<-plotOSCURS(tracks=lst$tracks,
                  stLLs=startLocs,
                  strCRS=strCRS,
                  basemap=basemap,
                  alpha=alpha,
                  showMap=showMap,
                  verbose=verbose);

  return(map);
}

