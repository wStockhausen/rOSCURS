#'
#' @title Run OSCURS and plot results
#'
#' @description Function to run OSCURS and plot results.
#'
#' @param fnBase - base for output filenames (default = "./OSCURS_")
#' @param nDays - number of days to run model
#' @param stYrs - vector of years for particle releases
#' @param stMDs - list of release dates, by month
#' @param fnStartLocs - filename for csv file with initial particle lat/lon locations
#' @param link - url to run OSCURS (default = "https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?")
#' @param strCRS - character representation of coordinate reference system for final map (default is Alaska Albers [EPSG=3338])
#' @param basemap - a base map for plotting the tracks (default is the EBS using CRS defined by strCRS)
#' @param  alpha - transparency for track lines
#' @param showMap - flag to show the map
#' @param verbose - flag to print diagnostic info
#'
#' @return list with elements "map" and  "tracks".
#'
#' @details Requires packages \code{readr}, \code{tmaptools}, \code{wtsGIS}.
#'
#' @export
#'
doOSCURS<-function(fnBase="OSCURS_",
                   nDays=90,           #number of days to track
                   stYrs=2017,         #years for releases
                   stMDs=list(APR=15), #months and days for releases
                   fnStartLocs="OSCURS_StartLocations.csv",
                   link="https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?",
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
                 nDays=nDays,         #number of days to track
                 stYrs=stYrs,         #years for releases
                 stMDs=stMDs,         #months and days for releases
                 stLLs=startLocs,
                 test=FALSE,
                 verbose=verbose);

  #convert OSCURS output to list with data.frame and sf tibble with a WGS84 lat/lon crs.
  lst<-convertOSCURStoTbl(fnBase=fnBase,
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

  return(map=map,tracks=lst$tracks);
}

