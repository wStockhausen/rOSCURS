#'
#' @title Collates sets of OSCURS model output to a list with a data.frame and a spatial tibble of type sf
#'
#' @description A function to collate sets of OSCURS model output created using \code{runOSCURS}
#' to a list with a data.frame and a spatial tibble of type sf.
#'
#' @param fnBase - base for OSCURS output filenames to process (default = "./OSCURS_")
#' @param stYrs - start years for OSCURS files (vector)
#' @param stMDs - start months/days for OSCURS files (list)
#' @param stLLs - a dataframe with starting locations for particle releases
#' @param verbose - flag to print diagnostic output
#'
#' @return list with elements "dfr" (a dataframe) and "tracks" (a spatial tibble using sf line geometry classes)
#'
#' @details Requires packages \code{magrittr} and \code{sf}.
#' Note that the coordinate reference system assigned to
#' tbl.track is WGS84, with longitude running -180 to 180 (EPSG=4326).
#'
#' @import magrittr
#'
#' @export
#'
convertOSCURStoTbl<-function(fnBase="./OSCURS_",
                             stYrs=2017,                          #years for releases
                             stMDs=list(APR=15,MAY=c(1,15),JUN=1),#months and days for releases
                             stLLs=NULL,
                             verbose=FALSE){
  # stLLs<-read.csv(stLLs,
  #                 stringsAsFactors = FALSE,
  #                 check.names = FALSE);
  if (is.null(stLLs))
    stop("Must provide parameter 'stLLs', a dataframe with initial particle locations.\n")
  if (!inherits(stLLs,"data.frame"))
    stop("parameter 'stLLs' must be a dataframe or tibble.\n")
  if (!all(c("LATITUDE","LONGITUDE") %in% names(stLLs)))
    stop("parameter 'stLLs' must have columns 'LATITUDE' and 'LONGITUDE'.\n")
  stLLs$LONGITUDE <- -stLLs$LONGITUDE;#change sign on longitude
  stLLs$latdeg <- floor(stLLs$LATITUDE);
  stLLs$latmin <- round(60*(stLLs$LATITUDE %% 1));
  stLLs$londeg <- floor(stLLs$LONGITUDE);
  stLLs$lonmin <- round(60*(stLLs$LONGITUDE %% 1));

  nRws <- nrow(stLLs);
  dfr<-NULL;
  tracks<-NULL;
  for (yr in stYrs){
    for (mon in names(stMDs)){
      for (day in stMDs[[mon]]){
        for (rw in 1:nRws){
          ll <- stLLs[rw,c("latdeg","latmin","londeg","lonmin")];
          fn<-paste0(fnBase,
                     paste(yr,mon,day,ll$latdeg,ll$latmin,ll$londeg,ll$lonmin,sep="_"),
                     ".txt");
          message(paste0("convertOSCURStoTbl: processing '",fn,"'\n\n"));
          if (file.exists(fn)){
            lst<-parseFile.OSCURS(fn,verbose);
            dfr<-rbind(dfr,lst$dfr);
            if (is.null(tracks)){
              tracks<-lst$track;
            } else {
              tracks<-rbind(tracks,lst$track);
            }
          } else {
            message(paste0("--convertOSCURStoTbl: Skipping this one: file does not exist.\n\n"));
          } #--if (file.exists(fn))
        }#--rw loop
      }#--day loop
    }#--mon loop
  }#--yr loop
  tracks <- tracks %>% sf::st_set_crs(4326);#set crs to WGS84, lat/lon (-180 to 180)
  return(list(dfr=dfr,tracks=tracks));
}

