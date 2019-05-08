#'
#' @title Parse an OSCURS output trajectory file to a dataframe
#'
#' @description Function to parse an OSCURS output trajectory file to a dataframe.
#'
#' @param fn - name of file to parse
#' @param verbose - flag to print diagnostic info
#'
#' @return list with elements "dfr" and "track". The latter is a spatial tibble using sf classes.
#'
#' @details Parses output text file from OSCURS run. Requires packages \code{stringr}, \code{tibble} and
#' \code{tmap_tools}.
#'
#' @export
#'
parseFile.OSCURS<-function(fn,
                           verbose=FALSE){
  con<-file(fn,open="rt");
  txt<-readLines(con=con);
  close(con);
  n<-length(txt);
  lat <-  as.numeric(stringr::str_split(string=txt[1],pattern=":|,")[[1]][2]);
  lon <- 360 -as.numeric(stringr::str_split(string=txt[2],pattern=":|,")[[1]][2]);#converted to east longitude, 0-360
  yr  <-  as.numeric(stringr::str_split(string=txt[3],pattern=":|,")[[1]][2]);
  mn  <-  as.numeric(stringr::str_split(string=txt[4],pattern=":|,")[[1]][2]);
  dy  <-  as.numeric(stringr::str_split(string=txt[5],pattern=":|,")[[1]][2]);

  nd<-as.numeric(stringr::str_split(string=txt[6],pattern=":|,")[[1]][2]);
  dt<-vector(mode="character",length=nd+2);
  lt<-vector(mode="numeric",length=nd+2);
  ln<-vector(mode="numeric",length=nd+2);
  dt[1]<-paste(yr,mn,dy,sep="-");
  lt[1]<-lat;
  ln[1]<-lon;
  for (i in 1:(nd+1)){
    trk<-stringr::str_split(string=txt[9+i],pattern='\\[|,|\\]|\\\"')[[1]];
    dt[i+1]<-stringr::str_sub(trk[3],1,10); #extract date as string
    lt[i+1]<-as.numeric(trk[5]);            #extract lat
    ln[i+1]<-as.numeric(trk[6]);            #extract lon as east longitude, 0-360
  }
  ln<-atan2(sin(2*pi*ln/360),cos(2*pi*ln/360))*360/(2*pi);#convert to east longitude, -180-180
  dt<-as.Date(dt,"%Y-%m-%d");#convert date strings to Dates
  dt[nd+2]<-dt[nd+2]+1;      #need to round up on last date (position given at end of day)
  edt<-as.numeric(dt-dt[1]); #calculate elapsed time, in days

  dfr<-data.frame(dayStart=dt[1],latStart=lt[1],lonStart=ln[1],
                  date=dt,elapsedTime=edt,lat=lt,lon=ln,stringsAsFactors=FALSE);
  if (verbose) print(head(dfr))
  sfg.line<-sf::st_linestring(as.matrix(dfr[,c("lon","lat")]),dim="XY");
  if (verbose) print(sfg.line);
  if (verbose) cat("class = ",class(sfg.line),"\n");
  sfc.line<-sf::st_sfc(sfg.line);
  if (verbose) str(tibble::tibble(dayStart=dt[1],latStart=lt[1],lonStart=ln[1],
                                  dayEnd=dt[nd+2],latEnd=lt[nd+2],lonEnd=ln[nd+2]));
  tbl.track<-tmaptools::append_data(sfc.line,
                                    tibble::tibble(dayStart=dt[1],latStart=lt[1],lonStart=ln[1],
                                                   dayEnd=dt[nd+2],latEnd=lt[nd+2],lonEnd=ln[nd+2]),
                                   fixed.order=TRUE);
  return(list(dfr=dfr,track=tbl.track));
}
