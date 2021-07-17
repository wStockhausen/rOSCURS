#'
#' @title Make a series of OSCURS runs
#'
#' @description Function to make a series of OSCURS runs and download the results to a set of files.
#'
#' @param fnBase - base for filenames (default = "OSCURS_")
#' @param path - path to use with output filenames (default = ".")
#' @param nDays - number of days to run model
#' @param stYrs - vector of years for particle releases
#' @param stMDs - list of release dates, by month
#' @param stLLs - a dataframe or tibble with initial particle lat/lon locations (see details below)
#' @param wcsc - wind/current speed coefficient (default=1.0; see https://oceanview.pfeg.noaa.gov/oscurs/)
#' @param wad - wind angle deviation (default=0.0; see https://oceanview.pfeg.noaa.gov/oscurs/)
#' @param gsf - geostrophic speed factor (default=1.0; see https://oceanview.pfeg.noaa.gov/oscurs/)
#' @param link - url to run OSCURS (default = "https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?")
#' @param randNum - random number to add to server-side file name to ensure uniqueness
#' @param test - flag to print diagnostic output but NOT run OSCURS (useful to see what commands will be sent)
#' @param verbose - flag to print diagnostic output
#'
#' @return NULL
#'
#' @details Runs the OSCURS model at the link given. An output file is created for each particle release of the
#' form fnBase_year_month_day_latdegree_latminute_londegree_lonminute.csv, where lat and lon is the initial
#' particle location.
#'
#' The input parameter \code{stLLs} should be a dataframe or tibble. The following columns are required:
#'  * LATITUDE   - latitude in decimal degrees (-90 to 90)
#'  * LONGITUDE  - longitude in decimal degrees (-180 to 180)
#'
#' @export
#'
runOSCURS<-function(fnBase="OSCURS_",
                    path=".",
                    nDays=90,                            #number of days to track
                    stYrs=2017,                          #years for releases
                    stMDs=list(APR=15,MAY=c(1,15),JUN=1),#months and days for releases
                    stLLs=NULL,
                    link="https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?",
                    wcsc=1.0,
                    wad=0.0,
                    gsf=1.0,
                    randNum=round(stats::runif(1,1,100000)),
                    test=TRUE,
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

  if (tolower(Sys.info()["sysname"])=="windows"){
    str0<-paste0('wget --wait=60 -O !!lclFile.txt "!!link',
                 'cl=1&latdeg=!!latdeg&latmin=!!latmin&londeg=!!londeg&lonmin=!!lonmin&',
                 'year=!!year&mon=!!mon&day=!!day&nnnn=!!nnn&factor=!!wcsc&angle=!!wad&ddfac=!!gsf&outfile=!!remFile.csv" --no-check-certificate');
  } else {
    str0<-paste0('curl -o !!lclFile.txt "!!link',
                 'cl=1&latdeg=!!latdeg&latmin=!!latmin&londeg=!!londeg&lonmin=!!lonmin&',
                 'year=!!year&mon=!!mon&day=!!day&nnnn=!!nnn&factor=!!wcsc&angle=!!wad&ddfac=!!gsf&outfile=!!remFile.csv"');
  }

  str0 <- gsub("!!link",link,str0,fixed=TRUE);
  str0 <- gsub("!!wcsc",wcsc,str0,fixed=TRUE);
  str0 <- gsub("!!wad", wad,str0,fixed=TRUE);
  str0 <- gsub("!!gsf", gsf,str0,fixed=TRUE);

  if (!dir.exists(path)) dir.create(path);

  nRws <- nrow(stLLs);
  for (yr in stYrs){
    for (mon in names(stMDs)){
      for (day in stMDs[[mon]]){
        for (rw in 1:nRws){
          ll <- stLLs[rw,c("latdeg","latmin","londeg","lonmin")];
          fnR<-paste0(fnBase,
                      paste(yr,mon,day,ll$latdeg,ll$latmin,ll$londeg,ll$lonmin,randNum,sep="_")); #remote file
          fnL<-paste0(fnBase,
                      paste(yr,mon,day,ll$latdeg,ll$latmin,ll$londeg,ll$lonmin,sep="_")); #local file without path
          fnL<-file.path(path,fnL);                                                       #local file with path
          str1<-str0;
          str1<-gsub("!!lclFile",fnL,      str1,fixed=TRUE);
          str1<-gsub("!!remFile",fnR,      str1,fixed=TRUE);
          str1<-gsub("!!latdeg", ll$latdeg,str1,fixed=TRUE);
          str1<-gsub("!!latmin", ll$latmin,str1,fixed=TRUE);
          str1<-gsub("!!londeg", ll$londeg,str1,fixed=TRUE);
          str1<-gsub("!!lonmin", ll$lonmin,str1,fixed=TRUE);
          str1<-gsub("!!year",   yr,       str1,fixed=TRUE);
          str1<-gsub("!!mon",    mon,      str1,fixed=TRUE);
          str1<-gsub("!!day",    day,      str1,fixed=TRUE);
          str1<-gsub("!!nnn",    nDays,    str1,fixed=TRUE);
          if (verbose||test) cat(str1,"\n\n");
          if (!test) system(str1);
        }
      }
    }
  }
  return(NULL);
}
