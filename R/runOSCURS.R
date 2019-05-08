#'
#' @title Make a set of OSCURS runs
#'
#' @description Function to make a series of OSCURS runs
#'
#' @param fnBase - base for output filenames (default = "./OSCURS_")
#' @param nDays - number of days to run model
#' @param stYrs - vector of years for particle releases
#' @param stMDs - list of release dates, by month
#' @param stLLs - a tibble with initial particle lat/lon locations
#' @param link - url to run OSCURS (default = "https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?")
#' @param test - flag to print diagnostic output but NOT run OSCURS (useful to see what commands will be sent)
#' @param verbose - flag to print diagnostic output
#'
#' @return NULL
#'
#' @details Runs the OSCURS model at the link given. An output file is created for each particle release of the
#' form fnBase_year_month_day_latdegree_latminute_londegree_lonminute.csv, where lat and lon is the initial
#' particle location.
#'
#' @export
#'
runOSCURS<-function(fnBase="./OSCURS_",
                    nDays=90,                            #number of days to track
                    stYrs=2017,                          #years for releases
                    stMDs=list(APR=15,MAY=c(1,15),JUN=1),#months and days for releases
                    stLLs=NULL,
                    link="https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?",
                    test=TRUE,
                    verbose=FALSE){
  # stLLs<-read.csv(stLLs,
  #                 stringsAsFactors = FALSE,
  #                 check.names = FALSE);
  if (is.null(stLLs)) stop("Must provide parameter 'stLLs', a tibble with initial particle locations.\n")
  stLLs$LONGITUDE <- -stLLs$LONGITUDE;#change sign on longitude
  stLLs$latdeg <- floor(stLLs$LATITUDE);
  stLLs$latmin <- round(60*(stLLs$LATITUDE %% 1));
  stLLs$londeg <- floor(stLLs$LONGITUDE);
  stLLs$lonmin <- round(60*(stLLs$LONGITUDE %% 1));

  str0<-paste0('wget --wait=60 -O !!lclFile.txt "!!link',
               'cl=1&latdeg=!!latdeg&latmin=!!latmin&londeg=!!londeg&lonmin=!!lonmin&',
               'year=!!year&mon=!!mon&day=!!day&nnnn=!!nnn&factor=1&angle=0&ddfac=1&outfile=!!remFile.csv"');

  str0 <- gsub("!!link",link,str0,fixed=TRUE);

  nRws <- nrow(stLLs);
  #nRws<-2;#for testing
  for (yr in stYrs){
    for (mon in names(stMDs)){
      for (day in stMDs[[mon]]){
        for (rw in 1:nRws){
          ll <- stLLs[rw,c("latdeg","latmin","londeg","lonmin")];
          fn<-paste0(fnBase,
                     paste(yr,mon,day,ll$latdeg,ll$latmin,ll$londeg,ll$lonmin,sep="_"));
          str1<-str0;
          str1<-gsub("!!lclFile",fn,       str1,fixed=TRUE);
          str1<-gsub("!!remFile",fn,       str1,fixed=TRUE);
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
