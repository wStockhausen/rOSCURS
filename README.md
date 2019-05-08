# rOSCURS
An R package to run OSCURS models and process the results.

## Installation requirements

The following packages are required use rOSCURS:

  * dplyr (>= 0.8.0.1),
  * ggplot2 (>= 1.0.0),
  * magrittr (>= 1.5),
  * readr (>= 1.3.1),
  * reshape2 (>= 1.4.1),
  * sf (>= 0.7-3),
  * stringr (>= 1.4.0),
  * tibble (>= 2.0.1),
  * tmap (>= 2.2),
  * tmaptools (>= 2.0-1),
  * wtsGIS (>= 2019.02.07),
  * wtsUtilities (>= 2016.09.02)
  
The latter two packages are available on GitHub at https://github.com/wStockhausen/wtsGIS and https://github.com/wStockhausen/wtsUtilities. The remaining are avaialble from CRAN. Each of the above packages may have further dependencies not noted here.

## Runnning OSCURS

The following R script illustrates the basic usage of the functions in the package:

```{r}
library(rOSCURS);

#--read in a csv file with starting particle locations
stLLs<-readr::read_csv("OSCURS_StartLocations.csv"); #only really need LATITUDE (-90:90) and LONGITUDE (-180:180) columns

#--run the OSCURS model
#--the following runs the model for nDays per particle location for each year x month/day release
#--[NOTE: try not to overwhelm the remote server with many tracks at once]
res1<-runOSCURS(fnBase="Test_", #base file name for output tracks
                path="./test",  #path to output track files
                nDays=90,              #number of days to track
                stYrs=2017,            #years for releases
                stMDs=list(APR=15),    #months and days for releases
                stLLs=stLLs,           #spatial "tibble" for start locations
                test=FALSE,            #use to run the function without running OSCURS
                verbose=TRUE);         #print diagnostic info

#--convert OSCURS output to list with data.frame and sf tibble with a WGS84 lat/lon crs.
#--[NOTE: stYrs and stMDs here might be different from above if you broke a large batch of
#--runs into several smaller batchs so as not to overwhelm the server.]
lst<-convertOSCURStoTbl(fnBase="./test/Test_",                #base name here can include a path
                        stYrs=2017,                           #years for releases
                        stMDs=list(APR=15,MAY=c(1,15),JUN=1), #months and days for releases
                        stLLs=stLLs,
                        verbose=FALSE);

#--plot map with tracks
map<-plotOSCURS(tracks=lst$tracks,  #spatial "tibble" for tracks created by last function
                stLLs=stLLs,        #spatial "tibble" for start locations
                alpha=0.5,          #use 50% transparency for track lines
                showMap=TRUE);      #print the map immediately
```
