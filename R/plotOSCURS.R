#'
#' @title Plot particle tracks from OSCURS model runs on a map
#'
#' @description
#'
#' @param tracks - a tibble with OSCURS tracks (e.g., the \code{tracks} list element from a call to \code{convertOSCURStoTbl})
#' @param stLLs - a tibble with starting particle locations (e.g., from reading the initial particle locations csv file using \code{readr::read_csv})
#' @param strCRS - character representation of coordinate reference system for final map (default is Alaska Albers [EPSG=3338])
#' @param basemap - a base map for plotting the tracks (default is the EBS using CRS defined by strCRS)
#' @param  alpha - transparency for track lines
#' @param showMap - flag to show the map
#' @param verbose - flag to print diagnostic info
#'
#' @return - a tmap object
#'
#' @details The map object is in the Alaska Albers projection.
#' Requires packages \code{dply}, \code{magrittr}, \code{sf}, \code{tmap}, \code{wtsGIS}.
#'
#' @import magrittr
#'
#' @export
#'
plotOSCURS<-function(tracks,
                     stLLs,
                     strCRS=tmaptools::get_proj4(3338,output="character"),
                     basemap=wtsGIS::createBaseTMap(layer.land=wtsGIS::getPackagedLayer("Alaska"),
                                                    layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                                                    strCRS.finl=strCRS),
                     alpha=1,
                     showMap=TRUE,
                     verbose=FALSE
                     ){
  nr<-nrow(tracks);

  #create the required coordinate systems (WGS84 and Alaska Albers projection [EPSG=3338])
  strWGS84<-tmaptools::get_proj4("longlat",output="character");

  #create spatial table with initial particle locations
  tbl.uniqStartLocs <- unique(stLLs[,c("STATION_ID")]);
  tbl.uniqStartLocs <- tbl.uniqStartLocs %>%
                         dplyr::left_join(stLLs[,c("STATION_ID",
                                                       "DISTRICT",
                                                       "LATITUDE",
                                                       "LONGITUDE")]);
  nr<-nrow(tbl.uniqStartLocs);
  for (rw in 1:nr){
    pt <- sf::st_point(x=c(tbl.uniqStartLocs$LONGITUDE[rw],
                           tbl.uniqStartLocs$LATITUDE[rw]),
                       dim="XY");
    if (rw==1){
      geoms<-sf::st_sfc(pt);
    } else {
      geoms[rw]<-sf::st_sfc(pt);
    }
  }
  #add in point geometries in WGS84 as "startLocs"
  tbl.uniqStartLocGeoms <- dplyr::bind_cols(tbl.uniqStartLocs,
                                            sf::st_sf(startLocs=geoms,
                                                      crs=strWGS84));
  #transform to Alaska Albers
  tbl.uniqStartLocGeoms <- sf::st_sf(tbl.uniqStartLocGeoms) %>%
                             sf::st_transform(strCRS);

  #plot map with initial particle locations
  bb<-sf::st_bbox(tbl.uniqStartLocGeoms);
  basemap$tm_shape$bbox <- bb;
  basemap+tmap::tm_shape(tbl.uniqStartLocGeoms)+tmap::tm_squares(col="black",size=0.02)+
          tmap::tm_legend(legend.position=c("right","top"));


  #wrap the dateline on the particle tracks
  tracks.DL<-tracks %>%
               sf::st_wrap_dateline(options=c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"),
                                    quiet=TRUE) %>%
               sf::st_transform(crs=tmaptools::get_proj4(strCRS,output="crs"));
  bb<-sf::st_bbox(tracks.DL);
  basemap$tm_shape$bbox <- bb;

  #create the map
  map<-basemap+tmap::tm_shape(tbl.uniqStartLocGeoms)+tmap::tm_squares(col="black",size=0.02)+
               tmap::tm_shape(tracks.DL)+tmap::tm_lines(col="dayStart",title.col="start day",alpha=alpha)+
               tmap::tm_legend(legend.position=c("right","top"));

  if (showMap) print(map);

  return(map);
}
