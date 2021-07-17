#'
#' @title Plot particle tracks from OSCURS model runs on a map for the PAPA Trajectory Index
#'
#' @description Function to plot particle tracks from OSCURS model runs on a map for the PAPA Trajectory Index.
#'
#' @param tracks - a tibble with OSCURS tracks (e.g., the \code{tracks} list element from a call to \code{convertOSCURStoTbl})
#' @param trackID_col - column with unique track id
#' @param emphasizeID - if not NULL, id of track to emphasize (default=\code{as.character(max(as.numeric(tracks[[trackID_col]])))})
#' @param addText - flag (T/F) to add text in \code{text_col} to end point of each track
#' @param text_col - column for text labels
#' @param text_size - size for "year" label on last end point (others will be 0.8*text_size)
#' @param strCRS - character representation of coordinate reference system for final map (default is Alaska Albers [EPSG=3338])
#' @param basemap - a base map for plotting the tracks (default is the EBS using CRS defined by strCRS)
#' @param bounding_box - bounding box for map (or NULL to calculate from track limits)
#' @param gridLines - list w/ info to plot grid lines on map (or NULL for no grid lines)
#' @param style - \code{tmap} style for map (default is NULL, which uses current tmap style)
#' @param color_lines - color for track lines (or column name)
#' @param color_text - color for track line text (or column name)
#' @param alpha_lines - transparency for track lines
#' @param alpha_text - transparency for track line text
#' @param palette - color palette for text
#' @param showMap - flag to show the map
#' @param verbose - flag to print diagnostic info
#'
#' @return - a list (see details)
#'
#' @details The returned list has the following elements:
#'  * map       - a tmap object
#'  * tracks    - a sf tibble with tracks that span the IDL to "roll your own" map
#'  * startLocs - a sf tibble with starting track locations as points to "roll your own" map
#'  * endLocs - a sf tibble with ending track locations as points to "roll your own" map
#'  * basemap   - a basemap to "roll your own" map
#'
#' The map object is in the coordinate reference system given by strCRS.
#'
#' Requires packages \code{dplyr}, \code{magrittr}, \code{sf}, \code{tmap}, \code{wtsGIS}.
#'
#' @import magrittr
#'
#' @export
#'
plotPTI<-function(tracks,
                  trackID_col="year",
                  emphasizeID=as.character(max(as.numeric(tracks[[trackID_col]]))),
                  addText=TRUE,
                  text_col="year",
                  text_size=1,
                  strCRS=tmaptools::get_proj4(3338,output="character"),
                  basemap=wtsGIS::createBaseTMap(layer.land=wtsGIS::getPackagedLayer("Alaska"),
                                                  layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                                                  strCRS.finl=strCRS),
                  bounding_box=tmaptools::bb(xlim=c(-165,-135),
                                             ylim=c(48,59),
                                             current.projection=tmaptools::get_proj4("longlat",output="character"),
                                             projection=strCRS),
                  gridLines=list(x=seq(from=-165,to=-135,by=5),
                                 y=seq(from=48,  to=59,  by=2),
                                 projection=tmaptools::get_proj4("longlat",output="character")),
                  style=NULL,
                  color_lines="blue",
                  color_text="blue",
                  alpha_lines=1.0,
                  alpha_text=1.0,
                  palette=NULL,
                  showMap=TRUE,
                  verbose=FALSE
                   ){
  opts<-tmap::tmap_options();
  on.exit(tmap::tmap_options(opts));#--reset options
  if (!is.null(style)) tmap::tmap_style(style);

  nr<-nrow(tracks);
  if (verbose) message(paste0("plotPTI: number of tracks to plot is: ",nr));
  tmap::tmap_options(max.categories=nr);
  if (verbose) message(paste0("plotPTI: number of categories is: ",tmap::tmap_options()$max.categories));
  if (verbose){
    if (!is.null(emphasizeID)) {
      message(paste0("plotPTI: will emaphasize track with ID = ",emphasizeID));
    } else {
      message(paste0("plotPTI: no track emphasized"));
    }
  }

  #create the required coordinate system for lat/lon coordinates (WGS84)
  strWGS84<-tmaptools::get_proj4("longlat",output="character");

  # #add "year" as column.
  # #NOTE: PTI tracks run Dec 1 to Feb 29/Mar 1. "year" corresponds to end point
  # tracks$year<-as.character(as.numeric(substr(tracks$dayStart,1,4))+1);

  #create spatial table with initial particle location
  tbl.uniqStartLocs <- unique(sf::st_drop_geometry(tracks[,c("latStart","lonStart")]));
  nr<-nrow(tbl.uniqStartLocs);
  for (rw in 1:nr){
    pt <- sf::st_point(x=c(tbl.uniqStartLocs$lonStart[rw],
                           tbl.uniqStartLocs$latStart[rw]),
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

  #create spatial table with final particle locations
  if (trackID_col==text_col)
    tbl.uniqEndLocs <- unique(sf::st_drop_geometry(tracks[,c(trackID_col,"latEnd","lonEnd")]));
  if (trackID_col!=text_col)
    tbl.uniqEndLocs <- unique(sf::st_drop_geometry(tracks[,c(trackID_col,text_col,"latEnd","lonEnd")]));
  nr<-nrow(tbl.uniqEndLocs);
  for (rw in 1:nr){
    pt <- sf::st_point(x=c(tbl.uniqEndLocs$lonEnd[rw],
                           tbl.uniqEndLocs$latEnd[rw]),
                       dim="XY");
    if (rw==1){
      geoms<-sf::st_sfc(pt);
    } else {
      geoms[rw]<-sf::st_sfc(pt);
    }
  }
  #add in point geometries in WGS84 as "startLocs"
  tbl.uniqEndLocGeoms <- dplyr::bind_cols(tbl.uniqEndLocs,
                                          sf::st_sf(endLocs=geoms,
                                                    crs=strWGS84));
  #transform to Alaska Albers
  tbl.uniqEndLocGeoms <- sf::st_sf(tbl.uniqEndLocGeoms) %>%
                             sf::st_transform(strCRS);

  # #plot map with initial particle locations
  # bb<-sf::st_bbox(tbl.uniqStartLocGeoms);
  # basemap$tm_shape$bbox <- bb;
  # basemap+tmap::tm_shape(tbl.uniqStartLocGeoms)+tmap::tm_squares(col="black",size=0.02)+
  #         tmap::tm_legend(legend.position=c("right","top"));
  #
  #
  #wrap the dateline on the particle tracks
  tracks.DL<-tracks %>%
               sf::st_wrap_dateline(options=c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"),
                                    quiet=TRUE) %>%
               sf::st_transform(crs=tmaptools::get_proj4(strCRS,output="crs"));
  bbx<-sf::st_bbox(tracks.DL);#bounding box based on tracks

  #set bounding box
  if (is.null(bounding_box)){
    basemap$tm_shape$bbox <- bbx;
  } else {
    basemap$tm_shape$bbox <- bounding_box;
  }

  palette_text<-NULL;
  if (!is.null(palette))
    palette_text<-wtsUtilities::addTransparency(palette,alpha=alpha_text);

  #create the map
  map<-basemap;
  #--start locations
  map<-map+tmap::tm_shape(tbl.uniqStartLocGeoms)+
                  tmap::tm_squares(col="black",size=0.02);
  if (!is.null(emphasizeID)){
    #--past trajectories
    map<-map+tmap::tm_shape(tracks.DL[tracks.DL[[trackID_col]]!=emphasizeID,])+
                    tmap::tm_lines(col=color_lines,lwd=3.0,alpha=alpha_lines,
                                   palette=palette,stretch.palette=TRUE);
    map<-map+tmap::tm_shape(tbl.uniqEndLocGeoms[tbl.uniqEndLocGeoms[[trackID_col]]!=emphasizeID,])+
                    tmap::tm_squares(col=color_lines,size=0.02,alpha=1.0,
                                     palette=palette,stretch.palette=TRUE);
    if (addText){
      map<-map+tmap::tm_shape(tbl.uniqEndLocGeoms[tbl.uniqEndLocGeoms[[trackID_col]]!=emphasizeID,])+
                      tmap::tm_text(text_col,size=0.8*text_size,col=color_text,alpha=alpha_text,
                                    palette=palette_text,stretch.palette=TRUE,
                                    auto.placement=TRUE);
    }
    #--current trajectory
    map<-map+tmap::tm_shape(tracks.DL[tracks.DL[[trackID_col]]==emphasizeID,])+
                    tmap::tm_lines(col="black",lwd=4.0,alpha=1.0);
    map<-map+tmap::tm_shape(tbl.uniqEndLocGeoms[tbl.uniqEndLocGeoms[[trackID_col]]==emphasizeID,])+
                    tmap::tm_squares(col="black",size=0.04,alpha=1.0);
    if (addText){
      map<-map+tmap::tm_shape(tbl.uniqEndLocGeoms[tbl.uniqEndLocGeoms[[trackID_col]]==emphasizeID,])+
                      tmap::tm_text(text_col,size=text_size,col="black",alpha=1.0,
                                  auto.placement=TRUE);
    }
  } else {
    #--all trajectories
    map<-map+tmap::tm_shape(tracks.DL)+
                    tmap::tm_lines(col=color_lines,lwd=3.0,alpha=alpha_lines,
                                   palette=palette,stretch.palette=TRUE);
    map<-map+tmap::tm_shape(tbl.uniqEndLocGeoms)+
                    tmap::tm_squares(col=color_lines,size=0.02,alpha=1.0,
                                     palette=palette,stretch.palette=TRUE)
    if (addText){
      map<-map+tmap::tm_shape(tbl.uniqEndLocGeoms)+
                      tmap::tm_text(text_col,size=0.8*text_size,col=color_text,alpha=alpha_text,
                                    palette=palette_text,stretch.palette=TRUE,
                                    auto.placement=TRUE);
    }
  }
  #--legend
  map<-map+tmap::tm_legend(legend.show=FALSE);

  if (!is.null(gridLines))
    map <- map + tmap::tm_grid(x=gridLines$x,
                               y=gridLines$y,
                               alpha=0.5,
                               projection=gridLines$projection);


  if (showMap) print(map);

  return(list(map=map,
              tracks=tracks.DL,
              startLocs=tbl.uniqStartLocGeoms,
              endLocs=tbl.uniqEndLocGeoms,
              bbox=basemap$tm_shape$bbox,
              basemap=basemap));
}
