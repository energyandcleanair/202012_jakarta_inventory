utils.date_to_time_dimension <- function(d){
  # $units
  # [1] "hours since 2019-01-01 00:00 +0700"
  as.integer(difftime(d, as.POSIXct("2019-01-01", tz="Asia/Jakarta"), units="hour"))
}

utils.time_dimension <- function(date_from="2018-01-01", date_to="2018-12-31 23:00"){
  dh <- utils.date_to_time_dimension(seq(as.POSIXct(date_from, tz="Asia/Jakarta"),
      as.POSIXct(date_to, tz="Asia/Jakarta"),
      by="hour"
  ))

  ncdim_def('time',
            units="hours since 2019-01-01 00:00 +0700",
            calendar="standard",
            vals=dh)
}

utils.lat_dimension <- function(r){

  xy = xyFromCell(r,1:length(r))
  lats <- xy[,1] %>% unique()
  ncdim_def('y',
            units="meters",
            calendar="standard",
            vals=lats)
}

utils.lon_dimension <- function(r){

  xy = xyFromCell(r,1:length(r))
  lons <- xy[,2] %>% unique()
  ncdim_def('x',
            units="meters",
            calendar="standard",
            vals=lons)
}

utils.raster_grid_manual <- function(res_km=1, xi=-719489, xf=-654489, yi=-200500, yf=-135500, crs=crs('+init=EPSG:32748')){

  # UTM48S = EPSG:32748

  r <- raster(ncol=ceiling(yf-yi)/(res_km*1000), nrow=ceiling(xf-xi)/(res_km*1000), xmn=xi, xmx=xf, ymn=yi, ymx=yf)
  crs(r) <- crs
  return(r)
}

utils.raster_grid_existing <- function(f="data/d04.grid.tif"){
  r <- raster(f)
  r2 <- raster(r)
  return(r2)
}


#' Convert points (sf) with date to a raster brick, stacked by date
#'
#' @param pts A dataframe with latitude, longitude, date column and value column.
#' @param r The raster to project onto
#' @param value_col The name of the value column to consider
#' @param value_fun The function to apply to the value column
#' @param poll Pollutant to consider
#'
#' @return
#' @export
#'
#' @examples
utils.emission_to_raster_brick <- function(emission_pts, r, time_dim, poll, value_col="emission_g_s", value_fun="sum"){

  pts.sf <- sf::st_as_sf(emission_pts %>% filter(poll==!!poll),
                         coords=c("longitude","latitude"),
                         crs=4326) %>%
    st_transform(crs=crs(r))


  # Create a layer per date (i.e. hour)
  pts.sf$time <- utils.date_to_time_dimension(pts.sf$date)

  rs <- pblapply(time_dim$vals,
         function(t, pts.sf){
           raster::rasterize(pts.sf %>% filter(time==t),
                             r,
                             field=value_col,
                             fun=value_fun,
                             background=0,
                             mask=FALSE,
                             update=FALSE,
                             updateValue='all',
                             na.rm=TRUE)

         },
         pts.sf=pts.sf)

  rb <- raster::brick(rs)
}


utils.ncvar_from_rb <- function(rb, lon_dim, lat_dim, time_dim, name, unit){

  ncvar_def(name=name,
            units=unit,
            dim=list(lon_dim, lat_dim, time_dim))
}


utils.nc_from_ncvars <- function(file, ncvars, rbs){

  nc <- nc_create(file, ncvars)
  for (i in 1:length(rbs)){
    ncvar_put(nc, ncvars[[i]], as.array(rbs[[i]]))
  }

  nc_close(nc)
  return(nc)
}
