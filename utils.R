utils.location_name_to_bps_id <- function(l){
  r <- read.csv("data/region_lookup.csv")


  tibble(id=seq_along(l), name=gsub("[[:space:]]*Regency","",l)) %>%
    left_join(r) %>% left_join(
    tibble(id=seq_along(l), name_local=gsub("[[:space:]]*Regency","",l)) %>%
      left_join(r),
    by="id") %>%
    mutate(bps_id=ifelse(is.na(ADM2_PCODE.y), ADM2_PCODE.x, ADM2_PCODE.y)) %>%
    dplyr::select(id, name=name.x, bps_id) %>%
    arrange(id) %>%
    pull(bps_id)
}


utils.equal_date_weights <- function(frequency="h"){
  d <- seq(as.POSIXct("2019-01-01", tz="Asia/Jakarta"),
           as.POSIXct("2019-12-31", tz="Asia/Jakarta"),
           by=frequency)
  tibble(date=d,
         weight=1/length(d))
}


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

utils.proj4string_from_nc <- function(f){
  library(ncmeta)
  ncdf4::nc_open(f) %>%
    ncdf4::ncatt_get("proj") %>%
    nc_gm_to_prj()
}



#' Take a rasterstack with pollutant emissions as variables
#' and transform it in a netcdf compatible with METEOSIM
#'
#' @param rs
#' @param d03_or_d04
#'
#' @return
#' @export
#'
#' @examples
utils.rasters_to_nc <- function(rs,
                                 grid_name,
                                 nc_file){

  library(ncdf4)

  fs <- list(
    "d03"="data/d03.nc",
    "d04"="data/d04.nc"
  )

  if(!grid_name %in% names(fs)){
    stop("Unknown grid: ", grid_name, ". Should be in ", paste(names(fs), collapse=", "))
  }else{
    f <- fs[[grid_name]]
  }

  nc <- ncdf4::nc_open(f)

  name_x <- intersect(ncmeta::nc_vars(f)$name, c("x","X"))
  name_y <- intersect(ncmeta::nc_vars(f)$name, c("y","Y"))

  val_x <- ncvar_get(nc, name_x)
  val_y <- ncvar_get(nc, name_y)

  dim_x <- ncdim_def(name_x, "", vals=val_x)
  dim_y <- ncdim_def(name_y, "", vals=val_y)

  #--------------------------------------
  # Create vars along existing dimensions
  #--------------------------------------
  vars <- lapply(names(rs),
                 function(varname){
                   v <- ncvar_def(varname, "", dim = list(dim_x, dim_y))
                 })
  #---------------------
  # Create the test file
  #---------------------
  file.remove(nc_file, showWarnings=F)
  nc.new <- nc_create(nc_file, vars)

  #----------------------------
  # Write some data to the file
  #----------------------------
  lapply(names(rs),
         function(varname){
           ncvar_put( nc.new, varname, rs[[varname]] %>% as.matrix() %>% apply(2, rev) %>% t() %>% as.vector()) # no start or count: write all values
         })

  nc_close(nc.new)
}



#' Take a dated tibble of rasterstack with pollutant emissions as variables
#' and transform it in a netcdf compatible with METEOSIM
#'
#' @param rs
#' @param d03_or_d04
#'
#' @return
#' @export
#'
#' @examples
utils.ts_rasters_to_nc <- function(rs,
                                grid_name,
                                nc_file){

  library(ncdf4)

  fs <- list(
    "d02"="data/d02.nc",
    "d03"="data/d03.nc",
    "d04"="data/d04.nc"
  )

  if(!grid_name %in% names(fs)){
    stop("Unknown grid: ", grid_name, ". Should be in ", paste(names(fs), collapse=", "))
  }else{
    f <- fs[[grid_name]]
  }

  nc <- ncdf4::nc_open(f)

  name_x <- intersect(nc_vars(f)$name, c("x","X","easting"))
  name_y <- intersect(nc_vars(f)$name, c("y","Y","northing"))
  # name_date <- "date"
  name_date <- "datetime"

  name_x_new <- 'X'
  name_y_new <- 'Y'

  val_x <- ncvar_get(nc, name_x)
  val_y <- ncvar_get(nc, name_y)

  # dates <- as.numeric(as.Date(names(rs)) - lubridate::date("2019-01-01"), unit="days")
  # specify timezone to prevent timezone issues
  dates <- as.numeric(as.POSIXct(names(rs), tz='Asia/Jakarta') -
                        lubridate::as_datetime("2019-01-01 00:00:00", tz='Asia/Jakarta'),
                      unit="hours")
  val_date <- unique(dates)


  dim_x <- ncdim_def(name_x_new, "", vals=val_x)
  dim_y <- ncdim_def(name_y_new, "", vals=val_y)
  # dim_date <- ncdim_def(name_date, "Days since 2019-01-01", vals=val_date)
  dim_date <- ncdim_def(name_date, "Hours since 2019-01-01 00:00:00", vals=val_date)

  #--------------------------------------
  # Create vars along existing dimensions
  #--------------------------------------
  polls <- names(rs[[1]])

  vars <- lapply(polls,
                 function(varname){
                   v <- ncvar_def(varname, "", dim = list(dim_x, dim_y, dim_date))
                 })


  #---------------------
  # Create the file
  #---------------------
  file.remove(nc_file)
  nc.new <- nc_create(nc_file, vars)

  #----------------------------
  # Write some data to the file
  #----------------------------
  for(poll in polls){
    for(idate in seq_along(rs)){
      date <- names(rs)[[idate]]
        ncvar_put(nc.new,
                  varid=poll,
                  vals=rs[[date]][[poll]] %>%
                      as.matrix() %>%
                      apply(2, rev) %>%
                      t() %>%
                      as.vector(),
                  count = c(-1,-1,1),
                  start=c(1,1,idate))
    }
  }

  nc_close(nc.new)
}


utils.temporal_split <- function (emission.raster, date_weight) {

  if (!class(emission.raster)[[1]] %in% c("RasterStack", "RasterLayer")) {
    stop("emission.raster should be a RasterStack or a RasterLayer")
  }

  # check.fields(date_weight, c("date", "weight"))

  if("id" %in% names(date_weight)){
    # Temporal split is different per region or kabupaten
    g <- data.bps_map() %>%
      sf::st_transform(crs(emission.raster))


    date_weight <- date_weight[!is.na(date_weight$id),]
    # date_weight <- tidyr::crossing(date=seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="hour"),
    #                 id=unique(date_weight$id)) %>%
    #   left_join(date_weight) %>%
    #   mutate(weight=tidyr::replace_na(weight, 0))

    # check if not hourly or not complete
    if(nrow(date_weight)<((365*24) * (date_weight %>% distinct(id) %>% nrow()))){
      date_weight <- tidyr::crossing(date=seq(as.POSIXct("2019-01-01 00:00:00"),
                                              as.POSIXct("2019-12-31 23:00:00"), by="hour"),
                                     id=unique(date_weight$id)) %>%
        mutate(date_only=lubridate::date(date)) %>%
        left_join(date_weight, by=c('date_only'='date', 'id')) %>%
        select(-date_only) %>%
        mutate(weight=tidyr::replace_na(weight, 0))
    }


    r <- sapply(g, function(x) all(unique(date_weight$id) %in% x))
    id_refers_to <- names(which(r))

    g_dates <- g %>%
      select_at(id_refers_to) %>%
      rename_at(id_refers_to, ~"id") %>%
      left_join(
        date_weight %>% group_by(id) %>% mutate(weight=weight/sum(weight))
      ) %>%
      arrange(date)


    # To test the sum of weights = 1
    # r_weights <- lapply(split(g_dates, g_dates$date), function(g_date){
    #   raster::rasterize(g_date, emission.raster[[1]], field="weight")
    # })
    # plot(raster::calc(stack(r_weights), sum, na.rm=T))
    #
    #
    # all(unique(raster::calc(stack(r_weights), sum, na.rm=T)[]) %in% c(0, 1))
    grid <- emission.raster[[1]]
    result <- pbapply::pblapply(split(g_dates, g_dates$date), function(g_date){
      r <- (raster::rasterize(g_date[!sf::st_is_empty(g_date),], grid, field="weight") *
        emission.raster) %>% `names<-`(names(emission.raster))
      r[is.na(r)] <- 0
      return(r)
    })

    # plot(stack(lapply(result, function(x) x$BC)))

  }else{
    # Temporal split is uniform
    # check if not hourly or not complete
    if(nrow(date_weight)<(365*24)){
      date_weight <- tibble(date=seq(as.POSIXct("2019-01-01 00:00:00"),
                                     as.POSIXct("2019-12-31 23:00:00"), by="hour"),
                            date_only=lubridate::date(date)) %>%
        left_join(date_weight, by=c('date_only'='date')) %>%
        select(-date_only) %>%
        mutate(weight=tidyr::replace_na(weight, 0))
    }

    w <- date_weight %>%
      mutate(weight=weight/sum(weight)) %>%
      arrange(date)

    result <- as.list(w$weight) %>% `names<-`(w$date) %>%
      lapply(function(date_weight) emission.raster * date_weight)
  }

  return(result)
}
