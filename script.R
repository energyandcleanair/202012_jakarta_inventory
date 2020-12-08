# Read EDGAR to mimic format for METEOSIM
require(ncdf4)
require(raster)
require(tidyverse)
require(pbapply)

source('utils.R')
source('emission_power.R')


date_from <- "2018-01-01"
date_to <- "2018-01-31 23:00"
file <- file.path("results","input","power","2018","01","power.nc")


generate_power_nc <- function(date_from, date_to, file){

  dir.create(dirname(file), showWarnings = F, recursive = T)

  r <- utils.raster_grid_existing()
  crs <- crs(r)

  time_dim <- utils.time_dimension(date_from=date_from, date_to=date_to)
  lon_dim <- utils.lat_dimension(r)
  lat_dim <- utils.lon_dimension(r)

  emission_power <- build_emission_power(regulation = 2019, type="new")

  rb.nox <- utils.emission_to_raster_brick(emission_power, r, time_dim, poll="NOx", value_col="emission_g_s")
  rb.pm <- utils.emission_to_raster_brick(emission_power, r, time_dim, poll="PM", value_col="emission_g_s")
  rb.so2 <- utils.emission_to_raster_brick(emission_power, r, time_dim, poll="SO2", value_col="emission_g_s")

  polls <- c("NOx","PM","SO2")
  rbs <- lapply(polls,
                function(p){utils.emission_to_raster_brick(emission_power, r, time_dim, poll=p, value_col="emission_g_s")})

  ncvars <- mapply(function(rb, name){utils.ncvar_from_rb(rb, lon_dim, lat_dim, time_dim, name, unit="g.s-1")},
                   rb=rbs,
                   name=polls,
                   SIMPLIFY = F)

  nc <- utils.nc_from_ncvars(file, ncvars, rbs)

  return(nc)
}


generate_power_nc(date_from, date_to, file)
