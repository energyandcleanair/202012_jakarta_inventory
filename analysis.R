remotes::install_github("energyandcleanair/creainventory", upgrade=T)
# devtools::reload(pkgload::inst("creainventory"))
library(remotes)
install_github("statnmap/cartomisc")
library(cartomisc)
library(creainventory)
library(raster)
library(sf)
library(tidyverse)
library(creatrajs)
library(eixport)
library(ncdf4)
library(ncmeta)

source('utils.R')
source('edgar.R')
source('data.R')

select <- dplyr::select
sf::sf_use_s2(F)

lapply(list.files("sectors", "*.R$", recursive=T, full.names=T), source)

polls <- c("SO2", "NOx", "CO", "NMVOC",
           "NH3", "PM", "CH4", "BC", "OC")

sectors <- c(
"agroob",
"aviation",
"comres",
"forest",
"gasdist",
"industry",
"landfill",
"livestock",
 "power",
 "shipping",
    "solidwaste",
  "transport"
)


grids <- list(
 # "edgar"=data.grid.edgar()
  "d02"=data.grid.d02(),
  "d03"=data.grid.d03(),
  "d04"=data.grid.d04()
)


# To display warnings as they occur
options(warn=1)

prepare_sector <- function(sector, polls, grid, grid_name){

  message("======= ",sector," =======")

  tryCatch({
    # Get emission data and support
    emission.data <- get(paste0(sector,".get_emission"))() %>%
      filter(poll %in% polls,
             emission>0)

    emission_total <- emission.data %>% group_by(poll) %>% summarise_at("emission", sum, na.rm=T)

    support <- tryCatch({
      get(paste0(sector,".get_support"))()
    }, error=function(e){
      message("Building support")
      return(get(paste0(sector,".build_support"))())
    })

    # date_weight <- tryCatch({
    #    get(paste0(sector,".get_date_weight"))()
    # }, error=function(e){
    #   message("Couldn't find get_date_weight function. Using steady emission rate.")
    #   return(tibble(date=seq.Date(as.Date("2019-01-01"), as.Date("2019-12-31"), by="day"), weight=1))
    # })

    date_weight <- tryCatch({
      get(paste0(sector,".get_datetime_weight"))()
    }, error=function(e){
      message("Couldn't find get_datetime_weight function. Using steady emission rate.")
      return(tibble(date=seq(as.POSIXct("2019-01-01 00:00:00"),
                             as.POSIXct("2019-12-31 23:00:00"), by="hour"), weight=1))
    })

    # Check emission data and support
    creainventory::check.emission.d(emission.data)
    creainventory::check.support.sp(support)

    # Combine data with support
    emission <- creainventory::combine(emission.data, support) %>% filter(!is.na(emission))

    # Check all region ids with emissions are covered by support data
    ids_with_emissions <- unique(emission.data$id[emission.data$emission>0])
    missing_ids <- setdiff(ids_with_emissions, unique(support$id))
    if(length(missing_ids)>0){
      warning("Missing ",length(missing_ids), " support locations: ", paste(missing_ids, collapse=", "))
      # emission <- emission %>% filter(!sf::st_is_empty(geometry))
      g <- data.bps_map()
      ggplot(g) + geom_sf(aes(fill=id %in% missing_ids))
    }

    # Create a raster stack representing whole year for all polls
    geom_unique_id <- if("osm_id" %in% names(emission)) "osm_id" else NULL

    # REwrite or read them
    existing_files <- list.files("results",sprintf("%s.(%s).%s.tif", sector, paste(polls, collapse="|"), grid_name))
    if(length(existing_files) < length(unique(emission$poll))){
      emission.raster <- creainventory::rasterize(emission, grid, geom_unique_id=geom_unique_id)
      sf::st_set_crs(emission, "EPSG4326")


      # Save yearly GEOTIFFs
      dir.create("results", showWarnings = F)
      lapply(names(emission.raster), function(poll){
        raster::writeRaster(emission.raster[[poll]],
                            file.path("results", sprintf("%s.%s.%s.tif", sector, poll, grid_name)),
                            overwrite=T)

        # Sanity check: emission conservation
        emission_total_poll <- emission_total[emission_total$poll==poll, "emission"]
        raster_total_poll <- raster::cellStats(emission.raster[[poll]], "sum")
        if(emission_total_poll!=raster_total_poll){
          warning(sprintf("Emissions not conserved for poll %s: (from data) %.0f != %.0f (from raster)",
                          poll, emission_total_poll, raster_total_poll))
        }
      })
    }

    nc_file <- file.path("results", sprintf("%s.%s.nc", sector, grid_name))
    if(!file.exists(nc_file)){
      # Create a list (days) of raster stacks
      print("===Creating nc file===")

      # Read geotiffs
      emission.raster <- lapply(unique(emission$poll), function(poll){
        raster::raster(file.path("results", sprintf("%s.%s.%s.tif", sector, poll, grid_name)))
      }) %>%
        raster::stack() %>%
        `names<-`(unique(emission$poll))

      # Add missing pollutants
      missing_polls <- setdiff(polls, names(emission.raster))
      new_layers <- lapply(missing_polls, function(missing_poll) emission.raster[[1]] %>% `values<-`(0) %>% `names<-`(missing_poll))
      emission.raster <- raster::stack(c(emission.raster, unlist(new_layers)))

      # Split by date
      emission.rasters <- utils.temporal_split(emission.raster, date_weight)

      # Save as NETCDF for METEOSIM
      utils.ts_rasters_to_nc(rs=emission.rasters,
                             grid_name = grid_name,
                             nc_file = nc_file)
    }

    return(emission.rasters)
  }, error=function(e){
    print(e)
    return(NA)
  })
}

lapply(names(grids), function(grid_name){
  lapply(sectors, function(sector){
    prepare_sector(sector, polls, grids[[grid_name]], grid_name)
  })
}) -> emission.rasters



# # Create scenarios --------------------------------------------------------
create_scenario <- function(sector_omitted, grid_name){
  d <- tibble(path=list.files("results", ".*\\.nc$", full.names = T)) %>%
    mutate(filename=basename(path)) %>%
    tidyr::separate(filename, c("sector", "grid"), extra = "drop", remove=F) %>%
    filter(grid==!!grid_name,
           sector!=sector_omitted)

  n_sectors <- 12
  if(nrow(d) != n_sectors -1){
    stop(sprintf("Missing sectors: %s", paste(setdiff(sectors, c(d$sector, sector_omitted)), collapse=", ")))
  }


  try_to_set_nan_to_zero <- function(f){
    system(sprintf("ncatted -O -a _FillValue,,a,d,NaN %s", f))
    system(sprintf("ncatted -O -a missing_value,,a,d,NaN %s", f))
    system(sprintf("ncatted -O -a _FillValue,,o,f,0.0 %s", f))
    system(sprintf("ncatted -O -a missing_value,,o,f,0.0 %s", f))
    system(sprintf("ncatted -O -a _FillValue,,d,f,0.0 %s", f))
    system(sprintf("ncatted -O -a missing_value,,d,f,0.0 %s", f))
  }

  filepath <- file.path("results", sprintf("scenario_wo_%s.%s.nc", sector_omitted, grid_name))
  if(file.exists(filepath)) file.remove(filepath)
  file.copy(d$path[1], filepath, overwrite=T)
  try_to_set_nan_to_zero(filepath)

  for(i in seq(2,nrow(d))){
    initial_bc <- ncdf4::nc_open(filepath) %>% ncdf4::ncvar_get("BC") %>% sum(na.rm=T)
    # print(sprintf("Adding %s", d$path[i]))
    tmp_file <- "tmp.nc"
    if(file.exists(tmp_file)) file.remove(tmp_file)
    file.copy(d$path[i], tmp_file, overwrite=T)
    try_to_set_nan_to_zero(tmp_file)
    tmp_bc <- ncdf4::nc_open(tmp_file) %>% ncdf4::ncvar_get("BC") %>% sum(na.rm=T)
    cmd <- sprintf("ncbo --op_typ=add --overwrite %s %s %s", tmp_file, filepath, filepath)
    system(cmd)
    # print(file.size(filepath))
    ncdf4::nc_open(filepath)
    new_bc <- ncdf4::nc_open(filepath) %>% ncdf4::ncvar_get("BC") %>% sum(na.rm=T)
    # print(sprintf("%s + %s = %s", initial_bc, tmp_bc, new_bc))
  }


  # Double check sum is equal to sum
  vars <- c("datetime", polls)

  # Sum 1: original NC files
  sum1 <- lapply(vars, function(v){
    sum(unlist(lapply(d$path, function(f){
      r <- ncdf4::nc_open(f)
      if(v %in% names(r$var)){
        ncdf4::ncvar_get(r, v) %>% sum(na.rm=T)
      }else{
        0
      }
    })))
  }) %>%
    `names<-`(vars)

  # Sum 2: original Tif files
  sum2 <- lapply(polls, function(p){
    sum(unlist(lapply(sprintf("results/%s.%s.%s.tif", d$sector, p, grid_name), function(f){
      if(file.exists(f)){
        raster(f) %>% cellStats(sum)
        }else{
          0
        }})))
      }) %>%
    `names<-`(polls)

  # Sum 3: created filepath
  sum3 <- lapply(vars, function(v){
    r <- ncdf4::nc_open(filepath)
    ncdf4::ncvar_get(r, v) %>% sum(na.rm=T)
  }) %>%
    `names<-`(vars)

  # Compare
  for(v in polls){
    print(sprintf("%s: %.0f==%.0f==%.0f", v, sum1[[v]], sum2[[v]], sum3[[v]]))
  }
}

for(grid_name in names(grids)){
  for(sector_omitted in sectors){
    print(sprintf("=== Sector ommited: %s | %s ===", sector_omitted, grid_name))
    create_scenario(sector_omitted, grid_name)
  }
}


