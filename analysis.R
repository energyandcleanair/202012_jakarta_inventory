# remotes::install_github("energyandcleanair/creainventory", upgrade=F)
# devtools::reload(pkgload::inst("creainventory"))
library(creainventory)
library(raster)
library(sf)
library(tidyverse)
library(creatrajs)
library(eixport)

source('utils.R')
source('edgar.R')
source('data.R')

lapply(list.files("sectors", "*.R$", recursive=T, full.names=T), source)


polls <- c("SO2", "NOx", "CO", "NMVOC",
           "NH3", "PM", "CH4", "BC", "OC")

sectors <- c(
             "agroob",
             "aviation",
             "comres",
             "gasdist",
             "landfill",
             "power",
             "shipping",
             "solidwaste",
             "transport",
             "forest")

# Adjust grid
grid <- data.grid.d03()
grid_name <- "d03"

# grid <- data.grid.d04()
# grid_name <- "d04"
#
# grid <- data.grid(res_m=1e4)
# grid_name <- "10km"

lapply(sectors, function(sector){

  message("======= ",sector," =======")

  tryCatch({
    # Get emission data and support
    emission.data <- get(paste0(sector,".get_emission"))() %>%
      filter(poll %in% polls,
             emission>0)

    emission_total <- emission.data %>% group_by(poll) %>% summarise_at("emission", sum, na.rm=T)

    tryCatch({
      support <- get(paste0(sector,".get_support"))()
    }, error=function(e){
      message("Building support")
      support <-  get(paste0(sector,".build_support"))()
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
      emission <- emission %>% filter(!sf::st_is_empty(geometry))
    }
    # Create a single raster layer representing whole year
    emission.raster <- creainventory::grid.rasterize(emission, grid)

    # Save
    dir.create("results", showWarnings = F)
    lapply(names(emission.raster), function(poll){
      raster::writeRaster(emission.raster[[poll]],
                          file.path("results", sprintf("%s.%s.%s.tiff", sector, poll, grid_name)),
                          overwrite=T)

      # Sanity check: emission conservation
      emission_total_poll <- emission_total[emission_total$poll==poll, "emission"]
      raster_total_poll <- raster::cellStats(emission.raster[[poll]], "sum")
      if(emission_total_poll!=raster_total_poll){
        warning(sprintf("Emissions not conserved for poll %s: (from data) %.0f != %.0f (from raster)",
                        poll, emission_total_poll, raster_total_poll))
      }
    })

    return(emission.raster)
  }, error=function(e){
    return(NA)
  })
}) -> emission.rasters


# Create scenarios --------------------------------------------------------
create_scenario <- function(sector_omitted, grid_name){
  d <- tibble(file=list.files("results", ".*")) %>%
    tidyr::separate(file, c("sector", "poll", "grid"), extra = "drop", remove=F) %>%
    filter(grid==!!grid_name,
           sector!=sector_omitted)

  # Sum by poll and stack
  rs <- lapply(split(d$file, d$poll),
               function(fs){do.call(raster::stack, as.list(file.path("results", fs))) %>%
                   raster::calc(sum, na.rm=T)}) %>%
    raster::stack()

  # Export into one netcdf (n polls -> n layers)
  raster::writeRaster(rs, file.path("results", sprintf("scenario_wo_%s.%s.nc", sector_omitted, grid_name)))
}


