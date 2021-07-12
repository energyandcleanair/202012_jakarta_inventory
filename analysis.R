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

lapply(list.files("sectors", "*.R", recursive=T, full.names=T), source)

polls <- c("SO2", "NOx", "CO", "NMVOC",
           "NH3", "PM", "CH4", "BC", "OC")

sectors <- c("agroob",
             "aviation",
             "comres",
             "forest",
             "gasdist",
             "landfill",
             "power",
             "shipping",
             "solidwaste",
             "transport")

# Adjust grid
grid <- data.grid.d04()
grid_name <- "d04"


lapply(sectors, function(sector){

  message("======= ",sector," =======")

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
    emission_total_poll <- emission_total[emission_total$poll==poll,"emission"]
    raster_total_poll <- raster::cellStats(emission.raster[[poll]], "sum")
    if(emission_total_poll!=raster_total_poll){
      warning("Emissions not conserved: ",emission_total_poll, " != ", raster_total_poll)
    }
  })

  return(emission.raster)
}) -> emission.rasters


# Create scenarios --------------------------------------------------------



