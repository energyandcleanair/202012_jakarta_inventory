# remotes::install_github("energyandcleanair/creainventory", upgrade=F)
# devtools::reload(pkgload::inst("creainventory"))
library(creainventory)
library(raster)
library(sf)
library(tidyverse)
library(creatrajs)
library(eixport)
library(osmdata)

lapply(list.files(".", "data.*.R"),source)
source('utils.R')
source('edgar.R')

sectors <- list(
  comres=list(emission=data.comres_emission, support=data.comres_support),
  power=list(emission=data.power_emission, support=data.power_support),
  transport=list(emission=data.transport_emission, support=data.transport_support),
  comres=list(emission=data.comres_emission, support=data.comres_support),
  gasdist=list(emission=data.gasdist_emission, support=data.gasdist_support),
  agroob=list(emission=data.agroob_emission, support=data.agroob_support),
  landfill=list(emission=data.landfill_emission, support=data.landfill_support),
  aviation=list(emission=data.aviation_emission, support=data.aviation_support),
  shipping=list(emission=data.shipping_emission, support=data.shipping_support),
  forest=list(emission=data.forest_emission, support=data.forest_support),
  solidwaste=list(emission=data.solidwaste_emission, support=data.solidwaste_support)
)

grid <- data.grid.edgar()
grid_name <- "edgar"
polls <- "NMVOC"

lapply(names(sectors), function(sector){

  message("======= ",sector," =======")
  emission.data <- sectors[[sector]]$emission() %>% filter(poll %in% polls) %>% filter(emission>0)
  emission_total <- emission.data %>% group_by(poll) %>% summarise_at("emission", sum, na.rm=T)
  support <- sectors[[sector]]$support()

  # Check
  creainventory::check.emission.d(emission.data)
  creainventory::check.support.sp(support)

  # Combine data with support
  emission <- creainventory::combine(emission.data, support) %>% filter(!is.na(emission))

  # Check all ids are there
  ids_with_emissions <- unique(emission.data$id[emission.data$emission>0])
  missing_ids <- setdiff(ids_with_emissions, unique(support$id))
  if(length(missing_ids)>0){
    warning("Missing ",length(missing_ids), " support locations: ", paste(missing_ids, collapse=", "))
    emission <- emission %>% filter(!sf::st_is_empty(geometry))
  }


  # Create a single layer representing whole year
  emission.raster <- creainventory::grid.rasterize(emission, grid)

  # Save
  dir.create("results", showWarnings = F)
  lapply(names(emission.raster),function(poll){
    raster::writeRaster(emission.raster[[poll]],
                        file.path("results",sprintf("%s.%s.%s.tiff",s,poll,grid_name)),
                        overwrite=T)

    # Sanity check
    emission_total_poll <- emission_total[emission_total$poll==poll,"emission"]
    raster_total_poll <- raster::cellStats(emission.raster[[poll]], "sum")
    if(emission_total_poll!=raster_total_poll){
      warning("Emissions not conserved: ",emission_total_poll, " != ", raster_total_poll)
    }
  })
})
