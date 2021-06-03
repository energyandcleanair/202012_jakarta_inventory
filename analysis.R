# remotes::install_github("energyandcleanair/creainventory")
# devtools::reload(pkgload::inst("creainventory"))
library(creainventory)
library(raster)
library(sf)
library(tidyverse)

lapply(list.files(".", "data.*.R"),source)
source('utils.R')


sectors <- list(
  power=list(emission=data.power_emission, support=data.power_support),
  transport=list(emission=data.transport_emission, support=data.transport_support),
  comres=list(emission=data.comres_emission, support=data.comres_support)
)

grid <- data.grid.edgar()
grid_name <- "edgar"
polls <- "NOx"

lapply(names(sectors), function(s){

  message("======= ",s," =======")
  emission.data <- sectors[[s]]$emission() %>% filter(poll %in% polls) %>% filter(emission>0)
  emission_total <- emission.data %>% group_by(poll) %>% summarise_at("emission", sum, na.rm=T)
  support <- sectors[[s]]$support()

  # Check
  creainventory::check.emission.d(emission.data)
  creainventory::check.support.sp(support)


  # Combine data with support
  emission <- creainventory::combine(emission.data, support) %>% filter(!is.na(emission))


  # Check all ids are there
  ids_with_emissions <- unique(emission.data$id[emission.data$emission>0])
  missing_ids <- setdiff(ids_with_emissions, unique(support$id))
  if(length(missing_ids)>0){
    warning("Missing support locations: ", missing_ids)
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
