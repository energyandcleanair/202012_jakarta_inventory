require(sf)
require(dplyr)
require(creainventory)
require(creahelpers)

source('data.R')
source('data.transport.R')

# Transport emissions
emission.d.transport <- data.emission_transport()
support.sp.transport <- sf::read_sf("data/transport/transport_spatial.shp")

# Check
creainventory::check.emission.d(emission.d.transport)
creainventory::check.support.sp(support.sp.transport)

# Combine data with support
emission.transport <- creainventory::combine(emission.d.transport, support.sp.transport)


# METEOSIM resolution
grid.04 <- data.grid(0.04, extent=emission.transport)
r.transport.04 <- creainventory::grid.rasterize(emission.transport, grid.04)
raster::plot(r.transport.04)
saveRDS(r.transport.04, "results/r.transport.04.RDS")
r.transport.04 <- readRDS("results/r.transport.04.RDS")

# Finer resolution
grid.008 <- data.grid(0.008, extent=emission.transport)
r.transport.008 <- creainventory::grid.rasterize(emission.transport, grid.008)
raster::plot(r.transport.008)
saveRDS(r.transport.008, "results/r.transport.008.RDS")
r.transport.008 <- readRDS("results/r.transport.008.RDS")

# Sanity check
sum.raster <- raster::cellStats(r.transport.1, "sum") %>%
  tibble(value=., poll=names(.))
sum.input <- emission.transport %>%
  group_by(poll) %>%
  summarise(value=sum(emission))
sum.compared <- left_join(sum.raster, sum.input, by="poll")

sum.raster <- raster::cellStats(r, "sum") %>%
  tibble(value=., poll=names(.))
sum.input <- e.short %>%
  as.data.frame() %>%
  group_by(poll) %>%
  summarise(value=sum(emission))
sum.compared <- inner_join(sum.raster, sum.input, by="poll")

lapply(seq(nrow(sum.compared)), function(i){
  if(sum.compared[[i,"value.x"]]!=sum.compared[[i,"value.y"]]){
    stop("Emissions not conserved")
  }
})

# Create a stack representing monthly variations
month_shares <- rep(1/12, 12)
r.monthly.power <- temporal.split_months(r.power, month_shares)


# Export
export.monthly(r.monthly.power,
               poll="no2",
               sector="power",
               year=2019,
               folder="results")
