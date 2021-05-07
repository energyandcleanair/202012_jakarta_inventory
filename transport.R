require(sf)
require(dplyr)
require(creainventory)
require(creahelpers)

source('data.R')


# Transport emissions
emission.d.transport <- data.emission_transport()
support.sp.transport <- sf::read_sf("data/transport/transport_spatial.shp")

# Check
creainventory::check.emission.d(emission.d.transport)
creainventory::check.support.sp(support.sp.transport)

# Combine data with support
emission.transport <- creainventory::combine(emission.d.transport, support.sp.transport)

# Read grid support
# grid <- data.grid.d04()
grid <- data.grid(0.008, extent=emission.transport)

# Create a single layer representing whole year
r.transport <- creainventory::grid.rasterize(emission.transport, grid)

# Sanity check
if(sum(emission.transport$emission) != raster::cellStats(r.transport, "sum")){
  stop("Emissions not conserved")
}


# Create a stack representing monthly variations
month_shares <- rep(1/12, 12)
r.monthly.power <- temporal.split_months(r.power, month_shares)


# Export
export.monthly(r.monthly.power,
               poll="no2",
               sector="power",
               year=2019,
               folder="results")
