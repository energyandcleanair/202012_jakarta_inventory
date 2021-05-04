# Jakarta Emissions Inventory

Emission inventory in Jakarta region

## Variables
- emission.d.sector: total emission data
  - location_id (e.g. a province code, a powerplant id)
  - total
  - year
  - unit
  - sector


- support.sp.sector: vector support
  - location_id (matching )
  - weight (will be normalised to 1)
  - geometry
  - sector

  
- emission.sp.sector: total emission spatial data
  - location_id
  - emission
  - unit
  - year
  - geometry
  - sector

- grid: the original grid to project on


- emission.grid.sector: projected emissions on a raster 


- temp.month: monthly profile
- temp.weekly: monthly profile
- temp.hourly: monthly profile


- emission.stack.sector: projected emissions on a rasterstack with temporal dimensions 


## Acronyms in code

- surr: surrogate
- sp: spatial
- spt: spatio-temporal

e.g. `surr.sp` = spatial surrogate, `surr.spt` = spatio-temporal surrogate


`surr.sp` must be a spatial dataframe with the following fields:
  - id (feature id)
  - sector (sector)
  - weight (one weight per feature)

`surr.spt` must be a spatial dataframe with the following fields:
  - id
  - sector
  - date
  - weight (one weight per feature per date)

`surr.stack` is a raster stack where each layer corresponds to a specific date

  
## Sector classification


## Access results
netCDF files will be stored in [https://data.energyandcleanair.org/data/studies/202012_jakarta_inventory/](https://data.energyandcleanair.org/data/studies/202012_jakarta_inventory/)
