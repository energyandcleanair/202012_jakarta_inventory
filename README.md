# Jakarta Emissions Inventory

Emission inventory around Jakarta.

## Progress
We haven't found yet a relevant support for each sector. Progress can be tracked/updated on this [spreadsheet](https://docs.google.com/spreadsheets/d/1WU8LVqEdHLG3Orsdqgm2-d1QvUggon5mK9b8-zhPzOI/edit?usp=sharing).

## Code structure
The code is structured by sector in the `sectors` folder. The main analysis (i.e. creating the emission .nc files0 is in [analysis.R](analysis.R).

### Emission x Support
Support can be point, line or polygon dataframe. Each feature has an id (typically a region_id) and a weight associated with it. Many features can belong to a single id (e.g. several power plants in a kabupaten); the weight of each feature is used to distribute emission associated with that id (e.g. a kabupaten), and *NOT across all features of the support data*. The `id` field is used to match with emission data.

### Functions
For each sector, there are three functions:

- `{sector}.build_support()`: building the support shapefile, saved as `support.shp` in the corresponding sector directory;
- `{sector}.get_support()`: simply reads the support shapefile and return the support sf;
- `{sector}.get_emission()`: return emission tibble.


## Access results
netCDF files are stored in [results](results) folder.
