require(sf)
require(dplyr)

# Create example data


osm.lu <- sf::read_sf("~/Downloads/indonesia-latest-free.shp/gis_osm_landuse_a_free_1.shp")

osm.lu.lite <- osm.lu %>%
  dplyr::filter(fclass %in% c("farmland"))


gadm1 <- sf::read_sf("data/boundaries/gadm36_IDN_1.shp")


require(sp)
poly1 <- as(osm.lu.lite, "Spatial")
poly2 <- as(gadm1, "Spatial")

poly1$id <- sp::over(poly1, poly2)$GID_1
poly1$weight <- 1

sf::st_as_sf(lines)%>%
  sf::write_sf("example/farmland_spatial.shp")
