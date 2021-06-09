sector <- "transport"
poll <- "NOx"

r.result <- raster::raster(sprintf("results/%s.%s.edgar.tif", sector, poll))
r.edgar <- edgar.emission(sector=sector, poll=poll)
r.edgar[r.edgar==0] <- NA
raster::stack(c(r.result, r.edgar)) %>% raster::plot(colNA="black")
