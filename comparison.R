library(creainventory)
library(raster)
library(sf)
library(tidyverse)
library(creatrajs)
library(eixport)

lapply(list.files(".", "data.*.R"),source)
source('utils.R')
source('edgar.R')

sector <- "power"
poll <- "NOx"

r.result <- raster::raster(sprintf("results/%s.%s.edgar.tif", sector, poll))
total.result.tonne <- raster::cellStats(r.result, sum)
n <- names(r.result)
r.result <- r.result / raster::area(r.result)
names(r.result) <- n
r.edgar <- edgar.emission(sector=sector, poll=poll)
total.edgar.tonne <- raster::cellStats(r.edgar * raster::area(r.edgar),sum)
r.edgar[r.edgar==0] <- NA
r.result[r.result==0] <- NA

names(r.edgar) <- sprintf("%s\n Total 2015: %f tonnes", names(r.edgar), total.edgar.tonne)
raster::stack(c(r.result, r.edgar)) %>% raster::plot(colNA="black")
