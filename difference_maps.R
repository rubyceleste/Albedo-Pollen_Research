library(tidyr)
library(raster)
library(rgdal)
library(dplyr)
#read in tif file of season wish to be used 
dat_all = raster('data/ABOVE/season-winter.tif')

dat = readRDS('data/dat_all.RDS')

# EPSG: 102001 (Canada Albers Equal Area Conic, North American Datum, 1983)
alb_proj = crs(albedo_dat)
#creats border with outmost coords of the albedo data
borders <- sp::bbox(albedo_dat)

#creating a grid
#for each coordinate find which grid cell it is in and create a column with the 
#grid cell number
#for each value in a grid cell average those values

#changing resolution of tiles
out_rast <- raster::raster(xmn = floor(borders[1,1]),
                           xmx = ceiling(borders[1,2]),
                           ymn = floor(borders[2,1]),
                           ymx = ceiling(borders[2,2]),
                           resolution =200000,
                           crs = alb_proj)

out_rast <- raster::setValues(out_rast, 1:ncell(out_rast))

# +proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +ellps=GRS80
# +units=m +no_defs 
xy = dat[1:2]

spdf <- SpatialPointsDataFrame(coords = xy, data = dat, 
proj4string = CRS(" +proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83
+ +units=m +no_defs"))

pol_transform = spTransform(spdf, "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83
+ +units=m +no_defs ")
coords = coordinates(pol_transform)

cell = raster::extract(out_rast, coords)

merge = data.frame(dat, cell)

foo = merge%>%
  group_by(cell) %>%
  summarise(average = mean(alb_winter, na.rm=TRUE), .groups = 'keep')

