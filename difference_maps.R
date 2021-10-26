library(tidyr)
library(raster)
library(rgdal)
library(dplyr)
#read in tif file of season wish to be used 
albedo_dat = raster('data/ABOVE/season-winter.tif')

dat = readRDS('data/dat_all.RDS')
dat_time = readRDS('data/pollen_time-full.RDS')
prediction_dat = readRDS('data/prediction_data.RDS')

# EPSG: 102001 (Canada Albers Equal Area Conic, North American Datum, 1983)
alb_proj = crs(albedo_dat)
#creats border with outmost coords of the albedo data
borders <- sp::bbox(albedo_dat)

#creating a grid
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

#coordinates
xy = dat_time[1:2]
xy = prediction_dat[4:5]

#reassigning new CRS
spdf <- SpatialPointsDataFrame(coords = xy, data = prediction_dat, 
proj4string = CRS(" +proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83
+ +units=m +no_defs"))


pol_transform = spTransform(spdf, "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83
+ +units=m +no_defs ")

#new coordinates?
coords = coordinates(pol_transform)

#getting the cell numbers where each coordinate lies
cell = raster::extract(out_rast, coords)

#merging column with full data frame 
merge = data.frame(prediction_dat, cell)

merge = data.frame(dat_winter, cell)

# 
# #this works with dat and calculates the averages
# #calculating average of each season
# spring_mean = merge%>%
#   group_by(cell) %>%
#   summarise(spring_mean = mean(alb_spring, na.rm=TRUE), .groups = 'keep')
# summer_mean = merge%>%
#   group_by(cell) %>%
#   summarise(summer_mean = mean(alb_sum, na.rm=TRUE), .groups = 'keep')
# fall_mean = merge%>%
#   group_by(cell) %>%
#   summarise(fall_mean = mean(alb_fall, na.rm=TRUE), .groups = 'keep')
# winter_mean = merge%>%
#   group_by(cell) %>%
#   summarise(winter_mean = mean(alb_winter, na.rm=TRUE), .groups = 'keep')


time_mean = merge%>%
  group_by(cell, cut) %>%
  summarise(time_mean = mean(predict_time, na.rm=TRUE), .groups = 'keep')


xyCell = xyFromCell(out_rast, time_mean$cell)

time_mean = data.frame(time_mean, xyCell)

foo =time_mean %>%
  group_by(cell, x, y) %>%
  mutate(diff = time_mean - lag(time_mean, order_by =cut))


# #pivot data
# pivot_foo = time_mean %>%
#   pivot_wider(names_from = cut, values_from = time_mean)

ggplot()+
  geom_tile(data=foo, aes(x=x, y=y, fill = diff))+
  scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white')+
  facet_wrap(~cut)
  # scale_fill_brewer(type = "div", palette = 'RdBu')+
 








# #merging averages together in one data frame with cell number 
# averages_merge = data.frame(spring_mean, summer_mean$summer_mean, 
#                             fall_mean$fall_mean, winter_mean$winter_mean)

