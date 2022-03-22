library(elevatr)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

modern = readRDS('data/pollen_modern_pivot.RDS')

#this changes back to regular lat long
#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = modern[,c('x','y')], data = modern,
                               proj4string = CRS('+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

#transforming to the albedo crs: epsg 102001
pol_transform = spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

latLong =data.frame(coordinates(pol_transform), modern)

latLong = latLong[-c(3:7)]

ele_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ele_get = get_elev_point(latLong, ele_proj, src = "aws")

elevationn = data.frame(ele_get, latLong)

#saveRDS(elevationn, "data/elevation.RDS")
# ggplot()+
#      geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
#      geom_point(data=modern, aes(x=x,y=y))


dat_all = readRDS('data/albfine_snow.RDS')

dat = cbind(dat_all, elevation = elevationn$elevation )

saveRDS(dat, 'data/all-data.RDS')
