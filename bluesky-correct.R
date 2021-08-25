library(tidyr)
library(raster)
library(rgdal)
albedo_dat = raster('bluesky/season-winter.tif')

# EPSG: 102001 (Canada Albers Equal Area Conic, North American Datum, 1983)
alb_proj = crs(albedo_dat)

borders <- sp::bbox(albedo_dat)


out_rast <- raster::raster(xmn = floor(borders[1,1]),
                           xmx = ceiling(borders[1,2]),
                           ymn = floor(borders[2,1]),
                           ymx = ceiling(borders[2,2]),
                           resolution = 24000,
                           crs = alb_proj)

out_rast <- raster::setValues(out_rast, 1:ncell(out_rast))

# y is the raster with the new grid we want to resample to
# need to construct y (or have downloaded object on a grid that you want to resample to)
dat_resamp = resample(albedo_dat, out_rast, method="bilinear")

#saveRDS(dat_resamp, 'R scripts/albedo_raster.RDS')

# get x and y coordinates for each cell
xy_alb = coordinates(dat_resamp)


# extract the corresponding values for each xy coordinate
vals_alb = data.frame(raster::extract(dat_resamp, xy_alb, cellnumbers=T))
colnames(vals_alb) = c('cell', 'value')

# combine xy coordinates and values into a long format data frame
df = data.frame(xy_alb, value = vals_alb[,'value'])
head(df)


# read in ecoregion data
library(rgdal)

# ecoregions for canada only
# eco = readOGR('data/Ecozones/', layer='ecozones')

# ecoregions for north america
eco = readOGR('ecoregion data', layer='NA_CEC_Eco_Level2')

# curious what the native projection is
# proj_eco = crs(eco)

# shapefile reprojection
eco_reproj = spTransform(eco, alb_proj)

# # plot the north american ecoregion shapes to make sure shapefile is correct
# ggplot() +
#   geom_path(data = eco, aes(x=long, y=lat, group=group))

# convert data frame to spatial object
# not sure if this step is necessary
df_sp = SpatialPointsDataFrame(coords=df[,c('x', 'y')], data=df, proj4string = alb_proj)

# for each x,y in our albedo grid, get the ecoregion
df_eco = over(df_sp, eco_reproj)

# put all the information in a single data frame
df_all = data.frame(cbind(data.frame(df_sp)[,1:3], df_eco))
saveRDS(df_all, 'R scripts/albedo_eco.RDS')

# # plot the ecoregions
# ggplot() + 
#   geom_point(data=df_eco, aes(x=x, y=y, color=NA_L2NAME))


#here we are starting to merge the data together 
library(sp)

# +proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
# +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
#alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' 
#alb = readRDS('R scripts/albedo_eco.RDS')
#alb = readRDS('R scripts/albedo_raster.RDS')

lct = readRDS('R scripts/lct_longversion.RDS')
lct = data.frame(lct)

# convert data frame to spatial object
df_sp = SpatialPoints(lct[,c('long', 'lat')], proj4string = CRS("+init=epsg:4326"))
df_transform = spTransform(df_sp, alb_proj)

lct_coords = coordinates(df_transform)
colnames(lct_coords) = c('x', 'y')


# curious what the native projection is
# proj_eco = crs(eco)

# for each x,y in our albedo grid, get the ecoregion
lct_eco = over(df_transform, eco_reproj)


# get albedo value for each pollen sample

lct_alb = raster::extract(dat_resamp, df_transform)



library(reshape2)
#binding data- LCT, with prop-summed and the albedo values 
dat_all = cbind(lct_coords, lct[,c('LCT', 'prop_summed')], lct_alb, lct_eco)

saveRDS(dat_all, 'R scripts/dat_all-winter.RDS')
#wide = dcast(dat_all_simple, x + y + prop_summed + lct_alb ~ LCT)




#do this for each season 
dat_OL = dat_all[which(dat_all$LCT == 'OL'),]
dat_ET = dat_all[which(dat_all$LCT == 'ET'),]
dat_ST = dat_all[which(dat_all$LCT == 'ST'),]
OLcover=lm(prop_summed ~ lct_alb , data=dat_OL)
ET=lm(prop_summed ~ lct_alb, data=dat_ET)
ST=lm(prop_summed ~ lct_alb, data=dat_ST)

OLcover=lm(lct_alb~prop_summed , data=dat_OL)


summary(OLcover)
summary(ET)
summary(ST)
