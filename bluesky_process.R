library(tidyr)
library(raster)
library(rgdal)
#read in tif file of season wish to be used 
albedo_dat = raster('bluesky/season-winter.tif')

# EPSG: 102001 (Canada Albers Equal Area Conic, North American Datum, 1983)
alb_proj = crs(albedo_dat)
#creats border with outmost coords of the albedo data
borders <- sp::bbox(albedo_dat)

#changing resolution of tiles
out_rast <- raster::raster(xmn = floor(borders[1,1]),
                           xmx = ceiling(borders[1,2]),
                           ymn = floor(borders[2,1]),
                           ymx = ceiling(borders[2,2]),
                           resolution =24000,
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

# ecoregions for canada only
# eco = readOGR('data/Ecozones/', layer='ecozones')
library(rgdal)
# ecoregions for north america
eco = readOGR('ecoregion data', layer='NA_CEC_Eco_Level2')

# curious what the native projection is
proj_eco = crs(eco)

# shapefile reprojection
eco_reproj = spTransform(eco, alb_proj)

saveRDS(eco_reproj, "R scripts/eco_reproj.RDS")

# # plot the north american ecoregion shapes to make sure shapefile is correct
# ggplot() +
#   geom_path(data = eco, aes(x=long, y=lat, group=group))

# convert data frame to spatial object
# not sure if this step is necessary
df_sp = SpatialPointsDataFrame(coords=df[,c('x', 'y')], data=df, proj4string = alb_proj)
df_transform = spTransform(df_sp, alb_proj)
# 
# # for each x,y in our albedo grid, get the ecoregion
# df_eco = over(df_sp, eco_reproj)
# #keeping only the eco region columns and removing everything else
# df_eco = df_eco[,c('NA_L1NAME', 'NA_L2NAME')]

# put all the information in a single data frame
#df_all = data.frame(coordinates(df_sp), df_eco)
#saveRDS(df_all, 'R scripts/albedo_eco.RDS')

#here we are starting to merge the data together 
library(sp)

pollen_modern = readRDS('R scripts/pollen_modern_longversion.RDS')
pollen_modern = data.frame(pollen_modern)


# for each x,y in our albedo grid, get the ecoregion
lct_eco = over(df_transform, eco_reproj)

# get albedo value for each pollen sample
lct_alb = raster::extract(dat_resamp, df_transform)


library(reshape2)
#binding data- LCT, with prop-summed and the albedo values 
dat_all = cbind(coordinates(df_sp),pollen_modern[,c('LCT', 'prop_summed')], lct_alb, lct_eco[,c('NA_L1NAME', 'NA_L2NAME')])

saveRDS(dat_all, 'R scripts/dat_all-winter.RDS')


# #do this for each season 
# dat_OL = dat_all[which(dat_all$LCT == 'OL'),]
# dat_ET = dat_all[which(dat_all$LCT == 'ET'),]
# dat_ST = dat_all[which(dat_all$LCT == 'ST'),]
# OLcover=lm(prop_summed ~ lct_alb , data=dat_OL)
# ET=lm(prop_summed ~ lct_alb, data=dat_ET)
# ST=lm(prop_summed ~ lct_alb, data=dat_ST)
# 
# OLcover=lm(lct_alb~prop_summed , data=dat_OL)
# 
# 
# summary(OLcover)
# summary(ET)
# summary(ST)
