library(tidyr)
library(raster)
library(rgdal)
#read in tif file of season wish to be used 
albedo_dat = raster('data/ABOVE/new-month-12.tif')

# EPSG: 102001 (Canada Albers Equal Area Conic, North American Datum, 1983)
alb_proj = crs(albedo_dat)
#creats border with outmost coords of the albedo data
borders <- sp::bbox(albedo_dat)

#changing resolution of tiles, creating a grid 
out_rast <- raster::raster(xmn = floor(borders[1,1]),
                           xmx = ceiling(borders[1,2]),
                           ymn = floor(borders[2,1]),
                           ymx = ceiling(borders[2,2]),
                           resolution =8000,
                           crs = alb_proj)

out_rast <- raster::setValues(out_rast, 1:ncell(out_rast))


# y is the raster with the new grid we want to resample to
# need to construct y (or have downloaded object on a grid that you want to resample to)
dat_resamp = resample(albedo_dat, out_rast, method="ngb")

saveRDS(dat_resamp, 'data/albedo_december8km.RDS')
# # 
# # get x and y coordinates for each cell
# xy_alb = coordinates(dat_resamp_fall)
# 
# # extract the corresponding values for each xy coordinate
# vals_alb = data.frame(raster::extract(dat_resamp_fall, xy_alb, cellnumbers=T))
# colnames(vals_alb) = c('cell', 'alb_fall')
# 
# # combine xy coordinates and values into a long format data frame
# df = data.frame(xy_alb, value = vals_alb[,'alb_fall'])


# albedo_dat = raster('data/ABOVE/season-winter.tif')
# #creats border with outmost coords of the albedo data
# borders <- sp::bbox(albedo_dat)
# 
# #changing resolution of tiles
# out_rast <- raster::raster(xmn = floor(borders[1,1]),
#                            xmx = ceiling(borders[1,2]),
#                            ymn = floor(borders[2,1]),
#                            ymx = ceiling(borders[2,2]),
#                            resolution =24000,
#                            crs = alb_proj)
# 
# out_rast <- raster::setValues(out_rast, 1:ncell(out_rast))
# 
# 
# # y is the raster with the new grid we want to resample to
# # need to construct y (or have downloaded object on a grid that you want to resample to)
# dat_resamp_winter = resample(albedo_dat, out_rast, method="ngb")
# 
# # #saveRDS(dat_resamp, 'R scripts/albedo_raster.RDS')

# #delete any NA albedo values
# any(is.na(df$value))
# df <- df[apply(df, 1, function(row) all(row !=0 )), ]
# #saveRDS(df, 'R scripts/alb_all-winter.RDS')


# ecoregions for canada only
# eco = readOGR('data/Ecozones/', layer='ecozones')
library(rgdal)
# ecoregions for north america
eco = readOGR('data/Ecoregions/', layer='NA_CEC_Eco_Level2')

# curious what the native projection is
proj_eco = crs(eco)

# shapefile reprojection
eco_reproj = spTransform(eco, alb_proj)

# saveRDS(eco_reproj, "data/eco_reproj.RDS")

# # plot the north american ecoregion shapes to make sure shapefile is correct
# ggplot() +
#   geom_path(data = eco, aes(x=long, y=lat, group=group))

#here we are starting to merge the data together 
library(sp)

pollen_modern = readRDS('data/pollen_modern_pivot.RDS')
df_pm = SpatialPointsDataFrame(coords=pollen_modern[,c('x', 'y')], data=pollen_modern, proj4string = alb_proj)


jan_reproj = readRDS('data/albedo_january8km.RDS')
feb_reproj = readRDS('data/albedo_february8km.RDS')
mar_reproj = readRDS('data/albedo_march8km.RDS')
april_reproj = readRDS('data/albedo_april8km.RDS')
may_reproj = readRDS('data/albedo_may8km.RDS')
june_reproj = readRDS('data/albedo_june8km.RDS')
july_reproj = readRDS('data/albedo_july8km.RDS')
aug_reproj = readRDS('data/albedo_august8km.RDS')
sept_reproj = readRDS('data/albedo_september8km.RDS')
oct_reproj = readRDS('data/albedo_october8km.RDS')
nov_reproj = readRDS('data/albedo_november8km.RDS')
dec_reproj = readRDS('data/albedo_december8km.RDS')


# for each x,y in our albedo grid, get the ecoregion
lct_eco = over(df_pm, eco_reproj)
# get albedo value for each pollen sample
lct_alb_jan = raster::extract(jan_reproj, df_pm)
lct_alb_feb = raster::extract(feb_reproj, df_pm)
lct_alb_mar = raster::extract(mar_reproj, df_pm)
lct_alb_apr = raster::extract(april_reproj, df_pm)
lct_alb_may = raster::extract(may_reproj, df_pm)
lct_alb_june = raster::extract(june_reproj, df_pm)
lct_alb_july = raster::extract(july_reproj, df_pm)
lct_alb_aug = raster::extract(aug_reproj, df_pm)
lct_alb_sept = raster::extract(sept_reproj, df_pm)
lct_alb_oct = raster::extract(oct_reproj, df_pm)
lct_alb_nov = raster::extract(nov_reproj, df_pm)
lct_alb_dec = raster::extract(dec_reproj, df_pm)


library(reshape2)
#binding data- LCT, with prop-summed and the albedo values 
dat_all = data.frame(coordinates(df_pm),pollen_modern[,c('ET', 'OL', 'ST')],
                     alb_jan=lct_alb_jan, alb_feb=lct_alb_feb, alb_march=lct_alb_mar, 
                     alb_april=lct_alb_apr, alb_may=lct_alb_may, alb_june=lct_alb_june,
                     alb_july=lct_alb_july, alb_aug=lct_alb_aug, alb_sept=lct_alb_sept,
                     alb_oct=lct_alb_oct, alb_nov=lct_alb_nov, alb_dec=lct_alb_dec,
                     lct_eco[,c('NA_L1NAME', 'NA_L2NAME')])

# don't remeber what this does 
# dat_all= dat_all %>%
#   mutate(alb_winter = replace(alb_winter, alb_winter==0, NA),
#          alb_spring = replace(alb_spring, alb_spring==0, NA),
#          alb_summer = replace(alb_summer, alb_sum==0, NA),
#          alb_fall = replace(alb_fall, alb_fall==0, NA))

#apply(dat_all, 1, function(x) x[which(x==0)]=NA)
#dat_all[which(dat_all==0)]=NA

# any(is.na(dat_all$alb))
# dat_all = dat_all[which(!is.na(dat_all$alb)),]
# 
# dat_all = dat_all[which(dat_all$alb>0),]

saveRDS(dat_all, 'data/dat_all_monthly8km.RDS')


