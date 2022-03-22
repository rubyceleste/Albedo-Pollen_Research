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




library(tidyr)
library(raster)
library(rgdal)
#read in tif file of season wish to be used 
resamp_jan = readRDS('data/albedo_january8km.RDS')
resamp_feb = readRDS('data/albedo_february8km.RDS')
resamp_march = readRDS('data/albedo_march8km.RDS')
resamp_april = readRDS('data/albedo_april8km.RDS')
resamp_may = readRDS('data/albedo_may8km.RDS')
resamp_june = readRDS('data/albedo_june8km.RDS')
resamp_july = readRDS('data/albedo_july8km.RDS')
resamp_aug = readRDS('data/albedo_august8km.RDS')
resamp_sept = readRDS('data/albedo_september8km.RDS')
resamp_oct = readRDS('data/albedo_october.RDS')
resamp_nov = readRDS('data/albedo_november8km.RDS')
resamp_dec = readRDS('data/albedo_december8km.RDS')


# # 
# # get x and y coordinates for each cell
xy_alb1 = coordinates(resamp_jan)
xy_alb2 = coordinates(resamp_feb)
xy_alb3 = coordinates(resamp_march)
xy_alb4 = coordinates(resamp_april)
xy_alb5 = coordinates(resamp_may)
xy_alb6 = coordinates(resamp_june)
xy_alb7 = coordinates(resamp_july)
xy_alb8 = coordinates(resamp_aug)
xy_alb9 = coordinates(resamp_sept)
xy_alb10 = coordinates(resamp_oct)
xy_alb11 = coordinates(resamp_nov)
xy_alb12 = coordinates(resamp_dec)


# 
# # extract the corresponding values for each xy coordinate
vals_alb1 = data.frame(raster::extract(resamp_jan, xy_alb1, cellnumbers=T))
colnames(vals_alb1) = c('cell', 'alb_jan')
# combine xy coordinates and values into a long format data frame
df1 = data.frame(xy_alb1, value = vals_alb1[,'alb_jan'])
names(df1)[3] <- "alb_jan"
df1= na.omit(df1)
saveRDS(df1, 'data/df1.RDS')

vals_alb2 = data.frame(raster::extract(resamp_feb, xy_alb2, cellnumbers=T))
colnames(vals_alb2) = c('cell', 'alb_feb')
# combine xy coordinates and values into a long format data frame
df2 = data.frame(xy_alb2, value = vals_alb2[,'alb_feb'])
names(df2)[3] <- "alb_feb"
df2= na.omit(df2)
saveRDS(df2, 'data/df2.RDS')

vals_alb3 = data.frame(raster::extract(resamp_march, xy_alb3, cellnumbers=T))
colnames(vals_alb3) = c('cell', 'alb_march')
# combine xy coordinates and values into a long format data frame
df3 = data.frame(xy_alb3, value = vals_alb3[,'alb_march'])
names(df3)[3] <- "alb_march"
df3= na.omit(df3)
saveRDS(df3, 'data/df3.RDS')

vals_alb4 = data.frame(raster::extract(resamp_april, xy_alb4, cellnumbers=T))
colnames(vals_alb4) = c('cell', 'alb_april')
# combine xy coordinates and values into a long format data frame
df4 = data.frame(xy_alb4, value = vals_alb4[,'alb_april'])
names(df4)[3] <- "alb_april"
df4= na.omit(df4)
saveRDS(df4, 'data/df4.RDS')

vals_alb5 = data.frame(raster::extract(resamp_may, xy_alb5, cellnumbers=T))
colnames(vals_alb5) = c('cell', 'alb_may')
# combine xy coordinates and values into a long format data frame
df5 = data.frame(xy_alb5, value = vals_alb5[,'alb_may'])
names(df5)[3] <- "alb_may"
df5= na.omit(df5)
saveRDS(df1, 'data/df5.RDS')


vals_alb6 = data.frame(raster::extract(resamp_june, xy_alb6, cellnumbers=T))
colnames(vals_alb6) = c('cell', 'alb_june')
# combine xy coordinates and values into a long format data frame
df6 = data.frame(xy_alb6, value = vals_alb6[,'alb_june'])
names(df6)[3] <- "alb_june"
df6= na.omit(df6)
saveRDS(df6, 'data/df6.RDS')

vals_alb7 = data.frame(raster::extract(resamp_july, xy_alb7, cellnumbers=T))
colnames(vals_alb7) = c('cell', 'alb_july')
# combine xy coordinates and values into a long format data frame
df7 = data.frame(xy_alb7, value = vals_alb7[,'alb_july'])
names(df7)[3] <- "alb_july"
df7= na.omit(df7)
saveRDS(df7, 'data/df7.RDS')

vals_alb8 = data.frame(raster::extract(resamp_aug, xy_alb7, cellnumbers=T))
colnames(vals_alb8) = c('cell', 'alb_aug')
# combine xy coordinates and values into a long format data frame
df8 = data.frame(xy_alb8, value = vals_alb8[,'alb_aug'])
names(df8)[3] <- "alb_aug"
df8= na.omit(df8)
saveRDS(df8, 'data/df8.RDS')

vals_alb9 = data.frame(raster::extract(resamp_sept, xy_alb9, cellnumbers=T))
colnames(vals_alb9) = c('cell', 'alb_sept')
# combine xy coordinates and values into a long format data frame
df9 = data.frame(xy_alb9, value = vals_alb9[,'alb_sept'])
names(df9)[3] <- "alb_sept"
df9= na.omit(df9)
saveRDS(df9, 'data/df9.RDS')

vals_alb10 = data.frame(raster::extract(resamp_oct, xy_alb10, cellnumbers=T))
colnames(vals_alb10) = c('cell', 'alb_oct')
# combine xy coordinates and values into a long format data frame
df10 = data.frame(xy_alb10, value = vals_alb10[,'alb_oct'])
names(df10)[3] <- "alb_oct"
df10= na.omit(df10)
saveRDS(df10, 'data/df10.RDS')

vals_alb11 = data.frame(raster::extract(resamp_nov, xy_alb11, cellnumbers=T))
colnames(vals_alb11) = c('cell', 'alb_nov')
# combine xy coordinates and values into a long format data frame
df11 = data.frame(xy_alb11, value = vals_alb11[,'alb_nov'])
names(df11)[3] <- "alb_nov"
df11= na.omit(df11)
saveRDS(df11, 'data/df11.RDS')

vals_alb12 = data.frame(raster::extract(resamp_dec, xy_alb12, cellnumbers=T))
colnames(vals_alb12) = c('cell', 'alb_dec')
# combine xy coordinates and values into a long format data frame
df12 = data.frame(xy_alb12, value = vals_alb12[,'alb_dec'])
names(df12)[3] <- "alb_dec"
df12= na.omit(df12)
saveRDS(df12, 'data/df12.RDS')



# jan_reproj = readRDS('data/albedo_january.RDS')
# feb_reproj = readRDS('data/albedo_february.RDS')
# mar_reproj = readRDS('data/albedo_march.RDS')
# april_reproj = readRDS('data/albedo_april.RDS')
# may_reproj = readRDS('data/albedo_may.RDS')
# june_reproj = readRDS('data/albedo_june.RDS')
# july_reproj = readRDS('data/albedo_july.RDS')
# aug_reproj = readRDS('data/albedo_august.RDS')
# sept_reproj = readRDS('data/albedo_september.RDS')
# oct_reproj = readRDS('data/albedo_october.RDS')
# nov_reproj = readRDS('data/albedo_november.RDS')
# dec_reproj = readRDS('data/albedo_december.RDS')
# 

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

saveRDS(dat_all, 'data/dat_all_monthly8km.RDS')

#corser data
foo = readRDS('data/dat_all_monthly.RDS')


