library(raster)

snow_jan = raster('data/snow/snow-month-04.tif')

alb_proj = crs(snow_jan)
#creats border with outmost coords of the albedo data
borders <- sp::bbox(snow_jan)
#changing resolution of tiles, creating a grid 
out_rast <- raster::raster(xmn = floor(borders[1,1]),
                           xmx = ceiling(borders[1,2]),
                           ymn = floor(borders[2,1]),
                           ymx = ceiling(borders[2,2]),
                           resolution =24000,
                           crs = snow_jan)
out_rast <- raster::setValues(out_rast, 1:ncell(out_rast))
# y is the raster with the new grid we want to resample to
# need to construct y (or have downloaded object on a grid that you want to resample to)
dat_resamp = resample(snow_jan, out_rast, method="ngb")
# get x and y coordinates for each cell
xy_snow = coordinates(dat_resamp)
# extract the corresponding values for each xy coordinate
vals_snow = data.frame(raster::extract(dat_resamp, xy_snow, cellnumbers=T))
colnames(vals_snow) = c('cell', 'snow')
# combine xy coordinates and values into a long format data frame
df = data.frame(xy_snow, value = vals_snow[,'snow'])



library(tidyr)
library(raster)
library(rgdal)
#read in tif file of season wish to be used 
snow_jan = raster('data/snow/snow-month-01.tif')
snow_feb = raster('data/snow/snow-month-02.tif')
snow_march = raster('data/snow/snow-month-03.tif')
snow_april = raster('data/snow/snow-month-04.tif')
snow_may = raster('data/snow/snow-month-05.tif')
snow_june = raster('data/snow/snow-month-06.tif')
snow_july = raster('data/snow/snow-month-07.tif')
snow_aug = raster('data/snow/snow-month-08.tif')
snow_sept = raster('data/snow/snow-month-09.tif')
snow_oct = raster('data/snow/snow-month-10.tif')
snow_nov = raster('data/snow/snow-month-11.tif')
snow_dec = raster('data/snow/snow-month-12.tif')


# # 
# # get x and y coordinates for each cell
xy_snow1 = coordinates(snow_jan)
xy_snow2 = coordinates(snow_feb)
xy_snow3 = coordinates(snow_march)
xy_snow4 = coordinates(snow_april)
xy_snow5 = coordinates(snow_may)
xy_snow6 = coordinates(snow_june)
xy_snow7 = coordinates(snow_july)
xy_snow8 = coordinates(snow_aug)
xy_snow9 = coordinates(snow_sept)
xy_snow10 = coordinates(snow_oct)
xy_snow11 = coordinates(snow_nov)
xy_snow12 = coordinates(snow_dec)


# 
# # extract the corresponding values for each xy coordinate
vals_snow1 = data.frame(raster::extract(snow_jan, xy_snow1, cellnumbers=T))
colnames(vals_snow1) = c('cell', 'snow_jan')
# combine xy coordinates and values into a long format data frame
df1 = data.frame(xy_snow1, value = vals_snow1[,'snow_jan'])
names(df1)[3] <- "snow_jan"
df1= na.omit(df1)
saveRDS(df1, 'data/df_snow1.RDS')

vals_snow2 = data.frame(raster::extract(snow_feb, xy_snow2, cellnumbers=T))
colnames(vals_snow2) = c('cell', 'snow_feb')
# combine xy coordinates and values into a long format data frame
df2 = data.frame(xy_snow2, value = vals_snow2[,'snow_feb'])
names(df2)[3] <- "snow_feb"
df2= na.omit(df2)
saveRDS(df2, 'data/df_snow2.RDS')

vals_snow3 = data.frame(raster::extract(snow_march, xy_snow3, cellnumbers=T))
colnames(vals_snow3) = c('cell', 'snow_march')
# combine xy coordinates and values into a long format data frame
df3 = data.frame(xy_snow3, value = vals_snow3[,'snow_march'])
names(df3)[3] <- "snow_march"
df3= na.omit(df3)
saveRDS(df3, 'data/df_snow3.RDS')

vals_snow4 = data.frame(raster::extract(snow_april, xy_snow4, cellnumbers=T))
colnames(vals_snow4) = c('cell', 'snow_april')
# combine xy coordinates and values into a long format data frame
df4 = data.frame(xy_snow4, value = vals_snow4[,'snow_april'])
names(df4)[3] <- "snow_april"
df4= na.omit(df4)
saveRDS(df4, 'data/df_snow4.RDS')

vals_snow5 = data.frame(raster::extract(snow_may, xy_snow5, cellnumbers=T))
colnames(vals_snow5) = c('cell', 'snow_may')
# combine xy coordinates and values into a long format data frame
df5 = data.frame(xy_snow5, value = vals_snow5[,'snow_may'])
names(df5)[3] <- "snow_may"
df5= na.omit(df5)
saveRDS(df1, 'data/df_snow5.RDS')


vals_snow6 = data.frame(raster::extract(snow_june, xy_snow6, cellnumbers=T))
colnames(vals_snow6) = c('cell', 'snow_june')
# combine xy coordinates and values into a long format data frame
df6 = data.frame(xy_snow6, value = vals_snow6[,'snow_june'])
names(df6)[3] <- "snow_june"
df6= na.omit(df6)
saveRDS(df6, 'data/df_snow6.RDS')

vals_snow7 = data.frame(raster::extract(snow_july, xy_snow7, cellnumbers=T))
colnames(vals_snow7) = c('cell', 'snow_july')
# combine xy coordinates and values into a long format data frame
df7 = data.frame(xy_snow7, value = vals_snow7[,'snow_july'])
names(df7)[3] <- "snow_july"
df7= na.omit(df7)
saveRDS(df7, 'data/df_snow7.RDS')

vals_snow8 = data.frame(raster::extract(snow_aug, xy_snow7, cellnumbers=T))
colnames(vals_snow8) = c('cell', 'snow_aug')
# combine xy coordinates and values into a long format data frame
df8 = data.frame(xy_snow8, value = vals_snow8[,'snow_aug'])
names(df8)[3] <- "snow_aug"
df8= na.omit(df8)
saveRDS(df8, 'data/df_snow8.RDS')

vals_snow9 = data.frame(raster::extract(snow_sept, xy_snow9, cellnumbers=T))
colnames(vals_snow9) = c('cell', 'snow_sept')
# combine xy coordinates and values into a long format data frame
df9 = data.frame(xy_snow9, value = vals_snow9[,'snow_sept'])
names(df9)[3] <- "snow_sept"
df9= na.omit(df9)
saveRDS(df9, 'data/df_snow9.RDS')

vals_snow10 = data.frame(raster::extract(snow_oct, xy_snow10, cellnumbers=T))
colnames(vals_snow10) = c('cell', 'snow_oct')
# combine xy coordinates and values into a long format data frame
df10 = data.frame(xy_snow10, value = vals_snow10[,'snow_oct'])
names(df10)[3] <- "snow_oct"
df10= na.omit(df10)
saveRDS(df10, 'data/df_snow10.RDS')

vals_snow11 = data.frame(raster::extract(snow_nov, xy_snow11, cellnumbers=T))
colnames(vals_snow11) = c('cell', 'snow_nov')
# combine xy coordinates and values into a long format data frame
df11 = data.frame(xy_snow11, value = vals_snow11[,'snow_nov'])
names(df11)[3] <- "snow_nov"
df11= na.omit(df11)
saveRDS(df11, 'data/df_snow11.RDS')

vals_snow12 = data.frame(raster::extract(snow_dec, xy_snow12, cellnumbers=T))
colnames(vals_snow12) = c('cell', 'snow_dec')
# combine xy coordinates and values into a long format data frame
df12 = data.frame(xy_snow12, value = vals_snow12[,'snow_dec'])
names(df12)[3] <- "snow_dec"
df12= na.omit(df12)
saveRDS(df12, 'data/df_snow12.RDS')



library(reshape2)
#binding data- LCT, with prop-summed and the albedo values 
dat_all = data.frame(coordinates(df_pm),pollen_modern[,c('ET', 'OL', 'ST')],
                     alb_jan=lct_alb_jan, alb_feb=lct_alb_feb, alb_march=lct_alb_mar, 
                     alb_april=lct_alb_apr, alb_may=lct_alb_may, alb_june=lct_alb_june,
                     alb_july=lct_alb_july, alb_aug=lct_alb_aug, alb_sept=lct_alb_sept,
                     alb_oct=lct_alb_oct, alb_nov=lct_alb_nov, alb_dec=lct_alb_dec,
                     lct_eco[,c('NA_L1NAME', 'NA_L2NAME')])





