library(raster)
library(ncdf4)
library(sp)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'


pollen_modern = readRDS('data/pollen_modern_pivot.RDS')
df_pm = SpatialPointsDataFrame(coords=pollen_modern[,c('x', 'y')], data=pollen_modern, proj4string = crs(alb_proj))

# get temps
tmax_pm = data.frame(matrix(NA, nrow(df_pm), 12))
tmin_pm = data.frame(matrix(NA,  nrow(df_pm), 12))
ppt_pm = data.frame(matrix(NA,  nrow(df_pm), 12))

for (i in 1:12){

  # tmax_month = raster('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
  tmax_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
  tmax_month_alb = projectRaster(tmax_month$X3, crs = crs(alb_proj))
  tmax_month_pm = raster::extract(tmax_month_alb, df_pm)
  tmax_pm[,i] = tmax_month_pm
  
  tmin_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmin')
  tmin_month_alb = projectRaster(tmax_month$X3, crs = crs(alb_proj))
  tmin_month_pm = raster::extract(tmin_month_alb, df_pm)
  tmin_pm[,i]   = tmin_month_pm
  
  ppt_month = brick('data/GCM/ccsm3_22-0k_prcp.nc', level=i, varname='prcp')
  ppt_month_alb = projectRaster(ppt_month$X3, crs = crs(alb_proj))
  ppt_month_pm = raster::extract(ppt_month_alb, df_pm)
  ppt_pm[,i]   = ppt_month_pm
  
}

tmax_pm = data.frame(df_pm[,c('x', 'y')], tmax_pm)
tmin_pm = data.frame(df_pm[,c('x', 'y')], tmin_pm)
ppt_pm  = data.frame(df_pm[,c('x', 'y')], ppt_pm)

saveRDS(tmax_pm, 'data/tmax_pm.RDS')
saveRDS(tmin_pm, 'data/tmin_pm.RDS')
saveRDS(ppt_pm, 'data/ppt_pm.RDS')

library(reshape2)

# get temps
# tmax_pm = data.frame(matrix(NA, nrow(df_pm), 12))
# tmin_pm = data.frame(matrix(NA,  nrow(df_pm), 12))
# ppt_pm = data.frame(matrix(NA,  nrow(df_pm), 12))

tmax_pm = data.frame(month=numeric(0),
                     site=numeric(0),
                     year=character(0),
                     tmax=numeric(0))

tmin_pm = data.frame(month=numeric(0),
                     site=numeric(0),
                     year=character(0),
                     tmin=numeric(0))

ppt_pm = data.frame(month=numeric(0),
                     site=numeric(0),
                     year=character(0),
                     ppt=numeric(0))

for (i in 1:12){
  
  # tmax_month = raster('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
  tmax_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
  tmax_month_alb = projectRaster(tmax_month[[c('X1', 'X2', 'X3')]], crs = crs(alb_proj))
  tmax_month_pm = raster::extract(tmax_month_alb, df_pm)
  tmax_month_pm = melt(tmax_month_pm)
  colnames(tmax_month_pm) = c('site', 'year', 'tmax')
  
  tmax_pm = rbind(tmax_pm, 
              data.frame(month = i, tmax_month_pm))
  
  # tmax_pm[,i] = tmax_month_pm
  
  tmin_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmin')
  tmin_month_alb = projectRaster(tmax_month[[c('X1', 'X2', 'X3')]], crs = crs(alb_proj))
  tmin_month_pm = raster::extract(tmin_month_alb, df_pm)
  tmin_month_pm = melt(tmin_month_pm)
  colnames(tmin_month_pm) = c('site', 'year', 'tmin')
  
  tmin_pm = rbind(tmin_pm, 
                  data.frame(month = i, tmin_month_pm))
  # tmin_pm[,i]   = tmin_month_pm
  
  ppt_month = brick('data/GCM/ccsm3_22-0k_prcp.nc', level=i, varname='prcp')
  ppt_month_alb = projectRaster(ppt_month[[c('X1', 'X2', 'X3')]], crs = crs(alb_proj))
  ppt_month_pm = raster::extract(ppt_month_alb, df_pm)
  ppt_month_pm = melt(ppt_month_pm)
  colnames(ppt_month_pm) = c('site', 'year', 'ppt')
  
  ppt_pm = rbind(ppt_pm, 
                  data.frame(month = i, ppt_month_pm))
  # ppt_pm[,i]   = ppt_month_pm
  
}

# tmax_pm = data.frame(df_pm[,c('x', 'y')], tmax_pm)
# tmin_pm = data.frame(df_pm[,c('x', 'y')], tmin_pm)
# ppt_pm  = data.frame(df_pm[,c('x', 'y')], ppt_pm)

saveRDS(tmax_pm, 'data/tmax_pm.RDS')
saveRDS(tmin_pm, 'data/tmin_pm.RDS')
saveRDS(ppt_pm, 'data/ppt_pm.RDS')
