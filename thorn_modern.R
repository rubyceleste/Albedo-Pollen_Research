library(ClimClass)
library(sp)
library(rgdal)
library(dplyr)

source('thornthwaite.R')

# 
# data(Trent_climate)
# 
# library(geosphere)
# 
# thornt_lst<-NULL
# lista_cli <- lista_cli[1:3] ## lista_cli is reduced to diminish elapsed time of execution!
# for(k in 1 : length(lista_cli[1:3])) {
#   thornt_lst[[k]]<-thornthwaite(series=lista_cli[[k]], 
#                                 clim_norm=clima_81_10[[k]],
#                                 latitude = 46, first.yr=1981, 
#                                 last.yr=1983, snow_melt_coeff=c(0.5,0.5 )  )
# }
# names(thornt_lst)<-names(lista_cli)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +datum=NAD83'

# CRS("+init=EPSG:4326")
proj = '+init=epsg:4326 +proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0'

pollen_modern = readRDS('data/pollen_modern_pivot.RDS')
df_pm    = SpatialPointsDataFrame(coords=pollen_modern[,c('x', 'y')], data=pollen_modern, proj4string = CRS(alb_proj))
df_pm_ll = spTransform(df_pm, proj)
coords = coordinates(df_pm_ll)
colnames(coords) = c('long', 'lat')
df_pm = data.frame(coords, df_pm)[,1:7]

tmax = read.csv('data/tmax_CRU.csv', stringsAsFactors = FALSE)
tmax = tmax[which(tmax$year>2000),]
tmin = read.csv('data/tmin_CRU.csv', stringsAsFactors = FALSE)
tmin = tmin[which(tmin$year>2000),]
ppt  = read.csv('data/ppt_CRU.csv', stringsAsFactors = FALSE)
ppt = ppt[which(ppt$year>2000),]

# why won't this join work?!?!
clim = left_join(ppt, tmin)
clim = left_join(clim, tmax)

clim = clim[order(clim$site, clim$year, clim$month),]
clim = clim[,c('site', 'year', 'month', 'ppt', 'tmin', 'tmax')]
colnames(clim) = c('site', 'year', 'month', 'P', 'Tn', 'Tx')

sites = unique(foo$site)
N_sites = length(sites)

thorn = list()
snow = data.frame(month = numeric(0),
                   site = numeric(0), 
                   year = numeric(0), 
                   snow = numeric(0))

for (i in 1:N_sites){
  # print(i)
  
  site = sites[i]
  dat_sub = foo[which(foo$site == site),2:6]
  if (all(is.na(dat_sub$P))){
    print(i)
    next
  }
  
  normals = climate(dat_sub, first.yr=1961, last.yr=1963, max.perc.missing=15)
  
  thorn[[i]] <-thornthwaite(series=dat_sub, 
                           clim_norm=normals,
                           latitude = df_pm[i,'lat'], 
                           first.yr=1961, 
                           last.yr=1963, 
                           snow_melt_coeff=c(0.5,0.5))
  
  snow_site = data.frame(site=i, month=as.numeric(seq(1,12)), thorn[[i]]$W_balance$Snowpack)
  snow_site_melt = melt(snow_site, id.vars=c('month', 'site'))
  colnames(snow_site_melt) = c('month', 'site', 'year', 'snow')
  snow_site_melt$year = as.numeric(substr(snow_site_melt$year, 2, 5))
  
  print(nrow(snow_site_melt))
  
  snow = rbind(snow, 
               snow_site_melt)
}

saveRDS(snow, 'data/snow_pm.RDS')
