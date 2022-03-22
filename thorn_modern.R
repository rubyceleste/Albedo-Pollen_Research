library(ClimClass)
library(sp)
library(rgdal)
library(dplyr)
library(geosphere)
library(reshape2)
library(tidyr)
library(ggplot2)


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
tmax = tmax[which((tmax$year>=2000)&(tmax$year<=2017)),]
tmin = read.csv('data/tmin_CRU.csv', stringsAsFactors = FALSE)
tmin = tmin[which((tmin$year>=2000)&(tmin$year<=2017)),]
ppt  = read.csv('data/ppt_CRU.csv', stringsAsFactors = FALSE)
ppt = ppt[which((ppt$year>=2000)&(ppt$year<=2017)),]

# why won't this join work?!?!
clim = left_join(ppt, tmin)
clim = left_join(clim, tmax)

clim = clim[order(clim$site, clim$year, clim$month),]
clim = clim[,c('site', 'year', 'month', 'ppt', 'tmin', 'tmax')]
colnames(clim) = c('site', 'year', 'month', 'P', 'Tn', 'Tx')

first_year = min(clim$year)
last_year  = max(clim$year)

sites = unique(clim$site)
N_sites = length(sites)

# try varying snow_melt_coeff
# default value is 1
snow_melt_coeff = c(0.75, 0.25) # c(0.5, 0.25, 0.25)

Tsnow = 1

thorn = list()
snow = data.frame(month = numeric(0),
                   site = numeric(0), 
                   year = numeric(0), 
                   snow = numeric(0),
                   et = numeric(0))

for (i in 1:N_sites){
  print(i)
  
  site = sites[i]
  dat_sub = clim[which(clim$site == site),2:6]
  if (all(is.na(dat_sub$P))){
    print(i)
    next
  }
  
  normals = climate(dat_sub, 
                    first.yr = first_year, 
                    last.yr  = last_year, 
                    max.perc.missing = 15)
  
  thorn[[i]] <-thornthwaite(series    = dat_sub, 
                            clim_norm = normals,
                            latitude  = df_pm[i,'lat'], 
                            first.yr  = first_year, 
                            last.yr   = last_year, 
                            snow_melt_coeff = snow_melt_coeff,
                            Tsnow = Tsnow)
  
  snow_site = data.frame(site=i, month=as.numeric(seq(1,12)), thorn[[i]]$W_balance$Snowpack)
  snow_site_melt = melt(snow_site, id.vars=c('month', 'site'))
  colnames(snow_site_melt) = c('month', 'site', 'year', 'snow')
  
  et_site = data.frame(site=i, month=as.numeric(seq(1,12)), thorn[[i]]$W_balance$Et0)
  et_site_melt = melt(et_site, id.vars=c('month', 'site'))
  colnames(et_site_melt) = c('month', 'site', 'year', 'et')
  
  
  
  snow_site_melt$year = as.numeric(substr(snow_site_melt$year, 2, 5))
  
  # print(nrow(snow_site_melt))
  
  snow = rbind(snow, 
               data.frame(snow_site_melt, et=et_site_melt$et))
}

saveRDS(snow, 'data/snow_pm.RDS')



# snow1 = snow
# snow2 = snow
snow3 = snow

ggplot()+
  geom_histogram(data = snow, aes(x = snow))

summary(snow$snow)

thresh = 75

snow$snowcover = NA
snow[which(snow$snow >= thresh), 'snowcover'] = 1
snow[which(snow$snow < thresh), 'snowcover'] = 0

snow_site = snow[which(snow$site==1),]
ggplot(data=snow_site) +
  geom_boxplot(aes(x=factor(month), y=snow))
ggplot(data=snow_site) +
  geom_point(aes(x=year, y=snow)) +
  facet_wrap(~month)


snow = merge(snow, clim)

# snow2 =snow %>% 
#   group_by('lat', 'long', 'month') %>% 
#   summarize(snow = mean(snowcover))

# snowNy = snow[-c(3,4)]
# pivot_mod = snowNy %>%
#   pivot_wider(names_from = month, values_from = snowcover)

# snow2 = snowNy %>% 
#   group_by(site, month) %>% 
#   summarise(meansnow = mean(snowcover))


# dat_mod = readRDS('data/pollen_modern_pivot.RDS')
# dat_mod = readRDS('data/dat_all_monthly.RDS')
# dat_mod = readRDS('data/snow_pm.RDS')
dat_mod = readRDS('data/all-data.RDS')

snow_mean = snow %>% 
  group_by(site, month) %>% 
  summarise(snowpack = median(snow), tsnow=mean(snowcover), et=mean(et), ppt=median(P), tmin=median(Tn))
# snow_mean$month = paste0('tsnow', snow_mean$month)

psnow = dat_mod[,c(6:17)]
psnow$site = seq(1,nrow(psnow))
psnow_melt = melt(psnow, id.vars=c('site'))
psnow_melt$month = substr(psnow_melt$variable, 6,7)
psnow_melt$month = as.numeric(psnow_melt$month)
psnow_melt$variable = substr(psnow_melt$variable, 1,5)
colnames(psnow_melt) = c('site', 'variable', 'psnow', 'month')



alb = dat_mod[,c(18:29)]
alb$site = seq(1,nrow(psnow))
alb_melt = melt(alb, id.vars=c('site'))
alb_melt$month = substr(alb_melt$variable, 8,9)
alb_melt$month = as.numeric(alb_melt$month)
alb_melt$variable = substr(alb_melt$variable, 1,7)
colnames(alb_melt) = c('site', 'variable', 'alb', 'month')


dat = merge(snow_mean, psnow_melt, by=c('site', 'month'))
dat = merge(dat, alb_melt[,-2], by=c('site', 'month'))
dat$alb = dat$alb/1000


# dat = data.frame(snow_mean, psnow=psnow_melt$value, alb=alb_melt$value/1000)

## compare variables with alb
ggplot(data=dat) +
  geom_point(aes(x=psnow, y=alb)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=snowpack, y=alb)) +
  # geom_smooth(aes(x=snowpack, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=log(snowpack), y=alb)) +
  # geom_smooth(aes(x=log(snowpack), y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=tsnow, y=alb)) +
  # geom_smooth(aes(x=tsnow, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=ppt, y=alb)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=tmin, y=alb)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=et, y=alb)) +
  # geom_smooth(aes(x=et, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

## compare variables with psnow

ggplot(data=dat) +
  geom_point(aes(x=psnow, y=tsnow)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(y=et, x=psnow)) + 
  # geom_smooth(aes(y=et, x=psnow), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(y=ppt, x=psnow)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(y=snowpack, x=psnow)) +
  # geom_smooth(aes(y=snowpack, x=psnow), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(y=log(snowpack), x=psnow)) +
  facet_wrap(~month)

## compare others from thorn

ggplot(data=dat) +
  geom_point(aes(x=snowpack, y=tsnow)) +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(y=et, x=tsnow)) +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(y=et, x=snowpack)) +
  facet_wrap(~month, scales="free")







# 
# # snow3$month = paste0('snowpack', snow3$month)
# snowpack_wide = snow_wide[c(1,2,3)] %>%
#   pivot_wider(names_from = month, values_from = snowpack)


