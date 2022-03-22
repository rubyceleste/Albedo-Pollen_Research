library(ggplot2)
library(tidyverse)
library(tidyr)
library(reshape2)
library(mapdata)
library(maps)
library(sp)
library(scatterpie)

all_data = readRDS('data/dat_all_monthly.RDS')
pollen_props = readRDS('pollen-modern-proportions.RDS')


all_data$type=NA
#if statements >0.6 then decidious 

#this code provides a map of canada 
world <- map_data('world')
world = world[which((world$region %in% c('Canada')) | (world$subregion %in% 'Alaska')),]

world_sp = SpatialPointsDataFrame(coords=world[,1:2], data=world, proj4string=CRS("+init=epsg:4326"))
world_proj = spTransform(world_sp, alb_proj)
coords_world = coordinates(world_proj)
colnames(coords_world) = c('x', 'y')
world_proj = data.frame(coords_world, world)

world = world[world$long<0,]


#########################################################################################
#########################################################################################

melt_all = melt(all_data, id.vars=c('x', 'y', 'alb_jan', 'alb_feb', 'alb_march', 'alb_april',
                                   'alb_may', 'alb_june', 'alb_july', 'alb_aug', 'alb_sept', 'alb_oct', 
                                   'alb_nov', 'alb_dec', 'NA_L1NAME', 'NA_L2NAME'))


melt_all_prop = melt(pollen_props, id.vars=c('x','y'))

#creates a map with the points and color coded to see proportions of LCT in each region 
ggplot()+
  geom_point(data = pollen_props, aes(x=x, y=y, color = POACEAE))+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), color='white', fill=NA)+
  scale_color_distiller(type = 'seq', palette = "YlOrBr")


#boxplot for each of the ecoregions of Level 1 using all_data for OL LCT
ggplot()+
  geom_boxplot(data = all_data, aes(x= NA_L1NAME, y = ET), outlier.colour="black", outlier.shape=2,
             outlier.size=2, notch=FALSE)
  

#map zlbedo values across canada and alaska
ggplot()+
  geom_point(data = all_df, aes(x=x, y=y, color= finealb01))+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), color='white', fill=NA)+
  scale_color_distiller(type = 'seq', palette = "YlOrBr")



##plotting pollen proportions of a taxon on the map
# ggplot()+
#   geom_point(data = pollen_props, aes(x=x, y=y, color= ARTEMISIA)+
#   geom_polygon(data=world_proj, aes(x=x, y=y, group=group), color='white', fill=NA)+
#   scale_color_distiller(type = 'seq', palette = "YlOrBr")

##not working 
# ggplot()+
#   geom_scatterpie(data = melt_all_prop, aes(x=x, y=y), cols='variable', long_fromat =TRUE)+
#   coord_fixed()+
#   geom_scatterpie_legend(d$radius, x=-160, y=-55)+
#   geom_polygon(data=world_proj, aes(x=x, y=y, group=group), color='white', fill=NA)+
#   scale_color_distiller(type = 'seq', palette = "YlOrBr")


ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black")+
  coord_quickmap()+
  geom_scatterpie(aes(x=long, y=lat, group=region, r=radius),
                    data=pollen_props, cols=ARTEMISIA, color=NA, alpha=.8) +
  geom_scatterpie_legend(pollen_propsradius, x=-160, y=-55)
