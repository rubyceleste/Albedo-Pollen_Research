library(ggplot2)
library(tidyverse)
library(tidyr)
library(reshape2)
library(mapdata)
library(maps)
library(sp)

#this code provides a map of canada 
world <- map_data('world')
world = world[which((world$region %in% c('Canada')) | (world$subregion %in% 'Alaska')),]

world_sp = SpatialPointsDataFrame(coords=world[,1:2], data=world, proj4string=CRS("+init=epsg:4326"))
world_proj = spTransform(world_sp, alb_proj)
coords_world = coordinates(world_proj)
colnames(coords_world) = c('x', 'y')
world_proj = data.frame(coords_world, world)

world = world[world$long<0,]

##################################################################################

dat_all = readRDS('R scripts/dat_all-fall.RDS')


# #df is albedo values
# any(is.na(df$value))
# df = df[which(!is.na(df$value)),]
# df = df[which(df$value >0.1),]
#
# #getting rid of 0.000 values for better map data
# dat_all = dat_all[which(dat_all$alb >0.1),]

#need long version for graphs
dat_all_melt = melt(dat_all, id.vars=c('x', 'y','ET', 'ST','OL',
                                       'NA_L1NAME', 'NA_L2NAME'))


#figure 1
#map of albedo
 ggplot() +
  geom_point(data= dat_all_melt, aes(x=x, y=y, color=value))+
  geom_point(data=xy, aes(x=long, y=lat))
  #scale_colour_gradient(low="black", high="white")

#
 #TEST
 ggplot() +
   geom_point(data=pivot_foo, aes(x=x, y=y,))+
    geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white') +
   # geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='white') +
   geom_point(data=pivot_foo, aes(x=x, y=y,))
   # scale_colour_gradient(low="black", high="light pink")+
   # facet_grid(variable~.) +
   #coord_equal()
#
#
#plotting with coordinates
ggplot() +
   geom_point(data= df, aes(x=x, y=y, color=value))+
  geom_point(data= dat_all, aes(x=x, y=y),color='red')+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), color='white', fill=NA) +
  scale_colour_gradient(low="black", high="light blue")
ggplot() +
  geom_point(data= dat_all, aes(x=x, y=y, color=alb))+
  scale_colour_gradient(low="black", high="light blue")
#
#
# #scatterplot vs y
ggplot() +
  geom_point(data=dat_all_melt, aes(x=y, y=value))+
  facet_wrap(~variable)
# #scatterplot vs x
# alb_scat_x <- ggplot() +
#   geom_point(data=df, aes(x=x, y=value, color=value))+
#   scale_colour_gradient(low="black", high="light blue")
#
# # ggsave('figures/2001_182_scatter_vsX.png')
# #histogram
# alb_hist<-ggplot(data=df, aes(df$value)) +
#   geom_histogram(color= "darkblue", fill="light blue")
#
# library(grid)
# library(gridExtra)
#
# #puts 4 figures together
# plots <- list(alb_map, alb_hist, alb_scat_x, alb_scat_y)
# layout <- rbind(c(1,2),c(3,4))
# grid.arrange(grobs=plots, layout_matrix = layout, top= "Albedo Figures", vp=viewport(width = 0.8, height=1))
#
#
# #figure 2
# ggplot(data=dat_all_melt, aes(value, color=variable)) +
#   geom_histogram(fill='white') +
#   facet_grid(variable~.)
# ggsave('figures/fig2.png')
#
# #figure 3
# #LCT vs long and lat
#  ggplot() +
#    geom_point(data=dat_all_melt, aes(x=x, y=value, color=variable))
# ggsave('figures/fig3.0.png')
# #same as fig 4 but 4 has lm
# #  ggplot() +
# #    geom_point(data=dat_all_melt, aes(x=y, y=value, color=variable))
# # ggsave('figures/fig3.1.png')
#
#
# #figure 4
# ggplot(data=dat_all_melt, aes(x=y, y=value, color=variable)) +
#   geom_point() +
#   geom_smooth(method = 'lm', se = FALSE)
#  # facet_grid(variable~.)
# ggsave('figures/fig4.png')
#
#
# #WORLD MAP
#figure 5
ggplot() +
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white') +
  geom_point(data=dat_all, aes(x=x, y=y, color=OL)) +
  scale_colour_gradient(low="black", high="light pink")
#  facet_grid(variable~.) +
  coord_equal()
ggsave('figures/fig5.png')

#figure 6
ggplot() +
  geom_point(data=dat_all_melt, aes(x=alb, y=value, color=variable))
  #geom_smooth(method = 'lm', se=FALSE )

#  facet_grid(variable~.)
# #ggsave('figures/LCT-albedo.png')
#
# #figure 6 with hex bins
# ggplot(data=dat_all_melt, aes(x=alb, y=value, color=variable)) +
#   geom_hex(bins=15) +
#   geom_smooth(method = 'lm', se = FALSE) +
#   facet_grid(variable~.)


#figure 7
#histogram of albedo values in the different ecoregions
ggplot(data=dat_all_melt, aes(alb)) +
  geom_histogram(color= "darkblue", fill="light blue") +
  facet_wrap(~NA_L1NAME)
ggsave('figures/fig7.png')


#figure 8
#scatterplot vs x with ecoregions
ggplot() +
  geom_point(data=dat_all_melt, aes(x=x, y=alb)) +
  facet_wrap(~NA_L1NAME)
#ggsave('figures/fig8.png')

#figure 9
#scatterplot vs y with ecoregions
ggplot() +
  geom_point(data=dat_all_melt, aes(x=y, y=alb)) +
  facet_wrap(~NA_L1NAME)
ggsave('figures/fig9.png')

#figure 10
#histogram of LCT for each ecoregion
ggplot(data=dat_all_melt, aes(value)) +
  geom_histogram(fill="light blue") +
  facet_grid(NA_L1NAME~variable)
# ggsave('figures/LCT-ecoregion-prop_summed.png')


#figure 11
#lct vs prop_summed divided by eco_region
ggplot() +
  geom_point(data=dat_all_melt, aes(x=alb, y=value, color=variable)) +
  facet_wrap(~NA_L1NAME)
ggsave('figures/fig11.png')


#figure 12
ggplot() +
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white') +
  geom_point(data=predict_bind, aes(x=x, y=y, color=predict_winter)) +
  scale_colour_gradient(low="black", high="light pink")+
  facet_wrap(~cut) +
  coord_equal()
ggsave('figures/fig5.png')

