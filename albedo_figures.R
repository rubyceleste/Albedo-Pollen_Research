library(ggplot2)
library(tidyverse)
#starting to graph the data 



#dat_all = readRDS('R scripts/dat_all-spring.RDS')
#dat_all = readRDS('R scripts/dat_all-summer.RDS')
#dat_all = readRDS('R scripts/dat_all-fall.RDS')

#figure 1 
#map of albedo
alb_map <- ggplot() + 
  geom_point(data= df, aes(x=x, y=y, color=value))+
  scale_colour_gradient(low="black", high="light blue")
#scatterplot vs y 
alb_scat_y <- ggplot() + 
  geom_point(data=df, aes(x=y, y=value, color=value))+
  scale_colour_gradient(low="black", high="light blue")
#scatterplot vs x
alb_scat_x <- ggplot() + 
  geom_point(data=df, aes(x=x, y=value, color=value))+
  scale_colour_gradient(low="black", high="light blue")
  
# ggsave('figures/2001_182_scatter_vsX.png')
#histogram
alb_hist<-ggplot(data=df, aes(df$value)) +
  geom_histogram(color= "darkblue", fill="light blue")

library(grid)
library(gridExtra)

#puts 4 figures together 
plots <- list(alb_map, alb_hist, alb_scat_x, alb_scat_y)
layout <- rbind(c(1,2),c(3,4))
grid.arrange(grobs=plots, layout_matrix = layout, top= "Albedo Figures", vp=viewport(width = 0.8, height=1))


#value here is the albedo
eco_dat = readRDS('R scripts/albedo_eco.RDS')


#plots all the ecoregions
ggplot() +
  geom_point(data=eco_dat, aes(x=x, y=y, color=NA_L1NAME, fill = "white"))

#figure 7 
#histogram of albedo values in the different ecoregions 
ggplot(data=dat_all, aes(lct_alb)) +
  geom_histogram(color= "darkblue", fill="light blue") +
  facet_wrap(~NA_L1NAME)
# ggsave('figures/alb-ecoregion.png')


#figure 8 
#scatterplot vs x with ecoregions 
ggplot() + 
  geom_point(data=dat_all, aes(x=x, y=lct_alb)) +
  facet_wrap(~NA_L1NAME)
#ggsave('figures/alb-lct-ecoregion-x.png')

#figure 9 
#scatterplot vs y with ecoregions 
ggplot() + 
  geom_point(data=dat_all, aes(x=y, y=lct_alb)) +
  facet_wrap(~NA_L1NAME)
#ggsave('figures/alb-lct-ecoregion-y.png')

#figure 10 
#histogram of LCT for each ecoregion 
# ggplot(data=dat_all, aes(prop_summed)) + 
#   geom_histogram(fill="light blue") + 
#   facet_grid(NA_L1NAME~LCT)
# ggsave('figures/LCT-ecoregion-prop_summed.png')


#figure 11
#lct vs prop_summed divided by eco_region
ggplot() +
  geom_point(data=dat_all, aes(x=lct_alb, y=prop_summed, color=LCT)) +
  facet_wrap(~NA_L1NAME)
#ggsave('figures/LCT-albedo+ecoregion.png')


#dat_all = readRDS('R scripts/dat_all.RDS')
#dat_all_simple = dat_all[-c(6,8,12,13)]


#figure 3
#LCT vs long and lat 
# ggplot() +
#   geom_point(data=dat_all, aes(x=x, y=prop_summed, color=LCT))
#ggsave('figures/lct-longitude.png')
# ggplot() +
#   geom_point(data=dat_all, aes(x=y, y=prop_summed, color=LCT))
# ggsave('figures/lct-latitude.png')


#figure 2 
# ggplot(data=dat_all, aes(prop_summed, color=LCT)) +
#   geom_histogram(fill='white') + 
#   facet_grid(LCT~.)
#ggsave('figures/lct-histo.png')


#figure 4
# ggplot(data=dat_all, aes(x=y, y=prop_summed, color=LCT)) +
#   geom_point() + 
#   geom_smooth(method = 'lm', se = FALSE) 
#  facet_grid(LCT~.)


#lct with albedo histo
#ggplot(data=dat_all, aes(lct_alb, color=LCT)) +
#  geom_histogram(fill='white')

#figure 6
ggplot(data=dat_all, aes(x=lct_alb, y=prop_summed, color=LCT)) +
  geom_point()  +
  geom_smooth(method = "gam", formula = prop_summed ~s(lct_alb))
#  facet_grid(LCT~.)
#ggsave('figures/LCT-albedo.png')

#figure 6 with hex bins 
ggplot(data=dat_all, aes(x=lct_alb, y=prop_summed, color=LCT)) +
  geom_hex(bins=10) + 
  geom_smooth(method = 'lm', se = FALSE) +
  facet_grid(LCT~.)


library(tidyr)
library(reshape2)
library(mapdata)
library(maps)
library(sp)
#dat_long <- gather(dat_pollen, type, prop, Alder:ncol(dat_pollen) , factor_key=TRUE)


world <- map_data('world')
world = world[which((world$region %in% c('Canada')) | (world$subregion %in% 'Alaska')),]


world_sp = SpatialPointsDataFrame(coords=world[,1:2], data=world, proj4string=CRS("+init=epsg:4326"))
world_proj = spTransform(world_sp, alb_proj)
coords_world = coordinates(world_proj)
colnames(coords_world) = c('x', 'y')
world_proj = data.frame(coords_world, world)



library(dplyr)
summary_LCT <- (dat_all %>%
                  group_by(LCT) %>%
                  summarise(mean_prop = mean(prop_summed), sd_prop = sd(prop_summed), mean_alb = mean(lct_alb, na.rm = TRUE), sd_alb = sd(lct_alb, na.rm = TRUE)))

#data summarized by eco region and land cover type
# summary_eco <- ( dat_all %>%
#                    group_by(LCT, NA_L1NAME) %>%
#                    summarise(mean_prop = mean(prop_summed), sd_prop = sd(prop_summed), mean_alb = mean(lct_alb, na.rm = TRUE), sd_alb = sd(lct_alb, na.rm = TRUE),.groups = 'keep' ))
# 
# summary_all = data.frame(LCT=summary_LCT$LCT,
#                          NA_L1NAME=rep('All', nrow(summary_LCT)),
#                          summary_LCT[,2:5])
# 
# table_all = rbind(summary_eco,summary_all)
# 
# write.csv(table_all, 'summarized-data-table.csv', row.names = FALSE)



#figure 5 
p<- ggplot() +
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white') +
  geom_point(data=dat_all, aes(x=x, y=y, color=prop_summed)) +
  scale_colour_gradient(low="black", high="light pink")+
  facet_grid(LCT~.) +
  coord_equal()
#prints map
p

#figure 6??? with hex 
library(hexbin)
ggplot(dat_all, aes(y, prop_summed)) +
  geom_hex(bins=10)+
  facet_grid(LCT~.)



model <- lm(prop_summed~ lct_alb +LCT, data = dat_all)
summary(model)

dat_OL = dat_all[which(dat_all$LCT == 'OL'),]
regr= lm(prop_summed ~ lct_alb, data=dat_OL)
summary(regr)

#estimate is the slope, std error, t value, and p value
# linearre = lm(prop_summed ~ lct_alb, data= dat_all)
# summary(linearre)

# attributes(linearre)
# linearre$coefficients
#confidence interval, level tells you the interval 
# confint(linearre, level = 0.99)
# anova(linearre)

# models = dat_all %>% group_by(LCT) %>% do(model = lm(prop_summed ~ lct_alb, data = .))
# models$model



#summary(models)
#models$model[[]]
#names(models$model[[1]])
# rowwise(models) %>% tidy(models)


# modek_summary= (dat_all %>%
#           group_by(LCT) %>%
#           lm(prop_summed~lct_alb, data=.))
# 
# summary(model)
# 
# 
# fitted_model = dat_all %>% group_by(LCT) %>% do(model = lm(prop_summed ~ lct_alb, data = .))
# 
# fitted_model %>% broom::glance(model)


