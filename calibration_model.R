library(gam)
library(mgcv)
library(ggplot2)

dat_all = readRDS('data/dat_all-winter.RDS')
# dat_all = dat_all[which(dat_all$alb != 0),]
# dat_all = dat_all[which(!is.na(dat_all$alb)),]
dat_all$alb_winter = dat_all$alb_winter/1000

#df is the snow and fine albedo data
df = all_df
all_data = readRDS('data/dat_all_monthly8km.RDS')
ALL = data.frame(df, all_data)

ALL = subset(ALL, select = -c(x.1,y.1,x.2,y.2,x.3,y.3,ET.1,ST.1,OL.1,ET.2,ST.2,OL.2
                              ,optional,optional.1, x.1.1,y.1.1) )
saveRDS(ALL, 'data/snow_alb8km_df')


library(ggplot2)
library(tidyverse)
library(tidyr)
library(reshape2)
library(mapdata)
library(maps)
library(sp)

#starting to graph the data 
# 
world <- map_data('world')
world = world[which((world$region %in% c('Canada')) | (world$subregion %in% 'Alaska')),]

world_sp = SpatialPointsDataFrame(coords=world[,1:2], data=world, proj4string=CRS("+init=epsg:4326"))
world_proj = spTransform(world_sp, alb_proj)
coords_world = coordinates(world_proj)
colnames(coords_world) = c('x', 'y')
world_proj = data.frame(coords_world, world)

world = world[world$long<0,]


ggplot()+    
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
  geom_point(data=df, aes(x=x,y=y, colour=value))


#############################################################################
## MODEL 1
#############################################################################

mod = gam(alb_winter ~ te(x, y, k=10) + s(ST, k=5) + s(OL, k=5) + s(ET, k=5), 
          data=dat_all, 
          family=binomial(link="logit"),
          method="REML")

gam.check(mod)

plot(mod)

plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)

new_data = dat_all

preds = predict.gam(mod, 
                    type="response", 
                    data=new_data,
                    se.fit=TRUE)

dat_all$alb_pred = preds$fit

ggplot(data=dat_all) + 
  geom_point(aes(x=alb, y=alb_pred)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

#############################################################################
## MODEL 2
#############################################################################

mod = gam(alb ~ te(x, y, k=8) + s(ST, k=5) + s(OL, k=5) + s(ET, k=5), 
          data=dat_all, 
          family=binomial(link="logit"),
          method="REML")

gam.check(mod)

plot(mod)

plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)

new_data = dat_all

preds = predict.gam(mod, 
                    type="response", 
                    data=new_data,
                    se.fit=TRUE)

dat_all$alb_pred = preds$fit

ggplot(data=dat_all) + 
  geom_point(aes(x=alb, y=alb_pred)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))



#############################################################################
## MODEL 3
#############################################################################

dat = ALL[which(!is.na(ALL$alb_april)),]

mod = gam(alb_april/1000 ~ s(x, y, bs='tp') + s(ET) + s(ST) + s(OL) + s(snow_prob04),  
          data=ALL, 
          family=betar(link="logit"), 
          method="REML", na.action=na.omit)

gam.check(mod)

plot(mod)

plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)

new_data = dat


preds = predict.gam(mod, 
                    type="response", 
                    newdata=dat,
                    se.fit=TRUE)

dat$alb_april_pred = preds$fit
dat$predict = preds$fit

ggplot(data=dat) + 
  geom_point(aes(x=alb_april/1000, y=alb_april_pred)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

dat$diff = dat$alb_april_pred - dat$alb_april/1000
saveRDS(dat, 'data/april_modern_pred.RDS')

ggplot()+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
  geom_point(data=dat, aes(x=x, y=y, colour = diff))+
  scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.4,0.4))+
  theme_bw()
ggsave('figures/april_modern_diff.png')
# scale_fill_brewer(type = "div", palette = 'RdBu')+

#############################################################################
## MODEL 4
#############################################################################

mod = gam(alb ~ s(x, y, bs='tp', k=150) + s(ET, k=5) + s(ST, k=20) + s(OL, k=19), 
          data=dat_all, 
          family=betar(link="logit"), 
          method="REML")

gam.check(mod)

plot(mod)

plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)

new_data = dat_all

preds = predict.gam(mod, 
                    type="response", 
                    data=new_data,
                    se.fit=TRUE)

dat_all$summer_pred = preds$fit

predictions = data.frame(predictions, dat_all[-c(1:8)])

ggplot(data=dat_all) + 
  geom_point(aes(x=alb, y=alb_pred)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))


