
dat_time = readRDS('data/pivot_table_full.RDS')
dat_winter = readRDS('data/dat_all-winter.RDS')


# class(dat_all$NA_L2NAME)
# #changing the column to a factor from a character 
# dat_all$NA_L2NAME = factor(dat_all$NA_L2NAME)



#gam_winter = gam(alb ~ s(OL) + s(ST) + s(y), data= dat_winter, method = "REML")
# 
# predict_winter = predict(gam_winter, newdata = dat_winter)
# head(predict_winter)
# 
# winter_bind = data.frame(predict_winter, dat_winter)


predict_time= predict.gam(mod, newdata = dat_time, type='response')
predict_bind = data.frame(predict_time, dat_time)  

#this shows that there are still weird coordinates not in Canada
ggplot()+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
  geom_point(data=predict_bind, aes(x=x,y=y))


#not sure how to delte weird values 
# any(is.na(winter_bind$alb<1))
# any(is.na(winter_bind$alb==0))

#this changes back to regular lat long
#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = predict_bind[,c('x','y')], data = predict_bind,
                               proj4string = CRS('+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

#transforming to the albedo crs: epsg 102001
pol_transform = spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

jjj=data.frame(coordinates(pol_transform), predict_bind)

#dividing into chunks
#paleo <- tree.cores[tree.cores$age >= 150, ]
lat_bins <- seq(40, 80, by = 10)
#using the age column to break up the data by the paleo_bins vector
lat_cut <- cut(jjj$y, include.lowest = TRUE, breaks = lat_bins)
#adds the column cut, 1 cut represents data for 2000 years
jjj$lat_cut <- as.integer(lat_cut)


foo = jjj%>%
  group_by(cut,lat_cut) %>%
  summarise(average = mean(predict_time, na.rm=TRUE), .groups = 'keep')

ggplot()+
  geom_line(data=jjj, aes(x=cut, y=predict_time, group=interaction(x,y)))+
  facet_wrap(~lat_cut)

ggplot()+
  geom_line(data=foo, aes(x=cut, y=average, color=factor(lat_cut), group=lat_cut))



ggplot()+
  geom_point(data=jjj, aes(x=x.1, y=y.1, color=predict_time ))+
  scale_colour_gradient(low="black", high="light pink")+
  facet_wrap(~cut)
