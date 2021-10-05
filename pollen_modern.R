library(raster)
library(maps)
library(reshape2)
library(dplyr)
library(tidyr)

pollen_data=readRDS('data/pollen-modern-slice_v2.0.RDS')


alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

#sums pollen counts for each site  
complete_dat <- (pollen_data %>%
                   group_by(long, lat, sitename) %>%
                   summarise(across(ARTEMISIA:TAXUS, sum), .groups='keep'))


complete_dat = data.frame(complete_dat)

xy = complete_dat[,1:2]

#don't know what to rename k lol
#map.where() indicates what part of the world those coordinates are located
k=map.where(database = "world", xy[,1],xy[,2])
k=data.frame(k,xy,age=complete_dat[,4],complete_dat[,8:ncol(complete_dat)])
#k[,1] = sapply(k[,1], function(x) if (is.na(x)){x=1} else {x=0})

k = k[which(!is.na(k$k)),]
#if name has Canada and USA:Al then keep, else give name NA
get_c <- function(x) {if (substr(x, 1,6) =="Canada") {"Canada"} else if (substr(x, 1,6) =="USA:Al") {"Alaska"} else {NA}}

k$country=sapply(k$k, get_c)
#substr(k$k, 1,6) == "Canada"

#delete NAs (which are countries not Canada and Alaska)
k = k[which(!is.na(k$country)),]

#seperating coordinates in different dataframe
xy_new = k[-c(1,4:ncol(k))]

#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = xy_new, data = k,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#transforming to the albedo crs
pol_transform = spTransform(spdf, alb_proj)

coords = coordinates(pol_transform)
counts = k[,5:ncol(k)]

colnames(coords)[1] <- 'x'
colnames(coords)[2] <- 'y'


#defining proportion function
prop = function(x){
  x/sum(x)}

#calculating proportions for each site
# pollen_props = t(apply(counts, 1, prop))
# rowSums(pollen_props)
# #merging pollen props and new reprojected coords
# dat_pollen = data.frame(coords, pollen_props)


dat_pollen = data.frame(coords, counts)

library(reshape2)
#melt will turn it into the long format 
dat_pollen_melt = melt(dat_pollen, id.vars=c('x', 'y'))


#reading in LCT
LCT = read.csv('data/taxon2LCT_translation_v2.0.csv', stringsAsFactors = FALSE)
LCT = LCT[,c('LCT', 'taxon')]


saveRDS(LCT, "R scripts/LCT_table.RDS")

#matches the taxon from the dat_pollen_melt file to the LCT file and forms a  new column 'LCT' with the classification 
#dat_pollen_melt$variable pulls the column variable from that dataframe
#variable is the column name with all the taxons
#LCT$taxon pulls the column taxon from the csv file 
#dat_pollen_melt$LCT before the equal sign makes a new column witht that name
dat_pollen_melt$LCT = LCT[match(dat_pollen_melt$variable, LCT$taxon), 'LCT']

#removing Na LCT
any(is.na(dat_pollen_melt$LCT))
dat_pollen_melt= dat_pollen_melt[!is.na(dat_pollen_melt$LCT),]

#don't need variable colunmn anymore
dat_pollen_melt= dat_pollen_melt[-c(3)]

dat_pollen_melt$value=as.numeric(dat_pollen_melt$value)


foo = dat_pollen_melt %>%
  group_by(x,y,LCT) %>%
  summarise(count = sum(value))

# propor=t(apply(foo, 1, prop))
# b=foo %>%
#   group_by(x,y, LCT)%>%
#   #summarise(total = n())%>%
#   mutate(prop_sum = count/sum(count))
#   

pivot_mod = foo %>%
  pivot_wider(names_from = LCT, values_from = count)

propor=t(apply(pivot_mod[,3:5], 1, prop))

pivot_mod = data.frame(pivot_mod[,1:2], propor)
saveRDS(pivot_mod, 'R scripts/pollen_modern_pivot.RDS')



