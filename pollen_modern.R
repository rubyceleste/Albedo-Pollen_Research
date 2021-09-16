pollen_data=readRDS('pollen/pollen-modern-slice_v2.0.RDS')

library(dplyr)
#deleting other for now
#pollen_data =pollen_data[-c(22)]

#sums pollen counts for each site  
complete_dat <- (pollen_data %>%
                   group_by(long, lat, sitename) %>%
                   summarise(across(ARTEMISIA:TAXUS, sum), .groups='keep'))

#saveRDS(complete_dat, 'pollen/complete count.RDS')

#rename column to match with LCT .csv
#names(complete_dat)[6] <- 'Artemisia'

xy = complete_dat[,1:2]

#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = xy, data = complete_dat,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#transforming to the albedo crs
pol_transform = spTransform(spdf, alb_proj)

coords = coordinates(pol_transform)
counts = complete_dat[,4:ncol(complete_dat)]

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
LCT = read.csv('taxon2LCT_translation_v2.0.csv', stringsAsFactors = FALSE)
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




# new <- group_by(dat_pollen_melt, x, y, LCT)
# pol_summary = summarise(new, prop_summed = sum(value), .groups = 'keep')
# foo = group_by(pol_summary, x,y)  %>%
#   summarise(total = sum(prop_summed), .groups = 'keep')
#saveRDS(pol_summary, 'R scripts/pollen_modern_longversion.RDS')





# pollen = readRDS("R scripts/pollen_modern_longversion.RDS")
# pivot_pol = pollen %>%
#   pivot_wider(names_from = LCT, values_from = prop_summed)
# any(is.na(pivot_dat$alb))
# # #which !(not) deleting NAs
# pivot_dat = pivot_dat[which(!is.na(pivot_dat$alb)),]
