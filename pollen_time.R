pollen_time = readRDS('pollen/pollen-sites-time-series.RDS')


#renaming to match LCT file 
names(pollen_time)[10] <- 'Artemisia'


#creating function to calculate proportions 
prop = function(x){
  x/sum(x)}

counts = pollen_time[,8:ncol(pollen_time)]
coords = pollen_time[,c(1:2,4)]


# dat_pollen = data.frame(coords, pollen_props)
dat_pollen = data.frame(coords, counts)

library(reshape2)
#melt will turn it into the long format 
dat_pollen_melt = melt(dat_pollen, id.vars=c('long', 'lat','age'))



LCT = read.csv('taxon2LCT_translation.csv', stringsAsFactors = FALSE)
LCT=LCT[-c(2:3,5:6)]

LCT = rbind(LCT, c('ET', 'Other.conifer'))
LCT = rbind(LCT, c('ST', 'Other.hardwood'))
#adding LCT 



#matches the taxon from the dat_pollen_melt file to the LCT file and forms a  new column 'LCT' with the classification 
#dat_pollen_melt$variable pulls the column variable from that dataframe
#variable is the column name with all the taxons
#LCT$taxon pulls the column taxon from the csv file 
#dat_pollen_melt$LCT before the equal sign makes a new column witht that name
dat_pollen_melt$LCT = LCT[match(dat_pollen_melt$variable, LCT$taxon), 'LCT']


#here NA values are removed. ie. Other category, will return to this 
dat_pollen_melt = dat_pollen_melt[-which(is.na(dat_pollen_melt$LCT)),]
any(is.na(dat_pollen_melt$LCT))


library(dplyr)
new <- group_by(dat_pollen_melt, long, lat, LCT)

#dividing into chunks 
#paleo <- tree.cores[tree.cores$age >= 150, ]
paleo_bins <- seq(0, 20000, by = 2000)
paleo_cut <- cut(new$age, include.lowest = TRUE, breaks = paleo_bins)
new$cut <- as.integer(paleo_cut)

#deleting age column and variable column which is taxon names
#not necessary for grouping 
new = new[-c(3,4)]


grouped_data<- group_by(new, long, lat, LCT, cut)
pol_summary = summarise(grouped_data, summed_counts = sum(value, na.rm=TRUE), .groups = 'keep')
foo = pol_summary %>%
  group_by(long, lat, cut) %>%
  mutate(pol_prop = summed_counts / sum(summed_counts))




#not sure what this does.....
#dat_pollen_melt <- dat_pollen_melt[!is.na(dat_pollen_melt$cut),] # remove ages > highest time bin
# assign unique ID to each distinct x, y coordinate
#paleo_xyid <- paleo %>% dplyr::select(x,y) %>% distinct()
#paleo_xyid$id <- as.character(seq(1, nrow(paleo_xyid), by = 1))

# #grouped_data<- group_by(new, long, lat, LCT, cut)
# pol_summary = summarise(new, summed_counts = sum(value), .groups = 'keep')
# 
# 
# grouped <-new %>%
#     group_by(long,lat,LCT,cut) %>%
#     summarise(summed_counts = sum(value)) 
# 
# #converting into a data frame 
# grouped = data.frame(grouped)
# 
# #need to convert into proportions...
# pollen_props = apply(grouped, 1, prop)
# 
# 
