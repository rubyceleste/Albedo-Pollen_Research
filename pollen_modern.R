pollen_data=readRDS('pollen/pollen-modern-slice.RDS')

library(dplyr)

#sums pollen counts for each taxon 
complete_dat <- (pollen_data %>%
                   group_by(long, lat, sitename) %>%
                   summarise(across(Alder:Willow, sum), .groups='keep'))

saveRDS(complete_dat, 'pollen/complete count.RDS')

#take the coordinates from the pollen data 
#coords = pollen_data[,1:2]
#rename column to match with LCT .csv
names(complete_dat)[6] <- 'Artemisia'


prop = function(x){
  x/sum(x)}

counts = complete_dat[,4:ncol(complete_dat)]
coords_p = complete_dat[,1:2]

pollen_props = t(apply(counts, 1, prop))

dat_pollen = data.frame(coords_p, pollen_props)

library(reshape2)
#melt will turn it into the long format 
dat_pollen_melt = melt(dat_pollen, id.vars=c('long', 'lat'))



LCT = read.csv('taxon2LCT_translation.csv', stringsAsFactors = FALSE)
LCT=LCT[-c(2:3,5:6)]

LCT = rbind(LCT, c('ET', 'Other.conifer'))
LCT = rbind(LCT, c('ST', 'Other.hardwood'))

pol_taxa = unique(dat_pollen_melt$variable)
pol_taxa[which(!(pol_taxa %in% unique(LCT$taxon)))]


#matches the taxon from the dat_pollen_melt file to the LCT file and forms a  new column 'LCT' with the classification 
#dat_pollen_melt$variable pulls the column variable from that dataframe
#variable is the column name with all the taxons
#LCT$taxon pulls the column taxon from the csv file 
#dat_pollen_melt$LCT before the equal sign makes a new column witht that name
dat_pollen_melt$LCT = LCT[match(dat_pollen_melt$variable, LCT$taxon), 'LCT']

dat_pollen_melt = dat_pollen_melt[-which(is.na(dat_pollen_melt$LCT)),]
any(is.na(dat_pollen_melt$LCT))


library(dplyr)
new <- group_by(dat_pollen_melt, long, lat, LCT)
pol_summary = summarise(new, prop_summed = sum(value), .groups = 'keep')

saveRDS(pol_summary, 'R scripts/lct_longversion.RDS')


#showing the proportion of each taxon around Canada
ggplot() +
  geom_point(data=dat_pollen_melt, aes(x=long, y=lat, color=value)) +
  facet_wrap(~variable)+
  scale_colour_gradient(low="black", high="pink")


