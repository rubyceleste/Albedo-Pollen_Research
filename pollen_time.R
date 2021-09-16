library(raster)
library(dplyr)
library(maps)
pollen_time = readRDS('pollen/pollen-sites-times-series-all_v2.0.RDS')

pollen_time = data.frame(pollen_time)

#deleting other column for now 
#pollen_time =pollen_time[-c(22)]


#renaming to match LCT file 
#names(pollen_time)[10] <- 'Artemisia'


xy= pollen_time[,1:2]

k=map.where(database = "world", xy[,1],xy[,2])
k=data.frame(k,xy)
#k[,1] = sapply(k[,1], function(x) if (is.na(x)){x=1} else {x=0})

k = k[which(!is.na(k$k)),]
get_c <- function(x) {if (substr(x, 1,6) =="Canada") {"Canada"} else if (substr(x, 1,6) =="USA:Al") {"Alaska"} else {NA}}

k$country=sapply(k$k, get_c)
#substr(k$k, 1,6) == "Canada"

k = k[which(!is.na(k$country)),]


#matches the taxon from the dat_pollen_melt file to the LCT file and forms a  new column 'LCT' with the classification 
#dat_pollen_melt$variable pulls the column variable from that dataframe
#variable is the column name with all the taxons
#LCT$taxon pulls the column taxon from the csv file 
#dat_pollen_melt$LCT before the equal sign makes a new column witht that name
pollen_time_melt = melt(pollen_time, id.vars=c('x', 'y', 'age'))

dat_pollen_melt$LCT = LCT[match(dat_pollen_melt$variable, LCT$taxon), 'LCT']

#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = xy, data = pollen_time,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#transforming to the albedo crs: epsg 102001
pol_transform = spTransform(spdf, alb_proj)

#separating counts and coordinates columns
counts = pollen_time[,8:ncol(pollen_time)]
coords = coordinates(pol_transform)

#renaming long and lat to x,y to be consistent with all other naming
colnames(coords)[1] <- 'x'
colnames(coords)[2] <- 'y'

# dat_pollen = data.frame(coords, pollen_props), merging
dat_pollen = data.frame(coords, counts)
#adding age column 
df_bind_age = data.frame(age=pollen_time[,4], dat_pollen)


library(reshape2)
#melt will turn it into the long format, id.vars keeps those columns 
dat_pollen_melt = melt(df_bind_age, id.vars=c('x', 'y','age'))


#read in LCT table
LCT = readRDS("R scripts/LCT_table.RDS")

#matches the taxon from the dat_pollen_melt file to the LCT file and forms a  new column 'LCT' with the classification 
#dat_pollen_melt$variable pulls the column variable from that dataframe
#variable is the column name with all the taxons
#LCT$taxon pulls the column taxon from the csv file 
#dat_pollen_melt$LCT before the equal sign makes a new column witht that name
dat_pollen_melt$LCT = LCT[match(dat_pollen_melt$variable, LCT$taxon), 'LCT']

#removing NA LCT whihc are taxons we don't want
any(is.na(dat_pollen_melt$LCT))
dat_pollen_melt= dat_pollen_melt[!is.na(dat_pollen_melt$LCT),]


#eco region reprojection
eco_reproj = readRDS("R scripts/eco_reproj.RDS")

# for each x,y  in our pollen data, get the ecoregion
lct_eco = over(pol_transform, eco_reproj)
#keeping only the eco region columns and removing everything else
lct_eco = lct_eco[,c('NA_L1NAME', 'NA_L2NAME')]

#merging data
df_weco = data.frame(dat_pollen_melt, lct_eco)

#grouping data
new <- group_by(df_weco, x, y, LCT)

#dividing into chunks 
#paleo <- tree.cores[tree.cores$age >= 150, ]
paleo_bins <- seq(0, 20000, by = 2000)
#using the age column to break up the data by the paleo_bins vector
paleo_cut <- cut(new$age, include.lowest = TRUE, breaks = paleo_bins)
#adds the column cut, 1 cut represents data for 2000 years
new$cut <- as.integer(paleo_cut)

#deleting age column and variable column which is taxon names
#not necessary for grouping 
new = new[-c(3,4)]


grouped_data<- group_by(new, x, y, LCT, cut, NA_L2NAME)
#summarizing data by summing the pollen counts 
pol_summary = summarise(grouped_data, summed_counts = sum(value, na.rm=TRUE), .groups = 'keep')
foo = pol_summary %>%
  group_by(x, y, cut, NA_L2NAME) %>%
  mutate(pol_prop = summed_counts / sum(summed_counts))

#deleting summed counts column no long necessary
foo = foo[-c(6)]

#pivot data so each LCT has its own column
pivot_foo = foo %>%
  pivot_wider(names_from = LCT, values_from = pol_prop)

#deleting cut with NA values because it has a negative NA which will be included in the modern pollen dataset
pivot_foo = pivot_foo[-which(is.na(pivot_foo$cut)),]

#deleting NA ecoregions, still to be determined why 
#there are still Na ecoregions maybe in america?
#come back to this
pivot_foo = pivot_foo[-which(is.na(pivot_foo$NA_L2NAME)),]

#saved pivot_foo with ecoregions - full dataset 
saveRDS(pivot_foo, 'R scripts/pivot_table_full.RDS')

pivot_foo=readRDS('R scripts/pivot_table_full.RDS')
