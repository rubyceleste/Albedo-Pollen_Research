pollen_modern = readRDS('data/pollen_modern_pivot.RDS')
df_alb = SpatialPointsDataFrame(coords=pollen_modern[,c('x', 'y')], data=pollen_modern, proj4string = alb_proj)
for(m in 1:12) {
  palb = raster(sprintf('data/ABOVE/new-month-%02d.tif', m))
  df_alb[[sprintf('fine-alb%02d', m)]] <- raster::extract(palb, df_alb)
}

all_df = data.frame(df_alb)

#removing extra coordinates not necessary 
all_df = all_df[-c(18:20)]

saveRDS(all_df, 'data, alb_fine.RDS')

