pollen_modern = readRDS('data/pollen_modern_pivot.RDS')
df_pm = SpatialPointsDataFrame(coords=pollen_modern[,c('x', 'y')], data=pollen_modern, proj4string = alb_proj)
for(m in 1:12) {
  psnow = raster(sprintf('data/snow/snow-month-%02d.tif', m))
  df_pm[[sprintf('psnow%02d', m)]] <- raster::extract(psnow, df_pm)
}

#After this, the df_pm dataframe will have new columns named psnow01, psnow02, ..., psnow12 for each month.

snow_prob = data.frame(df_pm)
snow_prob = snow_prob[,1:17]

saveRDS(snow_prob, 'data/modern_snowprob.RDS')

fine_alb = readRDS('data/alb_fine.RDS')

fine_alb = fine_alb[,6:17]

all_data = cbind(snow_prob, fine_alb)

saveRDS(all_data, 'data,albfine_snow.RDS')

