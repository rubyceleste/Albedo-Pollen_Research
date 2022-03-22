# Albedo-Pollen_Research

Step 1
Run pollen_modern.R code first, making sure pollen_modern_pivot.RDS is saved
  RDS files come from data folder
  
Step 2
Run pollen_time.R, making sure pollen_time-full.RDS is saved 

Step 3
Run monthly_ABOVE.R for fine albedo, which saves datafram with pollen LCT in a df 
bluesky_process.R is only necessary if resampling albedo data

 
Step 4 
snow_probability.R is the code for snow prob from MODIS data, saves df with x,y,LCT and snow prob from jan-dec. modern data
merges fine albedo and snow probabilty in one dataframe 
  
 
Step 5  
elevation.R adds elevation data for coordinates where we have pollen data, final df all-data.RDS includes x,y,LCT,elevation,finealb,snowprob


albedo_figures.R
  figures showing how each varible are related to each other


figures_main.R uses pollen-modern-proportions.RDS which is proportions of all taxon. comes from pollen_modern????
albedo_figure.R 
  both figure files need to be cleaned and more detailed descriptions of figures

step...
thornthwaite model script 



step ...
using all-data can be plugged into model


#all scripts here good to be pushed except github is being dumbbb
