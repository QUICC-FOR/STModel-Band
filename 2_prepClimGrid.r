# clean wd
rm(list=ls())

#load lib
library(raster)
library(doParallel)

# load ref raster
load("./data/0000_land_rs.Robj")

# Band extent
ext_study_area <- c(-74.6,-74,45.6,50.3)

# crop band
rs <- crop(rs,ext_study_area)

# create empty raster with res of 100 meters
ext_lcc <- extent(projectExtent(rs,"+proj=lcc +lat_0=45.6 +lon_0=-74.3 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs;"))
ref_rs <- raster(xmn=ext_lcc[1],xmx=ext_lcc[2]+400,ymn=ext_lcc[3],ymx=ext_lcc[4],res=100,crs="+proj=lcc +lat_0=45.6 +lon_0=-74.3 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs;")
lcc_proj <- proj4string(ref_rs)

# create ouptput dir
system("mkdir -p ./data/futClimGrid/rasterClim/res100")
system("mkdir -p ./data/futClimGrid/rasterClim/res1000")
system("mkdir -p ./data/futClimGrid/stmClim")

list_rs <- list.files("./data/futClimRaw/",recursive=TRUE,full.names=TRUE)

cl <- makeCluster(35)
registerDoParallel(cl)

foreach(i=1:length(list_rs),.packages=c('raster','rgdal'))%dopar%{

  # read csv
  fut_clim <- read.csv(list_rs[i],stringsAsFactors=FALSE)

  # set NA
  fut_clim[which(fut_clim[,"val"]==-9999),"val"] <- NA

  # get metadata
  clim_mod  <- unique(fut_clim$mod)
  year_max <- unique(fut_clim$max_yr)
  year_min  <- unique(fut_clim$min_yr)

  ###### CREATE CLIM PROJ #####

  # subset bio1
  bio1_clim <- subset(fut_clim, var == "bio1")[,c("lon","lat","val")]
  coordinates(bio1_clim) <- ~ lon + lat
  gridded(bio1_clim) <- TRUE
  bio1_clim <- raster(bio1_clim)

  # subset bio12
  bio12_clim <- subset(fut_clim, var == "bio12")[,c("lon","lat","val")]
  coordinates(bio12_clim) <- ~ lon + lat
  gridded(bio12_clim) <- TRUE
  bio12_clim <- raster(bio12_clim)

  # stack raster
  bioclim <- stack(bio1_clim,bio12_clim)
  names(bioclim) <- c('tp','pp')
  projection(bioclim) <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs;"
  bioclim_lcc <- projectRaster(bioclim,crs=lcc_proj)

  # transform to a res of 1000 meters
  bioclim_lcc <- resample(bioclim_lcc,ref_rs)
  bioclim_10_lcc <- aggregate(bioclim_lcc,10)

  ###### SAVE CLIM PROJ #####
  # save proj climate with 1000 meters of res
  saveRDS(bioclim_lcc,file=paste0("./data/futClimGrid/rasterClim/res100/rcp85-",clim_mod,"-",year_min,"-",year_max,".rda"))
  saveRDS(bioclim_10_lcc,file=paste0("./data/futClimGrid/rasterClim/res1000/rcp85-",clim_mod,"-",year_min,"-",year_max,".rda"))

  ###### CREATE CLIM STM GRID (UNPROJ) #####

  stm_clim_grid <- as.data.frame(bioclim_10_lcc,xy=TRUE)
  stm_clim_grid$x <- as.numeric(as.factor(stm_clim_grid$x))-1
  stm_clim_grid$y <- as.numeric(as.factor(stm_clim_grid$y))-1

  #### RESCALE
  load("./data/scale_info.Robj")
  stm_clim_grid$tp <- (stm_clim_grid$tp-vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']
  stm_clim_grid$pp <- (stm_clim_grid$pp-vars.means['tot_annual_pp'])/vars.sd['tot_annual_pp']
  stm_clim_grid$year <- 0

  # Manage NA
  stm_clim_grid[which(is.na(stm_clim_grid$tp)),"tp"] <- -9999
  stm_clim_grid[which(is.na(stm_clim_grid$pp)),"pp"] <- -9999

  # reformat names and columns order
  stm_clim_grid <- stm_clim_grid[,c('x','y','year','tp','pp')]
  names(stm_clim_grid)[4:5] <- c('env1','env2')

  ###### SAVE STM CLIM GRID UNPROJ #####
  write.csv(stm_clim_grid,file=paste0("./data/futClimGrid/stmClim/rcp85-",clim_mod,"-",year_min,"-",year_max,".csv"),row.names=FALSE,quote=FALSE)

}
