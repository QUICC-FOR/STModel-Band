# clean wd
rm(list=ls())

#load lib
library(raster)

# load ref raster
load("./data/0000_land_rs.robj")

# Band extent
ext_study_area <- c(-74.6,-74,45.6,50.3)

# crop band
rs <- crop(rs,ext_study_area)

# create empty raster with res of 100 meters
ext_lcc <- extent(projectExtent(rs,"+proj=lcc +lat_0=45.6 +lon_0=-74.3 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs;"))
ref_rs <- raster(xmn=ext_lcc[1],xmx=ext_lcc[2],ymn=ext_lcc[3],ymx=ext_lcc[4],res=100,crs="+proj=lcc +lat_0=45.6 +lon_0=-74.3 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs;")
lcc_proj <- proj4string(ref_rs)

# create ouptput dir
system("mkdir -p ./data/futGridProj/raster")
system("mkdir -p ./data/futGridProj/stm_grid")

list_rs <- list.files("./data/futClimSTM/",recursive=TRUE,full.names=TRUE)

# read csv
fut_clim <- read.csv(list_rs[1],stringsAsFactors=FALSE)

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
saveRDS(bioclim_10_lcc,file=paste0("./data/futGridProj/raster/rcp85-",clim_mod,"-",year_min,"-",year_max,".rda"))

###### CREATE CLIM STM GRID (UNPROJ) #####

stm_clim_grid <- as.data.frame(bioclim_10_lcc,xy=TRUE)
stm_clim_grid$x <- as.numeric(as.factor(stm_clim_grid$x))-1
stm_clim_grid$y <- as.numeric(as.factor(stm_clim_grid$y))-1

### Check if structure ok
