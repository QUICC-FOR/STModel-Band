# clean
rm(list=ls())

# load pcks
library(tidyr)

# load lib
source('./lib/stmodel.r')

# output folder


# list clim files
clim_files <- data.frame(files=list.files("./data/futClimGrid/stmClim",full.names=TRUE))

# separate clim files
clim_files <- separate(clim_files,files,c('path','mod','yr_min','yr_max'),sep="[-]",remove=FALSE)
clim_files$yr_max <- gsub(".csv","",clim_files$yr_max)
# remove clim_files with yr_min = 2000
clim_files <- subset(clim_files,!yr_max == 2000)
clim_files <- clim_files[,-c(2,4)]

# list states files
stm_files <- data.frame(files=list.files("./data/futStatesGrid/stm",full.names=TRUE))

# separate stm files
stm_files <- separate(stm_files,files,c('path','mod','yr_min','yr_max'),sep="[-]",remove=FALSE)
stm_files$yr_max <- gsub(".csv","",stm_files$yr_max)
stm_files <- stm_files[,-c(2,4)]

# create out filname
mods <- unique(stm_files$mod)
years <- seq(2005,2095,5)
comb <- expand.grid(mods,years)
