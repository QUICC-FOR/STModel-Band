# clean
rm(list=ls())

# load libs
library(doParallel)
library(raster)

# open cluster
cl <- makeCluster(35)
registerDoParallel(cl)

# load raster ref
grid_ref <- readRDS("./data/futClimGrid/rasterClim/res1000/rcp85-MIROC_ESM_CHEM-1985-2000.rda")

######### WINDOW 1985-2000

# list files
probs2000_files <- list.files("./data/futStatesGrid/probs",full.names=TRUE,pattern="1985-2000")
# prep stack res
TempGrids2000 <- stack()

foreach(i in 1:length(probs2000_files),.packages=c('raster','rgdal'))%dopar%{

  # read file and compute temperate prob
  probsGrid <- read.csv(probs2000_files[i])
  probsGrid$ProbTemp <- probsGrid$T + probsGrid$M

  # set coordinates from grid_ref
  probsGrid <- cbind(probsGrid,coordinates(grid_ref))

  # Turn df into raster
  df_probsTemp <- probsGrid[,c(8,7,6)]
  coordinates(df_probsTemp) <- ~ x + y
  gridded(df_probsTemp) <- TRUE
  rs_probsTemp <- raster(df_probsTemp)

  TempGrids2000 <- addLayer(TempGrids2000,rs_probsTemp)

}

metadata <- unlist(lapply(strsplit(probs2000_files,"[./-]"),function(x) x[7]))
names(TempGrids2000) <- metadata

saveRDS(TempGrids2000,"./res/2000_tempProbSolved.rda")

######### WINDOW 2045-2030

# list files
probs2045_files <- list.files("./data/futStatesGrid/probs",full.names=TRUE,pattern="2030-2045")
# prep stack res
TempGrids2045 <- stack()

foreach(i=1:length(probs2045_files),.packages=c('raster','rgdal'))%dopar%{

  # read file and compute temperate prob
  probsGrid <- read.csv(probs2045_files[i])
  probsGrid$ProbTemp <- probsGrid$T + probsGrid$M

  # set coordinates from grid_ref
  probsGrid <- cbind(probsGrid,coordinates(grid_ref))

  # Turn df into raster
  df_probsTemp <- probsGrid[,c(8,7,6)]
  coordinates(df_probsTemp) <- ~ x + y
  gridded(df_probsTemp) <- TRUE
  rs_probsTemp <- raster(df_probsTemp)

  TempGrids2045 <- addLayer(TempGrids2045,rs_probsTemp)

}

metadata <- unlist(lapply(strsplit(probs2045_files,"[./-]"),function(x) x[7]))
names(TempGrids2045) <- metadata

saveRDS(TempGrids2045,"./res/2045_tempProbSolved.rda")


######### WINDOW 2080-2095

# list files
probs2095_files <- list.files("./data/futStatesGrid/probs",full.names=TRUE,pattern="2080-2095")
# prep stack res
TempGrids2095 <- stack()

foreach(i=1:length(probs2095_files),.packages=c('raster','rgdal'))%dopar%{

  # read file and compute temperate prob
  probsGrid <- read.csv(probs2095_files[i])
  probsGrid$ProbTemp <- probsGrid$T + probsGrid$M

  # set coordinates from grid_ref
  probsGrid <- cbind(probsGrid,coordinates(grid_ref))

  # Turn df into raster
  df_probsTemp <- probsGrid[,c(8,7,6)]
  coordinates(df_probsTemp) <- ~ x + y
  gridded(df_probsTemp) <- TRUE
  rs_probsTemp <- raster(df_probsTemp)

  TempGrids2095 <- addLayer(TempGrids2095,rs_probsTemp)

}

metadata <- unlist(lapply(strsplit(probs2095_files,"[./-]"),function(x) x[7]))
names(TempGrids2095) <- metadata

saveRDS(TempGrids2095,"./res/2095_tempProbSolved.rda")
