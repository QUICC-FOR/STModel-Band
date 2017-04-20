#clean
rm(list=ls())

# load lib
library(doParallel)
library(raster)

# load params
pars <- read.table("./pars/GenSA_rf_all_2_5y_rep1.txt",row.names=1)

# list landscape
clim_files <- list.files("data/futClimGrid/stmClim",full.names=TRUE)

# source res analytics function
source('./lib/resAnalytics.r')
source('./lib/convertStates.r')

# create output dir
system('mkdir -p ./data/futStatesGrid/probs')
system('mkdir -p ./data/futStatesGrid/stm')

# open cluster
cl <- makeCluster(35)
registerDoParallel(cl)

###### PROBS

# foreach(i=1:length(clim_files),.packages=c('raster','rgdal'))%dopar%{
#
#   ### READ
#   climGrid <- read.csv(clim_files[i],header=TRUE,stringsAsFactors=FALSE)
#   names(climGrid)[4:5] <- c("tp","pp")
#
#   # set NA in order to solve
#   climGrid$tp[which(climGrid$tp == -9999)] <- NA
#   climGrid$pp[which(climGrid$pp == -9999)] <- NA
#
#   #### SOLVE
#   probsGrid <- solve_stm(climGrid_scale,pars)
#   probsGrid  <- data.frame(x=climGrid$x,y=climGrid$y,probsGrid,stringsAsFactors=FALSE)
#
#   # get metadata
#   filename <- strsplit(clim_files[i],"[/.]")[[1]][4]
#
#   # write
#   write.csv(probsGrid,file=paste0("./data/futStatesGrid/probs/",filename,".csv"),row.names=FALSE)
#
# }

###### STATES

probs_files <- list.files("./data/futStatesGrid/probs",full.names=TRUE,pattern="1985-2000")

#load raster ref
ref_rs100 <- readRDS("data/futClimGrid/rasterClim/res100/rcp85-ACCESS1_0-1985-2000.rda")
ref_rs1000 <- readRDS("data/futClimGrid/rasterClim/res1000/rcp85-ACCESS1_0-1985-2000.rda")

foreach(i=1:length(probs_files))%dopar%{

  probsGrid <- read.csv(probs_files[i])

  states <- character(0)

  for(r in 1:nrow(probsGrid)){
      if(any(is.na(probsGrid[r,3:5]))){
        states <- append(states,NA)
      } else {
        states <- append(states,names(which.max(probsGrid[r,3:5])))
      }
  }

  stmGrid <- data.frame(coordinates(ref_rs1000),state=stateToId(states),stringsAsFactors=FALSE)

  # turn into raster using clim rs
  rs_stmGrid <- stmGrid
  coordinates(rs_stmGrid) <- ~ x + y
  gridded(rs_stmGrid) <- TRUE
  rs_stmGrid <-  raster(rs_stmGrid)
  projection(rs_stmGrid) <- projection(ref_rs1000)

  # reample using ref_rs100
  rs_stmGrid_1000 <- resample(rs_stmGrid,ref_rs100,method="ngb")
  df_stmGrid_1000 <- as.data.frame(rs_stmGrid_1000,xy=TRUE)

  # get metadata
  filename <- strsplit(probs_files[i],"[/.]")[[1]][6]
  filename <- paste(strsplit(filename,"[-]")[[1]][2:3],"-")

  # turn coord into facteur
  df_stmGrid_1000$x <- as.numeric(as.factor(df_stmGrid_1000$x)) - 1
  df_stmGrid_1000$y <- as.numeric(as.factor(df_stmGrid_1000$y)) - 1
  df_stmGrid_1000$state <- idToState(df_stmGrid_1000$state)
  df_stmGrid_1000[is.na(df_stmGrid_1000$state),"state"] <- 0

  # save for stm input
  write.table(df_stmGrid_1000,file=paste0("./data/futStatesGrid/stm/",filename,".stm"),quote=FALSE,row.names=FALSE,col.names=FALSE,sep=",")

}
