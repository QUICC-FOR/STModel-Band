# libraries and source
source("./lib/convertStates.r")
library(raster)
library(plyr)

#clean wd
rm(list=ls())

grids_simus <- brick()

# LOCAL SIMU
#############################

ls <- list.files("./res/stacks",pattern="local-100.rda",full.names=TRUE)

# create empty stack
st_local_100_2000 <- stack()
st_local_100_2015 <- stack()
st_local_100_2045 <- stack()
st_local_100_2095 <- stack()

# fill stack by year
for(l in 1:length(ls)){
  grids <- readRDS(ls[l])
  st_local_100_2000 <- addLayer(st_local_100_2000,grids$X2000)
  st_local_100_2015 <- addLayer(st_local_100_2015,grids$X2015)
  st_local_100_2045 <- addLayer(st_local_100_2045,grids$X2045)
  st_local_100_2095 <- addLayer(st_local_100_2095,grids$X2095)
}

# GLOBAL SIMU
#############################

ls <- list.files("./res/stacks",pattern="global-100.rda",full.names=TRUE)

# create empty stack
st_global_100_2000 <- stack()
st_global_100_2015 <- stack()
st_global_100_2045 <- stack()
st_global_100_2095 <- stack()

# fill stack by year
for(l in 1:length(ls)){
  grids <- readRDS(ls[l])
  st_global_100_2000 <- addLayer(st_global_100_2000,grids$X2000)
  st_global_100_2015 <- addLayer(st_global_100_2015,grids$X2015)
  st_global_100_2045 <- addLayer(st_global_100_2045,grids$X2045)
  st_global_100_2095 <- addLayer(st_global_100_2095,grids$X2095)
}

##### set rasters stack by states
for(i in ls()[grep("st_",ls())]){
  rs <- get(i)
  rs[rs == 1] <- 0
  rs[rs == 2 ] <- 1 # mixed and temperate
  rs[rs == 3 ] <- 1 # mixed and temperate
  rs[rs != 1 ] <- 0
  rs <- sum(rs)/nlayers(rs)
  names(rs) <- i
  grids_simus <- addLayer(grids_simus,rs)
}

# SOLVED
#############################

st_solved_2000 <- mean(readRDS("./res/2000_tempProbSolved.rda"))
names(st_solved_2000) <- "st_solved_2000"
st_solved_2015 <- mean(readRDS("./res/2015_tempProbSolved.rda"))
names(st_solved_2015) <- "st_solved_2015"
st_solved_2045 <- mean(readRDS("./res/2045_tempProbSolved.rda"))
names(st_solved_2045) <- "st_solved_2045"
st_solved_2095 <- mean(readRDS("./res/2095_tempProbSolved.rda"))
names(st_solved_2095) <- "st_solved_2095"

grids_solved <- stack(st_solved_2000,st_solved_2015,st_solved_2045,st_solved_2095)

save(grids_solved,grids_simus,file="./res/prepGridsFigs.rda")
