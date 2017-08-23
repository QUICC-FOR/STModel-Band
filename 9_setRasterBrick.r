# clean spaces
rm(list=ls())

# load libs
library(doParallel)

# list_files all stm files
list_files <- list.files("./res", pattern='stm', full.names=TRUE, recursive=TRUE,all.files=TRUE)

# create output dataframe with four columns: path, resolution, mode_type, year, mod_rcp
resolution <- unlist(stringr::str_extract_all(unlist(lapply(strsplit(dirname(list_files),"[-]"),function(x) x[3])),pattern="(\\d)+"))
mod_type  <- unlist(lapply(strsplit(dirname(list_files),"[-]"),function(x) x[2]))
year <- gsub(".stm","",unlist(lapply(strsplit(basename(list_files),"[-]"),function(x) x[3])))
mod_rcp <- unlist(lapply(strsplit(basename(list_files),"[-]"),function(x) x[2]))
ids_splt <- paste(mod_rcp,mod_type,resolution,sep="-")

# create dataframe
out_df <- data.frame(path=as.vector(list_files),mod_rcp=mod_rcp,mod_type=mod_type,res=resolution,year=year,id=ids_splt,stringsAsFactors=FALSE)
out_df <- out_df[with(out_df,order(mod_rcp,mod_type,res,year)),]

# load ref grid
ref_res100 <- readRDS("data/futClimGrid/rasterClim/res100/rcp85-ACCESS1_0-1985-2000.rda")
ref_res1000 <- readRDS("data/futClimGrid/rasterClim/res1000/rcp85-ACCESS1_0-1985-2000.rda")

#load lib
source("./lib/convertStates.r")

# split using id
ls_out_df <- split(out_df,f=out_df$id)

##### OPEN CLUSTER
# open cluster
cl <- makeCluster(35)
registerDoParallel(cl)

# loop over list
foreach(i=1:length(ls_out_df),.packages=c('raster','reshape2'))%dopar%{

  if(unique(ls_out_df[[i]]$res == 100)){
    ref <- ref_res100
  } else if(unique(ls_out_df[[i]]$res == 1000)) {
    ref <- ref_res1000
  }

  # open stack
  st_grids <- stack()

  # loop over tim step
  for(step in 1:nrow(ls_out_df[[i]])){

    # prep outputs data.frame
    grid <- read.table(ls_out_df[[i]]$path[step],sep=",",header=FALSE,stringsAsFactors=FALSE)
    names(grid) <- c("x","y","state")

    # transform to raster
    rs_grid <- grid
    rs_grid$state <- stateToId(rs_grid$state)
    rs_grid <- as.matrix(reshape2::dcast(y ~ x, data=rs_grid)[,-1])

    # reverse lon band
    rs_grid <- apply(rs_grid,2,rev)

    # set sp
    rs_grid <- raster(rs_grid)
    extent(rs_grid) <- extent(ref)
    projection(rs_grid) <- projection(ref)

    st_grids <- addLayer(st_grids,rs_grid)

  }

  # named stack
  names(st_grids) <- ls_out_df[[i]]$year

  # get metadata
  md <- unique(ls_out_df[[i]]$id)

  # save
  saveRDS(st_grids,file=paste0("./res/stacks/rcp85_",md,".rda"))

}

####### STACK SOLVED MODEL

solv_g <- list.files('./data/futStatesGrid/analytic/probs',full.names=TRUE)
solv_f <- list.files('./data/futStatesGrid/analytic/probs')

### this step should be removed and set at STEP 9
ref <- readRDS("data/futClimGrid/rasterClim/res1000/rcp85-ACCESS1_0-1985-2000.rda")

for(i in 1:length(solv_g)){
  st_states_probs <- stack()

  grid <- read.csv(solv_g[i],header=TRUE,stringsAsFactors=FALSE)

  # transform to raster
  T_grid <- as.matrix(reshape2::dcast(y ~ x, data=grid[,c('x','y','T')])[,-1])
  B_grid <- as.matrix(reshape2::dcast(y ~ x, data=grid[,c('x','y','B')])[,-1])
  M_grid <- as.matrix(reshape2::dcast(y ~ x, data=grid[,c('x','y','M')])[,-1])

  # reverse lon band
  T_grid <- apply(T_grid,2,rev)
  B_grid <- apply(B_grid,2,rev)
  M_grid <- apply(M_grid,2,rev)

  # set sp
  T_grid <- raster(T_grid)
  B_grid <- raster(B_grid)
  M_grid <- raster(M_grid)
  extent(T_grid) <- extent(M_grid) <- extent(B_grid) <- extent(ref)
  projection(T_grid) <- projection(M_grid) <- projection(B_grid) <- projection(ref)

  # Stack layers
  st_states_probs <- addLayer(st_states_probs,T_grid)
  st_states_probs <- addLayer(st_states_probs,B_grid)
  st_states_probs <- addLayer(st_states_probs,M_grid)
  names(st_states_probs) <- c('T','B','M')

  saveRDS(st_states_probs,file=paste0("./data/futStatesGrid/analytic/stacks/",stringr::str_replace_all(solv_f[i],".csv",".rds")))

}
