# load libraries
library("raster")
library("rgdal")

#### Compute 95 quantiles on SIMULATION

fs <- list.files("./res/stacks",full.names=TRUE)

res_rate <- list()
res_prop <- list()

for(i in 1:length(fs)){

  cat("Computing",i,"/",length(fs),":",fs[i],"\n")

  # load file
  st_land <- readRDS(fs[i])

  # Transform T and M to 1
  st_land[st_land==0] <- NA
  st_land[st_land==1] <- 0 # B
  st_land[st_land==4] <- 0 # R
  st_land[st_land==2] <- 1 # T
  st_land[st_land==3] <- 1 # M

  # convert land as data.frame
  land_df <- as.data.frame(st_land,xy=T)
  land_df$y <- (land_df$y - min(land_df$y)) / 1000 # convert in km relative to the grid

  # get 95e percentile of T+M distrib for each year
  # Compute on the entire grid
  quan <- sapply(3:ncol(land_df),function(x){
    quantile(land_df[which(land_df[,x]==1),'y'],probs=c(.95))
  })

  # format rate output
  quan_df <- as.data.frame(t(c(fs[i],quan)))
  names(quan_df)[-1] <- names(land_df)[3:ncol(land_df)]
  names(quan_df)[1] <- "filename"

  ls_prop <- list()

  # compute prop on rasterStack
  for(l in 1:nlayers(st_land)){
    temp_land <- as.matrix(st_land[[l]])
    temp_land[is.na(temp_land)] <- 1
    prop <- apply(temp_land,1,sum)/ncol(temp_land)
    lat <- seq(0,(length(prop)-1)*res(st_land)[1],res(st_land)[1])/1000
    ls_prop[[l]] <- data.frame(filename=fs[i],year=names(st_land[[l]]),lat=rev(lat),prop=prop)
  }

  # Assign res_rate and res_prop
  res_prop[[i]] <- do.call(rbind,ls_prop)
  res_rate[[i]] <- quan_df

}

# Reshape for saving as RDS
df_prop <- do.call(rbind,res_prop)
df_prop$filename <- as.character(df_prop$filename)
df_prop$year <- as.character(df_prop$year)
df_prop$year <- as.numeric(stringr::str_replace_all(df_prop$year,"X",""))

# Extract model type and res
df_prop$model <- unlist(lapply(strsplit(df_prop$filename,"[-.]"),function(x) return(x[3])))
df_prop$res <- unlist(lapply(strsplit(df_prop$filename,"[-.]"),function(x) return(x[4])))

# SAVE PROP data.frame
saveRDS(df_prop,file='./res/df_prop_simus.rds')

# Reshape for saving as RDS
df_rate <- reshape2::melt(do.call(rbind,res_rate),id.vars=c('filename'))
names(df_rate)[2:3] <- c("year","perc_95")
df_rate$perc_95 <- as.numeric(df_rate$perc_95)
df_rate$year <- as.numeric(stringr::str_replace_all(df_rate$year,"X",""))
df_rate$filename <- as.character(df_rate$filename)

# Extract model type and res
df_rate$model <- unlist(lapply(strsplit(df_rate$filename,"[-.]"),function(x) return(x[3])))
df_rate$res <- unlist(lapply(strsplit(df_rate$filename,"[-.]"),function(x) return(x[4])))

# SAVE RATE data.frame
saveRDS(df_rate,file='./res/df_rate_simus.rds')

##################
#### Compute 95 quantiles on SOLVED model

fs <- list.files('./data/futStatesGrid/analytic/stacks',full.names=TRUE)

res_rate <- list()
res_prop <- list()

for(i in 1:length(fs)){
  land <- readRDS(fs[i])

  which.max2 <- function(x, ...){
    max_idx <- which.max(x)   # Get the max
    ifelse(length(max_idx)==0,return(NA),return(max_idx))
  }

  # get higher prob
  land_state <- calc(land,which.max2)
  land_state[land_state==2] <- 0 # B
  land_state[land_state==1] <- 1 # T
  land_state[land_state==3] <- 1 # M

  # convert land as data.frame
  land_df <- as.data.frame(land_state,xy=T)
  land_df$y <- (land_df$y - min(land_df$y)) / 1000 # convert in km relative to the grid
  land_df$x <- (land_df$x + abs(min(land_df$x))) / 1000

  # get 95e percentile of T+M distrib for each year
  # Compute on the entire grid
  quan <- sapply(0:max(land_df$x),function(pos){
    data <- subset(land_df, x == pos & layer == 1)
    return(quantile(data[,'y'],probs=c(.95)))
  })

  # format rate output
  md <- unlist(strsplit(fs[i],"[-.]"))
  res_rate[[i]] <- data.frame(filename=fs[i],year=md[5],perc_95=quan,model="analytic",res=1000)

  # compute prop along x
  prop <- aggregate(layer ~ y,data=land_df,FUN=mean)
  names(prop) <- c('lat','prop')
  res_prop[[i]] <- data.frame(filename=fs[i],year=md[5],prop,model="analytic",res=1000)

}

df_prop <- do.call(rbind,res_prop)
df_rate <- do.call(rbind,res_rate)

# reshape df_rate to save
df_rate$model <- as.character(df_rate$model)
df_rate$filename <- as.character(df_rate$filename)
df_rate$year <- as.numeric(as.character(df_rate$year))

# reshape df_prop to save
df_prop$model <- as.character(df_prop$model)
df_prop$filename <- as.character(df_prop$filename)
df_prop$year <- as.numeric(as.character(df_prop$year))

# SAVE RATE AND PROP as data.frame
saveRDS(df_rate,file='./res/df_rate_analytic.rds')
saveRDS(df_prop,file='./res/df_prop_analytic.rds')














