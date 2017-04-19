# load lib
library(doParallel)

# load params
pars <- read.table("./pars/GenSA_rf_all_2_5y_rep1.txt",row.names=1)

# list landscape
clim_files <- list.files("data/futClimGrid/stmClim",full.names=TRUE)

# source res analytics function
source('./lib/resAnalytics.r')

# create output dir
system('mkdir -p ./data/futStatesGrid/probs')
system('mkdir -p ./data/futStatesGrid/stm')

# open cluster
cl <- makeCluster(35)
registerDoParallel(cl)


foreach(i=1:length(clim_files))%dopar%{

  climGrid <- read.csv(clim_files[i],header=TRUE,stringsAsFactors=FALSE)
  names(climGrid)[4:5] <- c("tp","pp")

  climGrid$tp[which(climGrid$tp == -9999)] <- NA
  climGrid$pp[which(climGrid$pp == -9999)] <- NA

  #### RESCALE
  load("./data/scale_info.Robj")
  climGrid_scale <- climGrid
  climGrid_scale$tp <- (climGrid_scale$tp-vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']
  climGrid_scale$pp <- (climGrid_scale$pp-vars.means['tot_annual_pp'])/vars.sd['tot_annual_pp']

  probsGrid <- solve_stm(climGrid_scale,pars)

  states <- character(0)

  for(r in 1:nrow(probsGrid)){
      if(any(is.na(probsGrid[r,]))){
        states <- append(states,NA)
      } else {
        states <- append(states,names(which.max(probsGrid[r,])))
      }
  }

  stmGrid <- data.frame(x=climGrid$x,y=climGrid$y,state=states,stringsAsFactors=FALSE)
  probsGrid  <- data.frame(x=climGrid$x,y=climGrid$y,probsGrid,stringsAsFactors=FALSE)

  # get metadata
  filename <- strsplit(clim_files[i],"[/.]")[[1]][4]

  # write
  write.csv(stmGrid,file=paste0("./data/futStatesGrid/stm/",filename,".csv"),row.names=FALSE,quote=FALSE)
  write.csv(probsGrid,file=paste0("./data/futStatesGrid/probs/",filename,".csv"),row.names=FALSE)

}
