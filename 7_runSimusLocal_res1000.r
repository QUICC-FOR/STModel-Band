# clean
rm(list=ls())

# load pcks
library(tidyr)
library(doParallel)

# load lib
source('./lib/stmodel.r')

# output folder
out_local_folder <- "/ssdpool/users/sviss/stm-local-res1000"

#create folders
system(paste("mkdir -p",out_local_folder))


##### BEGIN: PREP OUT AND IN FILES

# list clim files
clim_files <- data.frame(files=list.files("./data/futClimGrid/stmClim",full.names=TRUE),stringsAsFactors=FALSE)

# separate clim files
clim_files <- separate(clim_files,files,c('path','mod','yr_min','yr_max'),sep="[-]",remove=FALSE)
clim_files$yr_max <- gsub(".csv","",clim_files$yr_max)
# remove clim_files with yr_min = 2000
clim_files <- subset(clim_files,!yr_max == 2000)
clim_files <- clim_files[,-c(2,4)]
names(clim_files)[3] <- "yr"

# list states files
stm_files <- data.frame(files=list.files("./data/futStatesGrid/stm1000",full.names=TRUE),stringsAsFactors=FALSE)

# separate stm files
stm_files <- separate(stm_files,files,c('path','mod','yr'),sep="[-]",remove=FALSE)
stm_files$yr <- gsub(".stm","",stm_files$yr)
stm_files <- stm_files[,-2]

# create out filname
mods <- unique(stm_files$mod)
years <- seq(2005,2095,5)
comb <- expand.grid(mods,years)
path <- paste(out_local_folder,"/",paste("rcp85",comb[,1],comb[,2],sep="-"),".stm",sep="")

out_files <- data.frame(files=path,mod=comb[,1],yr=comb[,2])
out_files <- rbind(out_files,stm_files)

# merge out_files with clim_files
out_files <- merge(out_files,clim_files,by=c("yr","mod"),all.x=TRUE)
names(out_files)[3:4] <- c("stm_file","clim_file")

# split by mod
out_files$mod <- as.factor(out_files$mod)
ls_out_file <- split(out_files,out_files$mod)

# sort by yr
ls_out_file <- lapply(ls_out_file,function(x) x[order(x$yr),])

##### END: PREP OUT AND IN FILES

##### OPEN CLUSTER
# open cluster
cl <- makeCluster(35)
registerDoParallel(cl)

##### RUN SIMU
foreach(i=1:length(ls_out_file))%dopar%{

  outin <- ls_out_file[[i]]

  for(step in 2:nrow(outin)){
    stmodel_local(params="./pars/GenSA_rf_all_2_5y_rep1.txt",
            inland=outin$stm_file[step-1],
            outland=outin$stm_file[step],
            x=47,
            y=526,
            a=47,
            b=526,
            clim_file=outin$clim_file[step])
  }
}

# cp all 2000 stm grids to out folders
system("cp ./data/futStatesGrid/stm/* /ssdpool/users/sviss/stm-local-res1000")
