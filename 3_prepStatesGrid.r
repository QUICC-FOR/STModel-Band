# load params
pars <- read.table("./pars/GenSA_rf_all_2_5y_rep1.txt",row.names=1)

# list landscape
clim_files <- list.files("data/futClimGrid/stmClim",full.names=TRUE)

# source res analytics function
source('./lib/resAnalytics.r')

# create output dir
system('mkdir -p ./data/futStatesGrid')

i = 1
climGrid <- read.csv(clim_files[1],header=TRUE,stringsAsFactors=FALSE)
names(climGrid)[4:5] <- c("tp","pp")

stm_solve(climGrid,pars)
