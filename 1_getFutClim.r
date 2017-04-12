# load libs
library("doParallel")

# read list of GCMs
GCM_df <- read.csv("./data/list_GCM.csv")
GCM_df <- subset(GCM_df, scenario == 'rcp85')

windows <- seq(2000,2095,5)
out_folder <- "./data/futClimSTM/"

# open cluster
cl <- makeCluster(20)
registerDoParallel(cl)

for (x in 1:dim(GCM_df)[1]){
    system(paste("mkdir -p ",out_folder,"GCM_id_",rownames(GCM_df)[x],sep=""))

    foreach(i=1:length(windows),.packages=c("RPostgreSQL"))%dopar%{
      # open db connection
      source('./con_quicc_db.r')

      # set query with params
      query_fut_climData <- paste("SELECT ST_X(geom) as lon, ST_Y(geom) as lat, x , y, var, ", windows[i]-15 ," as min_yr,",windows[i]," as max_yr, val, clim_center, mod, run, scenario FROM (
      SELECT var,clim_center, mod, run, scenario, (ST_PixelAsCentroids(ST_Union(ST_Clip(ST_Transform(raster,4269),1,env_plots,true),'MEAN'),1,false)).*
      FROM clim_rs.fut_clim_biovars,
      (SELECT ST_Transform(ST_GeomFromText('POLYGON((-74.6 45.6,-74.6 50.3,-74 50.3,-74 45.6,-74.6 45.6))',4326),4269) as env_plots) as envelope
      WHERE (var='bio1' OR var='bio12') AND (yr>=",windows[i]-15," AND yr<=",windows[i],") AND clim_center='",GCM_df[x,1],"' AND mod='",GCM_df[x,2],"' AND run='",GCM_df[x,3],"' AND scenario='",GCM_df[x,4],"' AND ST_Intersects(ST_Transform(raster,4269),env_plots)
      GROUP BY var,clim_center, mod, run, scenario
      ) as pixels;",sep="")

      cat("Querying id: ",rownames(GCM_df)[x],"; processing window:", windows[i]-15, "-", windows[i], "\n")

      # send query to postgresql
      fut_climData <- dbGetQuery(con, query_fut_climData)

      # write result
      write.table(fut_climData, file=paste(out_folder,"GCM_id_",rownames(GCM_df)[x],"/GCM_id_",rownames(GCM_df)[x],"_win_",windows[i]-15,"-",windows[i],".csv",sep=""), sep=',', row.names=FALSE)

      #close db connection
      dbDisconnect(con)
    }
}
