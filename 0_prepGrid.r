# clean wd
rm(list=ls())

# load ref raster
load("./data/0000_land_rs.robj")

# Band extent
ext_study_area <- c(-74.6,-74,45.6,50.3)

# crop band
rs <- crop(rs,ext_study_area)

# create empty raster with res of 100 meters
ext_lcc <- extent(projectExtent(rs,"+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs;"))
ref_rs <- raster(xmn=ext_lcc[1],xmx=ext_lcc[2],ymn=ext_lcc[3],ymx=ext_lcc[4],res=100,crs="+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs;")

saveRDS(ref_rs,file="./data/ref_rs.rda")
