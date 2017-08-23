# load data
load("./res/prepGridsFigs.rda")

# col function
hcols <- function (x, bias = 1)
{
   (grDevices::colorRampPalette(c("grey90", "steelblue4", "steelblue2",
       "steelblue1", "gold", "red1", "red4"), bias = bias))(x)
}

# load data files
df_prop_analytic <- readRDS('./res/df_prop_analytic.rds')
df_rate_analytic <- readRDS('./res/df_rate_analytic.rds')
df_prop_simus <- readRDS('./res/df_prop_simus.rds')
df_rate_simus <- readRDS('./res/df_rate_simus.rds')

df_prop <- rbind(df_prop_analytic,df_prop_simus)
df_rate <- rbind(df_rate_analytic,df_rate_simus)

# Aggregate prop along strip with mean and sd
df_stat_prop_mean <- aggregate(prop~year+model+res+lat,data=df_prop,FUN=mean,na.rm=TRUE)
df_stat_prop_sd <- aggregate(prop~year+model+res+lat,data=df_prop,FUN=sd,na.rm=TRUE)
df_stat_prop <- merge(df_stat_prop_mean,df_stat_prop_sd,by=c('year','model','res','lat'))
names(df_stat_prop)[5:6] <- c('avg','sd')

# set model and year,res as factor
df_stat_prop$model <- as.factor(df_stat_prop$model)
df_stat_prop$res <- as.factor(df_stat_prop$res)
df_stat_prop$year <- as.factor(df_stat_prop$year)

########### STRIP FIGURE

# Start creating figure
years <- c('2015','2045','2095')
proto_data <- subset(df_stat_prop,year %in% years)

##### prototype figure
library(ggplot2)
ggplot(proto_data,aes(x=lat,y=avg,group=interaction(res,year,model),colour=year)) + geom_line(size=0.2) + geom_ribbon(aes(ymin= avg - sd, ymax= avg + sd, fill=year), alpha = 0.4, colour=NA) + facet_grid(model~res) + xlab("Meters") + ylab("Proportion of Temperate forest (T+M)") + scale_x_continuous(limits = c(100, 500))

##### Publication figure
# remove res 1000
final_data <- subset(df_stat_prop, year %in% years & res == 100)
cols <- c('#038bbd','#ff8e3e','#fc4c4c','#8bc300')
years <- c('2015','2045','2095')
models <- c('local','global')

par(mar=c(4,4,1,1),oma=c(1,1,1,1),mfrow=c(2,1))
for(mo in models){
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 530), ylim=c(0, 1))
  for(y in 1:length(years)){
    data <- subset(final_data,year==years[y]&model==mo)[,c('lat','avg','sd')]
    data <- data[order(data$lat),]
    lines(data[,c('lat','avg')],col=cols[y],lwd=1)
    polygon(x=c(data$lat,rev(data$lat)),y=c(data$avg+data$sd,rev(data$avg-data$sd)),border=NA,col=paste0(cols[y],"af"))
  }
  legend(-10,0.3, c("2015", "2045", "2095"), col = cols, lty = rep(1,4),lwd=rep(2,4),merge = TRUE,cex=0.9,bty = "n")
}



########### GRIDS FIGURE


# load shapefile
zone_veg <- readOGR("./data","zone_veg")
zone_veg <- spTransform(zone_veg,projection(grids_simus))
zone_veg <- crop(zone_veg,extent(grids_simus))

plot(border,border=NA,col="grey80")
image(st_transitions$BtoM,axes=FALSE,xlab="",ylab=,asp=1,breaks=round(seq(0,0.30,length.out=brk),2),col=pal(brk-1),add=TRUE)
plot(great_lakes,lwd=0.4,col="white",border="white",add=TRUE)
plot(border,add=TRUE,lwd=0.6,border="white",col=NA)
llgridlines(border,cex=0.5,lty=3,col='grey50')

plot(border,border=NA,col="grey90")
image(st_transitions$MtoT,axes=FALSE,xlab="",ylab="",asp=1,zlim=c(0,1),breaks=round(seq(0,0.30,length.out=brk),2),col=pal(brk-1),add=TRUE)
plot(great_lakes,lwd=0.4,col="white",border="white",add=TRUE)
plot(border,add=TRUE,lwd=0.6,border="white",col=NA)
llgridlines(border,cex=0.5,lty=3,col='grey50')

labs<-as.character(round(seq(0,0.30,length.out=5),2))
labs[length(labs)] <- paste0(">",labs[length(labs)])

par(mar=c(3,20,2,2))
image(matrix(1:brk-1),col=pal(brk-1),axes=FALSE,ann=FALSE)
axis(1,at=seq(0,1,length.out=5),lab=labs,col='grey40',col.ticks='grey40',col.axis="grey40", cex.axis=0.65)
box(lwd=0.8,col='grey40')

mtext("Proportion of transitions observed",1,line=-0.75,at=-0.30,font=2,cex=0.65,col='grey20')

dev.off()
