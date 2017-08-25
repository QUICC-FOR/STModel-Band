# load data
load("./res/prepGridsFigs.rda")

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
proto_data <- subset(df_stat_prop,year %in% years & model != "analytic")

##### prototype figure
library(ggplot2)
ggplot(proto_data,aes(x=lat,y=avg,group=interaction(res,year,model),colour=year)) + geom_line(size=0.2) + geom_ribbon(aes(ymin= avg - sd, ymax= avg + sd, fill=year), alpha = 0.4, colour=NA) + facet_grid(model~res) + xlab("Meters") + ylab("Proportion of Temperate forest (T+M)") + scale_x_continuous(limits = c(100, 500))

##### Publication figure
# remove res 1000
final_data <- subset(df_stat_prop, year %in% years & (res == 100 | model == "analytic"))
cols <- c('#038bbd','#ff8e3e','#fc4c4c','#8bc300')
years <- c('2015','2045','2095')
models <- c('Local','Global','Analytic')

png("./res/figs/prop_strip.png",width=6,height=10,units="in",res=300)
par(mar=c(4,4,2,2),oma=c(1,1,1,1),mfrow=c(3,1))
for(mo in models){
  plot(1, type="n", xlab="", ylab="", xlim=c(100, max(final_data$lat)), ylim=c(0, 1.05),main=mo,cex.main=1.5,xaxs="i", yaxs="i")
  for(y in 1:length(years)){
    data <- subset(final_data,year==years[y]&model==tolower(mo))[,c('lat','avg','sd')]
    data <- data[order(data$lat),]
    lines(data[,c('lat','avg')],col=cols[y],lwd=1)
    polygon(x=c(data$lat,rev(data$lat)),y=c(data$avg+data$sd,rev(data$avg-data$sd)),border=NA,col=paste0(cols[y],"af"))
  }
  legend(110,0.3, c("2015", "2045", "2095"), col = cols, lty = rep(1,4),lwd=rep(2,4),merge = TRUE,cex=1.1,bty = "n")
}
mtext("Distance (kms)",side=1,line=-0.5,outer=T)
mtext("Proportion of the temperate forest (T+M)",side=2,line=-0.5,outer=T)
dev.off()

#### compute migration rate FOR ALL YEARS
## compute mean among lat band
df_rate_95 <- subset(df_rate,year == 2095 | year == 2000)
df_state_rate <- aggregate(perc_95 ~ filename + year + model + res, data=df_rate ,FUN=max)

ls_run <- strsplit(df_state_rate$filename,"[./-]")
df_state_rate$run <- unlist(lapply(ls_run, function(x){
  if('res' %in% x) {
    run <- x[grep('rcp85',x)]
  } else if ('data' %in% x) {
    run <- paste("rcp85",x[grep('rcp85',x)+1],sep="_")
  }
  return(run)
}))

df_state_rate$by <- paste(df_state_rate$model,df_state_rate$res,df_state_rate$run,sep="_")
df_state_rate <- split(df_state_rate,f=df_state_rate$by)

df_state_rate <- lapply(df_state_rate,function(x){
  x <- x[order(x$year),]
  x$rate <- c(NA,diff(x$perc_95))
  return(x)
})


### Compute rate by year and plot it
df_state_rate <- do.call(rbind,df_state_rate)
# df_state_rate_mean <- aggregate(rate ~ res + model + year, data=df_state_rate, FUN=mean)
# df_state_rate_sd <- aggregate(rate ~ res + model + year, data=df_state_rate, FUN=sd)
# df_state_rate_agg <- merge(df_state_rate_mean,df_state_rate_sd,by=c('res','model','year'))
# names(df_state_rate_agg)[4:5] <- c('avg','sd')

### PROTO FIG RATE
ggplot(df_state_rate,aes(x=year,y=rate)) + geom_boxplot(aes(group=year,colour=model),size=0.3) + facet_grid(model~res) +  ylab("Rate (km/year)") + xlab("Year")

### Compute average rate by model and res
# remove all 0 and negative rate
df_state_rate <- subset(df_state_rate, rate != 0 & rate > 0)
df_state_rate_mean <- aggregate(rate ~ res + model, data=df_state_rate, FUN=mean)
df_state_rate_sd <- aggregate(rate ~ res + model, data=df_state_rate, FUN=sd)
df_state_rate_agg <- merge(df_state_rate_mean,df_state_rate_sd,by=c('res','model'))
names(df_state_rate_agg)[3:4] <- c('avg','sd')


