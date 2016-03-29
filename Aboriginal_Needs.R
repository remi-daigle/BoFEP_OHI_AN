library(rgdal)
library(rgeos)
library(raster)
library(dplyr)
library(ggplot2)
library(plotrix)

# load raw data
food_gas_data <- read.csv('aboriginal_food_gas.csv')
ice_layer <- read.csv('ice_layer.csv')

# set 1979 baselines for each community
ice_layer$pC_base <- 0
food_gas_data$gas_base <- 0
food_gas_data$food_base <- 0

ice_layer <- arrange(ice_layer,Year,Type,Name)
food_gas_data <- arrange(food_gas_data,year,type,name)
l <- length(unique(paste(ice_layer$Name,ice_layer$Type)))
for(i in 1:l){
    index <- ice_layer$Name==ice_layer$Name[i]&ice_layer$Type==ice_layer$Type[i]
    ice_layer$pC_base[index] <- ice_layer$pCover[index&ice_layer$Year==1979]
    index <- as.character(food_gas_data$name)==as.character(food_gas_data$name[i])&as.character(food_gas_data$type)==as.character(food_gas_data$type[i])
    food_gas_data$gas_base[index] <- food_gas_data$gas[index&food_gas_data$year==1979]
    food_gas_data$food_base[index] <- food_gas_data$est_RFNB[index&food_gas_data$year==1979]
}


#ice index
ice_layer$ice_index <- ice_layer$pCover/ice_layer$pC_base
ice_layer$ice_index[is.na(ice_layer$ice_index)] <- 1
ice_layer$ice_index[ice_layer$ice_index>2] <- 1


#food to gas ratio/fgr_bas*pCover/pC_base
# Year=1979:2013
# ANmean=rep(0,length(Year))

# get NS and NB basemap
Canada <- getData('GADM', country="CAN", level=1)
USA <- getData('GADM', country="USA", level=1)

NS <- Canada[Canada$NAME_1=="Nova Scotia",]
NB <- Canada[Canada$NAME_1=="New Brunswick",]
PEI <- Canada[Canada$NAME_1=="Prince Edward Island",]
MA <- USA[USA$NAME_1=="Maine",]


# Load BoFEP region
MRPA <- readOGR(getwd(),"MRPAOverallBoundaryPolygon(2014)")
proj <- proj4string(MRPA)
MRPAbuf <- gBuffer(MRPA,width=150000)

# tranform basemap and plot
NS <- spTransform(NS,CRS(proj))
NB <- spTransform(NB,CRS(proj))
PEI <- spTransform(PEI,CRS(proj))
MA <- spTransform(MA,CRS(proj))
other <- rbind(NS,PEI,MA)

jpeg(paste('Figures/map.jpg'),height=5,width=5,unit="in",res=400,qual=100)
par(mar=c(0,0,0,0))
plot(MRPAbuf,border='transparent')
plot(MRPA,add=TRUE,col='light blue')
plot(other,col="lightgrey",add=TRUE,border='transparent')
plot(NB,col="grey",add=TRUE,border='transparent')

# make spatialpointsdataframe for First Nations
FNspdf <- food_gas_data %>% 
    dplyr::filter(year==1979) %>%
    dplyr::select(longitude,latitude) %>% 
    SpatialPoints()
FNspdf <- SpatialPointsDataFrame(FNspdf,food_gas_data[food_gas_data$year==1979,])
proj4string(FNspdf) <- CRS("+proj=longlat")
FNspdf <- spTransform(FNspdf,CRS(proj))
plot(FNspdf,add=T,pch=19)
index <- gContains(MRPAbuf,FNspdf,byid = TRUE)
thigmophobe.labels(FNspdf$longitude[index],FNspdf$latitude[index],FNspdf$name[index])
plot(MRPAbuf,add=T,lty=2)
box()

dev.off()

# select communities that are within buffer
food_gas_data <- food_gas_data[as.vector(index),]
ice_layer <- ice_layer[as.vector(index),]


# Calculate AN
food_gas_data$ANindex <- (food_gas_data$est_RFNB/food_gas_data$gas)/(food_gas_data$food_base/food_gas_data$gas_base)*(ice_layer$ice_index)
write.csv(food_gas_data,'AN_data_communities.csv')
#food to gas ratio/fgr_bas*pCover/pC_base
Year=1979:2013
ANmean=rep(0,length(Year))

l <- length(unique(paste(ice_layer$Name,ice_layer$Type)))

for(i in 1:l){
    Aboriginal_Needs_Index=ANindex[ice_layer$Name==ice_layer$Name[i]&ice_layer$Type==ice_layer$Type[i]]
    ANmean=ANmean+(Aboriginal_Needs_Index*food_gas_data$population[food_gas_data$name==food_gas_data$name[i]&food_gas_data$type==food_gas_data$type[i]])/sum(food_gas_data$population[1:l])
}
write.csv(food_gas_data,'AN_data_communities.csv')


# plot
ggplot(food_gas_data,aes(x=year,y=ANindex,facet=name))+
    geom_line()+
    facet_wrap("name")+
    theme_bw()+
    ylim(0,1)+
    labs(x="Year",y="Aboriginal Needs Score")
ggsave('Figures/communities.jpg',height=7,width=7,units="in")

ANmean <- data.frame(Year,ANmean)
ggplot(ANmean,aes(x=Year,y=ANmean))+
    geom_line()+
    theme_bw()+
    ylim(0,1)+
    labs(x="Year",y="Aboriginal Needs Score")
ggsave('Figures/AN.jpg',height=4,width=4,units="in")


