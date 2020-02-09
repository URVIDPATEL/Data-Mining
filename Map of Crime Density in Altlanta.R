####---packages installation 
install.packages("ggpubr")
install.packages("arules", dependencies = T)
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")
install.packages("ggplot")
install.packages("ggmap")
install.packages("lubridate")
install.packages("doBy")
install.packages("psych")
install.packages("highcharter")
install.packages("maps")
install.packages("prophet")
install.packages("devtools")

devtools::install_github("dkahle/ggmap") ###----download devtools 


####---libraries need to be used 
library(ggplot2)
library(ggpubr)
library(grid)
library(arules)
library(arulesViz)
library(maptools)
library(rgdal)
library(maptools) 
library(ggmap)
library(lubridate)
library(chron)
library(dplyr)
library(doBy)
library(sp)
library(plyr)
library(psych)
library(MASS)
library(highcharter)
library(tidyr)
library(viridis)
library(plotly)
library(xts)
library(maps)
library(gridExtra)
library(prophet)


register_google(key = "AIzaSyA2hfOBxIH_hNQB63SfFrIM93wnJXj8myo") ###google map require API to activate the map

has_google_key() ##check whether the key is exist or not 

theme_set(theme_pubr())

getwd()
setwd() ### need to be modified when getwd()
#--------Reading in the csv file and converting it into a dataframe
d <- read.csv("COBRA-2009-2018.csv", na.strings=c("",".","NA")) 

k<- d 

##-----------google Map of Crime Density in Atlanta 
Atlanta <- get_map(location = "Atlanta", zoom = 14) ##Get the Atlanta map
AtlantaMap<-ggmap(Atlanta, extent = "device")       ##Prepare Map
AtlantaMap +
  stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = d) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map of Crime Density in Atlanta")+ theme(legend.text=element_text(size=13,face="bold"))+
  theme(legend.title=element_text(size=14,face="bold"))+theme(plot.title = element_text(size = 22, face = "bold"))
crime.agg <- ddply(k, .(crimetype, beat,date, Longitude,
                                 Latitude, time.split, weekday, month), summarise, count = length(date),
                   .progress= 'text')







