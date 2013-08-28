library(maps)
library(mapdata)

#create map of the US
map(database = "state")

#read in compiled zooplankton data from the NLA datasets
data = read.csv("NLAzooplankton", header=TRUE)
nla<-read.csv('NLA_all.csv',header=T)

#set colors and location for legend
#Note: I used locator() to find appropriate placement of legend
rainbow.colors = c(rainbow(12))
x = c(-121.96, -119.96, -117.96, -115.96, -113.96, -111.96, -121.96, -119.96, -117.96, -115.96, -112.96, -109.96) 
y = c(rep(28.9, 6), rep(26.9, 6))      


#a loop that plots points on the map. Each loop plots all lakes with
#i number of species (1 through 12 species) using 12 different colors

for (i in 1:12) {  
points(data$LON_DD[data$num_species == i], 
       data$LAT_DD[data$num_species == i], 
       pch = 20, 
       col = rainbow.colors[i])
}

#add legend text
text(x, y, 
     labels = paste(c(1:12), sep=" "), 
     col=rainbow.colors, cex=1.2)

#Next, try the average number of species per lake in each HUC region, 
#then apply heat map to HUC regions instead of just points
#compare this to phosphorus or chl map
#

for (i in 1:2) {  
  points(nla$LON_DD[nla$LAKE_ORIGIN.x == "MAN-MADE"], 
         nla$LAT_DD[nla$LAKE_ORIGIN.x == "NATRUAL"], 
         pch = 20, 
         col = rainbow.colors[i])
}
pdf("laketype.pdf")
map(database = "state")
points(nla$LON_DD.x[nla$LAKE_ORIGIN.x == "MAN-MADE"], 
       nla$LAT_DD.x[nla$LAKE_ORIGIN.x == "MAN-MADE"],
       col = "red", 
       cex = 0.5)
points(nla$LON_DD.x[nla$LAKE_ORIGIN.x == "NATURAL"], 
       nla$LAT_DD.x[nla$LAKE_ORIGIN.x == "NATURAL"],
       col = "black", 
       cex = 0.5)
dev.off()


map("worldHires","USA")
summary(nla$SITE_TYPE)
summary(nla$LAKE_ORIGIN.x)
