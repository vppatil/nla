#NLA spatial overlay script

#Vijay Patil
#Feb 12 2013


#this version no longer contains code for overlays of nla coordinates and nhd shapefiles
#that code is located in the nhdNLA.r script.

#at the moment, it is still difficult to get NHD data off the website in any form other than a proprietary arcGIS 
#file geodatabase '.gdb'. 
#currently, I am extracting the shapefiles from these gdbs and exporting them using the python language in arcGIS (gdptoshp.py)
#the NHD polygon file for all NLA lakes is available in the NLA analyses shared dropbox folder.

#this script DOES include a section for extracting landcover information from an NLA lake buffer and doing
#basic calculations on the extracted information.

####National Landcover Database files used in this script are available for download at
#http://www.mrlc.gov/nlcd2006.php

#both a landcover layer and an impermeable surfaces layer are available.



######################################preliminary stuff ######################################################
#first read in the nla data
setwd('C:/Users/sam/documents/GLEON files/')
nla<-read.csv('NLA.csv')


#this is completely unrelated, but I just found out about an R package for interacting with google maps.
#as you might expect, its called RGoogleMaps.
#I just wanted to write it down somewhere before I forgot about it.


# #loading a gazillion spatial data packages
# install.packages('maps') #for plotting state boundaries
# install.packages('sp')   #r's workhorse spatial analysis package
# install.packages('maptools') #functions for reading and writing shapefiles
# install.packages('shapefiles') #more of the same
# install.packages('raster') #for reading raster (grid-based) spatial data
# install.packages('proj4')  #this is a package for specifying map projections and datums
#                            

#the next two were a pain in the ***. on linux I had to install some extra software
#to get them to work. doesn't seem to be a problem for windows.
#I suspect all that might be handled opaquely by the windows r package installer

# install.packages('rgeos') #this seems to be something used in computation by other sp packages
# install.packages('rgdal') #this is also used for reading and writing spatial data

library(maps)
library(maptools) #maptools seems better at reading/plotting shapefiles than the shapefiles package. don't know why.
library(sp)
library(rgeos)
library(rgdal)
library(proj4)
library(shapefiles)
library(raster)

#As luke mentioned, there are two basic kinds of gis data: raster data (a grid of pixels which each have
#some value), and shapefile data, which is a collection of geometric objects (polygons, lines, or points), each of
#which has some attributes.

#############################################creating a points shapefile with the locations of all nla lakes ################


##First I'll plot the coordinates with a US map overlay
plot(nla[,3],nla[,2],add=T)
map('state',interior=FALSE,add=T)
#making a shapefile:

#
#We need to make two dataframes- the first contains an Id column, x and y coordinates
#the second contains all the other information for each lake (i.e. the nla dataset itself)

nla.points<-data.frame(Id=nla$SITE_ID,X=nla[,3],Y=nla[,2]) #latitude is col 3, longitude is col 2
nla.dat<-data.frame(Id=nla.points$Id,nla) #here I am just attaching the Id column to the nla dataset

#the convert.to.shapefile function in shapefiles does what its name implies
points.shp <- convert.to.shapefile(nla.points,nla.dat, "Id", 1) #args are coordinate table, data table, Id column name, data type

#the newly created shapefile can then be written out to a file
#there will be three files created (*.shp, *.shx, *.dbf)
write.shapefile(points.shp,'nlapoints') #set the path as appropriate, but leave off the file extension 

#the shapefile can now be used to spatially overlay information contained in the nla dataset with other 
#spatial information.


##################### if the nla points shapefil is already made, skip to here ##########################################

#the read.shapefile function in shapefiles was giving me problems, but the equivalent function 
#in the sp package works
setwd('C:/Users/sam/documents/GLEON files/')
nla<-readOGR('.','nlapoints',p4s='+proj=latlong +datum=NAD83') #this reads in the file and gives it projection information

#we can take a look at the overview plot again
plot(nla)
map('state',interior=FALSE,add=T)
map('state',boundary=FALSE,col='red',add=TRUE)

#we can also read in the nla nhd polygon file, and the 100m buffer created around that file
nhdnla<-readOGR('.','combine_lakes_fulldata',p4s='+proj=latlong +datum=NAD83')
nlabuffer<-readOGR('.','NLAbufferNAD83',p4s='+proj=latlong +datum=NAD83')

#read in the nla polygons as lines so you can extract landcover based on intersection with lake boundary
#setwd('C:/Users/sam/documents/GLEON files/')
#nhdnlaline<-readShapeLines('combine_lakes_fulldata',proj4string=CRS('+proj=latlong +datum=NAD83'))

################################### Importing and overlaying raster data ################################################

#first open up a connection to the NLCD data layer.
setwd('c:/users/sam/desktop/yrb_gis/nlcd')

library(raster)
nlcd.file="nlcd2006_landcover_4-20-11_se5.img"
nlcd<-raster(nlcd.file)
plot(nlcd) #nlcd is a 30m resolution landcover map for the lower 48

#we can extract the nlcd proj4string in order to convert our data files to the same projection
#reproject the buffer layer to the nlcd projection for overlay
buffer.proj<-spTransform(nlabuffer,CRS(proj4string(nlcd)))

#extract nlcd landcover types for all cells in each lake buffer
#this will take a while
lakenum=1159 #change to number of lakes in data file
nlcd.extract<-extract(nlcd,buffer.proj,df=T)
names(nlcd.extract)[2]='Value'

#need to convert these to actual landcover types using the legend from the nlcd website
nlcd.legend<-read.csv('NLCDlegend.csv')
nla.landcover<-merge(nlcd.extract,nlcd.legend)

#also need to get dataframe of actual site ids, since the extract function doesn't return these
id.df<-data.frame('ID' = 1:lakenum,'Permanent_' = buffer.proj@data$Permanent_[1:lakenum])
nla.landcover<-merge(nla.landcover,id.df)

#now we have a dataframe with lakeids and landcover, and we can do some calculations about landcover!
nla.table<-table(nla.landcover$Permanent_,nla.landcover$Landcover)
nla.table<-nla.table[apply(nla.table,1,sum)>0,]
nla.perc.cov<-nla.table/apply(nla.table,1,sum)

write.csv(nla.table,'nlaNLCDlandcover.csv')

#this information can be merged back into the nla dataset-

#this section could be made more elegant. right now it is clunky but it works.
#for example, to put in the % woody wetlands
#we can first make a new data column
buffer.proj@data$pWoodyWetlands = 0 
data.indices<-match(row.names(nla.perc.cov),buffer.proj@data$Permanent_) #figure out which rows to put the data in

#finally, add in the data.
buffer.proj@data$pWoodyWetlands[data.indices]=nla.perc.cov[colnames(nla.perc.cov)=='woody wetlands']


############### overlay of nla data with NLCD impervious surfaces layer #############################
setwd('c:/users/sam/desktop/YRB_GIS')
isfilename='nlcd2006_impervious_5-4-11.img'
impSurf<-raster(isfilename) #file is % impervious surfaces

#now to extract the %impervious surface values from cells in each lake buffer:

lakenum=1159
impSurf.extract<-extract(impSurf,buffer.proj[1:lakenum,],df=T)
names(impSurf.extract)[2] = 'PctImpervious'
#also need to get dataframe of actual site ids, since the extract function doesn't return these
id.df<-data.frame('ID' = 1:lakenum,'Permanent_' = buffer.proj@data$Permanent_[1:lakenum])
nla.impSurf<-merge(impSurf.extract,id.df)
nla.impSurf$Permanent_ = factor(nla.impSurf$Permanent_)
impsurf.means<-tapply(nla.impSurf$PctImpervious,nla.impSurf$Permanent_,mean)

nhdnla@data$PCT_IMP = 0
indices=match(names(impsurf.means),nhdnla@data$Permanent_)
nhdnla@data$PCT_IMP[indices]=impsurf.means #adding mean % impervious surfaces in 100m buffer to nhdnla layer

write.csv(nhdnla@data,'nla_impSurf.csv')