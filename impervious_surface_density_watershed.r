#### working with NLCD impervious surface.

library(raster)
library(rgeos)

nlcdImpPath = 'D:/BigDatasets/NLCD2006/'

nlcd.imp<-raster(paste(nlcdImpPath,'nlcd2006_impervious_5-4-11.img',sep=''))

watersheds.old<-readOGR('../data/shapefiles/nlaWatersheds','NLA_Sites_Sampled_DrainageBasins')
watersheds <- watersheds.old


#Lets take a look at how big these watersheds are
#watersheds.cae<-spTransform(watersheds.old,CRS('+proj=cea')) #convert to metric units
# areas = vector(mode='numeric',length=dim(watersheds.cae)[1])
# for(i in 1:dim(watersheds.cae)[1]){
#   areas[i] = gArea(watersheds.cae[i,])
#   
# }
# 
# sort.int(areas, index.return=TRUE)
#Back to real work


watershed.num=dim(watersheds)[1]

watersheds.proj<-spTransform(watersheds,CRS(proj4string(nlcd.imp)))
watershed.num<-dim(watersheds.proj)[1]
impsurf.df<-data.frame(SITEID='',mean.pct.imp=0,pixels.w.impsurf=0,pixel.count=0, stringsAsFactors=FALSE)

#rasterOptions(datatype="INT1U")


#was going really slowly trying to do the whole extraction at once, so I broke it out into a loop.
#for( i in c(747)) #This is a great example size watershed to push the limit
for( i in 1:watershed.num)
{
	print(paste(100*i/watershed.num,'% complete'))
  
  #First, crop out a subset of the whole NLCD that covers the watershed
	nlcd.crop<-crop(nlcd.imp, extent(watersheds.proj[i,]))

  #I rasterize the mask separately to control the datatype
	watershed.mask = rasterize(watersheds.proj[i,], nlcd.crop, datatype="INT1U")
  
  #Mask the cropped NLCD data using the watershed mask
  nlcd.masked = mask(nlcd.crop, watershed.mask, datatype="INT2S", maskvalue=0)
  #The two above commands could probably be combined, but I was having trouble
  # for a while, so they are separate. There is some nuance with the datatype
  
  #Ok, first, this crop here is redundant. "extract" does a crop itself
	#nlcd.crop<-crop(nlcd.imp,extent(watersheds.proj[i,]))
	#Get values from the raster for the watershed area
  #nla.watershed.impSurf<-extract(nlcd.imp, watersheds.proj[i,])
  
  #Ok, instead of pulling out all the values (which extract does), 
  # we iterate over the raster, getting subsets of rows. This prevents
  # the out of memory issues we were having with the huge watersheds
  blsz = blockSize(nlcd.masked) #How many rows can we reasonably grab
  cell.count = 0
  cell.sum = 0
  impv.cell.count = 0
  
  #Iterate over all rows
  for (j in 1:blsz$n){
    vals = getValues(nlcd.masked, row=blsz$row[j], nrows=blsz$nrows[j])
    #The 
    goodI           = !is.na(vals)
    cell.count      = cell.count + sum(goodI)
    goodVals        = vals[goodI]
    cell.sum        = cell.sum + sum(goodVals)
    impv.cell.count = impv.cell.count + sum(goodVals > 0)
    
    print(paste('---Summary: ', 100*j/blsz$n, '% complete'))
  }
  
  #Get the mean imperivous surface of the watershed
	#mean.pct.imp<-mean(nla.watershed.impSurf[[1]])
  mean.pct.imp = cell.sum/cell.count
	pixels.w.impsurf = impv.cell.count/cell.count
  
  #Get the fraction of pixels covered by any impervious surface
	#pixels.w.impsurf<-sum(table(nla.watershed.impSurf[[1]]>0)[2])/length(nla.watershed.impSurf[[1]])
	impsurf.df[i,]=data.frame(SITEID=as.character(watersheds.proj$SITEID[i]),mean.pct.imp=mean.pct.imp,pixels.w.impsurf=pixels.w.impsurf, pixel.count=cell.count,stringsAsFactors=FALSE)
	
	#closeConnection(nlcd.crop)
  #closeConnection(nlcd.mask)
  #rm(nlcd.mask)
}


#Get rid of all the temporary rasters that were created
removeTmpFiles(h=0.1)

write.csv(impsurf.df,'../output/nlawatersheds_imperviousSurface.csv',row.names=FALSE)

nlanhd<-subset(read.dbf('../nlaNHD/combine_lakes_fulldata.dbf')$dbf,select=c('SITE_ID','Permanent_'))
impsurf.df2<-merge(impsurf.df,nlanhd,by='Permanent_',all.x=T)
#write.csv(impsurf.df2,'c:/users/sam/dropbox/nla analyses/output/nlabuffers_imperviousSurface.csv',row.names=FALSE)
