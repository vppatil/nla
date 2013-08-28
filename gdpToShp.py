#jan 28,2013
#Vijay Patil

#python code to merge nhd layers into a single layer with polygons for all nla lakes
#note that this only works if you have ARCgis installed.

#there are equivalent ways to do almost everything in here in R (see nhdNLA.r)
#but this is apparently way faster.

#the geodatabase-shapefile conversion step may not yet be possible in R, but 
#can be done using qgis, a free/open source GIS program.
 
#first steps:
#read list of files from a directory
import os
os.chdir('C:/Users/brad griffith/Desktop/marmottemp/') #this is where all the geodatabases were stored.
filelist = os.listdir('.') #get list of all files in the working directory
gdbs = [s for s in filelist if ".mdb" in s] #get list of files with gdb extension
zips = [s for s in filelist if ".zip" in s] #get list of all zip files
 
# Import system modules
import arcpy
from arcpy import env
import shutil

from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
 
import string

#Set environment settings
outLocation = "C:/users/brad griffith/desktop/marmottemp/"

#this block does the conversion to shapefiles
for filename in gdbs:
	filepath=os.path.join('C:/users/brad griffith/desktop/marmottemp/',filename).replace('\\','/')
	arcpy.env.workspace = filepath
	#Set local variables
	inFeatures =arcpy.ListFeatureClasses("water*","ALL")  #each geodatabase contains a shapefile called NHDWaterbody. this is what we care about.
	#Execute FeatureClassToGeodatabase
	arcpy.FeatureClassToShapefile_conversion(inFeatures, outLocation)


delete all the gdbs when you're done with them.
for filename in gdbs:
	delpath=os.path.join('C:/users/brad griffith/desktop/marmottemp',filename)
	shutil.rmtree(delpath)
os.chdir('C:/Users/Sam/Desktop/gdbs')	
for filename in zips:
	os.remove(filename)
	

# now the shapefiles are made and the gdbs deleted as they are read in- 	
# the remainder of the code creates a single nhdnla lake polygon file

# Set environment settings
outLocation = "C:/users/sam/desktop/vijay/nhdShps"
arcpy.env.workspace = outLocation
featurenames=arcpy.ListFeatureClasses("NHD*","ALL") 
#get list of the shapefiles you care about.

join_feature = 'C:/Users/Sam/Documents/GLEON files/nlapoints.shp' 
#this is the layer with nla coordinates

#loop through nhd layers and spatially join with nla
for tf in featurenames:
	print tf
	target_feature = tf
	out_feature_class = os.path.join('C:/Users/Sam/Desktop/vijay/nhdnla/',target_feature.replace('NHD','nlaNHD')) #create a new list of shapefiles with nlaNHD in the name
	arcpy.SpatialJoin_analysis(target_feature,join_feature,out_feature_class)

#select and export only those lakes that overlap nla points.
outLocation = "C:/users/sam/desktop/vijay/nhdnla"
arcpy.env.workspace = outLocation
featurenames=arcpy.ListFeatureClasses("nlaNHD*","ALL")
for tf in featurenames:
	in_features=tf
	out_features = in_features.replace('nlaNHD','nlaNHDclip')
	where_clause = '"SITE_ID" NOT LIKE \'\''
	arcpy.Select_analysis(in_features, out_features, where_clause = "SITE_ID not in ('')")

#merge all the nhdnla files you just made into one file
arcpy.env.workspace = 'C:/Users/sam/desktop/vijay/nhdnla'
joinnames= arcpy.ListFeatureClasses('nlaNHDclip*','ALL')
nhdNLAmerge = 'C:/Users/Sam/Desktop/vijay/nhdnla/nhdNLAmerge.shp'
arcpy.Merge_management(joinnames,nhdNLAmerge)

#the nhdNLAmerge shapefile now contains nhd polygons for all nla lakes, with nla data in their attribute tables




