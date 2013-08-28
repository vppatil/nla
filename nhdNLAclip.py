
#it is possible to clip the road layer by a buffer polygon in R using the gIntersection() function in rgeos
#but it is incredibly slow on my computer, and the same operation takes a millionth of the time in ARC
#so I am creating a python script to clip each road layer by the buffer layer, then spatially join the clipped layers back to the buffer layer so they 
#retain the attributes of the nla lake they are near.


# Import system modules
import os
import arcpy
from arcpy import env
import shutil
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
import string

# Set environment settings
outLocation = "C:/users/sam/desktop/vijay/nhdShps"
arcpy.env.workspace = outLocation
featurenames=arcpy.ListFeatureClasses("NHD*","ALL") 
#get list of the shapefiles you care about.


#this is the layer with nla coordinates

#loop through nhd layers and spatially join with nla
for tf in featurenames:
	print tf
	target_feature = tf
	out_feature_class = os.path.join('C:/Users/Sam/Desktop/vijay/nhdnla/',target_feature.replace('NHDWaterbody','nlaNHDmiss')) #create a new list of shapefiles with nlaNHD in the name
	arcpy.Clip_analysis(target_feature,join_feature,out_feature_class)


#merge all the nhdnla files you just made into one file
arcpy.env.workspace = 'C:/Users/sam/desktop/vijay/nhdnla'
joinnames= arcpy.ListFeatureClasses('nlaNHDmiss*','ALL')
nhdNLAmerge = 'C:/Users/Sam/Desktop/vijay/nhdnla/nhdNLAmissmerge.shp'
arcpy.Merge_management(joinnames,nhdNLAmerge)
#this is the layer with nla coordinates
