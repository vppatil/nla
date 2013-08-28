
#it is possible to clip the road layer by a buffer polygon in R using the gIntersection() function in rgeos
#but it is incredibly slow on my computer, and the brad griffithe operation takes a millionth of the time in ARC
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

# # Set environment settings
# outLocation = "C:/users/brad griffith/documents/roads"
# arcpy.env.workspace = outLocation
# featurenames=arcpy.ListFeatureClasses("tl*","ALL") 

# clip_feature = 'C:/users/brad griffith/dropbox/NLA analyses/data/shapefiles/nlaBuffers/NLA200mbuf.shp' 
# xy_tolerance = ""
# # loop through nhd layers and spatially join with nla
# for tf in featurenames:
	# print tf
	# target_feature = tf
	# out_feature_class = os.path.join('C:/users/brad griffith/Documents/roadclip/',target_feature.replace('tl','cl')) #create a new list of shapefiles with nlaNHD in the name
	# arcpy.Clip_analysis(target_feature,clip_feature,out_feature_class,xy_tolerance)
	
# arcpy.env.workspace = 'C:/users/brad griffith/documents/roadclip'
# joinnames= arcpy.ListFeatureClasses('cl*','ALL')
# roadmerge = 'C:/users/brad griffith/documents/roadclip/roadbuffermerge.shp'
# arcpy.Merge_management(joinnames,roadmerge)

# join_feature = 'C:/users/brad griffith/dropbox/NLA analyses/data/shapefiles/nlaBuffers/NLA200mbuf.shp' 
# merge_join = roadmerge.replace('buffermerge','bufferjoin')
# arcpy.SpatialJoin_analysis(roadmerge,join_feature,merge_join)

# install_dir = arcpy.GetInstallInfo()['InstallDir']
# out_coordinate_system = os.path.join(install_dir, r"Coordinate Systems/Projected Coordinate Systems/UTM/Continental/North America/USA Contiguous Albers Equal Area Conic.prj")


###running clip for nla watershed polygons below:

# Set environment settings
outLocation = "C:/users/brad griffith/documents/roads"
arcpy.env.workspace = outLocation
featurenames=arcpy.ListFeatureClasses("tl*","ALL") 

clip_feature = 'C:/users/brad griffith/dropbox/NLA analyses/data/shapefiles/nlaWatersheds/NLA_Sites_Sampled_DrainageBasins.shp' 
xy_tolerance = ""

# loop through nhd layers and spatially join with nla
for tf in featurenames:
	target_feature = tf
	out_feature_class = os.path.join('C:/users/brad griffith/Documents/roadclipwatersheds/',target_feature.replace('tl','wcl')) #create a new list of shapefiles with nlaNHD in the name
	arcpy.Clip_analysis(target_feature,clip_feature,out_feature_class,xy_tolerance)

arcpy.env.workspace = 'C:/users/brad griffith/documents/roadclipwatersheds/'
joinnames= arcpy.ListFeatureClasses('wcl*','ALL')
roadmerge = 'C:/users/brad griffith/documents/roadclip/roadwshedmerge.shp'
arcpy.Merge_management(joinnames,roadmerge)

join_feature = 'C:/users/brad griffith/dropbox/NLA analyses/data/shapefiles/nlaWatersheds/NLA_Sites_Sampled_DrainageBasins.shp' 
merge_join = roadmerge.replace('merge','join')
arcpy.SpatialJoin_analysis(roadmerge,join_feature,merge_join)


arcpy.env.workspace = 'C:/users/brad griffith/documents/roadclip/'
clip_feature = 'C:/users/brad griffith/dropbox/NLA analyses/data/shapefiles/nlaBuffers/NLA10kmBuff.shp' 
xy_tolerance = ""
target_feature = 'roadwshedjoin.shp'
out_feature_class = target_feature.replace('join','bufclip') 
arcpy.Clip_analysis(target_feature,clip_feature,out_feature_class,xy_tolerance)



