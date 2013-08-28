executable=D:/Dropbox/NLA analyses (1)/R code/ParallelExample/run.bat
arguments=
transfer_input_files=D:/BigDatasets/TIGER2012/tl_2012_78030_roads.dbf,D:/BigDatasets/TIGER2012/tl_2012_78030_roads.prj,D:/BigDatasets/TIGER2012/tl_2012_78030_roads.shp,D:/BigDatasets/TIGER2012/tl_2012_78030_roads.shp.xml,D:/BigDatasets/TIGER2012/tl_2012_78030_roads.shx,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/.Renviron,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/roadmindist_condor.r,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/lib/maptools_0.8-23.zip,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/lib/raster_2.1-25.zip,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/lib/rgdal_0.8-9.zip,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/lib/rgeos_0.2-17.zip,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/lib/shapefiles_0.7.zip,D:/Dropbox/NLA analyses (1)/R code/ParallelExample/lib/sp_1.0-9.zip,D:/Dropbox/NLA analyses (1)/data/shapefiles/nlaNHD/roadproximitypoints.csv,D:/Dropbox/NLA analyses (1)/data/shapefiles/nlaNHD/roadproximitypoints.dbf,D:/Dropbox/NLA analyses (1)/data/shapefiles/nlaNHD/roadproximitypoints.prj,D:/Dropbox/NLA analyses (1)/data/shapefiles/nlaNHD/roadproximitypoints.shp,D:/Dropbox/NLA analyses (1)/data/shapefiles/nlaNHD/roadproximitypoints.shp.xml,D:/Dropbox/NLA analyses (1)/data/shapefiles/nlaNHD/roadproximitypoints.shx
universe   = vanilla
output = phase1.out
error = phase1.err
log = phase1.log
requirements = (TARGET.Arch == "X86_64") && ((TARGET.OpSys == "WINNT61")) && (Machine == "hanson-i5.ad.wisc.edu")
should_transfer_files = YES
+WantFlocking = false
when_to_transfer_output = ON_EXIT
notification = never
queue
