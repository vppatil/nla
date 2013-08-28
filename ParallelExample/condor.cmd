
executable = run.bat
transfer_input_files = .Renviron,roadproximitypoints.dbf,roadproximitypoints.prj,roadproximitypoints.shp,roadproximitypoints.shp.xml,roadproximitypoints.shx,tl_2012_13177_roads.dbf,tl_2012_13177_roads.shp,tl_2012_13177_roads.shp.xml,tl_2012_13177_roads.prj,tl_2012_13177_roads.shx,roadmindist_condor.r, lib/maptools_0.8-23.zip, lib/raster_2.1-25.zip, lib/rgdal_0.8-9.zip, lib/rgeos_0.2-17.zip, lib/shapefiles_0.7.zip, lib/sp_1.0-9.zip
arguments = 


universe   = vanilla
output = phase1.out
error = phase1.err
log = phase1.log
requirements = (TARGET.Arch == "X86_64") && ((TARGET.OpSys == "WINNT61"))


should_transfer_files = YES
+AccountingGroup = "Blah"
+WantFlocking = true
when_to_transfer_output = ON_EXIT
notification = never
queue
