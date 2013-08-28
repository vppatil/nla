## Run roadmindist on condor

source('htcondor-R.R')

roadproxshp = Sys.glob('D:/Dropbox/NLA analyses (1)/data/shapefiles/nlaNHD/roadproximitypoints.*')

roadfilepath<-'D:/BigDatasets/TIGER2012' #directory containing tiger shapefiles downloaded with ftpdownloadroads.r
roadfiles<-Sys.glob(file.path(roadfilepath, '*.shp'))
#roadfiles<-roadfiles[-grep('.xml',roadfiles)] 
#roadfiles<-gsub('.shp','',roadfiles)

codeDir = getwd()

libFiles = Sys.glob(file.path(codeDir, 'lib/*.zip'))
codeFile = file.path(codeDir, 'roadmindist_condor.r')
miscFiles = file.path(codeDir, '.Renviron')
executable = file.path(codeDir, 'run.bat')
  
runsLoc = 'D:/NlaCondorRuns'

for(i in 2:length(roadfiles)){
#for(i in 1:1){ 
  shpPattern = substr(roadfiles[i],1, nchar(roadfiles[i])-3)
  roadShp = Sys.glob(paste(shpPattern, '*', sep=''))
  
  filesToTransfer = c(roadShp, miscFiles, codeFile, libFiles, roadproxshp)
  
  runDir = file.path(runsLoc, i)
  
  dir.create(runDir)
  
  condor.write.submit(file.path(runDir,'submit.cmd'), executable, input.files=filesToTransfer)
  
  setwd(runDir)
  system('condor_submit submit.cmd')
  setwd(codeDir)
}

