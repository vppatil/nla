# Conodrize the ARC lake connectivity analysis!
source('htcondor-R.R')


nhd.files.dir   = 'D:/BigDatasets/Nhd'
condor.run.dir  = 'D:/CondorLakeConn'
codeDir         = getwd()

#Get list of NHD zip files
nhd.zipfiles = Sys.glob(file.path(nhd.files.dir,'*.zip'))

#Tools and code that need to accompany run
tools = file.path(getwd(), c('tools/bzip2.dll','tools/unzip.exe',
                             'tools/zip.exe', 'tools/zip32z64.dll'))
code = file.path(getwd(), c('CSI Tools.tbx','LakeConnectivityCondor.py'))

executable = file.path(getwd(), 'condorRun.bat')

#Create and submit 

for(i in 1:length(nhd.zipfiles)){
#for(i in 1:1){
  runDir = file.path(condor.run.dir, i)
  
  if(file.exists(runDir) && file.exists(file.path(runDir,'out.zip'))){
    next
    
  }else if(!file.exists(runDir)){
    dir.create(runDir)
  }
  
  cat(i, "\n")
  
  input.files = c(tools, code, nhd.zipfiles[i])
  
  condor.write.submit(file.path(runDir,'submit.cmd'), executable, input.files=input.files)
  setwd(runDir)
  system('condor_submit submit.cmd')
  setwd(codeDir)
  
}




