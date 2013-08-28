

condor.run.dir  = 'D:/CondorLakeConn'
all.output.path = 'D:/Dropbox/NLA analyses (1)/output/nhd.connectivity'

all.run.zips = Sys.glob(file.path(condor.run.dir,'*/*.zip'))



for(i in 1:length(all.run.zips)){
  
  cat(i, "\n")
  
  unzip(all.run.zips[i], exdir=all.output.path)
}