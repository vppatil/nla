#downloads file GDB from NHD file directory 


#setwd('D:/Jake/GIS/')
zip.files=read.table('filenames.txt',header=FALSE,stringsAsFactor=FALSE)[,1]
zip.files=zip.files[grep('v210',zip.files)]
url = "ftp://nhdftp.usgs.gov/DataSets/Staged/SubRegions/FileGDB/HighResolution/"

dlDir = 'D:/BigDatasets/Nhd'

for(i in 187:length(zip.files)){
  download.url = paste(url,zip.files[i],sep='')
  
  filename = file.path(dlDir,zip.files[i])
  
  download.file(download.url, filename, 'curl')
  unzip(filename, exdir=dlDir)
}

