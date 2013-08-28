## Plot some simple relationships between key parameters

d = read.table('../data/NLA_all.csv', sep=',', header=TRUE)

response.var = 'DOC'

driver.vars = c('DEPTHMAX', 'AREA_HA', 'PCT_FOREST_BSN', 'PCT_AGRIC_BSN')


i = 1
tiff(paste('../output/simpleScatterplots/', driver.vars[i], '-', response.var, '.tiff', sep=''), 
     width=3200, height=3200, res=300, compression="lzw")
par(mfrow=c(2, 1), mar=c(4, 4, 2, 2))
plot(d[[driver.vars[i]]], d[[response.var]], ylab=response.var, xlab=driver.vars[i])
plot(d[[driver.vars[i]]], d[[response.var]], ylab=response.var, xlab=driver.vars[i], log='xy')
dev.off()

