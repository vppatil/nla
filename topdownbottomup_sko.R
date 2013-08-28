#initial script for looking at trophic status data
#vp 11/17

setwd('~/Documents/GLEON\ files/NLA_Jan_plots/')
#temporary
setwd("C:/users/sam/desktop")
nla<-read.csv('NLA.csv')
str(nla)
sub<-subset(nla, select=c('SITE_ID','AREAHA','SECCHI'))


sub$loga=log(sub$AREAHA)
sub$logs=log(sub$SECCHI)

plot(sub$AREAHA,sub$CHLA)

#need zoop data stil
zoop<-read.csv('nla_zoop_count.csv',header=TRUE)
plank_den = read.csv('nla_zoop_density.csv', header=TRUE)
trophic<-read.csv('Lake_Trophic_Cond.csv')

#manipulate zooplankton density data to only include zooplankton, not phytoplankton.
zoop_den = data.frame(subset(plank_den, plank_den$OTU_OE5 %in% zoop$OTU_CPH == "TRUE"))

#I went through and classified each level (zoop_den$OTU_OE5 ~ essentially the species identification) of the species as either an insect, rotifer, crustacean or unknown
#based on my own knowledge as well as quick google searches. There were 82 categories, and the classification
#was not too bad as in some instances, many species from the sampe genus (i.e. Daphnia) were
#obviously easy to classify. 

#rotifers = 1, 3, 4, 5, 8:21, 26, 27, 40, 41, 44, 45, 47, 50:60, 62, 63, 68, 69, 70, 71, 73, 74, 78, 80, 81, 82 
#crustacean_zooplankton = 2, 6, 7, 22, 24, 28, 29:39, 42, 43, 46, 48, 49, 61, 64, 65, 66, 67, 72, 75, 76, 77, 79,    
#insects = 23 
#unknown = 25

#this is a loop that creates a new column in the dataset ("zoop_den$category")
#that looks at the species name and classifies it based on my documentation of 
#which category of animal it belongs to

i = 1
for (i in 1:length(zoop_den$OTU_OE5)) {
  
if (zoop_den$OTU_OE5[i] %in% levels(factor(zoop_den$OTU_OE5)) 
    [c(1, 3, 4, 5, 8:21, 26, 27, 40, 41, 44, 45, 47, 50:60, 62, 63, 68, 69, 70, 71, 73, 74, 78, 80, 81, 82)]=="TRUE") {
  zoop_den$category[i] = "rotifer"
} else if (zoop_den$OTU_OE5[i] %in% levels(factor(zoop_den$OTU_OE5))
    [c(2, 6, 7, 22, 24, 28, 29:39, 42, 43, 46, 48, 49, 61, 64, 65, 66, 67, 72, 75, 76, 77, 79)]=="TRUE") {
  zoop_den$category[i] = "crustacean"
} else if (zoop_den$OTU_OE5[i] %in% levels(factor(zoop_den$OTU_OE5))
    [23] == "TRUE") {
  zoop_den$category[i] = "insect"  
} else if (zoop_den$OTU_OE5[i] %in% levels(factor(zoop_den$OTU_OE5))
    [25] == "TRUE") {
  zoop_den$category[i] = "unknown"      
}
i = i+1
}
           

all<-merge(trophic,sub)
#all<-merge(all,trophic)
all=na.omit(all)
all.sub<-subset(all,select=c('AREAHA','SECCHI'))

all.dist<-dist(all.sub)
all.k<-kmeans(all.sub,4)
all.sub<-cbind(all.sub,all.k$cluster)

all.clust<-hclust(all.dist)
plot(all.clust,xlab='')
groups<-cutree(all.clust,k=4)
rect.hclust(all.clust,k=4,border='red')

area.cut=mean(all$loga) #get lake area cutoff value
all$small=ifelse(all$loga<area.cut,1,0) #if small = 1, lake is smaller than log area cutoff

secchi.cut=mean(all$logs)
all$clear=ifelse(all$logs>secchi.cut,1,0) #if clear = 1, lake is deeper than log area cutoff 

 par(mfrow=c(2,2))
 plot(log(CHLA)~log(PTL),data=all,subset=clear==0 | small == 0)
 plot(log(CHLA)~log(PTL),data=all,subset=clear==0 | small == 1)
 plot(log(CHLA)~log(PTL),data=all,subset=clear==1 | small == 0)
 plot(log(CHLA)~log(PTL),data=all,subset=clear==1 | small == 1)

#looks like there is a pretty strong linear relationship for all of them. the grouping variable may not be necessary.


all$clearSmall=factor(paste(all$clear,all$small,sep='')) #making a new factor variable combining secchi and size classifications

par(mfrow=c(1,1))
plot(log(CHLA)~log(PTL),data=all,xlab='log(Total P)',main='chlorphyll vs. phosphorous, all data')

nogroup<-lm(log(CHLA)~log(PTL),data=all)
abline(nogroup,col='red')
#simple linear plot of the data.the linear trend is highly significant, but looks nonlinear/weird at very large phosphorous values.
#interpretation might be complicated since chla and secchi covary.

#trying a simple random effects model with clearSmall variable as a grouping factor
library(nlme)
chlp.group<-groupedData(log(CHLA)~log(PTL)|clearSmall,data=all)
plot(chlp.group) #lattice plot- kind of smushed

chlp.nlme<-lme(chlp.group) #fitting the mixed effects model

summary(chlp.nlme) #doesn't look like the random effect explained much



plot(log(CHLA)~log(PTL),data=all,subset=clear==0 | small == 0,type='p')
points(log(CHLA)~log(PTL),data=all,subset=clear==0 | small == 1,col='green')
points(log(CHLA)~log(PTL),data=all,subset=clear==1 | small == 0,col='blue')
points(log(CHLA)~log(PTL),data=all,subset=clear==1 | small == 1,col='red')



#so there is a linear relationship between CHLA and PTL. the question is whether
#this relationship is improved by the inclusion of lake size and/or secchi depth

#also, zoops have a sample volume, an abundance, and a taxanomic class- als
#there are multiple sample no per site id
#need to see how often sites were resampled on multiple dates
#best plan seems to be sum abundance across sample id, then average sample sums by site.


#Need to sum density of crustaceans per site, and account for multiple visits
zoop_den_crustacean = subset(zoop_den, zoop_den$category == "crustacean")

#a loop to combine the three identifiers (site, visit, sample type) 
#to give a unique ID for later use
for (i in 1:length(zoop_den_crustacean$SITE_ID)) {
  zoop_den_crustacean$uid[i] = 
    paste(zoop_den_crustacean$SITE_ID[i], 
          zoop_den_crustacean$VISIT_NO[i], 
          zoop_den_crustacean$SAMPLE_CATEGORY[i],
          sep = "")    
}

#sum densities from each unique site, visit, sample type
errors = c(which(zoop_den_crustacean$ABUND_OE5 == -999))
zoop_den_crustacean$ABUND_OE5[errors] = 0

crustacean_sum = tapply(X=zoop_den_crustacean$ABUND_OE5, INDEX=zoop_den_crustacean$uid, FUN=sum)
crustacean_sum = data.frame(as.numeric(crustacean_sum[]), row.names(crustacean_sum))
colnames(crustacean_sum) = c("totDen", "uid")


#add the site ID back to the data frame

for (i in 1:length(crustacean_sum$totDen)) {
  matches = match(crustacean_sum$uid, zoop_den_crustacean$uid)
  crustacean_sum$SITE_ID[i] = as.character(zoop_den_crustacean$SITE_ID[matches[i]]) 
}

#take the average of all samples per site

avg_crust_den_bysite = tapply(X= crustacean_sum$totDen, INDEX = crustacean_sum$SITE_ID, FUN=mean)
avg_crust_den_bysite = data.frame(as.numeric(avg_crust_den_bysite[]), row.names(avg_crust_den_bysite))
colnames(avg_crust_den_bysite) = c("avgDen", "SITE_ID")

#combine the crustacean data with other lake parameters by merging
#with dataset "nla" and picking the variables we want

#subCHLA = subset(nla, select = c('SITE_ID', 'CHLA', 'SECCHI', 'AREAHA', 'DOC', 'COLOR', 'PCT_DEVHIGH_BSN', 'PCT_FOREST_BSN'))
avg_crust_den_bysite = merge(nla, avg_crust_den_bysite, by = 'SITE_ID')

#Hypothesis: diversity
#Create a variable that counts how many species each lake has

for (i in 1:length(zoop_den_crustacean$SITE_ID)) {
  zoop_den_crustacean$num_species[i] = length(levels
                                              (factor(zoop_den_crustacean$OTU_OE5
                                                      [zoop_den_crustacean$SITE_ID == 
                                                         zoop_den_crustacean$SITE_ID[i]])))
}

#add a column with number of species by site
subSPEC = subset(zoop_den_crustacean, select = c('SITE_ID', 'num_species'))
avg_crust_den_bysite = merge(subSPEC, avg_crust_den_bysite, by = 'SITE_ID')
avg_crust_den_bysite = subset(avg_crust_den_bysite, !duplicated(avg_crust_den_bysite$SITE_ID))                   

#plot various variables
pdf(file = "Zooplankton.pdf")
par(mfrow=c(2,2))
plot(log(avg_crust_den_bysite$avgDen), log(avg_crust_den_bysite$CHLA), xlab = "Zooplankton Density", ylab = "Chl")
plot(log(avg_crust_den_bysite$avgDen), log(avg_crust_den_bysite$SECCHI), xlab = "Zooplankton Density", ylab = "Secchi")
plot(log(avg_crust_den_bysite$avgDen), log(avg_crust_den_bysite$AREAHA), xlab = "Zooplankton Density", ylab = "Lake Area")
plot(log(avg_crust_den_bysite$avgDen), log(avg_crust_den_bysite$DOC), xlab = "Zooplankton Density", ylab = "DOC")
dev.off()
#same plots but with zooplankton diversity instead of density
plot((avg_crust_den_bysite$num_species), log(avg_crust_den_bysite$CHLA), xlab = "Zooplankton Diversity", ylab = "Chl")
plot((avg_crust_den_bysite$num_species), log(avg_crust_den_bysite$SECCHI), xlab = "Zooplankton Diversity", ylab = "Secchi")
plot((avg_crust_den_bysite$num_species), log(avg_crust_den_bysite$AREAHA), xlab = "Zooplankton Diversity", ylab = "Lake Area")
plot((avg_crust_den_bysite$num_species), log(avg_crust_den_bysite$DOC), xlab = "Zooplankton Diversity", ylab = "DOC")

#Zoop density vs Chl By HUC regions
for (i in 1:18) {
  
plot(log(avg_crust_den_bysite$avgDen[avg_crust_den_bysite$HUC_2 == i]), log(avg_crust_den_bysite$CHLA[avg_crust_den_bysite$HUC_2 == i]), xlab = "Zooplankton Density", ylab = "Chl", main = i)

}

#are there more than one sample per site? how are they distributed

#zoop.samp.sums<-cbind(xtabs(ABUND_OE5~SAMPLE_ID,data=zoop_den))
#zoop.samp.sums<-data.frame(totAbund=zoop.samp.sums,SAMPLE_ID=row.names(zoop.samp.sums))
#zoop.ids<-subset(zoop,select=c('SAMPLE_ID','SITE_ID'))
#zoop.counts<-merge(zoop.samp.sums,zoop.ids)
                       
 #zoop.means<-tapply(zoop.counts$totAbund,zoop.counts$SITE_ID,mean)
 #zoop.means=data.frame(mean.abund=zoop.means,SITE_ID=names(zoop.means))
 
 #all.zoop=merge(all,zoop.means)


#so mean zoop abundance per site says nothing about chla  or tp                       
 plot((mean.abund)~log(CHLA),data=all.zoop)  
a<-lm(mean.abund~CHLA,data=all.zoop,na.action=na.omit)
  
#--------------------------------------------------------------
#Create a dataframe that excludes all species except Daphnia
daphnia = zoop_den_crustacean[grep(x = zoop_den_crustacean$OTU_OE5, pattern = "Daphnia|daphnia"), ]
#sum densities from each unique site, visit, sample type
daphnia_sum = tapply(X=daphnia$ABUND_OE5, INDEX=daphnia$uid, FUN=sum)
daphnia_sum = data.frame(as.numeric(daphnia_sum[]), row.names(daphnia_sum))
colnames(daphnia_sum) = c("totDen", "uid")
#add site ID back to the dataframe
for (i in 1:length(daphnia_sum$totDen)) {
  matches = match(daphnia_sum$uid, zoop_den_crustacean$uid)
  daphnia_sum$SITE_ID[i] = as.character(zoop_den_crustacean$SITE_ID[matches[i]]) 
}
#take the average density by site
avg_daphnia_den_bysite = tapply(X= daphnia_sum$totDen, INDEX = daphnia_sum$SITE_ID, FUN=mean)
avg_daphnia_den_bysite = data.frame(as.numeric(avg_daphnia_den_bysite[]), row.names(avg_daphnia_den_bysite))
colnames(avg_daphnia_den_bysite) = c("avgDen", "SITE_ID")

#add other NLA variables to the data frame
avg_daphnia_den_bysite = merge(nla, avg_daphnia_den_bysite, by = 'SITE_ID')

#make the same zooplankton plots, but now with Daphnia instead of all crustaceans
pdf(file = "Daphnia.pdf")
par(mfrow=c(2,2))
plot(log(avg_daphnia_den_bysite$avgDen), log(avg_daphnia_den_bysite$CHLA), xlab = "Zooplankton Density", ylab = "Chl")
plot(log(avg_daphnia_den_bysite$avgDen), log(avg_daphnia_den_bysite$SECCHI), xlab = "Zooplankton Density", ylab = "Secchi")
plot(log(avg_daphnia_den_bysite$avgDen), log(avg_daphnia_den_bysite$AREAHA), xlab = "Zooplankton Density", ylab = "Lake Area")
plot(log(avg_daphnia_den_bysite$avgDen), log(avg_daphnia_den_bysite$DOC), xlab = "Zooplankton Density", ylab = "DOC")
dev.off()
