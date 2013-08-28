#initial script for looking at trophic status data
#vp 1/17

#change working directory as required
setwd('~/Documents/GLEON\ files/NLA_Jan_plots/')

nla<-read.csv('NLA.csv')
str(nla)
sub<-subset(nla, select=c('SITE_ID','AREAHA','SECCHI')) #convenient subset for later


sub$loga=log(sub$AREAHA) #make log variables for later use
sub$logs=log(sub$SECCHI)


#need zoop data
zoop<-read.csv('nla_zoop_count.csv',header=TRUE)
trophic<-read.csv('Lake_Trophic_Cond.csv') #trophic status datafile- not used. 
#both these files can be downloaded from the nla website.

all<-merge(trophic,sub)
all=na.omit(all)
all.sub<-subset(all,select=c('AREAHA','SECCHI'))


############################################### chlorophyll a (CHLA) vs. total phosphorous (PTL)

#initial plots
area.cut=mean(all$loga) #get lake area cutoff value
all$small=ifelse(all$loga<area.cut,1,0) #if small = 1, lake is smaller than log area cutoff

secchi.cut=mean(all$logs)
all$clear=ifelse(all$logs>secchi.cut,1,0) #if clear = 1, lake is deeper than log area cutoff 

 par(mfrow=c(2,2))
 plot(log(CHLA)~log(PTL),data=all,subset=clear==0 | small == 0)
 plot(log(CHLA)~log(PTL),data=all,subset=clear==0 | small == 1)
 plot(log(CHLA)~log(PTL),data=all,subset=clear==1 | small == 0)
 plot(log(CHLA)~log(PTL),data=all,subset=clear==1 | small == 1)
all$clearSmall=factor(paste(all$clear,all$small,sep='')) #making a new factor variable combining secchi and size classifications


#looks like there is a pretty strong linear relationship for all of them. the grouping variable may not be necessary.

#trying a simple random effects model with clearSmall variable as a grouping factor
library(nlme)
chlp.group<-groupedData(log(CHLA)~log(PTL)|clearSmall,data=all)
plot(chlp.group) #lattice plot- kind of smushed

chlp.nlme<-lme(chlp.group) #fitting the mixed effects model
summary(chlp.nlme)        #doesn't look like the random effect explained much variance


#so we'll try lumping the data together and looking at the relationship between p and chla for the whole dataset
nogroup<-glm(log(CHLA)~log(PTL),data=all)
summary(nogroup)

#plot of chla vs. phosphorous for all data
par(mfrow=c(1,1))
plot(log(CHLA)~log(PTL),data=all,xlab='log(Total P)',main='chlorphyll vs. phosphorous, all data')
abline(nogroup,col='red')
#simple linear plot of the data.the linear trend is highly significant, but looks nonlinear/weird at very large phosphorous values.
#interpretation might be complicated since chla and secchi covary.

#we can check for statistical departure from a linear relationship with gam()
install.packages('gam')
library(gam)
nogroup.gam<-gam(log(CHLA)~s(log(PTL)),data=all)
summary(nogroup.gam)
AIC(nogroup.gam,nogroup) #so there is a highly significant nonlinear relationship. We can try fitting it with a quadratic

nogroup.quadratic<-lm(log(CHLA)~I(log(PTL)^2)+log(PTL),data=all)
AIC(nogroup,nogroup.quadratic)
#deltaAIC is almost 50, and the quadratic model has the smaller AIC value

#so the quadratic model fits the data much better than simple linear regression. It might be worth trying to identify specific types of lakes that are responsible
#for this pattern


################################ ZOOPLANKTON VS. CHLA #################################################

#zoops have a sample volume, an abundance, and a taxanomic clas
#there are multiple sample no per site id
#need to see how often sites were resampled on multiple dates
#best plan seems to be sum abundance across sample id, then average sample sums by site.

#are there more than one sample per site? how are they distributed

#calc density based on pdf from nla website
zoop$cv=(zoop$VOL_COUNT/zoop$INIT_VOL) / 2
zoop$volSampled=zoop$DEPTH_OF_TOW * (0.0065^2 * pi)*1000 #65mm nets
zoop$adj_abund= (zoop$ABUND/(zoop$cv*zoop$volSampled)) * .001 #in number/ml

#adj_abundance = adjusted abundance = density estimate

#I will try working only with index samples - note that I still need to remove reference lakes
zoop.indexsamp<-subset(zoop,zoop$INDXSAMP_ZOOP =='YES' & zoop$VISIT_NO==1)
zoop.indexsamp<-subset(zoop.indexsamp,zoop.indexsamp$FLAG_ZOOP_FLD != 'X') #these are bad for some reason so we'll take them out

zoop.samp.sums<-cbind(xtabs(adj_abund~SAMPLE_ID,data=zoop.indexsamp))  #get sum of all zoops counted per sample
zoop.samp.sums<-data.frame(totAbund=zoop.samp.sums,SAMPLE_ID=row.names(zoop.samp.sums)) #reformat into a data frame with sample id

zoop.ids<-unique(zoop.indexsamp$SAMPLE_ID) #get a dataframe of sample ids and site ids
zoop.siteids<-zoop.indexsamp$SITE_ID[match(zoop.ids,zoop.indexsamp$SAMPLE_ID)]
zoop.id.dataframe<-data.frame(SAMPLE_ID=zoop.ids,SITE_ID=zoop.siteids)

zoop.counts<-merge(zoop.samp.sums,zoop.id.dataframe,by='SAMPLE_ID') #zoop abundance per sample.- site id has been merged in.
                       
  all.zoop=merge(all,zoop.counts)  #zoop data merged with other nla data

plot(log(all.zoop$CHLA),log(all.zoop$totAbund),xlab='log(chla)',ylab='log(zoop.density)') #plotting zooplankton density vs. chla for all data
#looks pretty messy.

#zoops and phosphorous also have a messy but apparent relationship
plot(log(all.zoop$PTL),log(all.zoop$totAbund),xlab='log(p)',ylab='log(zoop density)')

all.zoop=all.zoop[log(all.zoop$totAbund)>-1000000000000000,] #removing samples where volume was entered as 0 (density estimated as -Inf)

#plotting by subset
par(mfrow=c(2,2))
plot(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==0 | small == 0)
plot(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==0 | small == 1)
plot(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==1 | small == 0)
plot(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==1 | small == 1)


#we'll fit the regression for all data and each subset individually.
zoop.lm1<-lm(log(totAbund)~log(CHLA),data=all.zoop,na.action=na.omit)
zoop.lm2<-lm(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==0 | small == 0)
zoop.lm3<-lm(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==0 | small == 1)
zoop.lm4<-lm(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==1 | small == 0)
zoop.lm5<-lm(log(totAbund)~log(CHLA),data=all.zoop,subset=clear==1 | small == 1)

#all are significant because there is a crapload of data but R2 is almost 0.
#so overall there is a much stronger correlation between chla and p than between chla and zoops