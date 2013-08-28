#comparison of linear regression and randomForest regression tree analysis of water chemistry as a function of landcover
#5/8/2013- now includes function easy.formula(), which generates a model formula from character vectors of response and predictor variables.

#also includes code to calculate correlation matrices for input variables, run pca on multiple landcover variables, and fit a glm of water chemistry using pc scores instead of the original predictor variables.

#set wd to dropbox data folder

library(rpart)
library(randomForest)
library(mgcv)

#function to create model formula from vectors of variables
easy.formula<-function(response,predictors){  easy <- as.formula(paste(paste(response," ~"), paste(predictors, collapse= "+"))); return(easy)}
#example: 
#easy.formula("log(NTL)",c(pc.cov.names,intrinsic.vars))



#reading in the data:
nla<-read.csv('nla_all_connectivity.csv',header=T) #updated with road metrics


#################################################################################333

#SELECTING THE INITIAL SUBSET OF LAKES

#uncomment the next line to select only non-reference lakes
#nla<-subset(nla,nla$SITE_TYPE == 'PROB_Lake')

#uncomment the next line to select only natural (non-manmade lakes)
#nla<-subset(nla,nla$LAKE_ORIGIN.x=='NATURAL')

#uncomment the next line to select only manmade lakes
#nla<-subset(nla,nla$LAKE_ORIGIN.x=='MAN-MADE')


####################################################################################

#################################################################################333

#SELECTING connectivity types

HW<-ifelse(nla$connectivity_class=='HW',TRUE,FALSE)
SE<-ifelse(nla$connectivity_class=='SE',TRUE,FALSE)
ST<-ifelse(nla$connectivity_class=='ST',TRUE,FALSE)
STLA<-ifelse(nla$connectivity_class=='STLA',TRUE,FALSE)

#change this line to select different classes
#nla<-nla[na.omit(HW),]


####################################################################################

pc.cov.names<-names(nla)[grep('PCT',names(nla))] 	 #extracting landcover variable names
pc.cov.bufr<-pc.cov.names[grep('BUFR',pc.cov.names)] #buffer scale landcover
pc.cov.bsn<-pc.cov.names[grep('BSN',pc.cov.names)] 	 #basin-scale landcover
rch<-names(nla)[grep('RCH_',names(nla))] 			 #riparian veg
connectivity<-'connectivity_class'


#the roadvars list of road-related variables will have to be modified depending on which variables are available in NLA_all.csv
roadvars<-c('hipwRoads','hinRoads','RDIS_COND','RES_ROADS','RES_BRIDGES','mean.pct.imp_BUFR','roadDens_BUFR','roadlen_BUFR','mean.min.dist_inBUFR','mean.pct.imp_BSN','roadDens_BSN','roadlen_BSN','mean.min.dist_inBSN')
intrinsic.vars<-c('ALBERS_X','ALBERS_Y','AREA_HA','LAKEPERIM','DEPTHMAX','SLD','ELEV_PT','BasinAreaToLakeArea_HA','LakePerimToLakeArea')
CHEM.names=c('COND','CHLA','PTL','DOC','NTL','TURB') #response variable names

#the character vectors above are just meant to simplify the process of selecting groups of variables for model specification

#the next five lines convert two categorical variables: riparian disturbance and a road development metric, into ordinal form.
nla$RDIS_COND<-as.numeric(nla$RDIS_COND)
nla$RES_ROADS<-ifelse(nla$RES_ROADS=='H',4,nla$RES_ROADS)
nla$RES_ROADS<-ifelse(nla$RES_ROADS>1,nla$RES_ROADS-1,nla$RES_ROADS)
nla$RES_BRIDGES<-ifelse(nla$RES_BRIDGES=='H',4,nla$RES_BRIDGES)
nla$RES_BRIDGES<-ifelse(nla$RES_BRIDGES>1,nla$RES_BRIDGES-1,nla$RES_BRIDGES)


#the next few lines generate model formulas for each subset of variables at both scales, with and without connectivity.
#

bsn.formula=easy.formula("log(PTL)",c(pc.cov.bsn))
bfr.formula=easy.formula("log(PTL)",c(pc.cov.bufr))

bsn.formula.connect=easy.formula("log(PTL)",c(pc.cov.bsn,connectivity))
bfr.formula.connect=easy.formula("log(PTL)",c(pc.cov.bufr,connectivity))

road.formula=easy.formula("log(PTL)",roadvars)
road.formula.connect=easy.formula("log(PTL)",roadvars)

intrinsic.formula=easy.formula("log(PTL)",c(intrinsic.vars)) 
intrinsic.formula.connect=easy.formula("log(PTL)",c(intrinsic.vars,roadvars)) 

big.formula=easy.formula("log(PTL)",c(pc.cov.bsn,pc.cov.bufr,intrinsic.vars,roadvars,'SE','HW'))
big.formula.connect=easy.formula("log(PTL)",c(pc.cov.bsn,pc.cov.bufr,intrinsic.vars,roadvars,connectivity,'SE','HW'))


#section to test association between conductivity and specific ions
#using pct_forest,pct_crops,roadDens_BSN,and maxdepth
chem<-read.csv('c:/users/brad griffith/desktop/watqual.csv')
nla.chem<-merge(nla,chem)

cor.mat<-cor(as.matrix(subset(nla.chem,select=c('COND','NA.','MG','CA')))) #so cond is most closely associated with Na across the board.
pairs(as.matrix(subset(nla.chem,select=c('COND','NA.','MG','CA'))))

cond.model<-easy.formula('log(COND)',c('PCT_FOREST_BSN','PCT_CROPS_BSN','roadDens_BSN','DEPTHMAX'))
na.model<-easy.formula('log(NA.)',c('PCT_FOREST_BSN','PCT_CROPS_BSN','roadDens_BSN','DEPTHMAX'))
ca.model<-easy.formula('log(CA)',c('PCT_FOREST_BSN','PCT_CROPS_BSN','roadDens_BSN','DEPTHMAX'))
mg.model<-easy.formula('log(MG)',c('PCT_FOREST_BSN','PCT_CROPS_BSN','roadDens_BSN','DEPTHMAX'))

cond.glm<-glm(cond.model,data=nla.chem)
na.glm<-glm(na.model,data=nla.chem)
ca.glm<-glm(ca.model,data=nla.chem)
mg.glm<-glm(mg.model,data=nla.chem)

cond.rf<-randomForest(cond.model,nla.chem,na.action=na.omit)
na.rf<-randomForest(na.model,nla.chem,na.action=na.omit)
na.glm2<-glm(log(NA.)~PCT_FOREST_BSN+PCT_CROPS_BSN+DEPTHMAX,data=nla.chem,na.action=na.omit)
ca.rf<-randomForest(ca.model,nla.chem,na.action=na.omit)
mg.rf<-randomForest(mg.model,nla.chem,na.action=na.omit)



#here we select a subset of the nla_all dataset containing only variables of interest.
#we then remove missing values- it might be better to try and impute them.
#note that the more variables we have in this dataset, the smaller our sample size.

big.dat<-subset(nla,select=c(CHEM.names,pc.cov.names,intrinsic.vars,roadvars,connectivity))
big.dat<-na.omit(big.dat)


big.dat$SE<-ifelse(big.dat$connectivity_class=='SE',1,0)
big.dat$HW<-ifelse(big.dat$connectivity_class=='HW',1,0)



#each block of code below implements the following steps:
#1) fit a linear model of log conductivity as a function of all other variables
#2) carry out backwards stepwise regression on the model
#3) run a randomForest analysis on the same variable set.

# 
# big.glm<-glm(big.formula,data=big.dat)
# big.step<-step(big.glm,direction='backward')
# big.rf<-randomForest(big.formula,data=big.dat,importance=1)
# 
# road.glm<-glm(road.formula,data=big.dat)
# road.step<-step(road.glm,direction='backward')
# road.rf<-randomForest(road.formula,data=big.dat,importance=1)
# 
# 
# bufr.glm<-glm(bfr.formula,data=big.dat)
# bufr.step<-step(bufr.glm,direction='back')
# bufr.rf<-randomForest(bfr.formula,data=big.dat,importance=1)
# 
# bsn.glm<-glm(bsn.formula,data=big.dat)
# bsn.step<-step(bsn.glm,direction='back')
# bsn.rf<-randomForest(bsn.formula,data=big.dat,importance=1)
# 
# intrinsic.glm<-glm(intrinsic.formula,data=big.dat)
# intrinsic.step<-step(intrinsic.glm,direction='back')
# intrinsic.rf<-randomForest(intrinsic.formula,data=big.dat,importance=1)
# 
# 
# #AIC model comparison.
# glm.ranking=AIC(big.step,bufr.step,bsn.step,intrinsic.step,road.step)
# 
# 
# #variable importance plot
# varImpPlot(big.rf)
# 
# 
# #plot of partial effect for 1 predictor variable on respones
# partialPlot(big.rf,nla,'PCT_FOREST_BSN')


#######################################################################################################
#this code does the same thing, but with all the model formulas that include connectivity
#1) fit a linear model of log conductivity as a function of all other variables
#2) carry out backwards stepwise regression on the model
#3) run a randomForest analysis on the same variable set.


big.glm<-glm(big.formula,data=big.dat)
big.step<-step(big.glm,direction='backward')
big.rf<-randomForest(big.formula.connect,data=big.dat,importance=1)

road.glm<-glm(road.formula,data=big.dat)
road.step<-step(road.glm,direction='backward')
road.rf<-randomForest(road.formula.connect,data=big.dat,importance=1)


bufr.glm<-glm(bfr.formula,data=big.dat)
bufr.step<-step(bufr.glm,direction='back')
bufr.rf<-randomForest(bfr.formula.connect,data=big.dat,importance=1)

bsn.glm<-glm(bsn.formula,data=big.dat)
bsn.step<-step(bsn.glm,direction='back')
bsn.rf<-randomForest(bsn.formula.connect,data=big.dat,importance=1)

intrinsic.glm<-glm(intrinsic.formula,data=big.dat)
intrinsic.step<-step(intrinsic.glm,direction='back')
intrinsic.rf<-randomForest(intrinsic.formula.connect,data=big.dat,importance=1)


#AIC model comparison.
glm.ranking=AIC(big.step,bufr.step,bsn.step,intrinsic.step,road.step)


#variable importance plot
varImpPlot(big.rf,main='global randomforest with natural lakes, log(Phosphorous)')


#plot of partial effect for 1 predictor variable on respones
partialPlot(big.rf,nla,'PCT_FOREST_BSN')

#top variables from each scale subset: 
#PCT_FOREST_BUFR
#PCT_FOREST_BSN

best.var.glm<-lm(log(NTL)~PCT_FOREST_BUFR+PCT_FOREST_BSN+ALBERS_X+ALBERS_Y+ELEV_PT,data=big.dat)
best.var.rf<-randomForest(log(NTL)~PCT_FOREST_BUFR+PCT_FOREST_BSN+ALBERS_X+ALBERS_Y+ELEV_PT,data=big.dat)
#best.var.rf is just as informative as the big rf with all landcover variables. 


#try putting in a few apriori likely candidates: forest cover, open water, and developed
apriori.glm<-glm(log(NTL)~PCT_FOREST_BSN+PCT_WATER_BSN+PCT_DEVELOPED_BSN+PCT_FOREST_BUFR+PCT_WATER_BUFR+PCT_DEVELOPED_BUFR,data=big.dat)
AIC(best.var.glm,apriori.glm)

#adding in a couple important sounding variables like pct developed and pct water makes the model much worse.

#will try extracting landcover principle components and sticking them in.

bufr.prcomp.formula<-easy.formula('',pc.cov.bufr)
bufr.princomp<-princomp(bufr.prcomp.formula,data=big.dat)

bsn.prcomp.formula<-easy.formula('',pc.cov.bsn)
bsn.princomp<-princomp(bsn.prcomp.formula,data=big.dat)

summary(bufr.princomp)
summary(bsn.princomp)
#so it takes more than two axes to explain most of the variation in landcover at either scale.


#we can try fitting a glm with the scores from the bsn landcover pca as response variables
logCond<-log(big.dat$COND)
princomp.scores.glm<-glm(logCond~bsn.princomp$scores)
princomp.scores.step<-step(princomp.scores.glm,direction='back')

#make a model with just the bsn-scale forest cover as predictor
bsn.forest.glm<-glm(log(NTL)~PCT_FOREST_BSN,data=big.dat)

#rank the pca model vs. the general basin scale model 
AIC(bsn.glm,princomp.scores.step,bsn.forest.glm)

#so the model with pca scores has a higher aic (worse fit) than the original model with all the bsn scale landcover 
#variables.

#make a correlation matrix for landcover variables
require(Hmisc)

bufr.dat<-subset(big.dat,select=pc.cov.bufr)#selecting just the landcover variables at each scale
bsn.dat<-subset(big.dat,select=pc.cov.bsn)

#make correlation matrix object for bufr-scale landcover
bufr.cormat<-rcorr(as.matrix(bufr.dat))

#highlight variables that are significantly positively correlated with each other
sig.pos.mat<-bufr.cormat$r>0&bufr.cormat$P<0.05
#highlight variables that are significantly negatively correlated with each other
sig.neg.mat<-bufr.cormat$r<0&bufr.cormat$P<0.05



#example code for generating a single regression tree with conductivity data
nla.regtree<-rpart(big.formula,data=big.dat,na.action=na.omit)
plot(nla.regtree,main='regression tree of Conductivity = landcover, roads, and geomorphology')
text(nla.regtree)
par(mfcol=c(1,3))
rsq.rpart(nla.regtree) #output fit diagnostics for the model.
nla.prunetree<-prune(nla.regtree,cp=.026) #this cp value cutoff was based on a plot using the printcp() and plotcp() functions. code not shown.
plot(nla.prunetree,main='regression tree of Conductivity = landcover, roads, and geomorphology')
text(nla.prunetree)




#principal components analysis to reduce number of response variables
#extracting the first principal component from database of five chemistry variables: cond, chla, ptl, doc, ntl_ppm
chem.vars<-c('COND','CHLA','PTL','DOC','NTL_PPM')
chem.df<-data.frame(logcond=log(nla$COND),logchla=log(nla$CHLA),logptl=log(nla$PTL),logdoc=log(nla$DOC),logntl=log(nla$NTL_PPM))
chem.princomp<-princomp(~.,data=chem.df) #run the principal components analysis
plot(chem.princomp) #first principal component explains 71% of the variance in the above five variables.


first.comp<-chem.princomp$scores[,1] #extracting first principal component for use in regression/randomforest



nla.sub<-subset(nla,select=c(pc.cov.names,'LAT_DD','LON_DD','URBAN','LAKE_ORIGIN_00_1Manmade','AREAHA'))
nla.sub<-nla.sub[row.names(nla.sub) %in% names(first.comp),] #remove rows with missing data


princomp.lm<-lm(first.comp~.,data=nla.sub) #fit linear model with all landcover vars + lat/lon
princomp.rf<-randomForest(first.comp~.,data=nla.sub,na.action=na.omit) #fit random Forest model


summary(princomp.lm)
princomp.rf
varImpPlot(princomp.rf,main='randomForest analysis with first principal \ncomponent of water chemistry as response')

