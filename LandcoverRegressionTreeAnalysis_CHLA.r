)#comparison of linear regression and randomForest regression tree analysis of water chemistry as a function of landcover
#5/8/2013- now includes function easy.formula(), which generates a model formula from character vectors of response and predictor variables.
#hasn't been worked into the code yet.


setwd('~/Dropbox/NLA analyses (1)/data')
require(rpart)
require(randomForest)


#function to create model formula from vectors of variables
easy.formula<-function(response,predictors){  easy <- as.formula(paste(paste(response," ~"), paste(predictors, collapse= "+"))); return(easy)}
#example: 
test = easy.formula("log(CHLA)",c(pc.cov.names,intrinsic.vars))



#reading in the data:
nla<-read.csv('NLA_all.csv',header=T) #this file will need to be up to date with road density/proximity data in order for the script to work.
#need to merge in roadDensityByWatershed, roadDensityBynlabuffer, and nlabuffers_imperviousSurface.
#the watershed-scale impervious surface estimates and road proximity analyses are still ongoing.

setwd('~/Dropbox/NLA analyses (1)/output')
roadbywatershed = read.csv('roadDensityByWatershed.csv', header = TRUE)
roadbybybuffer = 
#################################################################################333

#SELECTING THE INITIAL SUBSET OF LAKES

#uncomment the next line to select only non-reference lakes
#nla<-subset(nla,nla$SITE_TYPE == 'PROB_Lake')
#uncomment the next line to select only natural (non-manmade lakes)
#nla<-subset(nla,nla$LAKE_ORIGIN.x=='NATURAL')
#uncomment the next line to select only manmade lakes
nla<-subset(nla,nla$LAKE_ORIGIN.x=='MAN-MADE')

#add in Jake's "class" variable that describes what kind of lake 
#it is (seepage, etc)
test = read.csv("newNLAall.csv", header = TRUE)
test_frame = merge(nla, test[ , c(2, 304)], 
                   by = 'Permanent_', 
                   all.y = FALSE, 
                   all.x = TRUE)
nla = test_frame




####################################################################################

pc.cov.names<-names(nla)[grep('PCT',names(nla))] 	 #extracting landcover variable names
pc.cov.bufr<-pc.cov.names[grep('BUFR',pc.cov.names)] #buffer scale landcover
pc.cov.bsn<-pc.cov.names[grep('BSN',pc.cov.names)] 	 #basin-scale landcover
rch<-names(nla)[grep('RCH_',names(nla))] 			 #riparian veg

pc.cov.bufr.dat<-subset(nla,select=pc.cov.bufr)
pc.cov.bufr.mat<-cor(pc.cov.bufr.dat)

#the roadvars list of road-related variables will have to be modified depending on which variables are available in NLA_all.csv
roadvars<-c('hipwRoads','hinRoads','RDIS_COND','RES_ROADS','RES_BRIDGES','pixels.w.impsurf_BUFR','mean.pct.imp_BUFR','roadDens_BUFR','roadlen_BUFR','sumRoadProxim_BUFR','wRoadDens','wRoadLen')
intrinsic.vars<-c('SITE_TYPE','ALBERS_X','ALBERS_Y','AREA_HA','LAKEPERIM','DEPTHMAX','SLD','ELEV_PT','BasinAreaToLakeArea_HA','LakePerimToLakeArea', 'Class')
CHEM.names=c('COND','CHLA','PTL','DOC','NTL_PPM') #response variable names

#the character vectors above are just meant to simplify the process of selecting groups of variables for model specification

#the next five lines convert two categorical variables: riparian disturbance and a road development metric, into ordinal form.
nla$RDIS_COND<-as.numeric(nla$RDIS_COND)
nla$RES_ROADS<-ifelse(nla$RES_ROADS=='H',4,nla$RES_ROADS)
nla$RES_ROADS<-ifelse(nla$RES_ROADS>1,nla$RES_ROADS-1,nla$RES_ROADS)
nla$RES_BRIDGES<-ifelse(nla$RES_BRIDGES=='H',4,nla$RES_BRIDGES)
nla$RES_BRIDGES<-ifelse(nla$RES_BRIDGES>1,nla$RES_BRIDGES-1,nla$RES_BRIDGES)
#Give classes a numeric value
#HW = 2, SE = 1, ST = 3, STLA = 4
new.levels = levels(factor(nla$Class)) 
new.levels = c(2,1,3,4)
levels(nla$Class) = new.levels





#each block of code below implements the following steps:
#1) select a subset of the nla dataset for analysis
#2) fit a linear model of log chlorophyll as a function of all other variables
#3) carry out backwards stepwise regression on the model
#4) run a randomForest analysis on the same variable set.



bsn.formula=easy.formula("log(CHLA)",c(pc.cov.bsn))
bfr.formula=easy.formula("log(CHLA)",c(pc.cov.bufr))
big.formula=easy.formula("log(CHLA)",c(pc.cov.bsn,pc.cov.bufr,intrinsic.vars))
intrinsic.formula=easy.formula("log(CHLA)",c(intrinsic.vars)) 
 

big.dat<-subset(nla,select=c('CHLA',pc.cov.names,intrinsic.vars))
big.dat<-na.omit(big.dat)

big.glm<-glm(big.formula,data=big.dat)
big.step<-step(big.glm,direction='backward')
big.rf<-randomForest(big.formula,data=big.dat,importance=1)

road.glm<-glm(road.formula,data=big.dat)
road.step<-step(road.glm,direction='backward')
road.rf<-randomForest(road.formula,data=big.dat,importance=1)

bufr.glm<-glm(bfr.formula,data=big.dat)
bufr.step<-step(bufr.glm,direction='back')
bufr.rf<-randomForest(bfr.formula,data=big.dat,importance=1)

bsn.glm<-glm(bsn.formula,data=big.dat)
bsn.step<-step(bsn.glm,direction='back')
bsn.rf<-randomForest(bsn.formula,data=big.dat,importance=1)

intrinsic.glm<-glm(intrinsic.formula,data=big.dat)
intrinsic.step<-step(intrinsic.glm,direction='back')
intrinsic.rf<-randomForest(intrinsic.formula,data=big.dat,importance=1)


#AIC model comparison.
glm.ranking=AIC(big.step,bufr.step,bsn.step,intrinsic.step)


#top variables from each scale subset: 


best.var.glm<-lm(log(CHLA)~PCT_FOREST_BUFR+PCT_FOREST_BSN+ALBERS_X+ALBERS_Y+ELEV_PT,data=big.dat)
best.var.rf<-randomForest(log(CHLA)~PCT_FOREST_BUFR+PCT_FOREST_BSN+ALBERS_X+ALBERS_Y+ELEV_PT,data=big.dat)
#best.var.rf is just as informative as the big rf with all landcover variables. 


#try putting in a few apriori likely candidates: forest cover, open water, and developed
apriori.glm<-glm(log(CHLA)~PCT_FOREST_BSN+PCT_WATER_BSN+PCT_DEVELOPED_BSN+PCT_FOREST_BUFR+PCT_WATER_BUFR+PCT_DEVELOPED_BUFR,data=big.dat)
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
logCHLA<-log(big.dat$CHLA)
princomp.scores.glm<-glm(logCHLA~bsn.princomp$scores)
princomp.scores.step<-step(princomp.scores.glm,direction='back')

#make a model with just the bsn-scale forest cover as predictor
bsn.forest.glm<-glm(log(CHLA)~PCT_FOREST_BSN,data=big.dat)

#rank the pca model vs. the general basin scale model 
AIC(bsn.glm,princomp.scores.step,bsn.forest.glm)

#so the model with pca scores has a higher aic (worse fit) than the original model with all the bsn scale landcover 
#variables.

#make a correlation matrix for landcover variables
install.packages("Hmisc")
library(Hmisc)
require(Hmisc)

bufr.dat<-subset(big.dat,select=pc.cov.bufr)#selecting just the landcover variables at each scale
bsn.dat<-subset(big.dat,select=pc.cov.bsn)

#make correlation matrix object for bufr-scale landcover
bufr.cormat<-rcorr(as.matrix(bufr.dat))

#highlight variables that are significantly positively correlated with each other
sig.pos.mat<-bufr.cormat$r>0&bufr.cormat$P<0.05
#highlight variables that are significantly negatively correlated with each other
sig.neg.mat<-bufr.cormat$r<0&bufr.cormat$P<0.05



#example code for generating a single regression tree with CHLA data
nla.regtree<-rpart(big.formula,data=big.dat,na.action=na.omit)
plot(nla.regtree,main='regression tree of CHLA = landcover, roads, and geomorphology')
text(nla.regtree)
par(mfcol=c(1,3))
rsq.rpart(nla.regtree) #output fit diagnostics for the model.
nla.prunetree<-prune(nla.regtree,cp=.026) #this cp value cutoff was based on a plot using the printcp() and plotcp() functions. code not shown.
plot(nla.prunetree,main='regression tree of CHLA = landcover, roads, and geomorphology')
text(nla.prunetree)



#variable importance plot
varImpPlot(big.rf, main = "All lakes, all variables")
varImpPlot(bsn.rf, main = "All lakes, basin variables")
varImpPlot(bufr.rf, main = "All lakes, buffer variables")
varImpPlot(intrinsic.rf, main = "All lakes, intrinsic variables")


#plot of partial effect for 1 predictor variable on respones
partialPlot(COND.rf,COND.sub,'PCT_FOREST')


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

