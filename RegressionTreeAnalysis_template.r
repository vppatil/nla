#comparison of linear regression and randomForest regression tree analysis of water chemistry as a function of landcover
#5/8/2013- now includes function easy.formula(), which generates a model formula from character vectors of response and predictor variables.


#this script includes code to:
#1) run a multi-model glm analysis contrasting landcover and road metrics as predictors of water chemistry
#2) use stepwise regression to reduce model size
#3) fit the same models using randomForest regression tree models
#4) summarize the output of both analyses, including plots showing some of the more important results.


#set wd to dropbox data folder before running code
#Currently, the response variable is set as 'total phosphorous' (PTL)
response ='log(COND)' #change this to change the response used in all analyses below.

require(rpart)
require(randomForest)
require(mgcv)
require(fields)

#function to create model formula from vectors of variables
easy.formula<-function(response,predictors){  easy <- as.formula(paste(paste(response," ~"), paste(predictors, collapse= "+"))); return(easy)}
#example: 
#easy.formula("log(PTL)",c(pc.cov.names,intrinsic.vars))

#reading in the data:
nla<-read.csv('newNLAall.csv',header=T,stringsAsFactors=FALSE) #updated with road metrics and lake connectivity class
nla<-nla[!duplicated(nla$SITE_ID),]
dim(nla)

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

HW<-ifelse(nla$Class=='HW',TRUE,FALSE)
SE<-ifelse(nla$Class=='SE',TRUE,FALSE)
ST<-ifelse(nla$Class=='ST',TRUE,FALSE)
STLA<-ifelse(nla$Class=='STLA',TRUE,FALSE)

#change this line to select different classes
#nla<-nla[na.omit(HW),]


####################################################################################
#these vectors contain lists of variables corresponding to different hypotheses
#roadvars are road metrics
#intrinsic vars are related to basin morphology and landscape position
#pc.cov is short for percent cover (landcover variables)
#the suffix _BSN means a variable that was estimated for the area drained by a lake
#the suffix _BFR means a variable that was estimated for a 200m buffer around a lake

#response variables are conductivity, chlorophyll a, total phosphorous (PTL),total nitrogen (NTL), DOC, and turbidity (TURB)


pc.cov.names<-names(nla)[grep('PCT',names(nla))] 	 #extracting landcover variable names
pc.cov.bufr<-pc.cov.names[grep('BUFR',pc.cov.names)] #buffer scale landcover
pc.cov.bsn<-pc.cov.names[grep('BSN',pc.cov.names)] 	 #basin-scale landcover
rch<-names(nla)[grep('RCH_',names(nla))] 			 #riparian veg
connectivity<-'Class'

roadvars<-c('mean.pct.imp_BUFR','roadDens_BUFR','roadlen_BUFR','mean.min.dist_inBUFR','mean.pct.imp_BSN','roadDens_BSN','roadlen_BSN','mean.min.dist_inBSN')
intrinsic.vars<-c('ALBERS_X','ALBERS_Y','AREA_HA','LAKEPERIM','DEPTHMAX','SLD','ELEV_PT','BasinAreaToLakeArea_HA','LakePerimToLakeArea')
CHEM.names=c('COND','CHLA','PTL','DOC','NTL','TURB') #response variable names

#the character vectors above are just meant to simplify the process of selecting groups of variables for model specification

#the next five lines convert two categorical variables: riparian disturbance and a road development metric, into ordinal form.
# nla$RDIS_COND<-as.numeric(nla$RDIS_COND)
# nla$RES_ROADS<-ifelse(nla$RES_ROADS=='H',4,nla$RES_ROADS)
# nla$RES_ROADS<-ifelse(nla$RES_ROADS>1,nla$RES_ROADS-1,nla$RES_ROADS)
# nla$RES_BRIDGES<-ifelse(nla$RES_BRIDGES=='H',4,nla$RES_BRIDGES)
# nla$RES_BRIDGES<-ifelse(nla$RES_BRIDGES>1,nla$RES_BRIDGES-1,nla$RES_BRIDGES)


#the next few lines generate model formulas for each subset of variables at both buffer and basin scales scales, with and without connectivity.


bsn.formula=easy.formula(response,c(pc.cov.bsn))
bfr.formula=easy.formula(response,c(pc.cov.bufr))

bsn.formula.connect=easy.formula(response,c(pc.cov.bsn,connectivity))
bfr.formula.connect=easy.formula(response,c(pc.cov.bufr,connectivity))

road.formula=easy.formula(response,roadvars)
road.formula.connect=easy.formula(response,roadvars)

intrinsic.formula=easy.formula(response,c(intrinsic.vars)) 
intrinsic.formula.connect=easy.formula(response,c(intrinsic.vars,roadvars)) 

big.formula=easy.formula(response,c(pc.cov.bsn,pc.cov.bufr,intrinsic.vars,roadvars))
big.formula.connect=easy.formula(response,c(pc.cov.bsn,pc.cov.bufr,intrinsic.vars,roadvars,connectivity,'SE','HW'))


#here we select a subset of the nla_all dataset containing only variables of interest.
#we then remove missing values- it might be better to try and imput them.
#note that the more variables we have in this dataset, the smaller our sample size.

big.dat<-subset(nla,select=c(CHEM.names,pc.cov.names,intrinsic.vars,roadvars,connectivity))
big.dat<-na.omit(big.dat)

big.dat$SE<-ifelse(big.dat$Class=='SE',1,0)
big.dat$HW<-ifelse(big.dat$Class=='HW',1,0)

###############################################################################################################
#each block of code below implements the following steps:

#1) fit a linear model of log conductivity as a function of all other variables
#2) carry out backwards stepwise regression on the model
#3) run a randomForest analysis on the same variable set.

big.glm<-glm(big.formula,data=big.dat)
big.step<-step(big.glm,direction='backward')
big.rf<-randomForest(big.formula,data=big.dat,importance=1)

road.glm<-glm(road.formula,data=big.dat)
road.step<-step(road.glm,direction='backward')
road.rf<-randomForest(road.formula,data=big.dat,importance=1,na.action=na.omit)

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
glm.ranking=AIC(big.step,bufr.step,bsn.step,intrinsic.step,road.step)
glm.ranking[order(glm.ranking[,2]),]

#variable importance plot example
X11()
varImpPlot(big.rf,main=paste('global randomforest for man-made lakes',response))

#some simple summary plots
#plot of partial dependence on one predictor with all others held constant
X11()
set.panel(m=2,n=2)
partialPlot(big.rf,big.dat,ALBERS_X,ylab=response)
partialPlot(big.rf,big.dat,PCT_FOREST_BSN,ylab=response)
partialPlot(big.rf,big.dat,PCT_FOREST_BUFR,ylab=response) 
partialPlot(big.rf,big.dat,PCT_CONIF_BUFR,ylab=response) 

#simple scatterplots
X11()
set.panel(m=2,n=2)
plot(easy.formula(response,'(ALBERS_X)'),data=big.dat)
plot(easy.formula(response,'log(PCT_FOREST_BSN)'),data=big.dat)
plot(easy.formula(response,'log(PCT_FOREST_BUFR)'),data=big.dat)
plot(easy.formula(response,'log(PCT_CONIF_BUFR)'),data=big.dat)

#anova for association with lake connectivity class

X11()
set.panel(m=1,n=1)
anova.formula=easy.formula(response,'Class')
connectivity.anova<-lm(anova.formula,data=big.dat)
anova(connectivity.anova)
summary(connectivity.anova)
boxplot(anova.formula,data=big.dat,main='water chemistry by connectivity class',ylab=response)

##########################################################################
#creating breakdown of conductivity by other water quality variables
watqual<-read.csv('watqual.csv')

cond.parts.rf<-randomForest(log(COND)~NA.+CL+MG+CA+K+NH4ION,data=watqual)
X11()
varImpPlot(cond.parts.rf)
