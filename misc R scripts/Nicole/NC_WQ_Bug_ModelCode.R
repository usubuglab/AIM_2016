#############################################################################

################               TN TP models                  ###############

#############################################################################
setwd("C:/Users/Nicole/Desktop/TNTP_Model_2014/TNTP_2014Addition")

#directory="M:\\"#to allow for multiple machines to run from share
#WD=sprintf("%sGIS\\Delineation\\Layers_HawklabCopyMay2012\\WQmodels\\",directory)
#setwd(WD)

##WARNING##
#print('If slopes were generated using the rasters in Natural\\Slope\\Slope (see HUCpathSLP below),
#      then need to divide value by 10000 before using as input.')


#RandomForests
#run after GIS stats compiled (see below for auto generation of GME gis processing script)


#1. read  csv as variable

TNTP.ws.pred.final=read.csv("TNTP.ws.pred.2014AllSites.csv")


#2. load R objects (package of formula, RF original input, RF model, etc)

load("rf11accTP12m2c")#TP
rf11accTP12m2c

load("rf24wmTN12a")#TN
rf24wmTN12a


#load("M:\\GIS\\Delineation\\Layers_HawklabCopyMay2012\\WQmodels\\Rfiles\\WinterRFModel.R")#WinterRef.rf
#3. load RF (download if needed)
library(randomForest)
#not needed - running new RF model#EC_UTblm_11_RF = randomForest(Y~., data=RFdriftVAR,importance=T,corr.bias=T,na.action=na.omit)
#4. run RF in predict mode

TN.pred.output=predict(rf24wmTN12a,TNTP.ws.pred.final)
data.frame(TN.pred.output)#note, TN for 2012 is provisional PRISM precip data 
write.csv(TN.pred.output, file="TN.pred.output.2014All.csv")

####TP####
TP.pred.output=predict(rf11accTP12m2c,TNTP.ws.pred.final)
data.frame(TP.pred.output)
write.csv(TP.pred.output, file="TP.pred.output.2014All.csv")





#############################################################################

################                EC Model                ###############

#############################################################################



#########Code for R to calculate predicted Conductivity:

#setting working directory and loading conductivity model
setwd("C:\\Users\\Nicole\\Desktop\\2014_Work_NV_OE_MMI\\PredictConductivity")
load("rf17bCnd9.rdata")

#read in watershed 
ws.preds=read.csv("2014Addition\\EC.ws.preds.2014Add.csv",row.names="SiteCode")

#load randomForest package, make conductivity predictions, export predictions as csv to working directory
library(randomForest)
PrdCond=predict(rf17bCnd9, newdata=ws.preds)
write.csv(file="2014Addition\\2014Add.PredCond.csv", cbind(PrdCond))

