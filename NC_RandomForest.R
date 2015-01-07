##############################################################################################################################################################################################
#Random Forest for Northern California to determine most important stressors to biological conditions
##############################################################################################################################################################################################

##loading package
library (randomForest)

##loading file
##For SFS abstract only use OE
NorCalBugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\ALL_BugModel_Results.csv")
NVMMI=NorCalBugs[,c(1,2,5,6)]
NVOE.0=NorCalBugs[,c(1:3)]

RF_Indicators=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Indicators_RFinput_5Jan2015.csv")

RFdata=merge(NVMMI,RF_Indicators, by.x="SiteCode", by.y="SITE_ID")

OE.RFdata=merge(NVOE.0,RF_Indicators, by.x="SiteCode", by.y="SITE_ID")

CorSubset=RFdata[,c(3,9:31)]
cor(CorSubset)

########################################################################
#2) All chem and Physical 
colnames(RFdata)
NV.all=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.all
varImpPlot(NV.all)
# Reduced
NV.all=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_TP+PH+XCMG+xbnk_h, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.all
varImpPlot(NV.all)

par(mfrow=c(2,4))
partialPlot(NV.all, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.all, RFdata,NTL, cex.main=1)
partialPlot(NV.all, RFdata,PTL, cex.main=1)
partialPlot(NV.all, RFdata,XCMG, cex.main=1)
partialPlot(NV.all, RFdata,OE_TP, cex.main=1)
partialPlot(NV.all, RFdata,xbnk_h, cex.main=1)
partialPlot(NV.all, RFdata,PH, cex.main=1)


OE.all=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=OE.RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
OE.all
varImpPlot(OE.all)
# Reduced
OE.subset=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+OE_TP+PH+XCMG+xbnk_h, data=OE.RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
OE.subset
varImpPlot(OE.subset)
par(mfrow=c(2,4))
partialPlot(OE.subset, OE.RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(OE.subset, OE.RFdata,NTL, cex.main=1)
partialPlot(OE.subset, OE.RFdata,PTL, cex.main=1)
partialPlot(OE.subset, OE.RFdata,XCMG, cex.main=1)
partialPlot(OE.subset, OE.RFdata,OE_TP, cex.main=1)
partialPlot(OE.subset, OE.RFdata,xbnk_h, cex.main=1)
partialPlot(OE.subset, OE.RFdata,PH, cex.main=1)








