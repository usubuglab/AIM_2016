#############################################################################
# Reducing p-hab indicators but starting with only BLM
#1
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkStability_BLM+XFC_NAT+xcdenmid+
                            LINCIS_H+PCT_SAFN+XCMG, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

#1
NV.BLM.indic=randomForest(NV_MMI~OE_TN+OE_TP+PH++OE_Conduct+
                            XCMG, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)




#2
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+
                            PCT_SAFN+XCMG, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

#3
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+XCMG, 
                          data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

par(mfrow=c(2,3))
partialPlot(NV.BLM.indic, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,NTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,XCMG, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PH, cex.main=1)


#4
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+XCMG, 
                          data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

# OE TRIAL RUN
NV.BLM.indic=randomForest(NV_MMI~OE_Conduct+OE_TN+OE_TP+PH+XCMG, 
                          data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)
####################################################################################################

par(mfrow=c(2,3))
partialPlot(NV.BLM.indic, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,NTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,XCMG, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PH, cex.main=1)