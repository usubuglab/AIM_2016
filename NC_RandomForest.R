##############################################################################################################################################################################################
#Random Forest for Northern California to determine most important stressors to biological conditions
##############################################################################################################################################################################################

##loading package
library (randomForest)

##load Bug File
#Bugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\NV_BugResults.csv")
#Load Indicator File
#RF_Indicators=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Indicators_RFinput_5Jan2015.csv")

#Bugs, Indicators, and Natural Variables. 
RFdata=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\All_BugsIndicatorsNatural_11Jan2015.csv")

####################
#Correlations
####################
# Indicators and Natural
IndicNatSubset=RFdata[,8:59]
IndicNatCor=cor(IndicNatSubset)
write.csv(IndicNatCor,'\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\CorrelationResults.csv')


#Still need to bring in natural predictors 
RF_NVnat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\NV_GISvariables_RFinput_11Jan2015.csv")
RF_CAnat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\CSCI_GISvariables_RFinput_11Jan2015.csv")
RF_LUnat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\NaturalVariables_RFinput_11Jan2015.csv")


###################################################################
# NV model natural variables.
colnames(RF_NVnat)
#1) Run RF For natural preds from the NV MMI GIS input file.
NVnat=randomForest(NV_MMI~ELVmin_WS+ELVmax_WS+ELVmean_WS+SQ_KM+HYDR_WS+WDmax_WS+Pmax_WS+Pmin_WS+Tmax_WS+
                     BFI_WS+Tmax_PT+PrdCond+Slope_WS,data=RF_NVnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NVnat
varImpPlot(NVnat)
#2) Run RF For subset natural preds from the NV MMI GIS input file.
NVnat=randomForest(NV_MMI~ELVmin_WS+SQ_KM+Tmax_PT+Slope_WS,
                   data=RF_NVnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NVnat
varImpPlot(NVnat)

par(mfrow=c(2,2))
partialPlot(NVnat, RF_NVnat,ELVmin_WS, cex.main=1)
partialPlot(NVnat, RF_NVnat,SQ_KM, cex.main=1)
partialPlot(NVnat, RF_NVnat,Tmax_PT, cex.main=1)
partialPlot(NVnat, RF_NVnat,Slope_WS, cex.main=1)

###################################################################################
###################################################################################
#CA model Natural Variables ONLY
###################################################################################
colnames(RF_CAnat)
#1) Run RF For natural preds from the CSCI GIS input file.
CAnat=randomForest(NV_MMI~TEMP_00_09+SumAve_P+SITE_ELEV+S_Mean+PRMH_AVE+PPT_00_09+PCT_SEDIM+P_MEAN+New_Long+New_Lat+N_MEAN+
                     MgO_Mean+LPREM_mean+KFCT_AVE+ELEV_RANGE+CaO_Mean+BDH_AVE+AREA_SQKM,
                   data=RF_CAnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
CAnat
varImpPlot(CAnat)
#2) Run RF For subset natural preds from the CSCI GIS input file.
CAnat=randomForest(NV_MMI~ELEV_RANGE+CaO_Mean+AREA_SQKM,
                   data=RF_CAnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
CAnat
varImpPlot(CAnat)

###################################################################################
###################################################################################
# Natural Variables from Land Use excel file. This is a combination of many natural variables. 
###################################################################################
colnames(RF_LUnat)
#1) Run RF For natural preds from the LU data file.
LUnat=randomForest(NV_MMI~Year+DOY+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+BFI_WS+HYDR_WS+GW_P_Sp_Mx+Slope_WS+
                     AREA_SQKM+PCT_SEDIM+Volcanic_7+Dom_Geol+SITE_ELEV+ELEV_RANGE+ELVcv_PT+ELVmax_WS+ELVmean_WS+ELVmin_WS+Pmax_WS+
                     Pmin_WS+SumAve_P+MEANP_WS+RH_WS+TMAX_WS+TMEAN_WS+TMIN_WS+UCS_Mean+CaO_Mean+MgO_Mean+S_Mean+Pct_Alfi+AWC_soil+BDH_AVE+
                     Db3rdbar+KFCT_AVE+PRMH_AVE+SOC+Awch_WS+alru_dom+Evergr_ave+EVI_AveAve+EVI_MaxAve+PerShrubC+PerHerbacC,
                   data=RF_LUnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LUnat
varImpPlot(LUnat)

#2) Run RF For subset natural preds from the LU data file.
LUnat=randomForest(NV_MMI~alru_dom+ELEV_RANGE+AREA_SQKM+StreamDens+S_Mean,data=RF_LUnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LUnat
varImpPlot(LUnat)

###################################################################################
###################################################################################
# Run RF with Indicators ONLY for NV MMI
###################################################################################
colnames(RFdata)

###################################################################################
# Run RF with all indicators, but reducing Water Quality indicators
# 1
NV.WQ=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                      PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)

# All p-hab, only OE water chem
# 2
NV.WQ=randomForest(NV_MMI~OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                      PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+
                      QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)

# All p-hab, only measured water chem
# 3
NV.WQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+
                      BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)



# All p-hab, only pred water chem
# 4
NV.WQ=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                      PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)

###################################################################################
# Reducing p-hab indicators
NV.allphab=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+
                      BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#1
NV.allphab=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+
                      BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#2
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkCover_BLM+XFC_NAT+xcdenmid+PCT_SAFN+xcdenbk+XCMG+XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#3
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkCover_BLM+XFC_NAT+xcdenbk+XCMG+xbnk_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#4
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+XCMG+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#5
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+XCMG+QR1,
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

par(mfrow=c(2,3))
partialPlot(NV.allphab, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.allphab, RFdata,NTL, cex.main=1)
partialPlot(NV.allphab, RFdata,PTL, cex.main=1)
partialPlot(NV.allphab, RFdata,XCMG, cex.main=1)
partialPlot(NV.allphab, RFdata,QR1, cex.main=1)
#6
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+XCMG,
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#############################################################################
# Reducing p-hab indicators but starting with only BLM
#1
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkStability_BLM+XFC_NAT+xcdenmid+
                      LINCIS_H+PCT_SAFN+XCMG, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
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
####################################################################################################

par(mfrow=c(2,3))
partialPlot(NV.BLM.indic, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,NTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,XCMG, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PH, cex.main=1)




###################################################################################
###################################################################################
# Run RF with Indicators and Natural Preds for NV MMI
###################################################################################
colnames(RFdata)

###################################################################################
# Run with all natural and indicators
# 1
RFIndNat.all=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                        SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+HYDR_WS+GW_P_Sp_Mx+Slope_WS+AREA_SQKM+Volcanic_7+
                        SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce many natural variables.
# 2
RFIndNat.all=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                        SpNum800m+StreamDens+PerDensC+IntDensC+Slope_WS+AREA_SQKM+ELEV_RANGE+ELVmin_WS+CaO_Mean+S_Mean+alru_dom+Tmax_PT, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)


# Reduce indicators.
# 3
RFIndNat.all=randomForest(NV_MMI~PrdCond+CONDUCTIVITY+NTL+PTL+OE_TN+OE_TP+BnkStability_BLM+
                        xcdenmid+xcdenbk+XCMG+QR1+SpNum800m+IntDensC+Slope_WS+AREA_SQKM+
                        CaO_Mean+S_Mean+alru_dom+Tmax_PT, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce indicators more.
# 4
RFIndNat.all=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_TN+OE_TP+XCMG+QR1+SpNum800m+IntDensC+
                        Slope_WS+AREA_SQKM+CaO_Mean+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce indicators more.
# 5
RFIndNat.all=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_TN+OE_TP+XCMG+
                        QR1+IntDensC+Slope_WS+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce more.
# 6
RFIndNat.all=randomForest(NV_MMI~NTL+PTL+OE_TN+OE_TP+XCMG+QR1+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# 7
RFIndNat.all=randomForest(NV_MMI~NTL+OE_TN+OE_TP+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)
# 8
RFIndNat.all=randomForest(NV_MMI~NTL+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)


par(mfrow=c(2,3))
partialPlot(RFIndNat.all, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFIndNat.all, RFdata,NTL, cex.main=1)
partialPlot(RFIndNat.all, RFdata,S_Mean, cex.main=1)
partialPlot(RFIndNat.all, RFdata,XCMG, cex.main=1)
partialPlot(RFIndNat.all, RFdata,IntDensC, cex.main=1)



#9
RFIndNat.all=randomForest(NV_MMI~NTL+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

##############################################################################################
# Run with BLM select indicators (measured water chem only, not modeled) and all natural
# 1
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
                        SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+
                        StmOrd+HYDR_WS+GW_P_Sp_Mx+Slope_WS+AREA_SQKM+Volcanic_7+
                        SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+
                        CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)
# Run with BLM select indicators (measured water chem only, not modeled) and subset natural
# 2
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)

# Run with subset of BLM select indicators (measured water chem only, not modeled) and subset natural
# 3
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+
                        PH+BnkStability_BLM+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)

# Run with subset of BLM select indicators (measured water chem only, not modeled) and subset natural
# 4
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)

par(mfrow=c(2,4))
partialPlot(RFIndNat.BLM.MWQ, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,PTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,NTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,S_Mean, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,XCMG, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,IntDensC, cex.main=1)


# 5
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)
################################################################################################
par(mfrow=c(2,4))
partialPlot(RFIndNat.BLM.MWQ, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,NTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,PTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,XCMG, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,IntDensC, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,S_Mean, cex.main=1)


##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Trying some with OE
##############################################################################
#1
RFoe=randomForest(NV_OE0~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+
                        OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+
                        XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                        SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+
                        StmOrd+HYDR_WS+GW_P_Sp_Mx+Slope_WS+AREA_SQKM+Volcanic_7+
                        SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+
                        CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#2
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+xcdenmid+LINCIS_H+XEMBED+XCMG+SpNum300m+SpNum800m+IntDensC+
                    GW_P_Sp_Mx+Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmin_WS+TMAX_WS+PRMH_AVE+alru_dom+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#3
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+xcdenmid+XCMG+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmin_WS+alru_dom+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)
#4
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+xcdenmid+XCMG+AREA_SQKM+ELVmin_WS+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)
#5
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+XCMG+AREA_SQKM+ELVmin_WS+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#6
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+XCMG+AREA_SQKM+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#7
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+XCMG+AREA_SQKM, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)
##############################################################################
par(mfrow=c(2,3))
partialPlot(RFoe, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(RFoe, RFdata,NTL, cex.main=1)
#partialPlot(RFoe, RFdata,PTL, cex.main=1)
partialPlot(RFoe, RFdata,XCMG, cex.main=1)
#partialPlot(RFoe, RFdata,ELVmin_WS, cex.main=1)
partialPlot(RFoe, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFoe, RFdata,Tmax_PT, cex.main=1)

################################################################################
NVnat=randomForest(NV_OE0~ELVmin_WS+ELVmax_WS+ELVmean_WS+SQ_KM+HYDR_WS+WDmax_WS+
                     Pmax_WS+Pmin_WS+Tmax_WS+BFI_WS+Tmax_PT+PrdCond+Slope_WS,
                   data=RF_NVnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NVnat
varImpPlot(NVnat)
#2) Run RF For subset natural preds from the NV MMI GIS input file.
NVnat=randomForest(NV_MMI~ELVmin_WS+SQ_KM+Tmax_PT,
                   data=RF_NVnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NVnat
varImpPlot(NVnat)


########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
#3-D plots

##randomforest modeling - best model
MSD5red.rf = randomForest (MStdev5~ StrmDensity+PctForested+Precip30+Area, data = MSD5, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
MSD5red.rf
varImpPlot(MSD5red.rf)
partialPlot(MSD5red.rf,MSD5,Precip30)

par(mai=c(1.2,1.2,0.2,0.2))
sapply(unique(MSD5$MStdev5),function(grp){
  partialPlot(MSD5red.rf,pred.data=MSD5, x.var= CVPrecip12m,main=paste("",""),xlab="CV of 12 m Precipitation",ylab="SD of modeled O/E scores", lwd=5, mai=c(0.75,0.75,0.75,0.75), cex.lab=2, cex.axis=2)});

##random forest bivariate partial plot - first block of codes is boilerplate required for partial plot setup


bivarpartialPlot.randomForest <-
  function (x, pred.data, x1.var, x2.var, which.class, w,
            n1.pt = min(length(unique(pred.data[, x1name])), 51),
            n2.pt = min(length(unique(pred.data[, x2name])), 51),
            x1lab=deparse(substitute(x1.var)),
            x2lab=deparse(substitute(x2.var)), ylab="",
            main=paste("Partial Dependence on", deparse(substitute(x1.var)),"and",deparse(substitute(x2.var))),
            ...)
  {
    classRF <- x$type != "regression"
    if (is.null(x$forest)) stop("The randomForest object must contain the forest.\\n")
    x1.var <- substitute(x1.var)
    x2.var <- substitute(x2.var)
    x1name <- if (is.character(x1.var)) x1.var else {
      if (is.name(x1.var)) deparse(x1.var) else {
        eval(x1.var)
      }
    }
    x2name <- if (is.character(x2.var)) x2.var else {
      if (is.name(x2.var)) deparse(x2.var) else {
        eval(x2.var)
      }
    }
    n <- nrow(pred.data)
    if (missing(w)) w <- rep(1, n)
    if (classRF) {
      if (missing(which.class)) {
        focus <- 1
      }
      else {
        focus <- charmatch(which.class, colnames(x$votes))
        if (is.na(focus))
          stop(which.class, "is not one of the class labels.")
      }
    }
    # the first predictor variable
    xv1 <- pred.data[, x1name]
    x1.pt <- seq(min(xv1), max(xv1), length = n1.pt)
    # the second predictor variable
    xv2 <- pred.data[, x2name]
    x2.pt <- seq(min(xv2), max(xv2), length = n2.pt)
    # y is big!
    y.pt <- matrix(0, nrow=n1.pt, ncol=n2.pt)
    for (i in 1:n1.pt) {
      for (j in 1:n2.pt) {
        x.data <- pred.data
        x.data[, x1name] <- rep(x1.pt[i], n)
        x.data[, x2name] <- rep(x2.pt[j], n)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i,j] <- weighted.mean(log(ifelse(pr[, focus] == 0, 1, pr[, focus]))
                                     - rowMeans(log(ifelse(pr == 0, 1, pr))), w, na.rm=TRUE)
        } else {
          y.pt[i,j] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        }
      }
    }
    # output is ready for persp
    persp(y.pt, xlab=x1lab, ylab=x2lab, zlab="",main=main,...)
  }

## once you have defined the bivariate plot function (with above code), use this code to create one.  it takes 5-10 mins, so be patient:


nump = 10
bpp.out = bivarpartialPlot.randomForest(MSD5red.rf, MSD5, StrmDensity, Precip30, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors


























