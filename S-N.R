####################
##determine total var for TN, TP, and EC
####################
## Total maximum variance that can be explained. See VanderLaan et al. 2013, FWS paper, Linking land use, in stream stressors, and bio condition
## Page 806
NC=RFdata
refcompare=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\OE_Modeling\\NAMC_Supported_OEmodels\\WQ models\\Rfiles\\TPTN_data.csv')
refcompare$OE_TN=refcompare$TN_measured-refcompare$TN_pred
TNmod=na.omit(refcompare[,c('OE_TN','SiteCode')])
###
NC$Project='NorCal'
TNmod$Project='TNref'
####
NC=NC[,c('OE_TN','Project')]
TNmod=TNmod[,c('OE_TN','Project')]
ALL=rbind(NC,TNmod)
##############
N=var(TNmod$OE_TN)
S=var(ALL$OE_TN)
MaxVar=100*((S/N)/((S/N)+1))
MaxVar
########
########
NC=RFdata
refcompare$OE_TP=refcompare$TP_measured-refcompare$TP_pred
TPmod=na.omit(refcompare[,c('OE_TP','SiteCode')])
###
NC$Project='NorCal'
TPmod$Project='TPref'
####
NC=NC[,c('OE_TP','Project')]
TPmod=TPmod[,c('OE_TP','Project')]
ALL=rbind(NC,TPmod)
##############
N=var(TPmod$OE_TP)
S=var(ALL$OE_TP)
MaxVar=100*((S/N)/((S/N)+1))
MaxVar
###################
###################EC
NC=RFdata
refcompare=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\OE_Modeling\\NAMC_Supported_OEmodels\\WQ models\\Rfiles\\EC_Model_ReferenceData.csv')
refcompare$OE_EC=refcompare$EC_measured-refcompare$TN_pred
TNmod=na.omit(refcompare[,c('OE_TN','SiteCode')])
###
NC$Project='NorCal'
TNmod$Project='TNref'
####
NC=NC[,c('OE_TN','Project')]
TNmod=TNmod[,c('OE_TN','Project')]
ALL=rbind(NC,TNmod)
##############
N=var(TNmod$OE_TN)
S=var(ALL$OE_TN)
MaxVar=100*((S/N)/((S/N)+1))
MaxVar



###################################################################
# : XCMGLU
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~A_Prop_YrPr+A_Prop_3YrPrPr+
                      Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                      SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                      Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                      UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~A_Prop_YrPr+A_Prop_3YrPrPr+
                      Density_RdCross+RdDensC+Percent_HMA+MINEden_WS+
                      SpDensity300m+PerDensC+IntDensC+StmOrd+
                      Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+MEANP_WS+TMAX_WS+TMIN_WS+
                      UCS_Mean+KFCT_AVE+PRMH_AVE+HYDR_WS+PCT_SEDIM+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Density_RdCross+Percent_HMA+
                      IntDensC+
                      Log_AREA_SQKM+ELEV_RANGE+TMAX_WS+
                      KFCT_AVE+PRMH_AVE+HYDR_WS+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Density_RdCross+Percent_HMA+
                      ELEV_RANGE+TMAX_WS+
                      PRMH_AVE+HYDR_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Density_RdCross+Percent_HMA+
                      ELEV_RANGE+TMAX_WS+
                      PRMH_AVE+HYDR_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)


#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      
                      ELEV_RANGE+
                     HYDR_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

par(mfrow=c(1,2))

partialPlot(XCMGLU, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMGLU, RFLU,HYDR_WS, cex.main=1)



partialPlot(XCMGLU, RFLU,Percent_HMA, cex.main=1)













































#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Density_RdCross+Percent_HMA+
                      ELEV_RANGE+TMAX_WS+
                      KFCT_AVE+PRMH_AVE,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Density_RdCross+Percent_HMA+
                      ELEV_RANGE+
                      KFCT_AVE,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Density_RdCross+Percent_HMA+
                     KFCT_AVE,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Density_RdCross+Percent_HMA+
                      StmOrd,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      Percent_HMA+StmOrd+
                      KFCT_AVE+HYDR_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)


#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~
                      StmOrd+
                      HYDR_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

par(mfrow=c(1,2))

partialPlot(XCMGLU, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGLU, RFLU,HYDR_WS, cex.main=1)
par(mfrow=c(1,1))
bpp.out = bivarpartialPlot.randomForest(XCMGLU, RFLU, HYDR_WS, StmOrd, ylab="rating", n1.pt=nump, n2.pt=nump, theta=1300) #change theta on this one, can't use factors


?cor
cor(RFLU[,c('StmOrd','HYDR_WS')],method='pearson')
