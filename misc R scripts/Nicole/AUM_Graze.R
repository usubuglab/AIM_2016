#AUMs only and not Props (AUM active/AUM permitted)
#1
TN3=randomForest(OE_TN_3~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                  Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+
                  SprgDensity_WS+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+
                  KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+Volcanic_7,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3
varImpPlot(TN3)
#1
TN3=randomForest(OE_TN_3~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+
                   Slope_WS+AREA_SQKM+MEANP_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3
varImpPlot(TN3)

#1
TN3=randomForest(OE_TN_3~
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   Density_RdCross+
                   Slope_WS+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3
varImpPlot(TN3)


par(mfrow=c(2,3))
partialPlot(TN3, RFLU,A_AUM_YrSp, cex.main=1)
partialPlot(TN3, RFLU,A_AUM_4YrPrSp, cex.main=1)
partialPlot(TN3, RFLU,A_AUM_YrPr, cex.main=1)
partialPlot(TN3, RFLU,A_AUM_3YrPrPr, cex.main=1)
partialPlot(TN3, RFLU,Density_RdCross, cex.main=1)
partialPlot(TN3, RFLU,Slope_WS, cex.main=1)
partialPlot(TN3, RFLU,AREA_SQKM, cex.main=1)









###
#1
TP3=randomForest(OE_TP_3~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                   UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)

###
#2
TP3=randomForest(OE_TP_3~A_P_AUM_4YrPrSp+A_P_AUM_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   Density_RdCross+Percent_HMA+DAMden_WS+
                   SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+StmOrd+
                   Slope_WS+AREA_SQKM+SITE_ELEV+SumAve_P+MEANP_WS+TMIN_WS+
                   UCS_Mean+PRMH_AVE+alru_dom+PCT_SEDIM+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)

#2
TP3=randomForest(OE_TP_3~
                  A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   Density_RdCross+Percent_HMA+
                   StreamDens+StmOrd+
                   Slope_WS+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)

#2
TP3=randomForest(OE_TP_3~
                   A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   Density_RdCross+Percent_HMA+
                   StreamDens+
                   Slope_WS+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)

#2
TP3=randomForest(OE_TP_3~
                   A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   Density_RdCross+
                   StreamDens+
                   AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)


par(mfrow=c(2,3))
partialPlot(TP3, RFLU,A_AUM_4YrPrSp, cex.main=1)
partialPlot(TP3, RFLU,A_AUM_YrPr, cex.main=1)
partialPlot(TP3, RFLU,A_AUM_3YrPrPr, cex.main=1)
partialPlot(TP3, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP3, RFLU,StreamDens, cex.main=1)
partialPlot(TP3, RFLU,Slope_WS, cex.main=1)
partialPlot(TP3, RFLU,AREA_SQKM, cex.main=1)



