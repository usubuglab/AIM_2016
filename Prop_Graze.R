#Keeping prop's

TN3=randomForest(OE_TN_3~A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+
                   A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   Density_RdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+
                   SpDensity300m+A_SpDensity800m+StreamDens+IntDensC+StmOrd+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+
                   PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3
varImpPlot(TN3)

TN3=randomForest(OE_TN_3~
                   A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   Density_RdCross+Slope_WS+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3
varImpPlot(TN3)

TN3=randomForest(OE_TN_3~
                   A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_3YrPrPr+
                   Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3
varImpPlot(TN3)


par(mfrow=c(2,3))
partialPlot(TN3, RFLU,A_Prop_YrSp, cex.main=1)
partialPlot(TN3, RFLU,A_Prop_4YrsPrSp, cex.main=1)
partialPlot(TN3, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TN3, RFLU,Slope_WS, cex.main=1)



###

TP3=randomForest(OE_TP_3~A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                   A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                   UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)

#2
TP3=randomForest(OE_TP_3~A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                   A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   Density_RdCross+Percent_HMA+MINEden_WS+DAMvol_Stand_WS+
                   SpDensity300m+A_SpDensity800m+StreamDens+IntDensC+StmOrd+
                   Slope_WS+AREA_SQKM+ELEV_RANGE+ELVmax_WS+MEANP_WS+
                   UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+PCT_SEDIM+Volcanic_7,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)

#2
TP3=randomForest(OE_TP_3~
                   A_Prop_4YrsPrSp+A_Prop_3YrPrPr+
                   Density_RdCross+
                   StreamDens+
                   Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3
varImpPlot(TP3)


par(mfrow=c(2,3))
partialPlot(TP3, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP3, RFLU,A_Prop_4YrsPrSp, cex.main=1)
partialPlot(TP3, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TP3, RFLU,StreamDens, cex.main=1)
partialPlot(TP3, RFLU,Slope_WS, cex.main=1)
