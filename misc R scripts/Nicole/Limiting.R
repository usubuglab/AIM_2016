####################################################
#TN
####################################################
#1

TN_AUM2=randomForest(OE_TN_3~A_AUM_YrPr+A_AUM_3YrPrPr+Density_RdCross+Slope_WS+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_AUM2
varImpPlot(TN_AUM2)

par(mfrow=c(2,3))
partialPlot(TN_AUM2, RFLU,A_AUM_YrPr, cex.main=1)
partialPlot(TN_AUM2, RFLU,A_AUM_3YrPrPr, cex.main=1)
partialPlot(TN_AUM2, RFLU,Density_RdCross, cex.main=1)
partialPlot(TN_AUM2, RFLU,Slope_WS, cex.main=1)
partialPlot(TN_AUM2, RFLU,AREA_SQKM, cex.main=1)

par(mfrow=c(3,4))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, A_AUM_YrPr, A_AUM_3YrPrPr, ylab="rating", n1.pt=nump, n2.pt=nump, theta=30) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, A_AUM_YrPr, Density_RdCross, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, A_AUM_YrPr, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, A_AUM_YrPr, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=320) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, A_AUM_3YrPrPr, Density_RdCross, ylab="rating", n1.pt=nump, n2.pt=nump, theta=120) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, A_AUM_3YrPrPr, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=150) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, A_AUM_3YrPrPr, AREA_SQKM,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, Density_RdCross, Slope_WS,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, Density_RdCross, AREA_SQKM,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM2, RFLU, Slope_WS, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=120) #change theta on this one, can't use factors

#1
TN_AUM=randomForest(OE_TN_3~A_AUM_YrSp+A_AUM_4YrPrSp+Density_RdCross+Slope_WS+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_AUM
varImpPlot(TN_AUM)

par(mfrow=c(2,3))
partialPlot(TN_AUM, RFLU,A_AUM_YrSp, cex.main=1)
partialPlot(TN_AUM, RFLU,A_AUM_4YrPrSp, cex.main=1)
partialPlot(TN_AUM, RFLU,Density_RdCross, cex.main=1)
partialPlot(TN_AUM, RFLU,Slope_WS, cex.main=1)
partialPlot(TN_AUM, RFLU,AREA_SQKM, cex.main=1)


par(mfrow=c(3,4))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, A_AUM_YrSp, A_AUM_4YrPrSp, ylab="rating", n1.pt=nump, n2.pt=nump, theta=330) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, A_AUM_YrSp, Density_RdCross, ylab="rating", n1.pt=nump, n2.pt=nump, theta=330) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, A_AUM_YrSp, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=330) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, A_AUM_YrSp, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, A_AUM_4YrPrSp, Density_RdCross, ylab="rating", n1.pt=nump, n2.pt=nump, theta=330) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, A_AUM_4YrPrSp, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=330) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, A_AUM_4YrPrSp, AREA_SQKM,ylab="rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, Density_RdCross, Slope_WS,ylab="rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, Density_RdCross, AREA_SQKM,ylab="rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_AUM, RFLU, Slope_WS, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=120) #change theta on this one, can't use factors

#######################################################
#1
TN3_Prop2=randomForest(OE_TN_3~A_Prop_3YrPrPr+Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3_Prop2
varImpPlot(TN3_Prop2)

par(mfrow=c(1,2))
partialPlot(TN3_Prop2, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TN3_Prop2, RFLU,Slope_WS, cex.main=1)

bpp.out = bivarpartialPlot.randomForest(TN3_Prop2, RFLU, A_Prop_3YrPrPr, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors



#1
TN3_Prop=randomForest(OE_TN_3~A_Prop_YrSp+A_Prop_4YrsPrSp+Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN3_Prop
varImpPlot(TN3_Prop)

par(mfrow=c(2,2))
partialPlot(TN3_Prop, RFLU,A_Prop_YrSp, cex.main=1)
partialPlot(TN3_Prop, RFLU,A_Prop_4YrsPrSp, cex.main=1)
partialPlot(TN3_Prop, RFLU,Slope_WS, cex.main=1)

par(mfrow=c(1,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TN3_Prop, RFLU, A_Prop_YrSp, A_Prop_4YrsPrSp, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN3_Prop, RFLU, A_Prop_YrSp, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN3_Prop, RFLU, A_Prop_4YrsPrSp, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors

#######################################################


#######################################################
#TP
#######################################################
#2
TP3_AUM2=randomForest(OE_TP_3~A_AUM_YrPr+A_AUM_3YrPrPr+Density_RdCross+StreamDens+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3_AUM2
varImpPlot(TP3_AUM2)

par(mfrow=c(2,3))
partialPlot(TP3_AUM2, RFLU,A_AUM_YrPr, cex.main=1)
partialPlot(TP3_AUM2, RFLU,A_AUM_3YrPrPr, cex.main=1)
partialPlot(TP3_AUM2, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP3_AUM2, RFLU,StreamDens, cex.main=1)
partialPlot(TP3_AUM2, RFLU,AREA_SQKM, cex.main=1)

par(mfrow=c(3,4))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, Density_RdCross, A_AUM_3YrPrPr, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, Density_RdCross, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, A_AUM_3YrPrPr, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, StreamDens, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, Density_RdCross, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, A_AUM_3YrPrPr, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, A_AUM_YrPr, A_AUM_3YrPrPr, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, A_AUM_YrPr, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, A_AUM_YrPr, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM2, RFLU, A_AUM_YrPr, Density_RdCross, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors


#2
TP3_AUM=randomForest(OE_TP_3~A_AUM_4YrPrSp+Density_RdCross+StreamDens+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3_AUM
varImpPlot(TP3_AUM)

par(mfrow=c(2,3))
partialPlot(TP3_AUM, RFLU,A_AUM_4YrPrSp, cex.main=1)
partialPlot(TP3_AUM, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP3_AUM, RFLU,StreamDens, cex.main=1)
partialPlot(TP3_AUM, RFLU,AREA_SQKM, cex.main=1)

par(mfrow=c(2,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TP3_AUM, RFLU, Density_RdCross, A_AUM_4YrPrSp, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM, RFLU, Density_RdCross, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM, RFLU, A_AUM_4YrPrSp, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM, RFLU, StreamDens, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM, RFLU, Density_RdCross, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_AUM, RFLU, A_AUM_4YrPrSp, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors

######################################################
#2
TP3_Prop2=randomForest(OE_TP_3~A_Prop_3YrPrPr+Density_RdCross+StreamDens+Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3_Prop2
varImpPlot(TP3_Prop2)

par(mfrow=c(2,3))
partialPlot(TP3_Prop2, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP3_Prop2, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TP3_Prop2, RFLU,StreamDens, cex.main=1)
partialPlot(TP3_Prop2, RFLU,Slope_WS, cex.main=1)

par(mfrow=c(2,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TP3_Prop2, RFLU, Density_RdCross, A_Prop_3YrPrPr, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop2, RFLU, Density_RdCross, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop2, RFLU, A_Prop_3YrPrPr, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop2, RFLU, StreamDens, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop2, RFLU, Density_RdCross, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop2, RFLU, A_Prop_3YrPrPr, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors


#2
TP3_Prop=randomForest(OE_TP_3~A_Prop_4YrsPrSp+Density_RdCross+StreamDens+Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP3_Prop
varImpPlot(TP3_Prop)

par(mfrow=c(2,3))
partialPlot(TP3_Prop, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP3_Prop, RFLU,A_Prop_4YrsPrSp, cex.main=1)
partialPlot(TP3_Prop, RFLU,StreamDens, cex.main=1)
partialPlot(TP3_Prop, RFLU,Slope_WS, cex.main=1)


par(mfrow=c(2,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TP3_Prop, RFLU, Density_RdCross, A_Prop_4YrsPrSp, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop, RFLU, Density_RdCross, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop, RFLU, A_Prop_4YrsPrSp, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop, RFLU, StreamDens, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop, RFLU, Density_RdCross, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP3_Prop, RFLU, A_Prop_4YrsPrSp, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors

