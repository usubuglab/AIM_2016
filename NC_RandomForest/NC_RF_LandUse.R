##############################################################################################################################################################################################
#Random Forest for Northern California to determine the land use associated with identified stressors 
##############################################################################################################################################################################################

##########################################################
#Set up
##########################################################

####################
##loading package
####################
library (randomForest)

####################
##loading data
####################
##load file with bugs, indicators, and naturals
RFLU=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run3_IDlanduse_pending\\LandUse_RFinput_Short_4March2015.csv")

#Variable Names:
#SiteCode++OE_TN+OE_TP+xcdenmid+XCMG+P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS

####################
# 3-D Plot function
####################
#Once function is run you can use it to create 3-D plots, below two lines of code are an example of info needed for function
#nump = 15
#bpp.out = bivarpartialPlot.randomForest(RF model, data, first variable, second variable, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) 
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
##########################################################################

####################
#Correlations
####################
# Indicators/Stressors, Grazing, Anthropogenic, and Natural variables 
RFLU_CorSubset=RFLU[,3:63]
RFLUdataCor=cor(RFLU_CorSubset)
#write.csv(RFLUdataCor,"\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run3_IDlanduse_pending\\RFLUdataCorrelations.csv")

########################################################################################################
#What data needs/should be transformed

boxplotdata=RFLU[,c(3:63)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}

# Transformations: 
# If values have negatives need to assess how to Log (e.g., OR WQ)
# Calculated summary stats to know if ) were present, ect. summary(RFdata$PerDensC)

RFLU$Log_AREA_SQKM=log10(RFLU$AREA_SQKM)
RFLU$Log_OE_TN=log10(ifelse(RFLU$OE_TN<0,0,RFLU$OE_TN)+1)
RFLU$Log_OE_TP=log10(ifelse(RFLU$OE_TP<0,0,RFLU$OE_TP)+1)
#RFLU$Log_alru_dom=log10(RFLU$alru_dom+1)
#RFLU$Log_SprgNum_WS=log10(RFLU$SprgNum_WS+1)
#RFLU$Log_PerDensC=log10(RFLU$PerDensC+1)
#RFLU$Log_Slope_WS=log10(RFLU$Slope_WS)
#RFLU$Log_HYDR_WS=log10(RFLU$HYDR_WS)

########################################################################################################
########################################################################################################

####################
#Random Forest Models
####################

#Variable Names:
#+P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS
#OE_TN+OE_TP+xcdenmid+XCMG
###################################################################
# Use anthropogenic only variables: TNLU
#colnames(RFLU)
###################################################################
#1) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#2) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                  NumRdCross+Percent_HBonly+Percent_Honly+Percent_Allotment+MINEnum_WS+
                  URBAN_WS+PctOtherOwn+BLMPct+PctFWS+PctFS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#3) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                    NumRdCross+Percent_Allotment+PctFS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#4) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+Percent_Allotment,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#5) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+Prop_3YrPrPr+Percent_Allotment,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#6) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_Prop_4YrsPrSp+Percent_Allotment,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

###################################################################
# Use anthropogenic and natural variables: TN
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                  StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+BLMPct+PctFWS+PctFS+
                  StmOrd+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+IntDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+Percent_Allotment+
                  DAMvol_WS+ArtPathDens+AG_WS+PctOtherOwn+BLMPct+PctFWS+PctFS+
                  StmOrd+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+StreamDens+IntDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+UCS_Mean,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_YrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                  RdDensC+Percent_HBonly+Percent_Honly+Percent_Allotment+
                  ArtPathDens+AG_WS+PctOtherOwn+BLMPct+PctFS+
                  PCT_SEDIM+SprgNum_WS+SpNum300m+StreamDens+IntDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+ELEV_RANGE+KFCT_AVE+PRMH_AVE+UCS_Mean,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrPr+Prop_3YrPrPr+Percent_Allotment+IntDensC+
                  AREA_SQKM+ELEV_RANGE+KFCT_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)


#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrPr+Prop_3YrPrPr+AREA_SQKM+ELEV_RANGE+KFCT_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)


#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_3YrPrPr+Prop_3YrPrPr+
                  AREA_SQKM+ELEV_RANGE+KFCT_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#8) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_Prop_4YrsPrSp+P_Prop_3YrPrPr+AREA_SQKM+ELEV_RANGE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)


par(mfrow=c(2,2))
partialPlot(TN, RFLU,P_Prop_4YrsPrSp, cex.main=1)
partialPlot(TN, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(TN, RFLU,P_Prop_3YrPrPr, cex.main=1)
partialPlot(TN, RFLU,AREA_SQKM, cex.main=1)


#9) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_Prop_4YrsPrSp+P_Prop_3YrPrPr+ELEV_RANGE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#10) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_Prop_4YrsPrSp+ELEV_RANGE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

par(mfrow=c(1,2))
partialPlot(TN, RFLU,P_Prop_4YrsPrSp, cex.main=1)
partialPlot(TN, RFLU,ELEV_RANGE, cex.main=1)


#11) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_Prop_4YrsPrSp+ELEV_RANGE+Log_AREA_SQKM,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

par(mfrow=c(2,2))
partialPlot(TN, RFLU,P_Prop_4YrsPrSp, cex.main=1)
partialPlot(TN, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(TN, RFLU,Log_AREA_SQKM, cex.main=1)

###################################################################
# Use transformed TN response variables: LTN
###################################################################
#1
LTN=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                   DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                   StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)
#2
LTN=randomForest(Log_OE_TN~P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   NumRdCross+Percent_HBonly+Percent_HMA+Percent_Allotment+
                   BLMPct+PctFS+
                   StmOrd+Slope_WS+StreamDens+
                   Slope_WS+AREA_SQKM+PRMH_AVE+MEANP_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)
#3
LTN=randomForest(Log_OE_TN~P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+PctFS+
                   AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#4
LTN=randomForest(Log_OE_TN~P_Prop_YrPr+PctFS+
                   AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)


#5
LTN=randomForest(Log_OE_TN~P_Prop_YrPr+PctFS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)
###################################################################
# Use transformed TN response and Predictor variables: LTNL
###################################################################
#1
LTNL=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                    RdDensC+Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+Log_DAMvol_WS+Log_ArtPathDens+
                    Log_AG_WS+Log_URBAN_WS+PctOtherOwn+PrivPct+BLMPct+Log_PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
                    StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
                    UCS_Mean+SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)
#2
LTNL=randomForest(Log_OE_TN~P_AUM_4YrPrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_3YrPrPr+Log_Prop_YrPr+
                    Log_NumRdCross+Percent_HBonly+Log_Percent_Honly+
                    Percent_HMA+Percent_Allotment+MINEnum_WS+DAMnum_WS+
                    Log_DAMvol_WS+Log_ArtPathDens+Log_AG_WS+Log_URBAN_WS+
                    PrivPct+BLMPct+PctFS+Slope_WS+Volcanic_7+SpNum300m+SpNum800m+
                    Log_PerDensC+Log_AREA_SQKM+SITE_ELEV+PRMH_AVE+Log_alru_dom+TMIN_WS+
                    UCS_Mean+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#3
LTNL=randomForest(Log_OE_TN~P_AUM_4YrPrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_3YrPrPr+Log_NumRdCross+Percent_Allotment+MINEnum_WS+DAMnum_WS+
                    Log_AG_WS+Log_URBAN_WS+PrivPct+PctFS+Slope_WS+Log_AREA_SQKM+PRMH_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#4
LTNL=randomForest(Log_OE_TN~P_AUM_4YrPrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_3YrPrPr+Log_NumRdCross+Percent_Allotment+Log_URBAN_WS+
                    PctFS+Slope_WS+Log_AREA_SQKM+PRMH_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)
#5
LTNL=randomForest(Log_OE_TN~P_AUM_4YrPrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+Percent_Allotment+PctFS+Log_AREA_SQKM+PRMH_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)
#6
LTNL=randomForest(Log_OE_TN~P_Prop_YrPr+Percent_Allotment+PctFS+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)


par(mfrow=c(2,2))
partialPlot(LTNL, RFLU,P_Prop_YrPr, cex.main=1)
partialPlot(LTNL, RFLU,Percent_Allotment, cex.main=1)
partialPlot(LTNL, RFLU,PctFS, cex.main=1)
partialPlot(LTNL, RFLU,Log_AREA_SQKM, cex.main=1)



#7
LTNL=randomForest(Log_OE_TN~P_Prop_YrPr+Percent_Allotment+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)
#8 Switch Log_OE_TN out for OE_TN
LTNL=randomForest(OE_TN~P_Prop_YrPr+Percent_Allotment+PctFS+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

########################################################################################################
########################################################################################################

###################################################################
# Use anthropogenic variables: TPLU
###################################################################
#1) Run RF for TP response and anthropogenic predictor variables
TPLU=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)

#2) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TPLU=randomForest(OE_TP~P_Prop_4YrsPrSp+NumRdCross+RdDensC+Percent_HBonly+Percent_Allotment+DAMvol_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)

par(mfrow=c(2,3))
partialPlot(TPLU, RFLU,P_Prop_4YrsPrSp, cex.main=1)
partialPlot(TPLU, RFLU,NumRdCross, cex.main=1)
partialPlot(TPLU, RFLU,RdDensC, cex.main=1)
partialPlot(TPLU, RFLU,Percent_HBonly, cex.main=1)
partialPlot(TPLU, RFLU,Percent_Allotment, cex.main=1)
partialPlot(TPLU, RFLU,DAMvol_WS, cex.main=1)



#3) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TPLU=randomForest(OE_TP~NumRdCross+Percent_HBonly,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)

###################################################################
# Use anthropogenic and natural variables: TP
###################################################################

#1) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                  StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)



#2) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_3YrPrPr+Prop_3YrPrPr+
                  NumRdCross+Percent_HBonly+Percent_HMA+PctXclsr+Percent_Allotment+
                  DAMvol_WS+ArtPathDens+AG_WS+PrivPct+BLMPct+PctFWS+PctFS+
                  Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+SITE_ELEV+KFCT_AVE+PRMH_AVE+alru_dom,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#3) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_3YrPrPr+
                  NumRdCross+Percent_HBonly+Percent_HMA+PctXclsr+Percent_Allotment+
                  DAMvol_WS+ArtPathDens+AG_WS+
                  Slope_WS+Volcanic_7+SpNum800m+StreamDens+PerDensC+
                  Slope_WS+AREA_SQKM+SITE_ELEV+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#4) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+
                  NumRdCross+Percent_Allotment+
                  DAMvol_WS+AG_WS+
                  Slope_WS+StreamDens+PerDensC+
                  Slope_WS+AREA_SQKM+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#5) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+
                  NumRdCross+Percent_Allotment+
                  DAMvol_WS+
                  Slope_WS+
                  Slope_WS+AREA_SQKM+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#6) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~
                  NumRdCross+Percent_Allotment+
                  DAMvol_WS+
                  Slope_WS+AREA_SQKM,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#7) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~NumRdCross+DAMvol_WS+AREA_SQKM,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)


#8) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~NumRdCross+Percent_Allotment+DAMvol_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)


#9) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  AUM_YrSp+AUM_4YrPrSp+Prop_YrPr+NumRdCross+RdDensC+Percent_Allotment+
                  DAMnum_WS+DAMvol_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#10) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_Prop_YrSp+P_Prop_YrPr+
                  Prop_YrPr+
                  NumRdCross+RdDensC+Percent_Allotment+
                  DAMnum_WS+DAMvol_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#11) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_Prop_YrSp+P_Prop_YrPr+NumRdCross+RdDensC+Percent_Allotment+DAMvol_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

par(mfrow=c(2,3))
partialPlot(TP, RFLU,P_Prop_YrSp, cex.main=1)
partialPlot(TP, RFLU,P_Prop_YrPr, cex.main=1)
partialPlot(TP, RFLU,NumRdCross, cex.main=1)
partialPlot(TP, RFLU,RdDensC, cex.main=1)
partialPlot(TP, RFLU,Percent_Allotment, cex.main=1)
partialPlot(TP, RFLU,DAMvol_WS, cex.main=1)

#12) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
TP=randomForest(OE_TP~P_Prop_YrPr+NumRdCross+RdDensC+Percent_Allotment+DAMvol_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

###################################################################
# Use transformed TP Response variables: LTP
###################################################################
#1) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                   DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                   StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#2) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_Prop_4YrsPrSp+NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+
                   DAMvol_WS+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+PctFWS+
                   StmOrd+Slope_WS+PCT_SEDIM+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+HYDR_WS+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMIN_WS+UCS_Mean+MEANP_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#3) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_Prop_4YrsPrSp+NumRdCross+DAMvol_WS+
                   StmOrd+Slope_WS+SprgNum_WS+SpNum300m+StreamDens+HYDR_WS+
                   Slope_WS+AREA_SQKM+KFCT_AVE+PRMH_AVE+alru_dom,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#4) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_Prop_4YrsPrSp+NumRdCross+Slope_WS+StreamDens+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

par(mfrow=c(2,3))
partialPlot(LTP, RFLU,P_Prop_4YrsPrSp, cex.main=1)
partialPlot(LTP, RFLU,NumRdCross, cex.main=1)
partialPlot(LTP, RFLU,Slope_WS, cex.main=1)
partialPlot(LTP, RFLU,StreamDens, cex.main=1)
partialPlot(LTP, RFLU,AREA_SQKM, cex.main=1)


#5) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~NumRdCross+StreamDens+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

###################################################################
# Use transformed TP Response and predictor variables: LTPL
####################################################################
#1
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                    RdDensC+Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+Log_DAMvol_WS+Log_ArtPathDens+
                    Log_AG_WS+Log_URBAN_WS+PctOtherOwn+PrivPct+BLMPct+Log_PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
                    StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
                    UCS_Mean+SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#2
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_NumRdCross+
                    Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+MINEnum_WS+Log_DAMvol_WS+
                    Log_AG_WS+Log_URBAN_WS+PrivPct+BLMPct+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
                    StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMIN_WS+
                    UCS_Mean+SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#3
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_NumRdCross+
                    Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+Log_DAMvol_WS+
                    Log_URBAN_WS+PrivPct+BLMPct+StmOrd+Slope_WS+PCT_SEDIM+SpNum300m+SpNum800m+
                    StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+PRMH_AVE+Log_alru_dom+TMIN_WS+
                    SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#4
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+Log_Prop_4YrsPrSp+Log_NumRdCross+
                    Percent_HMA+Log_PctXclsr+Log_DAMvol_WS+Log_URBAN_WS+StmOrd+Slope_WS+PCT_SEDIM+SpNum300m+
                    StreamDens+IntDensC+Log_Slope_WS+Log_AREA_SQKM+PRMH_AVE+Log_alru_dom+TMIN_WS+SumAve_P,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#5
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_Prop_YrPr+P_Prop_3YrPrPr+AUM_4YrPrSp+Log_NumRdCross+Log_DAMvol_WS+
                    StmOrd+Slope_WS+StreamDens+Log_Slope_WS+Log_AREA_SQKM+PRMH_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)
#6
LTPL=randomForest(Log_OE_TP~Log_NumRdCross+StreamDens+Log_Slope_WS+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)


par(mfrow=c(2,3))
partialPlot(LTPL, RFLU,Log_NumRdCross, cex.main=1)
partialPlot(LTPL, RFLU,Log_Slope_WS, cex.main=1)
partialPlot(LTPL, RFLU,StreamDens, cex.main=1)
partialPlot(LTPL, RFLU,Log_AREA_SQKM, cex.main=1)


#7
LTPL=randomForest(Log_OE_TP~Log_NumRdCross+StreamDens+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#8
LTPL=randomForest(OE_TP~Log_NumRdCross+StreamDens+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

########################################################################################################
########################################################################################################
par(mfrow=c(2,2))
partialPlot(TN, RFLU,P_Prop_4YrsPrSp, cex.main=1)
partialPlot(TN, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(TN, RFLU,P_Prop_3YrPrPr, cex.main=1)
partialPlot(TN, RFLU,AREA_SQKM, cex.main=1)

###################################################################
# Use anthropogenic variables: XCMGLU
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                    DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)


#2) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_4YrPrSp+P_Prop_4YrsPrSp+
                    NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                    DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)


#3) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_4YrPrSp+P_Prop_4YrsPrSp+NumRdCross+Percent_HBonly+Percent_HMA+AG_WS+PctOtherOwn+PrivPct+BLMPct,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#4) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_4YrPrSp+Percent_HBonly+Percent_HMA+PrivPct+BLMPct,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

par(mfrow=c(2,3))
partialPlot(XCMGLU, RFLU,P_AUM_4YrPrSp, cex.main=1)
partialPlot(XCMGLU, RFLU,Percent_HBonly, cex.main=1)
partialPlot(XCMGLU, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGLU, RFLU,PrivPct, cex.main=1)
partialPlot(XCMGLU, RFLU,BLMPct, cex.main=1)


#5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~Percent_HMA+PrivPct+BLMPct,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#6) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~PrivPct+BLMPct,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#7) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~Percent_HMA+BLMPct,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

###################################################################
# Use anthropogenic and natural variables: XCMG
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                  StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#2) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                    DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#3) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+
                    NumRdCross+RdDensC+Percent_HBonly+Percent_HMA+PctXclsr+MINEnum_WS+
                    DAMnum_WS+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#4) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~P_AUM_4YrPrSp+P_Prop_4YrsPrSp+
                    NumRdCross+Percent_HBonly+Percent_HMA+
                    AG_WS+PctOtherOwn+PrivPct+BLMPct,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#5) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~P_AUM_4YrPrSp+P_Prop_4YrsPrSp+NumRdCross+Percent_HBonly+Percent_HMA+PrivPct+BLMPct,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#6) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~P_AUM_4YrPrSp+P_Prop_4YrsPrSp+Percent_HBonly+Percent_HMA+BLMPct,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#7) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~P_AUM_4YrPrSp+P_Prop_4YrsPrSp+Percent_HBonly+BLMPct,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#8) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~P_AUM_4YrPrSp+Percent_HBonly+BLMPct,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


par(mfrow=c(1,3))
partialPlot(XCMG, RFLU,P_AUM_4YrPrSp, cex.main=1)
partialPlot(XCMG, RFLU,Percent_HBonly, cex.main=1)
partialPlot(XCMG, RFLU,BLMPct, cex.main=1)



#9) Run RF for XCMG response and all grazing, anthropogenic, 
XCMG=randomForest(XCMG~Percent_HBonly+BLMPct,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


###################################################################
# Use transformed anthropogenic and natural variables: XCMGL
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                     RdDensC+Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+Log_DAMvol_WS+Log_ArtPathDens+
                     Log_AG_WS+Log_URBAN_WS+PctOtherOwn+PrivPct+BLMPct+Log_PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
                     StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
                     UCS_Mean+SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#2) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_AUM_3YrPrPr+P_Prop_4YrsPrSp+
                     AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_3YrPrPr+Log_NumRdCross+
                     RdDensC+Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+Log_DAMvol_WS+Log_ArtPathDens+
                     Log_AG_WS+Log_URBAN_WS+BLMPct+PctFS+Slope_WS+PCT_SEDIM+Volcanic_7+SpNum300m+SpNum800m+
                     StreamDens+Log_PerDensC+Log_HYDR_WS+Log_Slope_WS+SITE_ELEV+ELEV_RANGE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
                     UCS_Mean+SumAve_P+MEANP_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#3) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_AUM_3YrPrPr+P_Prop_4YrsPrSp+Log_NumRdCross+
                     RdDensC+Percent_HBonly+Percent_HMA+Log_PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+Log_DAMvol_WS+
                     BLMPct+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+Log_Slope_WS+SITE_ELEV+ELEV_RANGE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
                     UCS_Mean+SumAve_P+MEANP_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#4) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_AUM_3YrPrPr+Log_NumRdCross+RdDensC+Percent_HBonly+Percent_HMA+
                     BLMPct+SITE_ELEV+ELEV_RANGE+PRMH_AVE+Log_alru_dom+TMAX_WS+MEANP_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~Log_NumRdCross+Percent_HBonly+Percent_HMA+BLMPct+ELEV_RANGE+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#6) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~Log_NumRdCross+BLMPct+ELEV_RANGE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#7) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~Log_NumRdCross+BLMPct,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#8) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_AUM_3YrPrPr+BLMPct+ELEV_RANGE+Percent_HMA,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)


par(mfrow=c(2,2))
partialPlot(XCMGL, RFLU,P_AUM_3YrPrPr, cex.main=1)
partialPlot(XCMGL, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMGL, RFLU,BLMPct, cex.main=1)
partialPlot(XCMGL, RFLU,Percent_HMA, cex.main=1)


########################################################################################################
########################################################################################################

###################################################################
# Use anthropogenic: denLU
###################################################################
#1) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                     NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                     DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#2) Run RF for XCMG response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_4YrsPrSp+
                     P_Prop_3YrPrPr+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                     PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+DAMvol_WS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#3) Run RF for XCMG response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~P_AUM_3YrPrPr+PctXclsr+Percent_Allotment+MINEnum_WS+DAMvol_WS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#4) Run RF for XCMG response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~P_AUM_3YrPrPr+PctXclsr+MINEnum_WS+DAMvol_WS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#5) Run RF for XCMG response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+DAMvol_WS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#6) Run RF for XCMG response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)


#7) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+DAMvol_WS+PrivPct+BLMPct+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#8) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+DAMvol_WS+PrivPct+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#9) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+DAMvol_WS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#10) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+SprgNum_WS+ELEV_RANGE+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#11) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+SprgNum_WS+ELEV_RANGE+StmOrd+DAMvol_WS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

par(mfrow=c(2,3))
partialPlot(denLU, RFLU,PctXclsr, cex.main=1)
partialPlot(denLU, RFLU,SprgNum_WS, cex.main=1)
partialPlot(denLU, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(denLU, RFLU,StmOrd, cex.main=1)
partialPlot(denLU, RFLU,DAMvol_WS, cex.main=1)
partialPlot(denLU, RFLU,PctFS, cex.main=1)



#12) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~PctXclsr+SprgNum_WS+ELEV_RANGE+DAMvol_WS+PctFS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)


###################################################################
# Use anthropogenic and natural variables: den
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                  StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~PctFS+TMIN_WS+SprgNum_WS+IntDensC+PrivPct+KFCT_AVE+P_AUM_3YrPrPr+P_AUM_YrPr+PRMH_AVE+DAMvol_WS+HYDR_WS+Percent_Allotment,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~PctFS+TMIN_WS+IntDensC+PrivPct+P_AUM_3YrPrPr+P_AUM_YrPr,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~PctFS+TMIN_WS+IntDensC+P_AUM_YrPr,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~PctFS+IntDensC,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)


#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~PctXclsr+SprgNum_WS+ELEV_RANGE+StmOrd+DAMvol_WS+PctFS+TMIN_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)


par(mfrow=c(2,4))
partialPlot(den, RFLU,PctXclsr, cex.main=1)
partialPlot(den, RFLU,SprgNum_WS, cex.main=1)
partialPlot(den, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(den, RFLU,StmOrd, cex.main=1)
partialPlot(den, RFLU,DAMvol_WS, cex.main=1)
partialPlot(den, RFLU,PctFS, cex.main=1)
partialPlot(den, RFLU,TMIN_WS, cex.main=1)



###################################################################
# Use transformed anthropogenic and natural variables: denL
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                    RdDensC+Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+Log_DAMvol_WS+Log_ArtPathDens+
                    Log_AG_WS+Log_URBAN_WS+PctOtherOwn+PrivPct+BLMPct+Log_PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
                    StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
                    UCS_Mean+SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    Log_Prop_YrSp+Log_Prop_YrPr+Log_NumRdCross+
                    RdDensC+Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+DAMnum_WS+Log_DAMvol_WS+Log_ArtPathDens+
                    Log_AG_WS+Log_URBAN_WS+PctOtherOwn+PrivPct+Log_PctFWS+PctFS+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
                    StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
                    UCS_Mean+SumAve_P+MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    Log_Prop_YrSp+Log_Prop_YrPr+
                    RdDensC+Percent_HBonly+Log_Percent_Honly+Log_PctXclsr+Log_DAMvol_WS+
                    Log_AG_WS+Log_URBAN_WS+PctOtherOwn+PrivPct+Log_PctFWS+PctFS+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
                    IntDensC+Log_HYDR_WS+Log_AREA_SQKM+ELEV_RANGE+KFCT_AVE+PRMH_AVE+TMAX_WS+TMIN_WS+
                    MEANP_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_4YrsPrSp+
                    Log_Prop_YrSp+Log_PctXclsr+Log_DAMvol_WS+
                    PrivPct+Log_PctFWS+PctFS+Log_SprgNum_WS+SpNum300m+
                    IntDensC+Log_HYDR_WS+Log_AREA_SQKM+KFCT_AVE+PRMH_AVE+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_4YrPrSp+Log_PctXclsr+PrivPct+PctFS+Log_SprgNum_WS+IntDensC+KFCT_AVE+PRMH_AVE+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_4YrPrSp+PrivPct+PctFS+IntDensC+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)


par(mfrow=c(2,3))
partialPlot(denL, RFLU,P_AUM_4YrPrSp, cex.main=1)
partialPlot(denL, RFLU,PrivPct, cex.main=1)
partialPlot(denL, RFLU,IntDensC, cex.main=1)
partialPlot(denL, RFLU,PctFS, cex.main=1)
partialPlot(denL, RFLU,TMIN_WS, cex.main=1)



#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_4YrPrSp+PctFS+IntDensC,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)









########################################################################################################
#What data needs/should be transformed

boxplotdata=RFLU[,c(3:63)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}

# Transformations: 
# If values have negatives need to assess how to Log (e.g., OR WQ)
# Calculated summary stats to know if ) were present, ect. summary(RFdata$PerDensC)

RFLU$Log_AREA_SQKM=log10(RFLU$AREA_SQKM)
RFLU$Log_OE_TN=log10(ifelse(RFLU$OE_TN<0,0,RFLU$OE_TN)+1)
RFLU$Log_OE_TP=log10(ifelse(RFLU$OE_TP<0,0,RFLU$OE_TP)+1)
RFLU$Log_alru_dom=log10(RFLU$alru_dom+1)
RFLU$Log_SprgNum_WS=log10(RFLU$SprgNum_WS+1)
RFLU$Log_PerDensC=log10(RFLU$PerDensC+1)
RFLU$Log_Slope_WS=log10(RFLU$Slope_WS)
RFLU$Log_HYDR_WS=log10(RFLU$HYDR_WS)

RFLU$Log_Prop_YrSp=log10(RFLU$Prop_YrSp+1)
RFLU$Log_Prop_4YrsPrSp=log10(RFLU$Prop_4YrsPrSp+1)
RFLU$Log_Prop_YrPr=log10(RFLU$Prop_YrPr+1)
RFLU$Log_Prop_3YrPrPr=log10(RFLU$Prop_3YrPrPr+1)

RFLU$Log_NumRdCross=log10(RFLU$NumRdCross+1)
RFLU$Log_Percent_Honly=log10(RFLU$Percent_Honly+1)
RFLU$Log_PctXclsr=log10(RFLU$PctXclsr+1)
#Mines and Dam numbers not transformed 
RFLU$Log_DAMvol_WS=log10(RFLU$DAMvol_WS+1)
RFLU$Log_ArtPathDens=log10(RFLU$ArtPathDens+1)
RFLU$Log_AG_WS=log10(RFLU$AG_WS+1)
RFLU$Log_URBAN_WS=log10(RFLU$URBAN_WS+1)
RFLU$Log_PctFWS=log10(RFLU$PctFWS+1)


boxplotdata=RFLU[,c(64:83)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}

colnames(RFLU)
#All columns
#P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+
#  AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+
#  PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+StmOrd+
#  Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+Slope_WS.1+AREA_SQKM+SITE_ELEV+
#  ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS+Log_AREA_SQKM+Log_OE_TN+Log_OE_TP+Log_alru_dom+
#  Log_SprgNum_WS+Log_PerDensC+Log_Slope_WS+Log_HYDR_WS+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
#  Log_Percent_Honly+Log_PctXclsr+Log_DAMvol_WS+Log_ArtPathDens+Log_AG_WS+Log_URBAN_WS+Log_PctFWS
#Relplacing the variables with Log variables
SiteCode+Year+Log_OE_TN+Log_OE_TP+xcdenmid+XCMG+P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
  RdDensC+Percent_HBonly+Log_Percent_Honly+Percent_HMA+Log_PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+Log_DAMvol_WS+Log_ArtPathDens+
  Log_AG_WS+Log_URBAN_WS+PctOtherOwn+PrivPct+BLMPct+Log_PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+Log_SprgNum_WS+SpNum300m+SpNum800m+
  StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+
  UCS_Mean+SumAve_P+MEANP_WS
  




########################################################################################################


########################################################################################################
