##############################################################################################################################################################################################
#Random Forest for Northern California to investigate the LU variables directly related to the MMI scores....
#16March2015
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

RFLU=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run5_MMI_to_LUdata\\LandUse_MMI_16March2015.csv")




####################
##Transform some variables
####################
#Boxplots were used to decide on transformed variables.
#Use all variables, but some transformed

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


#All variables in data
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


###################################################################
# Use MMI response variable and LU/Natural: MMILU
###################################################################

#1
MMILU=randomForest(NV_MMI~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                   DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                   StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

#2
MMILU=randomForest(NV_MMI~P_AUM_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     P_Prop_3YrPrPr+AUM_YrSp+AUM_YrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                     NumRdCross+RdDensC+Percent_HBonly+Percent_HMA+MINEnum_WS+
                     DAMnum_WS+DAMvol_WS+AG_WS+URBAN_WS+PctOtherOwn+BLMPct+PctFWS+PctFS+
                     Volcanic_7+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                     Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+UCS_Mean+SumAve_P,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)


#3
MMILU=randomForest(NV_MMI~P_AUM_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     P_Prop_3YrPrPr+AUM_YrSp+AUM_YrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                     RdDensC+Percent_HBonly+MINEnum_WS+
                     DAMvol_WS+AG_WS+URBAN_WS+BLMPct+PctFS+
                     Volcanic_7+SpNum800m+StreamDens+IntDensC+HYDR_WS+
                     Slope_WS+AREA_SQKM+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+UCS_Mean+SumAve_P,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

#3
MMILU=randomForest(NV_MMI~P_AUM_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     P_Prop_3YrPrPr+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                     MINEnum_WS+
                     AG_WS+BLMPct+PctFS+
                     Volcanic_7+SpNum800m+IntDensC+
                     Slope_WS+AREA_SQKM+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+UCS_Mean+SumAve_P,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)


#4
MMILU=randomForest(NV_MMI~P_Prop_4YrsPrSp+P_Prop_YrPr+
                     Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                     IntDensC+
                     Slope_WS+AREA_SQKM+ELEV_RANGE+KFCT_AVE+alru_dom,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

#5
MMILU=randomForest(NV_MMI~Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                     IntDensC+
                     AREA_SQKM,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

#6
MMILU=randomForest(NV_MMI~Prop_4YrsPrSp+
                     IntDensC+
                     AREA_SQKM,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

par(mfrow=c(2,2))
partialPlot(MMILU, RFLU,Prop_4YrsPrSp, cex.main=1)
partialPlot(MMILU, RFLU,IntDensC, cex.main=1)
partialPlot(MMILU, RFLU,AREA_SQKM, cex.main=1)


#6
MMILU=randomForest(NV_MMI~
                     IntDensC+
                     AREA_SQKM,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)


#6
MMILU=randomForest(NV_MMI~AREA_SQKM,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

####################
##Subset data
####################
##Remove watersheds with small watershed area
RFLU1=RFLU[(RFLU$AREA_SQKM>14),]



#1
MMILU=randomForest(NV_MMI~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                     NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                     DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                     StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                     Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                   data=RFLU1, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

#2
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_YrPr+
                     NumRdCross+Percent_Honly+Percent_Allotment+
                     AG_WS+URBAN_WS+BLMPct+PctFS+
                     SpNum300m+SpNum800m+
                     Slope_WS+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+UCS_Mean+SumAve_P+MEANP_WS,
                   data=RFLU1, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

#3
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_YrPr+
                    Percent_Honly+Percent_Allotment+
                     KFCT_AVE+alru_dom+MEANP_WS,
                   data=RFLU1, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

#3
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_YrPr,
                   data=RFLU1, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)



#################################################################################


#1
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                     DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+
                     StmOrd+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+
                     Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+UCS_Mean,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)


#1
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+RdDensC+
                     Percent_HBonly+Percent_Honly+Percent_HMA+Percent_Allotment+
                     ArtPathDens+AG_WS+URBAN_WS+
                     SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+
                     Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+UCS_Mean,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)


#1
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+RdDensC+
                     Percent_Honly+Percent_Allotment+
                     ArtPathDens+
                     StreamDens+PerDensC+IntDensC+
                     Slope_WS+AREA_SQKM+ELEV_RANGE+KFCT_AVE+PRMH_AVE+UCS_Mean,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)




#1
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+RdDensC+
                   StreamDens+IntDensC+
                     AREA_SQKM+ELEV_RANGE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)


#1
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_YrPr+
                     IntDensC+
                     AREA_SQKM,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

par(mfrow=c(2,2))
partialPlot(MMILU, RFLU,P_Prop_YrSp, cex.main=1)
partialPlot(MMILU, RFLU,P_Prop_YrPr, cex.main=1)
partialPlot(MMILU, RFLU,IntDensC, cex.main=1)
partialPlot(MMILU, RFLU,AREA_SQKM, cex.main=1)

#1
MMILU=randomForest(NV_MMI~P_Prop_YrPr+
                     IntDensC+
                     AREA_SQKM,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)

##############################################################
#1
MMILU=randomForest(NV_MMI~P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+
                     DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+
                     StmOrd+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+
                     Slope_WS+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+UCS_Mean,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
MMILU
varImpPlot(MMILU)
