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
A.RFLU=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run3_IDlanduse\\LU_Nat_Anthro_data_3April2015.csv")

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
RFLU_CorSubset=RFLU[,6:68]
RFLUdataCor=cor(RFLU_CorSubset)
#write.csv(RFLUdataCor,"\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run3_IDlanduse\\RFLUdataCorrelations.csv")

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

LUSub=RFLU[,c(63,6:14)]
pairs(LUSub, lower.panel=panel.smooth, upper.panel=panel.cor)

LUSub1=RFLU[,c(63,14:17)]
pairs(LUSub1, lower.panel=panel.smooth, upper.panel=panel.cor)

LUSub1=RFLU[,c(63,18:22)]
pairs(LUSub1, lower.panel=panel.smooth, upper.panel=panel.cor)

LUSub2=RFLU[,c(63,22:31)]
pairs(LUSub2, lower.panel=panel.smooth, upper.panel=panel.cor)

LUSub2=RFLU[,c(63,69:72)]
pairs(LUSub2, lower.panel=panel.smooth, upper.panel=panel.cor)



########################################################################################################
#What data needs/should be transformed
######
boxplotdata=RFLU[,c(6:68)]
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
#What data needs/should be transformed
############

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

####################
#Random Forest Models
####################

#Variable Names:
#+P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+StmOrd+Slope_WS+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS
#OE_TN+OE_TP+xcdenmid+XCMG

P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
  NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
  SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
  SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
  MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS

OE_TN~
OE_TP~
Log_OE_TN~
Log_OE_TP~
xcdenmid~
XCMG~
  
RFLU=RFLU[-c(71:74),]
  
###################################################################
# Use anthropogenic only variables: TNLU
#colnames(RFLU)
###################################################################
#1) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                    MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#2) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+Percent_HMA+PctXclsr+DAMden_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)


#3) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+NumRdCross,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#4) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+NumRdCross,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)

#5) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~P_AUM_4YrPrSp+P_AUM_3YrPrPr+NumRdCross,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)
###################################################################
# Use anthropogenic and natural variables: TN
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                  SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                  SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                  MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+
                  AUM_YrSp+AUM_4YrPrSp+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                  SpNum300m+SpNum800m+StreamDens+PerDensC+StmOrd+AREA_SQKM+
                  SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                  DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_4YrsPrSp+
                  AUM_YrSp+AUM_4YrPrSp+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_3YrPrPr+
                  NumRdCross+PerDensC+AREA_SQKM+SITE_ELEV+ELEV_RANGE+PRMH_AVE+PCT_SEDIM+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                  NumRdCross+PerDensC+AREA_SQKM+SITE_ELEV+ELEV_RANGE+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~P_AUM_YrSp+Prop_4YrsPrSp+Prop_3YrPrPr+NumRdCross+AREA_SQKM+SITE_ELEV,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)


#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~Prop_4YrsPrSp+Prop_3YrPrPr+AREA_SQKM+SITE_ELEV,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~Prop_3YrPrPr+SITE_ELEV,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

par(mfrow=c(2,2))
partialPlot(TN, RFLU,Prop_3YrPrPr, cex.main=1)
partialPlot(TN, RFLU,SITE_ELEV, cex.main=1)


#8) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
RFLU$Log_Prop_3YrPrPr=log10(RFLU$Prop_3YrPrPr+1)
TN=randomForest(OE_TN~Log_Prop_3YrPrPr+SITE_ELEV,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

par(mfrow=c(2,2))
partialPlot(TN, RFLU,Log_Prop_3YrPrPr, cex.main=1)
partialPlot(TN, RFLU,SITE_ELEV, cex.main=1)

###################################################################
# Use transformed TN response variables: LTN
###################################################################
#1
LTN=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                   SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                   MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#2
LTN=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+StmOrd+Slope_WS+AREA_SQKM+
                   ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+
                   MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+PCT_SEDIM+Volcanic_7,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#3
LTN=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_3YrPrPr+AUM_YrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   RdDensC+MINEden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+SpNum300m+StreamDens+PerDensC+AREA_SQKM+
                   ELVmax_WS+SumAve_P+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+Volcanic_7,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#4
LTN=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_3YrPrPr+AUM_YrPr+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   URBAN_WS+StreamDens+AREA_SQKM+KFCT_AVE+PRMH_AVE,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)


#5
LTN=randomForest(Log_OE_TN~P_AUM_YrSp+Prop_4YrsPrSp+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)



#6
LTN=randomForest(Log_OE_TN~Prop_3YrPrPr+SITE_ELEV,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

###################################################################
# Use transformed TN response and Predictor variables: LTNL
###################################################################
#1
LTNL=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+
                    Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                    RdDensC+Percent_HMA+Log_PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+Log_AG_WS+Log_URBAN_WS+
                    Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+Log_PerDensC+IntDensC+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                    MINEnum_WS+DAMnum_WS+DAMvol_WS+Log_ArtPathDens+Log_HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#2
LTNL=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_YrPr+
                    AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+
                    Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_3YrPrPr+Log_NumRdCross+
                    RdDensC+MINEden_WS+Log_AG_WS+
                    SpNum300m+SpNum800m+Log_PerDensC+IntDensC+Log_AREA_SQKM+
                    ELVmax_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                    MINEnum_WS+DAMnum_WS+Log_ArtPathDens+Log_HYDR_WS+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#3
LTNL=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_YrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+
                    Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_3YrPrPr+RdDensC+Log_PerDensC+IntDensC+Log_AREA_SQKM+
                    KFCT_AVE+PRMH_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#4
LTNL=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_YrPr+P_Prop_YrSp+P_Prop_YrPr+AUM_3YrPrPr+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#5
LTNL=randomForest(Log_OE_TN~P_AUM_YrSp+P_AUM_YrPr+AUM_3YrPrPr+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)


par(mfrow=c(2,2))
partialPlot(LTNL, RFLU,P_AUM_YrSp, cex.main=1)
partialPlot(LTNL, RFLU,P_AUM_YrPr, cex.main=1)
partialPlot(LTNL, RFLU,AUM_3YrPrPr, cex.main=1)
partialPlot(LTNL, RFLU,Log_AREA_SQKM, cex.main=1)


#5.5
LTNL=randomForest(Log_OE_TN~Log_P_AUM_YrSp+Log_P_AUM_YrPr+AUM_3YrPrPr+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

RFLU$Log_P_AUM_YrSp=log10(RFLU$P_AUM_YrSp+1)
RFLU$Log_P_AUM_YrPr=log10(RFLU$P_AUM_YrPr+1)
par(mfrow=c(2,2))
partialPlot(LTNL, RFLU,Log_P_AUM_YrSp, cex.main=1)
partialPlot(LTNL, RFLU,Log_P_AUM_YrPr, cex.main=1)
partialPlot(LTNL, RFLU,AUM_3YrPrPr, cex.main=1)
partialPlot(LTNL, RFLU,Log_AREA_SQKM, cex.main=1)

#6
LTNL=randomForest(Log_OE_TN~AUM_3YrPrPr+Log_AREA_SQKM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

########################################################################################################
########################################################################################################

###################################################################
# Use anthropogenic variables: TPLU
###################################################################
#1
TPLU=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                    MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)

#2
TPLU=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+
                    NumRdCross+DAMvol_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)


#3
TPLU=randomForest(OE_TP~P_AUM_4YrPrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+Prop_YrSp+
                    NumRdCross,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)


#4
TPLU=randomForest(OE_TP~P_Prop_4YrsPrSp+P_Prop_3YrPrPr+AUM_YrSp+AUM_YrPr+NumRdCross,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)

#5
TPLU=randomForest(OE_TP~AUM_YrPr+NumRdCross,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)



###################################################################
# Use anthropogenic and natural variables: TP
###################################################################
#1) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                  SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                  SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                  MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#2) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                  SpNum300m+SpNum800m+StreamDens+PerDensC+StmOrd+Slope_WS+AREA_SQKM+
                  SITE_ELEV+ELEV_RANGE+ELVmax_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                  MINEnum_WS+DAMvol_WS+ArtPathDens+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#3) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_AUM_YrSp+P_Prop_YrSp+P_Prop_YrPr+
                  P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+AG_WS+URBAN_WS+
                  SpNum300m+SpNum800m+PerDensC+StmOrd+Slope_WS+AREA_SQKM+
                  SITE_ELEV+ELEV_RANGE+ELVmax_WS+TMAX_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+alru_dom+
                  MINEnum_WS+ArtPathDens+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#4) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_Prop_YrSp+P_Prop_YrPr+P_Prop_3YrPrPr+AUM_YrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+Percent_HMA+DAMden_WS+URBAN_WS+SpNum800m+PerDensC+Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+
                  TMAX_WS+KFCT_AVE+PRMH_AVE+alru_dom+MINEnum_WS+ArtPathDens,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#5) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_Prop_YrSp+P_Prop_YrPr+AUM_YrSp+AUM_YrPr+Prop_YrSp+Prop_YrPr+Prop_3YrPrPr+
                  NumRdCross+SpNum800m+Slope_WS+AREA_SQKM+ELEV_RANGE+ELVmax_WS+
                  PRMH_AVE+MINEnum_WS+ArtPathDens,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#6) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~Prop_YrSp+NumRdCross+AREA_SQKM+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#7) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~Prop_YrSp+NumRdCross+AREA_SQKM,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#8) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_Prop_3YrPrPr+Prop_4YrsPrSp+NumRdCross+AREA_SQKM+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)


#9) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_Prop_3YrPrPr+Prop_4YrsPrSp+NumRdCross+AREA_SQKM+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#10) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~P_Prop_3YrPrPr+NumRdCross+AREA_SQKM+PRMH_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)
LTPL
varImpPlot(LTPL)
par(mfrow=c(2,2))
partialPlot(TP, RFLU,P_Prop_3YrPrPr, cex.main=1)
partialPlot(TP, RFLU,NumRdCross, cex.main=1)
partialPlot(TP, RFLU,AREA_SQKM, cex.main=1)
partialPlot(TP, RFLU,PRMH_AVE, cex.main=1)


###################################################################
# Use transformed TP Response variables: LTP
###################################################################
#1) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                   SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                   MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#2) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_YrPr+
                   AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgNum_WS+SpNum300m+SpNum800m+StreamDens+StmOrd+Slope_WS+AREA_SQKM+
                   SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMAX_WS+TMIN_WS+UCS_Mean+PRMH_AVE+alru_dom+
                   MINEnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#3) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_YrPr+
                   AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_4YrsPrSp+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgNum_WS+SpNum300m+SpNum800m+StreamDens+StmOrd+Slope_WS+AREA_SQKM+
                   SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMAX_WS+TMIN_WS+UCS_Mean+PRMH_AVE+alru_dom+
                   MINEnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#4) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_Prop_YrSp+P_Prop_YrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+
                   NumRdCross+RdDensC+PctXclsr+MINEden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                  SpNum300m+SpNum800m+StreamDens+StmOrd+Slope_WS+AREA_SQKM+
                   ELEV_RANGE+SumAve_P+TMAX_WS+TMIN_WS+UCS_Mean+PRMH_AVE+
                   MINEnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#5) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~P_Prop_YrPr+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+NumRdCross+RdDensC+DAMvol_Stand_WS+
                  StreamDens+StmOrd+Slope_WS+AREA_SQKM+SumAve_P+PRMH_AVE+DAMvol_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#6) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+NumRdCross+
                   StreamDens+StmOrd+Slope_WS+AREA_SQKM+SumAve_P+PRMH_AVE+DAMvol_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)


#7) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~AUM_4YrPrSp+AUM_YrPr+NumRdCross+StreamDens+StmOrd+Slope_WS+AREA_SQKM+PRMH_AVE,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)



#8) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~AUM_4YrPrSp+AUM_YrPr+NumRdCross+StreamDens+StmOrd+Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)



#9) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~AUM_4YrPrSp+NumRdCross+StreamDens+StmOrd,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

par(mfrow=c(2,2))
partialPlot(LTP, RFLU,AUM_4YrPrSp, cex.main=1)
partialPlot(LTP, RFLU,NumRdCross, cex.main=1)
partialPlot(LTP, RFLU,StreamDens, cex.main=1)
partialPlot(LTP, RFLU,StmOrd, cex.main=1)


#10) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~AUM_4YrPrSp+StreamDens+StmOrd,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#11) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~AUM_4YrPrSp+StmOrd,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#12) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~AUM_4YrPrSp+StreamDens,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)


###################################################################
# Use transformed TP Response and predictor variables: LTPL
####################################################################
#1
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+
                    Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                    RdDensC+Percent_HMA+Log_PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+Log_AG_WS+Log_URBAN_WS+
                    Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+Log_PerDensC+IntDensC+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                    MINEnum_WS+DAMnum_WS+DAMvol_WS+Log_ArtPathDens+Log_HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#2
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_4YrsPrSp+AUM_4YrPrSp+AUM_YrPr+
                    Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                    Percent_HMA+Log_PctXclsr+MINEden_WS+DAMvol_Stand_WS+Log_AG_WS+Log_URBAN_WS+
                    Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMIN_WS+UCS_Mean+PRMH_AVE+Log_alru_dom+
                    DAMnum_WS+DAMvol_WS+Log_ArtPathDens+Log_HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#3
LTPL=randomForest(Log_OE_TP~P_AUM_YrSp+P_AUM_4YrPrSp+AUM_4YrPrSp+AUM_YrPr+Log_Prop_YrSp+Log_Prop_YrPr+Log_NumRdCross+
                    MINEden_WS+DAMvol_Stand_WS+Log_AG_WS+Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                    SITE_ELEV+ELVmax_WS+SumAve_P+TMIN_WS+UCS_Mean+PRMH_AVE+DAMvol_WS+PCT_SEDIM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#4
LTPL=randomForest(Log_OE_TP~AUM_4YrPrSp+AUM_YrPr+Log_Prop_YrPr+Log_NumRdCross+DAMvol_Stand_WS+
                    StreamDens+StmOrd+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+SumAve_P+TMIN_WS+PRMH_AVE+DAMvol_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#5
LTPL=randomForest(Log_OE_TP~AUM_4YrPrSp+AUM_YrPr+Log_NumRdCross+StmOrd+Log_AREA_SQKM+StreamDens+Log_Slope_WS+PRMH_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#6
LTPL=randomForest(Log_OE_TP~AUM_4YrPrSp+Log_NumRdCross+StmOrd+StreamDens,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#7
LTPL=randomForest(Log_OE_TP~AUM_4YrPrSp+Log_NumRdCross+StmOrd+StreamDens,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

par(mfrow=c(2,2))
partialPlot(LTPL, RFLU,AUM_4YrPrSp, cex.main=1)
partialPlot(LTPL, RFLU,Log_NumRdCross, cex.main=1)
partialPlot(LTPL, RFLU,StmOrd, cex.main=1)
partialPlot(LTPL, RFLU,StreamDens, cex.main=1)

#8
LTPL=randomForest(Log_OE_TP~AUM_4YrPrSp+StmOrd+StreamDens,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)
par(mfrow=c(2,2))
partialPlot(LTPL, RFLU,AUM_4YrPrSp, cex.main=1)
partialPlot(LTPL, RFLU,StmOrd, cex.main=1)
partialPlot(LTPL, RFLU,StreamDens, cex.main=1)

#9
LTPL=randomForest(Log_OE_TP~AUM_4YrPrSp+StreamDens,
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
                      NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                      MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#2) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                      P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                      NumRdCross+RdDensC+Percent_HMA+AG_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#3) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_3YrPrPr+NumRdCross+Percent_HMA+AG_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#4) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_YrSp+P_Prop_YrSp+NumRdCross+Percent_HMA,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

par(mfrow=c(2,2))
partialPlot(XCMGLU, RFLU,P_AUM_YrSp, cex.main=1)
partialPlot(XCMGLU, RFLU,P_Prop_YrSp, cex.main=1)
partialPlot(XCMGLU, RFLU,NumRdCross, cex.main=1)
partialPlot(XCMGLU, RFLU,Percent_HMA, cex.main=1)


#5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_AUM_YrSp+P_Prop_YrSp+Percent_HMA,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)


#6) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGLU=randomForest(XCMG~P_Prop_YrSp+NumRdCross+Percent_HMA,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#7) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
RFLU$Log_P_AUM_YrSp=log10(RFLU$P_AUM_YrSp+1)
RFLU$Log_P_Prop_YrSp=log10(RFLU$P_Prop_YrSp+1)
RFLU$Log_Percent_HMA=log10(RFLU$Percent_HMA+1)

XCMGLU=randomForest(XCMG~Log_P_AUM_YrSp+Log_P_Prop_YrSp+Log_NumRdCross+Log_Percent_HMA,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

par(mfrow=c(2,2))
partialPlot(XCMGLU, RFLU,Log_P_AUM_YrSp, cex.main=1)
partialPlot(XCMGLU, RFLU,Log_P_Prop_YrSp, cex.main=1)
partialPlot(XCMGLU, RFLU,Log_NumRdCross, cex.main=1)
partialPlot(XCMGLU, RFLU,Log_Percent_HMA, cex.main=1)
 
###################################################################
# Use anthropogenic and natural variables: XCMG
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                    SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                    MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#2) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_3YrPrPr+
                    NumRdCross+Percent_HMA+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                    SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                    MINEnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


#3) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+
                    AUM_YrSp+Prop_YrSp+Prop_4YrsPrSp+Prop_3YrPrPr+
                    NumRdCross+Percent_HMA+MINEden_WS+DAMden_WS+URBAN_WS+
                    SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                    MINEnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


#4) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+
                    AUM_YrSp+Prop_YrSp+Prop_4YrsPrSp+
                    NumRdCross+Percent_HMA+DAMden_WS+
                    SprgNum_WS+SpNum300m+SpNum800m+IntDensC+StmOrd+AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMAX_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                    MINEnum_WS+DAMvol_WS+HYDR_WS+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


#5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+Prop_3YrPrPr+
                    NumRdCross+Percent_HMA+MINEden_WS+
                    SprgNum_WS+SpNum300m+StmOrd+AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+TMAX_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                    MINEnum_WS+DAMvol_WS+HYDR_WS+PCT_SEDIM,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


#6) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_Prop_YrSp+
                    NumRdCross+Percent_HMA+SprgNum_WS+StmOrd+
                    ELEV_RANGE+TMAX_WS+KFCT_AVE+PRMH_AVE+alru_dom,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)



#7) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_AUM_4YrPrSp+P_Prop_YrSp+NumRdCross+Percent_HMA+
                    StmOrd+ELEV_RANGE+KFCT_AVE+alru_dom,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


par(mfrow=c(2,2))
partialPlot(XCMG, RFLU,P_AUM_4YrPrSp, cex.main=1)
partialPlot(XCMG, RFLU,P_Prop_YrSp, cex.main=1)
partialPlot(XCMG, RFLU,NumRdCross, cex.main=1)
partialPlot(XCMG, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMG, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMG, RFLU,KFCT_AVE, cex.main=1)
partialPlot(XCMG, RFLU,alru_dom, cex.main=1)
partialPlot(XCMG, RFLU,StmOrd, cex.main=1)

#8) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_Prop_YrSp+NumRdCross+Percent_HMA+ELEV_RANGE+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

par(mfrow=c(2,2))
partialPlot(XCMG, RFLU,P_Prop_YrSp, cex.main=1)
partialPlot(XCMG, RFLU,NumRdCross, cex.main=1)
partialPlot(XCMG, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMG, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMG, RFLU,KFCT_AVE, cex.main=1)

#9) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~NumRdCross+Percent_HMA+ELEV_RANGE+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)


#10) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~P_Prop_YrSp+NumRdCross+ELEV_RANGE+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#11) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~NumRdCross+ELEV_RANGE+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)
#12) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~NumRdCross+ELEV_RANGE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)
#13) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~NumRdCross+P_Prop_YrSp+Percent_HMA+ELEV_RANGE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)



###################################################################
# Use transformed anthropogenic and natural variables: XCMGL
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+
                     Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                     RdDensC+Percent_HMA+Log_PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+Log_AG_WS+Log_URBAN_WS+
                     Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+Log_PerDensC+IntDensC+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                     SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                     MINEnum_WS+DAMnum_WS+DAMvol_WS+Log_ArtPathDens+Log_HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)


#2) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_AUM_YrSp+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                     AUM_YrPr+AUM_3YrPrPr+
                     Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                     RdDensC+Percent_HMA+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+Log_AG_WS+
                     Log_SprgNum_WS+SpNum300m+StreamDens+Log_PerDensC+IntDensC+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                     SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                     MINEnum_WS+DAMnum_WS+DAMvol_WS+Log_ArtPathDens+Log_HYDR_WS+PCT_SEDIM+Volcanic_7,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#3) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_Prop_YrPr+AUM_3YrPrPr+
                     Log_Prop_4YrsPrSp+Log_Prop_3YrPrPr+Log_NumRdCross+
                     RdDensC+Percent_HMA+DAMden_WS+DAMvol_Stand_WS+
                     SpNum300m+IntDensC+StmOrd+Log_AREA_SQKM+
                     SITE_ELEV+ELEV_RANGE+MEANP_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                     MINEnum_WS+DAMnum_WS+DAMvol_WS+PCT_SEDIM,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#4) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~P_Prop_YrPr+Log_NumRdCross+RdDensC+Percent_HMA+
                     StmOrd+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)


#5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~Log_NumRdCross+Percent_HMA+StmOrd+ELEV_RANGE+PRMH_AVE+Log_alru_dom,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

par(mfrow=c(2,2))
partialPlot(XCMGL, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMGL, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGL, RFLU,Log_NumRdCross, cex.main=1)
partialPlot(XCMGL, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGL, RFLU,PRMH_AVE, cex.main=1)
partialPlot(XCMGL, RFLU,Log_alru_dom, cex.main=1)

#5.5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
RFLU$Log_PRMH_AVE=log10(RFLU$PRMH_AVE+1)

XCMGL=randomForest(XCMG~Log_NumRdCross+Percent_HMA+StmOrd+ELEV_RANGE+Log_PRMH_AVE+Log_alru_dom,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

par(mfrow=c(2,2))
partialPlot(XCMGL, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMGL, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGL, RFLU,Log_NumRdCross, cex.main=1)
partialPlot(XCMGL, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGL, RFLU,Log_PRMH_AVE, cex.main=1)
partialPlot(XCMGL, RFLU,Log_alru_dom, cex.main=1)


#6) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~Log_NumRdCross+Percent_HMA+StmOrd+ELEV_RANGE+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

par(mfrow=c(2,2))
partialPlot(XCMGL, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMGL, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGL, RFLU,Log_NumRdCross, cex.main=1)
partialPlot(XCMGL, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGL, RFLU,PRMH_AVE, cex.main=1)

#7) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~Log_NumRdCross+ELEV_RANGE+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)


####

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
                     NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                     MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#2) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_3YrPrPr+AUM_4YrPrSp+AUM_3YrPrPr+DAMvol_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#3) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~P_Prop_3YrPrPr+AUM_4YrPrSp,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

###################################################################
# Use anthropogenic and natural variables: den
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                   NumRdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+AREA_SQKM+
                   SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                   MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                   P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_3YrPrPr+Prop_YrSp+
                   NumRdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+
                   SprgNum_WS+SpNum300m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+
                   ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+
                   MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)


#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+
                   P_Prop_3YrPrPr+AUM_4YrPrSp+Prop_YrSp+
                   NumRdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+
                   SprgNum_WS+SpNum300m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+
                   ELEV_RANGE+SumAve_P+MEANP_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+alru_dom+
                   DAMnum_WS+DAMvol_WS+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)
#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+
                   P_Prop_3YrPrPr+AUM_4YrPrSp+Prop_YrSp+
                   NumRdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+
                   SprgNum_WS+SpNum300m+StreamDens+PerDensC+IntDensC+StmOrd+Slope_WS+
                   ELEV_RANGE+SumAve_P+MEANP_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+alru_dom+
                   DAMnum_WS+DAMvol_WS+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_4YrsPrSp+
                   P_Prop_3YrPrPr+AUM_4YrPrSp+NumRdCross+Percent_HMA+PctXclsr+MINEden_WS+
                   SprgNum_WS+SpNum300m+StreamDens+IntDensC+StmOrd+
                   ELEV_RANGE+SumAve_P+MEANP_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+alru_dom+
                   DAMnum_WS+DAMvol_WS+HYDR_WS+PCT_SEDIM+Volcanic_7,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)
#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_4YrsPrSp+
                   P_Prop_3YrPrPr+AUM_4YrPrSp+Percent_HMA+PctXclsr+MINEden_WS+
                   SprgNum_WS+IntDensC+ELEV_RANGE+SumAve_P+TMIN_WS+KFCT_AVE+PRMH_AVE+alru_dom+
                   DAMvol_WS+HYDR_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~P_AUM_4YrPrSp+P_AUM_3YrPrPr+P_Prop_3YrPrPr+AUM_4YrPrSp+
                   PctXclsr+IntDensC+TMIN_WS+KFCT_AVE+PRMH_AVE+DAMvol_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)
#8) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~AUM_4YrPrSp+IntDensC+TMIN_WS+KFCT_AVE+PRMH_AVE,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#9) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~AUM_4YrPrSp+IntDensC+TMIN_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)
par(mfrow=c(2,2))
partialPlot(den, RFLU,AUM_4YrPrSp, cex.main=1)
partialPlot(den, RFLU,IntDensC, cex.main=1)
partialPlot(den, RFLU,TMIN_WS, cex.main=1)

#10) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~IntDensC+TMIN_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

den
varImpPlot(den)
par(mfrow=c(2,2))
partialPlot(den, RFLU,IntDensC, cex.main=1)
partialPlot(den, RFLU,TMIN_WS, cex.main=1)

#11) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~IntDensC,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#12) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~TMIN_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

###################################################################
# Use transformed anthropogenic and natural variables: denL
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+
                    Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+Log_NumRdCross+
                    RdDensC+Percent_HMA+Log_PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+Log_AG_WS+Log_URBAN_WS+
                    Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+Log_PerDensC+IntDensC+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                    MINEnum_WS+DAMnum_WS+DAMvol_WS+Log_ArtPathDens+Log_HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_YrSp+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_3YrPrPr+AUM_4YrPrSp+
                    Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_NumRdCross+
                    RdDensC+Percent_HMA+Log_PctXclsr+DAMden_WS+DAMvol_Stand_WS+Log_AG_WS+Log_URBAN_WS+
                    Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+Log_PerDensC+IntDensC+StmOrd+Log_Slope_WS+Log_AREA_SQKM+
                    ELEV_RANGE+ELVmax_WS+SumAve_P+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+
                    DAMvol_WS+Log_HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_3YrPrPr+P_Prop_3YrPrPr+AUM_4YrPrSp+Log_Prop_4YrsPrSp+Log_NumRdCross+
                    RdDensC+Log_PctXclsr+DAMden_WS+Log_AG_WS+Log_SprgNum_WS+SpNum300m+SpNum800m+IntDensC+StmOrd+Log_Slope_WS+
                    ELEV_RANGE+ELVmax_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+Log_alru_dom+DAMvol_WS+Log_HYDR_WS+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_3YrPrPr+P_Prop_3YrPrPr+AUM_4YrPrSp+Log_Prop_4YrsPrSp+
                   Log_PctXclsr+Log_SprgNum_WS+IntDensC+ELEV_RANGE+TMIN_WS+KFCT_AVE+PRMH_AVE+Log_HYDR_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)
#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_3YrPrPr+Log_PctXclsr+IntDensC+TMIN_WS+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_3YrPrPr+IntDensC+TMIN_WS+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)


par(mfrow=c(2,2))
partialPlot(denL, RFLU,P_AUM_3YrPrPr, cex.main=1)
partialPlot(denL, RFLU,IntDensC, cex.main=1)
partialPlot(denL, RFLU,TMIN_WS, cex.main=1)
partialPlot(denL, RFLU,KFCT_AVE, cex.main=1)

#6.5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
RFLU$Log_P_AUM_3YrPrPr=log10(RFLU$P_AUM_3YrPrPr+1)

denL=randomForest(xcdenmid~Log_P_AUM_3YrPrPr+IntDensC+TMIN_WS+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)


par(mfrow=c(2,2))
partialPlot(denL, RFLU,Log_P_AUM_3YrPrPr, cex.main=1)
partialPlot(denL, RFLU,IntDensC, cex.main=1)
partialPlot(denL, RFLU,TMIN_WS, cex.main=1)
partialPlot(denL, RFLU,KFCT_AVE, cex.main=1)


#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~P_AUM_3YrPrPr+IntDensC+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#8) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~IntDensC+TMIN_WS+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)


#9) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~IntDensC+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)


#9) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~PctXclsr+SprgNum_WS+ELEV_RANGE+DAMvol_WS+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)



########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################



AreaProb=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run6_AreaProblems\\BugsIndicatorsLandUseNatural_24March2015.csv")

AreaProb$Log_OE_TN=log10(ifelse(AreaProb$OE_TN<0,0,AreaProb$OE_TN)+1)
AreaProb$Log_OE_TP=log10(ifelse(AreaProb$OE_TP<0,0,AreaProb$OE_TP)+1)


panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


ARSub=AreaProb[,c(7,95:96,16,19)]
pairs(ARSub, lower.panel=panel.cor, upper.panel=panel.smooth)


ARSub=AreaProb[,c(7,8,20:24)]
pairs(ARSub, lower.panel=panel.cor, upper.panel=panel.smooth)

ARSub=AreaProb[,c(7,25:30,78)]
pairs(ARSub, lower.panel=panel.cor, upper.panel=panel.smooth)

#1) Run RF for TN response and all LU predictor variables
Area=randomForest(Log_AREA_SQKM~NV_MMI+NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+
                    PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                    PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                    P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment+
                    MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+AG_WS+URBAN_WS+PctOtherOwn+PrivPct+BLMPct+PctFWS+PctFS+
                    StmOrd+PCT_SEDIM+Volcanic_7+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                    Slope_WS+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                  data=AreaProb, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Area
varImpPlot(Area)

#20) Run RF for TN response and all LU predictor variables
Area=randomForest(Log_AREA_SQKM~NV_MMI+NV_Invasives+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                    PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW,
                  data=AreaProb, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Area
varImpPlot(Area)


par(mfrow=c(2,3))
partialPlot(Area, AreaProb,OE_Conduct, cex.main=1)
partialPlot(Area, AreaProb,NV_MMI, cex.main=1)
partialPlot(Area, AreaProb,BnkCover_BLM, cex.main=1)
partialPlot(Area, AreaProb,XEMBED, cex.main=1)
partialPlot(Area, AreaProb,OE_TP, cex.main=1)
partialPlot(Area, AreaProb,NV_Invasives, cex.main=1)
partialPlot(Area, AreaProb,xcdenbk, cex.main=1)
partialPlot(Area, AreaProb,PH, cex.main=1)
partialPlot(Area, AreaProb,LINCIS_H, cex.main=1)
partialPlot(Area, AreaProb,XCMG, cex.main=1)
partialPlot(Area, AreaProb,BnkStability_BLM, cex.main=1)
partialPlot(Area, AreaProb,XCMGW, cex.main=1)
partialPlot(Area, AreaProb,L_XCMGW, cex.main=1)
partialPlot(Area, AreaProb,XFC_NAT, cex.main=1)
partialPlot(Area, AreaProb,OE_TN, cex.main=1)







#11) Run RF for TN response and all LU predictor variables
Area=randomForest(Log_AREA_SQKM~NV_MMI+NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+
                    PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                    PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                    P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross+RdDensC+Percent_HBonly+Percent_Honly+Percent_HMA+PctXclsr+Percent_Allotment,
                  data=AreaProb, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Area
varImpPlot(Area)



#12) Run RF for TN response and all LU predictor variables
Area=randomForest(Log_AREA_SQKM~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross,
                  data=AreaProb, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Area
varImpPlot(Area)


#13) Run RF for TN response and all LU predictor variables
Area=randomForest(Log_AREA_SQKM~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Prop_YrSp+Prop_4YrsPrSp+Prop_YrPr+Prop_3YrPrPr+
                    NumRdCross,
                  data=AreaProb, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Area
varImpPlot(Area)




#13) Run RF for TN response and all LU predictor variables
Area=randomForest(Log_AREA_SQKM~P_AUM_YrSp+P_AUM_4YrPrSp+P_AUM_YrPr+P_AUM_3YrPrPr+P_Prop_YrSp+P_Prop_4YrsPrSp+P_Prop_YrPr+
                    P_Prop_3YrPrPr+AUM_YrSp+AUM_4YrPrSp+AUM_YrPr+AUM_3YrPrPr+Log_Prop_YrSp+Log_Prop_4YrsPrSp+Log_Prop_YrPr+Log_Prop_3YrPrPr+
                    NumRdCross,
                  data=AreaProb, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Area
varImpPlot(Area)



par(mfrow=c(2,3))
partialPlot(Area, AreaProb,AUM_YrSp, cex.main=1)
partialPlot(Area, AreaProb,AUM_4YrPrSp, cex.main=1)
partialPlot(Area, AreaProb,Log_Prop_YrSp, cex.main=1)
partialPlot(Area, AreaProb,Log_Prop_4YrsPrSp, cex.main=1)
partialPlot(Area, AreaProb,Log_Prop_3YrPrPr, cex.main=1)
partialPlot(Area, AreaProb,NumRdCross, cex.main=1)

AreaProb$Log_Prop_YrSp=log10(AreaProb$Prop_YrSp+1)
AreaProb$Log_Prop_4YrsPrSp=log10(AreaProb$Prop_4YrsPrSp+1)
AreaProb$Log_Prop_YrPr=log10(AreaProb$Prop_YrPr+1)
AreaProb$Log_Prop_3YrPrPr=log10(AreaProb$Prop_3YrPrPr+1)






