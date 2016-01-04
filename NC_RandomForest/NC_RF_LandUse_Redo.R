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
RFLU=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run3_IDlanduse\\New_21April2015\\LU_Nat_Anthro_data_21April2015.csv")

#Variable Names: done in excel
#OE_Conduct+OE_TN+OE_TP+PH+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
#A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
#A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
#Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
#SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
#Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
#UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+MINEnum_WS+DAMnum_WS+DAMvol_WS+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS

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
RFLU_CorSubset=RFLU[,6:81]
RFLUdataCor=cor(RFLU_CorSubset)
#write.csv(RFLUdataCor,"\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run3_IDlanduse\\New_21April2015\\Correlations.csv")

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
############

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
#
summary(RFLU$OE_TN)
RFLU$OE_TN_2=((RFLU$OE_TN+179.50)+1)
boxplot(RFLU$OE_TN_2)
RFLU$OE_TN_3=log10(RFLU$OE_TN_2)
par(mfrow=c(2,3))
boxplot(RFLU$OE_TN_3)
boxplot(RFLU$Log_OE_TN)
boxplot(RFLU$OE_TN)
hist(RFLU$OE_TN_3)
hist(RFLU$Log_OE_TN)
hist(RFLU$OE_TN)

#
summary(RFLU$OE_TP)
RFLU$OE_TP_2=((RFLU$OE_TP+40.76)+1)
boxplot(RFLU$OE_TP_2)
RFLU$OE_TP_3=log10(RFLU$OE_TP_2)
boxplot(RFLU$OE_TP_3)
boxplot(RFLU$Log_OE_TP)
par(mfrow=c(2,3))
boxplot(RFLU$OE_TP_3)
boxplot(RFLU$Log_OE_TP)
boxplot(RFLU$OE_TP)
hist(RFLU$OE_TP_3)
hist(RFLU$Log_OE_TP)
hist(RFLU$OE_TP)
#

#Can't arcsin square root because there are values >1
RFLU$SR_Prop_YrSp=sqrt(RFLU$A_Prop_YrSp)
RFLU$SR_Prop_4YrsPrSp=sqrt(RFLU$A_Prop_4YrsPrSp)
RFLU$SR_Prop_YrPr=sqrt(RFLU$A_Prop_YrPr)
RFLU$SR_Prop_3YrPrPr=sqrt(RFLU$A_Prop_3YrPrPr)
RFLU$Log_AUM_YrSp=log10(RFLU$A_AUM_YrSp+1)
RFLU$Log_AUM_4YrsPrSp=log10(RFLU$A_AUM_4YrPrSp+1)
RFLU$Log_AUM_YrPr=log10(RFLU$A_AUM_YrPr+1)
RFLU$Log_AUM_3YrPrPr=log10(RFLU$A_AUM_3YrPrPr+1)

#summary(RFLU$AG_WS)
RFLU$LGT_AG_WS=logit(RFLU$AG_WS+0.1)
RFLU$LGT_URBAN_WS=logit(RFLU$URBAN_WS+0.1)

#The distribution of these variables did not change enought to warrent transformation.
#RFLU$Log_SpDensity300m=log10(RFLU$SpDensity300m+1)
#RFLU$Log_A_SpDensity800m=log10(RFLU$A_SpDensity800m+1)
#RFLU$Log_MINEden_WS=log10(RFLU$MINEden_WS+1)
#RFLU$Log_DAMden_WS=log10(RFLU$DAMden_WS+1)
#RFLU$Log_DAMden_WS=log10(RFLU$DAMden_WS+1)

boxplotdata=RFLU[,c(68:78)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}



colnames(RFLU)
#All columns
A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
  Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
  SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
  UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS
#Transformed
A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
  Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
  Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
  SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
  Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
  UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS

########################################################################################################
########################################################################################################


###################################################################
# Use anthropogenic only variables: TNLU
#colnames(RFLU)
###################################################################
#1) Run RF for TN response and all LU predictor variables
TNLU=randomForest(OE_TN~,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNLU
varImpPlot(TNLU)
###################################################################
# Use anthropogenic and natural variables: TN
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                  Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                  SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                  UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                  RdDensC+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+URBAN_WS+
                  SprgDensity_WS+A_SpDensity800m+PerDensC+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+
                  KFCT_AVE+PRMH_AVE+HYDR_WS+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                  RdDensC+MINEden_WS+DAMden_WS+URBAN_WS+A_SpDensity800m+
                  Slope_WS+AREA_SQKM+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+KFCT_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_P_AUM_YrPr+A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                  AREA_SQKM+ELEV_RANGE+MEANP_WS+KFCT_AVE,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+A_Prop_3YrPrPr+AREA_SQKM,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)


#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_Prop_4YrsPrSp+A_Prop_3YrPrPr+AREA_SQKM,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)


par(mfrow=c(2,3))
partialPlot(TN, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TN, RFLU,A_Prop_4YrsPrSp, cex.main=1)
partialPlot(TN, RFLU,AREA_SQKM, cex.main=1)


#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_Prop_4YrsPrSp+A_Prop_3YrPrPr,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

par(mfrow=c(2,3))
partialPlot(TN, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TN, RFLU,A_Prop_4YrsPrSp, cex.main=1)




#7.5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
RFLU$SQRT_A_Prop_4YrsPrSp=sqrt(RFLU$A_Prop_4YrsPrSp)
RFLU$Log_alru_dom=sqrt(RFLU$alru_dom)

TN=randomForest(OE_TN~SQRT_A_Prop_4YrsPrSp+A_Prop_3YrPrPr,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

par(mfrow=c(2,3))
partialPlot(TN, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TN, RFLU,SQRT_A_Prop_4YrsPrSp, cex.main=1)

#8) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_Prop_3YrPrPr,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)

#9) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
TN=randomForest(OE_TN~A_Prop_4YrsPrSp,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN
varImpPlot(TN)
###################################################################
# Use transformed TN response variables: LTN
###################################################################
#1
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                   UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#2
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_Prop_YrSp+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+
                   A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgDensity_WS+A_SpDensity800m+StreamDens+PerDensC+StmOrd+Slope_WS+AREA_SQKM+SITE_ELEV+ELVmax_WS+SumAve_P+MEANP_WS+
                   TMAX_WS+UCS_Mean+KFCT_AVE+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#3
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_Prop_YrSp+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   RdDensC+MINEden_WS+DAMden_WS+AG_WS+SprgDensity_WS+A_SpDensity800m+StreamDens+
                   AREA_SQKM+SITE_ELEV+SumAve_P+UCS_Mean+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#4
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_Prop_YrSp+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   AREA_SQKM+SITE_ELEV+SumAve_P+HYDR_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#5
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+
                   A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+A_Prop_3YrPrPr,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#6
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_Prop_4YrsPrSp+A_Prop_3YrPrPr,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

par(mfrow=c(2,3))
partialPlot(LTN, RFLU,A_P_AUM_YrSp, cex.main=1)
partialPlot(LTN, RFLU,A_P_AUM_4YrPrSp, cex.main=1)
partialPlot(LTN, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(LTN, RFLU,A_P_AUM_YrPr, cex.main=1)
partialPlot(LTN, RFLU,A_Prop_4YrsPrSp, cex.main=1)





#7
LTN=randomForest(Log_OE_TN~A_P_AUM_4YrPrSp+A_Prop_3YrPrPr,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

par(mfrow=c(1,2))
partialPlot(LTN, RFLU,A_P_AUM_4YrPrSp, cex.main=1)
partialPlot(LTN, RFLU,A_Prop_3YrPrPr, cex.main=1)

#8
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_Prop_3YrPrPr,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

par(mfrow=c(1,2))
partialPlot(LTN, RFLU,A_P_AUM_YrSp, cex.main=1)
partialPlot(LTN, RFLU,A_Prop_3YrPrPr, cex.main=1)

#9
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_Prop_3YrPrPr+AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

#10
LTN=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTN
varImpPlot(LTN)

par(mfrow=c(2,3))
partialPlot(LTN, RFLU,A_P_AUM_YrSp, cex.main=1)
partialPlot(LTN, RFLU,A_P_AUM_4YrPrSp, cex.main=1)
partialPlot(LTN, RFLU,A_P_AUM_YrPr, cex.main=1)
partialPlot(LTN, RFLU,A_Prop_4YrsPrSp, cex.main=1)
###################################################################
# Use transformed TN response and Predictor variables: LTNL
###################################################################
#1
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                    Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                    Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                    UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#2
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_3YrPrPr+
                    Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                    SprgDensity_WS+StreamDens+PerDensC+StmOrd+
                    Slope_WS+Log_AREA_SQKM+ELEV_RANGE+ELVmax_WS+TMAX_WS+TMIN_WS+
                    UCS_Mean+PRMH_AVE+alru_dom+ArtPathDens+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#3
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_3YrPrPr+
                    Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+MINEden_WS+LGT_AG_WS+LGT_URBAN_WS+StreamDens+PerDensC+
                    Log_AREA_SQKM+ELEV_RANGE+PRMH_AVE+ArtPathDens+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#4
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_Prop_YrSp+
                    Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr+
                    Percent_HMA+Log_AREA_SQKM+ELEV_RANGE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

#5
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+
                    Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)


#6
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)
par(mfrow=c(2,3))
partialPlot(LTNL, RFLU,A_P_AUM_YrSp, cex.main=1)
partialPlot(LTNL, RFLU,A_P_AUM_4YrPrSp, cex.main=1)
partialPlot(LTNL, RFLU,SR_Prop_3YrPrPr, cex.main=1)
partialPlot(LTNL, RFLU,A_P_AUM_YrPr, cex.main=1)
partialPlot(LTNL, RFLU,SR_Prop_4YrsPrSp, cex.main=1)


#7
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)
par(mfrow=c(2,3))
partialPlot(LTNL, RFLU,A_P_AUM_YrSp, cex.main=1)
partialPlot(LTNL, RFLU,A_P_AUM_4YrPrSp, cex.main=1)
partialPlot(LTNL, RFLU,SR_Prop_3YrPrPr, cex.main=1)
partialPlot(LTNL, RFLU,SR_Prop_4YrsPrSp, cex.main=1)

#8
LTNL=randomForest(Log_OE_TN~A_P_AUM_YrSp+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTNL
varImpPlot(LTNL)

###################################################################
# Transformed predictors NOT transformed response
###################################################################
#1
TNL=randomForest(OE_TN~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                    Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                    Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                    UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNL
varImpPlot(TNL)

#2
TNL=randomForest(OE_TN~A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                   Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                   RdDensC+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                   SprgDensity_WS+SpDensity300m+StreamDens+PerDensC+IntDensC+
                   Log_AREA_SQKM+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                   UCS_Mean+KFCT_AVE+alru_dom+ArtPathDens+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNL
varImpPlot(TNL)

#3
TNL=randomForest(OE_TN~A_P_AUM_YrPr+A_P_Prop_YrPr+
                   Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                   RdDensC+DAMden_WS+IntDensC+Log_AREA_SQKM+ELEV_RANGE+ELVmax_WS+SumAve_P+
                   UCS_Mean+KFCT_AVE+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNL
varImpPlot(TNL)

#4
TNL=randomForest(OE_TN~SR_Prop_3YrPrPr+Log_AREA_SQKM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TNL
varImpPlot(TNL)



########################################################################################################
########################################################################################################


###################################################################
# Use anthropogenic variables: TPLU
###################################################################
#1
TPLU=randomForest(OE_TP~,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TPLU
varImpPlot(TPLU)


###################################################################
# Use anthropogenic and natural variables: TP
###################################################################
#1) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                  Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                  SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                  UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#2) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_3YrPrPr+
                  Density_RdCross+RdDensC+Percent_HMA+PctXclsr+DAMden_WS+DAMvol_Stand_WS+
                  SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+MEANP_WS+TMAX_WS+TMIN_WS+
                  UCS_Mean+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#3) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_3YrPrPr+
                  Density_RdCross+RdDensC+Percent_HMA+DAMden_WS+
                  SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+StmOrd+
                  Slope_WS+AREA_SQKM+ELEV_RANGE+MEANP_WS+UCS_Mean+ArtPathDens+PCT_SEDIM+Volcanic_7,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#4) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_3YrPrPr+
                  Density_RdCross+RdDensC+SprgDensity_WS+SpDensity300m+StreamDens+Slope_WS+AREA_SQKM+ELEV_RANGE+MEANP_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#5) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+
                  A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_3YrPrPr+Slope_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

#6) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~A_AUM_4YrPrSp+A_AUM_YrPr+Slope_WS,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)

par(mfrow=c(1,3))
partialPlot(TP, RFLU,A_AUM_4YrPrSp, cex.main=1)
partialPlot(TP, RFLU,A_AUM_YrPr, cex.main=1)
partialPlot(TP, RFLU,Slope_WS, cex.main=1)

#7) Run RF for TP response and anthropogenic predictor variables
TP=randomForest(OE_TP~A_AUM_4YrPrSp+A_AUM_YrPr,
                data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP
varImpPlot(TP)


###################################################################
# Use transformed TP Response variables: LTP
###################################################################
#1) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                   UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#2) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_P_AUM_4YrPrSp+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_3YrPrPr+
                   Density_RdCross+Percent_HMA+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+IntDensC+StmOrd+
                   Slope_WS+SITE_ELEV+ELEV_RANGE+ELVmax_WS+TMAX_WS+TMIN_WS+
                   KFCT_AVE+PRMH_AVE+alru_dom+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#3) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_P_AUM_4YrPrSp+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+A_Prop_3YrPrPr+
                   Density_RdCross+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+SpDensity300m+StreamDens+IntDensC+StmOrd+
                   Slope_WS+SITE_ELEV+TMAX_WS+PRMH_AVE+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#4) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_P_AUM_3YrPrPr+A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+A_Prop_3YrPrPr+
                   Density_RdCross+DAMvol_Stand_WS+StreamDens+StmOrd+Slope_WS+PRMH_AVE+HYDR_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#5) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_Prop_4YrsPrSp+A_Prop_3YrPrPr+
                   Density_RdCross+StreamDens+StmOrd+Slope_WS+PRMH_AVE,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

par(mfrow=c(2,4))
partialPlot(LTP, RFLU,A_AUM_YrSp, cex.main=1)
partialPlot(LTP, RFLU,A_AUM_4YrPrSp, cex.main=1)
partialPlot(LTP, RFLU,A_AUM_YrPr, cex.main=1)
partialPlot(LTP, RFLU,A_Prop_4YrsPrSp, cex.main=1)
partialPlot(LTP, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(LTP, RFLU,Density_RdCross, cex.main=1)
partialPlot(LTP, RFLU,Slope_WS, cex.main=1)
partialPlot(LTP, RFLU,StreamDens, cex.main=1)


#6) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_Prop_3YrPrPr+Density_RdCross+StreamDens+Slope_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#7) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_AUM_4YrPrSp+A_AUM_YrPr+A_Prop_3YrPrPr+Density_RdCross+StreamDens,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#8) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_AUM_YrPr+A_Prop_3YrPrPr+Density_RdCross+StreamDens,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

par(mfrow=c(2,2))
partialPlot(LTP, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(LTP, RFLU,A_AUM_YrPr, cex.main=1)
partialPlot(LTP, RFLU,Density_RdCross, cex.main=1)
partialPlot(LTP, RFLU,StreamDens, cex.main=1)

#9) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_AUM_YrPr+A_Prop_3YrPrPr+StreamDens,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)

#10) Run RF for TP response and all grazing, anthropogenic, and natural predictor variables
LTP=randomForest(Log_OE_TP~A_AUM_YrPr+StreamDens,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTP
varImpPlot(LTP)
###################################################################
# Use transformed TP Response and predictor variables: LTPL
####################################################################
#1
LTPL=randomForest(Log_OE_TP~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                    Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                    Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                    UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#2
LTPL=randomForest(Log_OE_TP~A_P_AUM_4YrPrSp+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                    Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr+
                    Density_RdCross+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+StmOrd+
                    Slope_WS+SITE_ELEV+ELEV_RANGE+SumAve_P+TMAX_WS+
                    UCS_Mean+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#3
LTPL=randomForest(Log_OE_TP~A_P_AUM_4YrPrSp+
                    Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr+
                    Density_RdCross+MINEden_WS+DAMvol_Stand_WS+LGT_AG_WS+
                    A_SpDensity800m+StreamDens+StmOrd+Slope_WS+SITE_ELEV+SumAve_P+UCS_Mean+alru_dom+HYDR_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#4
LTPL=randomForest(Log_OE_TP~Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_4YrsPrSp+SR_Prop_3YrPrPr+
                    Density_RdCross+DAMvol_Stand_WS+StreamDens+StmOrd+Slope_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#5
LTPL=randomForest(Log_OE_TP~Log_AUM_4YrsPrSp+Log_AUM_YrPr+SR_Prop_3YrPrPr+
                    Density_RdCross+StreamDens+Slope_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#6
LTPL=randomForest(Log_OE_TP~Log_AUM_4YrsPrSp+Log_AUM_YrPr+SR_Prop_3YrPrPr+Density_RdCross+StreamDens,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)

#7
LTPL=randomForest(Log_OE_TP~Log_AUM_YrPr+SR_Prop_3YrPrPr+Density_RdCross+StreamDens,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LTPL
varImpPlot(LTPL)
par(mfrow=c(2,2))
partialPlot(LTPL, RFLU,Log_AUM_YrPr, cex.main=1)
partialPlot(LTPL, RFLU,SR_Prop_3YrPrPr, cex.main=1)
partialPlot(LTPL, RFLU,Density_RdCross, cex.main=1)
partialPlot(LTPL, RFLU,StreamDens, cex.main=1)
###################################################################
# Use anthropogenic variables: XCMGLU
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)
###################################################################
# Use anthropogenic and natural variables: XCMG
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                    A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                    Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                    UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#2) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~A_P_AUM_YrSp+A_P_AUM_3YrPrPr+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                    A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                    Density_RdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+IntDensC+StmOrd+Slope_WS+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+
                    UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#3) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~A_P_AUM_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_3YrPrPr+A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+A_Prop_YrPr+
                    Density_RdCross+Percent_HMA+MINEden_WS+DAMden_WS+URBAN_WS+SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+IntDensC+StmOrd+
                    SITE_ELEV+ELEV_RANGE+ELVmax_WS+MEANP_WS+TMAX_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#4) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~A_P_AUM_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_3YrPrPr+A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_4YrsPrSp+
                    Density_RdCross+Percent_HMA+SpDensity300m+A_SpDensity800m+StmOrd+SITE_ELEV+ELEV_RANGE+MEANP_WS+TMAX_WS+
                    KFCT_AVE+PRMH_AVE+alru_dom+HYDR_WS+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~A_P_Prop_4YrsPrSp+A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_3YrPrPr+
                    Density_RdCross+Percent_HMA+StmOrd+ELEV_RANGE+MEANP_WS+KFCT_AVE+alru_dom+HYDR_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

#6) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~A_P_Prop_4YrsPrSp+A_AUM_3YrPrPr+Density_RdCross+Percent_HMA+ELEV_RANGE+alru_dom,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)

par(mfrow=c(2,3))
partialPlot(XCMG, RFLU,A_P_Prop_4YrsPrSp, cex.main=1)
partialPlot(XCMG, RFLU,A_AUM_3YrPrPr, cex.main=1)
partialPlot(XCMG, RFLU,Density_RdCross, cex.main=1)
partialPlot(XCMG, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMG, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMG, RFLU,alru_dom, cex.main=1)

#7) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMG=randomForest(XCMG~A_P_Prop_4YrsPrSp+A_AUM_3YrPrPr+Density_RdCross+Percent_HMA+alru_dom,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMG
varImpPlot(XCMG)
###################################################################
# Use transformed anthropogenic and natural variables: XCMGL
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                     Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                     Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                     SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                     Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                     UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#2) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+
                     Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                     Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_URBAN_WS+
                     SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+StmOrd+
                     Slope_WS+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+
                     UCS_Mean+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#3) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+
                     Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+
                     Density_RdCross+RdDensC+Percent_HMA+MINEden_WS+LGT_URBAN_WS+
                     SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+StmOrd+
                     ELEV_RANGE+ELVmax_WS+MEANP_WS+
                     UCS_Mean+PRMH_AVE+alru_dom+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#4) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+
                     Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_3YrPrPr+SR_Prop_YrSp+
                     Density_RdCross+RdDensC+Percent_HMA+StmOrd+ELEV_RANGE+MEANP_WS+
                     PRMH_AVE+alru_dom+HYDR_WS+ELVmean_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

par(mfrow=c(3,3))

partialPlot(XCMGL, RFLU,A_P_Prop_YrSp, cex.main=1)
partialPlot(XCMGL, RFLU,A_P_Prop_4YrsPrSp, cex.main=1)
partialPlot(XCMGL, RFLU,A_P_Prop_YrPr, cex.main=1)
partialPlot(XCMGL, RFLU,Log_AUM_YrSp, cex.main=1)
partialPlot(XCMGL, RFLU,Log_AUM_4YrsPrSp, cex.main=1)
partialPlot(XCMGL, RFLU,Log_AUM_3YrPrPr, cex.main=1)
partialPlot(XCMGL, RFLU,SR_Prop_YrSp, cex.main=1)
partialPlot(XCMGL, RFLU,Density_RdCross, cex.main=1)
partialPlot(XCMGL, RFLU,RdDensC, cex.main=1)
partialPlot(XCMGL, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGL, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGL, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMGL, RFLU, MEANP_WS, cex.main=1)
partialPlot(XCMGL, RFLU,PRMH_AVE, cex.main=1)
partialPlot(XCMGL, RFLU,alru_dom, cex.main=1)
partialPlot(XCMGL, RFLU,HYDR_WS, cex.main=1)
partialPlot(XCMGL, RFLU,ELVmean_WS, cex.main=1)


#5) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~A_P_Prop_YrSp+A_P_Prop_YrPr+Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_3YrPrPr+
                     Density_RdCross+Percent_HMA+ELEV_RANGE+PRMH_AVE+alru_dom,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)

#6) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~A_P_Prop_YrPr+Log_AUM_4YrsPrSp+Log_AUM_3YrPrPr+Density_RdCross+PRMH_AVE+alru_dom,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)
par(mfrow=c(2,3))

partialPlot(XCMGL, RFLU,A_P_Prop_YrPr, cex.main=1)
partialPlot(XCMGL, RFLU,Log_AUM_4YrsPrSp, cex.main=1)
partialPlot(XCMGL, RFLU,Log_AUM_3YrPrPr, cex.main=1)
partialPlot(XCMGL, RFLU,Density_RdCross, cex.main=1)
partialPlot(XCMGL, RFLU,PRMH_AVE, cex.main=1)
partialPlot(XCMGL, RFLU,alru_dom, cex.main=1)

#7) Run RF for XCMG response and all grazing, anthropogenic, and natural predictor variables
XCMGL=randomForest(XCMG~Log_AUM_4YrsPrSp+Log_AUM_3YrPrPr+Density_RdCross,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGL
varImpPlot(XCMGL)
########################################################################################################
########################################################################################################
A_P_Prop_YrSp+Density_RdCross+Percent_HMA+ELEV_RANGE+KFCT_AVE
###################################################################
# Use anthropogenic: denLU
###################################################################
#1) Run RF for Densiom response and all grazing, anthropogenic, 
denLU=randomForest(xcdenmid~,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)


###################################################################
# Use anthropogenic and natural variables: den
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                   A_AUM_YrSp+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+A_Prop_4YrsPrSp+A_Prop_YrPr+A_Prop_3YrPrPr+
                   Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                   SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                   UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~A_P_AUM_4YrPrSp+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+A_AUM_4YrPrSp+A_AUM_YrPr+A_AUM_3YrPrPr+A_Prop_YrSp+
                   Density_RdCross+RdDensC+PctXclsr+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+
                   AREA_SQKM+MEANP_WS+TMAX_WS+TMIN_WS+UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+A_AUM_3YrPrPr+A_Prop_YrSp+
                   PctXclsr+AG_WS+URBAN_WS+SprgDensity_WS+SpDensity300m+IntDensC+AREA_SQKM+TMAX_WS+TMIN_WS+
                   UCS_Mean+KFCT_AVE+PRMH_AVE+HYDR_WS+PCT_SEDIM,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_Prop_YrSp+
                   PctXclsr+SprgDensity_WS+IntDensC+TMAX_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+HYDR_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~A_P_Prop_4YrsPrSp+A_Prop_YrSp+SprgDensity_WS+IntDensC+TMIN_WS+KFCT_AVE+HYDR_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~A_Prop_YrSp+SprgDensity_WS+IntDensC+TMIN_WS+HYDR_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)
par(mfrow=c(2,3))
partialPlot(den, RFLU,A_Prop_YrSp, cex.main=1)
partialPlot(den, RFLU,SprgDensity_WS, cex.main=1)
partialPlot(den, RFLU,IntDensC, cex.main=1)
partialPlot(den, RFLU,TMIN_WS, cex.main=1)
partialPlot(den, RFLU,HYDR_WS, cex.main=1)

#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
den=randomForest(xcdenmid~SprgDensity_WS+IntDensC+TMIN_WS,
                 data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
den
varImpPlot(den)

par(mfrow=c(1,3))
partialPlot(den, RFLU,SprgDensity_WS, cex.main=1)
partialPlot(den, RFLU,IntDensC, cex.main=1)
partialPlot(den, RFLU,TMIN_WS, cex.main=1)

###################################################################
# Use transformed anthropogenic and natural variables: denL
###################################################################
#1) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_AUM_3YrPrPr+A_P_Prop_YrSp+A_P_Prop_4YrsPrSp+A_P_Prop_YrPr+A_P_Prop_3YrPrPr+
                    Log_AUM_YrSp+Log_AUM_4YrsPrSp+Log_AUM_YrPr+Log_AUM_3YrPrPr+SR_Prop_YrSp+SR_Prop_4YrsPrSp+SR_Prop_YrPr+SR_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+LGT_AG_WS+LGT_URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                    Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                    UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#2) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~A_P_AUM_YrSp+A_P_AUM_4YrPrSp+A_P_AUM_YrPr+A_P_Prop_YrSp+A_P_Prop_3YrPrPr+
                    Log_AUM_4YrsPrSp+Log_AUM_YrPr+SR_Prop_YrSp+SR_Prop_3YrPrPr+
                    Density_RdCross+RdDensC+Percent_HMA+PctXclsr+DAMden_WS+LGT_AG_WS+LGT_URBAN_WS+
                    SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                    Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+MEANP_WS+TMAX_WS+TMIN_WS+
                    UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+HYDR_WS+Volcanic_7,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#3) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~A_P_AUM_YrSp+A_P_AUM_YrPr+A_P_Prop_YrSp+A_P_Prop_3YrPrPr+Log_AUM_YrPr+SR_Prop_YrSp+
                    Density_RdCross+PctXclsr+DAMden_WS+LGT_URBAN_WS+SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+IntDensC+
                    Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+TMAX_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+HYDR_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#4) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~A_P_Prop_3YrPrPr+SR_Prop_YrSp+Density_RdCross+PctXclsr+SprgDensity_WS+IntDensC+
                    Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+TMAX_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+HYDR_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)

#5) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~A_P_Prop_3YrPrPr+SR_Prop_YrSp+SprgDensity_WS+IntDensC+TMIN_WS+KFCT_AVE,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)


#6) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~SR_Prop_YrSp+SprgDensity_WS+IntDensC+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)
par(mfrow=c(2,3))
partialPlot(denL, RFLU,SR_Prop_YrSp, cex.main=1)
partialPlot(denL, RFLU,SprgDensity_WS, cex.main=1)
partialPlot(denL, RFLU,IntDensC, cex.main=1)
partialPlot(denL, RFLU,TMIN_WS, cex.main=1)

#7) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~SR_Prop_YrSp+SprgDensity_WS+TMIN_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)
par(mfrow=c(1,3))
partialPlot(denL, RFLU,SR_Prop_YrSp, cex.main=1)
partialPlot(denL, RFLU,SprgDensity_WS, cex.main=1)
partialPlot(denL, RFLU,TMIN_WS, cex.main=1)

#8) Run RF for TN response and all grazing, anthropogenic, and natural predictor variables
denL=randomForest(xcdenmid~SR_Prop_YrSp+SprgDensity_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denL
varImpPlot(denL)


