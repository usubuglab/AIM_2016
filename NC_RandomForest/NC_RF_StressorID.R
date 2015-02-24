##############################################################################################################################################################################################
#Random Forest for Northern California to determine most important stressors to biological conditions 
#Redo-23Feb2015
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
RFdata=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run2_IDstressors_23Feb2014\\BugsIndicatorsNaturals_23Feb2014.csv")

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


####################
#Correlations
####################
# Indicators and Natural
RF_CorSubset=RFdata[,7:52]
RFdataCor=cor(RF_CorSubset)
write.csv(RFdataCor,'\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run2_IDstressors_23Feb2014\\RFdataCorrelations.csv')

####################
#Random Forest Models
####################
#All variables in data
NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+
  OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+
  xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS

#BLM variables only: These are the NorCal indicators chosen to report on. 
NV_Invasives+OE_Conduct+OE_TN+OE_TP+PH+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+

#Natural Variables only
SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS

###################################################################
# Use all variables
colnames(RFdata)
#1) Run RF for all predictor variables
ALL=randomForest(NV_MMI~NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+
                   OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+
                   xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
                   Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                 data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
ALL
varImpPlot(ALL)
#2) Run RF for all predictor variables
ALL=randomForest(NV_MMI~Pred_TP+CONDUCTIVITY+NTL+PTL+OE_TN+OE_TP+xcdenmid+XEMBED+xcdenbk+XCMG+
                   xbnk_h+NRSA_W1_HALL+QR1+SpNum800m+PerDensC+IntDensC+AREA_SQKM+TMIN_WS+UCS_Mean,
                 data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
ALL
varImpPlot(ALL)
#3) Run RF for all predictor variables
ALL=randomForest(NV_MMI~CONDUCTIVITY+NTL+OE_TN+XCMG+IntDensC+AREA_SQKM,
                 data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
ALL
varImpPlot(ALL)
#4) Run RF for all predictor variables
ALL=randomForest(NV_MMI~NTL+XCMG+IntDensC+AREA_SQKM,
                 data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
ALL
varImpPlot(ALL)

###################################################################
# Use all variables, but some transformed
RFtrans=RFdata
RFtrans$Log_AREA_SQKM=log10(RFtrans$AREA_SQKM)
RFtrans$Log_OE_TN=log10(ifelse(RFtrans$OE_TN<0,0,RFtrans$OE_TN)+1)
RFtrans$Log_OE_TP=log10(ifelse(RFtrans$OE_TP<0,0,RFtrans$OE_TP)+1)
RFtrans$Log_alru_dom=log10(RFtrans$alru_dom+1)
RFtrans$Log_SprgNum_WS=log10(RFtrans$SprgNum_WS+1)
RFtrans$Log_PerDensC=log10(RFtrans$PerDensC+1)
RFtrans$Log_Slope_WS=log10(RFtrans$Slope_WS)
RFtrans$Log_HYDR_WS=log10(RFtrans$HYDR_WS)
RFtrans$Sqrt_BnkStability_BLM=sqrt(RFtrans$BnkStability_BLM)

#All variables in data
NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+
  Log_OE_TN+Log_OE_TP+PH+BnkCover_BLM+Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+
  xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+
  Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS

#BLM variables only
NV_Invasives+OE_Conduct+Log_OE_TN+Log_OE_TP+PH+Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
  

#Natural Variables only
  Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+
  Log_Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS

###################################################################
# Use transformed variables
colnames(RFdata)
#1) Run RF for transformed predictor variables
Trans=randomForest(NV_MMI~NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+
                     Log_OE_TN+Log_OE_TP+PH+BnkCover_BLM+Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+
                     xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+
                     Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                   data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Trans
varImpPlot(Trans)
#2) Run RF for transformed predictor variables
Trans=randomForest(NV_MMI~NV_Invasives+Pred_TN+CONDUCTIVITY+NTL+PTL+OE_Conduct+
                     Log_OE_TN+Log_OE_TP+XFC_NAT+LINCIS_H+XEMBED+XCMG+XCMGW+L_XCMGW+
                     xinc_h+QR1+Log_SprgNum_WS+SpNum800m+StreamDens+IntDensC+Log_HYDR_WS+
                     Log_Slope_WS+Log_AREA_SQKM+KFCT_AVE+Log_alru_dom+TMAX_WS+MEANP_WS,
                   data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Trans
varImpPlot(Trans)
#3) Run RF for transformed predictor variables
Trans=randomForest(NV_MMI~Log_OE_TN+XCMG+IntDensC+Log_AREA_SQKM,
                   data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
Trans
varImpPlot(Trans)

###################################################################
# Use transformed and BLM variables
#1) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~NV_Invasives+OE_Conduct+Log_OE_TN+Log_OE_TP+PH+Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
                        Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+
                     Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                   data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)


#2) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+PH+Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+PCT_SAFN+XCMG+
                        SpNum300m+SpNum800m+Log_PerDensC+IntDensC+Log_HYDR_WS+
                        Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMIN_WS+UCS_Mean+SumAve_P,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

#3) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+PH+XCMG+
                        SpNum800m+IntDensC+Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+PRMH_AVE+Log_alru_dom+TMIN_WS+UCS_Mean,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

#4) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+XCMG+
                        SpNum800m+IntDensC+Log_Slope_WS+Log_AREA_SQKM+PRMH_AVE+UCS_Mean,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

#5) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+XCMG+IntDensC+Log_AREA_SQKM,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

par(mfrow=c(2,3))
partialPlot(TransBLM, RFtrans,Log_OE_TN, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_OE_TP, cex.main=1)
partialPlot(TransBLM, RFtrans,XCMG, cex.main=1)
partialPlot(TransBLM, RFtrans,IntDensC, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_AREA_SQKM, cex.main=1)

# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TP, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_AREA_SQKM, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=20) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TP, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TP, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, Log_OE_TP, ylab="rating", n1.pt=nump, n2.pt=nump, theta=70) #change theta on this one, can't use factors



#6) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~Log_OE_TN+XCMG+IntDensC+Log_AREA_SQKM,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

par(mfrow=c(2,3))
partialPlot(TransBLM, RFtrans,Log_OE_TN, cex.main=1)
partialPlot(TransBLM, RFtrans,XCMG, cex.main=1)
partialPlot(TransBLM, RFtrans,IntDensC, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_AREA_SQKM, cex.main=1)

# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_AREA_SQKM, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=20) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

#7) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~Log_OE_TN+XCMG+Log_AREA_SQKM,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

#8) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+XCMG+IntDensC+Log_AREA_SQKM,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

par(mfrow=c(2,3))
partialPlot(TransBLM, RFtrans,OE_Conduct, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_OE_TN, cex.main=1)
partialPlot(TransBLM, RFtrans,XCMG, cex.main=1)
partialPlot(TransBLM, RFtrans,IntDensC, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_AREA_SQKM, cex.main=1)
# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, OE_Conduct, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_AREA_SQKM, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=20) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC,OE_Conduct,  ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, OE_Conduct, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors


#9) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+XCMG+IntDensC+Log_AREA_SQKM,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)

par(mfrow=c(2,3))
partialPlot(TransBLM, RFtrans,OE_Conduct, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_OE_TN, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_OE_TP, cex.main=1)
partialPlot(TransBLM, RFtrans,XCMG, cex.main=1)
partialPlot(TransBLM, RFtrans,IntDensC, cex.main=1)
partialPlot(TransBLM, RFtrans,Log_AREA_SQKM, cex.main=1)

# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,4))
nump = 15
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, OE_Conduct, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TP, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_AREA_SQKM, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=20) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TP, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC,OE_Conduct,  ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, IntDensC, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, OE_Conduct, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TP, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TP, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=60) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TransBLM, RFtrans, Log_OE_TN, Log_OE_TP, ylab="rating", n1.pt=nump, n2.pt=nump, theta=70) #change theta on this one, can't use factors



#10) 
TransBLM=randomForest(NV_MMI~OE_Conduct+PH+Log_OE_TN+Log_OE_TP+XCMG+IntDensC+Log_AREA_SQKM,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)




#11) Run RF for transformed predictor variables
TransBLM=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+xcdenmid+IntDensC+Log_AREA_SQKM,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM
varImpPlot(TransBLM)










#################################################################
#1) Run RF for BLM only predictor variables
TransBLM.1=randomForest(NV_MMI~NV_Invasives+OE_Conduct+Log_OE_TN+Log_OE_TP+PH+
                        Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM.1
varImpPlot(TransBLM.1)

#2) Run RF for BLM only predictor variables
TransBLM.1=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+PH+XCMG,
                      data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM.1
varImpPlot(TransBLM.1)

#3) Run RF for BLM only predictor variables
TransBLM.1=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+PH+XCMG,
                        data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM.1
varImpPlot(TransBLM.1)

#4) Run RF for BLM only predictor variables
TransBLM.1=randomForest(NV_MMI~Log_OE_TN+PH+XCMG,
                        data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM.1
varImpPlot(TransBLM.1)

#5) Run RF for BLM only predictor variables
TransBLM.1=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+XCMG+PH,
                        data=RFtrans, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TransBLM.1
varImpPlot(TransBLM.1)