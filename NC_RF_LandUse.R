##############################################################################################################################################################################################
#Random Forest for Northern California to determine the land use associated with identified stressors 
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
RFLU=read.csv
#("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run2_IDstressors_23Feb2014\\BugsIndicatorsNaturals_23Feb2014.csv")

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

#RF_CorSubset=RFdata[,7:52]
#RFdataCor=cor(RF_CorSubset)
#write.csv(RFdataCor,'\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run2_IDstressors_23Feb2014\\RFdataCorrelations.csv')

####################
#Random Forest Models
####################
#All variables in data
#NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+
 # OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+
#  xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
#  Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS

#BLM variables only: These are the NorCal indicators chosen to report on. 
#NV_Invasives+OE_Conduct+OE_TN+OE_TP+PH+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
  
  #Natural Variables only
 # SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
  #Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS

###################################################################
# Use all variables
#colnames(RFdata)
#1) Run RF for all predictor variables
#ALL=randomForest(NV_MMI~NV_Invasives+PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+
 #                  OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+
  #                 xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+HYDR_WS+
   #                Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
    #             data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
#ALL
#varImpPlot(ALL)