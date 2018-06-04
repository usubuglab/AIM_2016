##############################################################################################################################################################################################
#Random Forest for Northern California to investigate if small watersheds may be underrepresented in the MMI model....
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
RFdata=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Run4_MMIresultInvestigation\\BugsIndicatorsNaturals_MinusSmallWS_16March2015.csv")

####################
##Subset data
####################
##Remove watersheds with small watershed area
RFdata=RFdata[(RFdata$AREA_SQKM>14),]


####################
##Transform some variables
####################
#Boxplots were used to decide on transformed variables. See NC_RF_StressorID.R for boxplot code. 
#The boxplots are not replicated here, I just copied over the transformations.
#Use all variables, but some transformed

RFdata$Log_AREA_SQKM=log10(RFdata$AREA_SQKM)
RFdata$Log_OE_TN=log10(ifelse(RFdata$OE_TN<0,0,RFdata$OE_TN)+1)
RFdata$Log_OE_TP=log10(ifelse(RFdata$OE_TP<0,0,RFdata$OE_TP)+1)
RFdata$Log_alru_dom=log10(RFdata$alru_dom+1)
RFdata$Log_SprgNum_WS=log10(RFdata$SprgNum_WS+1)
RFdata$Log_PerDensC=log10(RFdata$PerDensC+1)
RFdata$Log_Slope_WS=log10(RFdata$Slope_WS)
RFdata$Log_HYDR_WS=log10(RFdata$HYDR_WS)
RFdata$Sqrt_BnkStability_BLM=sqrt(RFdata$BnkStability_BLM)

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
# Use transformed and BLM variables with only watersheds larger than 14 sqkm
#1) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~NV_Invasives+OE_Conduct+Log_OE_TN+Log_OE_TP+PH+Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
                        Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+IntDensC+Log_HYDR_WS+
                        Log_Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMAX_WS+TMIN_WS+UCS_Mean+SumAve_P+MEANP_WS,
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)



#2) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~NV_Invasives+OE_Conduct+Log_OE_TN+Log_OE_TP+PH+Sqrt_BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+XCMG+
                       Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+Log_HYDR_WS+
                       Log_Slope_WS+Log_AREA_SQKM+KFCT_AVE+PRMH_AVE+Log_alru_dom+TMIN_WS+SumAve_P+MEANP_WS,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)



#3) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+Sqrt_BnkStability_BLM+xcdenmid+XCMG+
                       SpNum300m+SpNum800m+StreamDens+
                       Log_Slope_WS+Log_AREA_SQKM+KFCT_AVE+Log_alru_dom+TMIN_WS+SumAve_P+MEANP_WS,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)

#4) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+Sqrt_BnkStability_BLM+XCMG+
                       SpNum300m+SpNum800m+
                       KFCT_AVE+Log_alru_dom,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)

#5) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+XCMG+
                      Log_alru_dom,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)


#6) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+XCMG,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)



#7) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+XCMG,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)


par(mfrow=c(1,3))
partialPlot(LargeWS, RFdata,Log_OE_TN, cex.main=1)
partialPlot(LargeWS, RFdata,OE_Conduct, cex.main=1)
partialPlot(LargeWS, RFdata,XCMG, cex.main=1)

# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(1,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TN, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=140) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TN, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, OE_Conduct, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=50) #change theta on this one, can't use factors





#8) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~Log_OE_TN+XCMG,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)



#9) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+XCMG+Log_AREA_SQKM,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)


#10) Run RF for transformed predictor variables
LargeWS=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+XCMG+Log_AREA_SQKM,
                     data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LargeWS
varImpPlot(LargeWS)

par(mfrow=c(2,3))
partialPlot(LargeWS, RFdata,Log_OE_TN, cex.main=1)
partialPlot(LargeWS, RFdata,OE_Conduct, cex.main=1)
partialPlot(LargeWS, RFdata,XCMG, cex.main=1)
partialPlot(LargeWS, RFdata,Log_OE_TP, cex.main=1)
partialPlot(LargeWS, RFdata,Log_AREA_SQKM, cex.main=1)

# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,2))
nump = 15
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TN, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=140) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TN, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, OE_Conduct, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=50) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TN, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, XCMG, Log_AREA_SQKM,  ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, OE_Conduct, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=50) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TP, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=50) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TN, Log_OE_TP, ylab="rating", n1.pt=nump, n2.pt=nump, theta=140) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, Log_OE_TP, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(LargeWS, RFdata, OE_Conduct, Log_OE_TP, ylab="rating", n1.pt=nump, n2.pt=nump, theta=140) #change theta on this one, can't use factors














