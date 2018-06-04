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

#Get Variable Names in a NAME+NAME+NAME format: done in excel
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
nump = 15
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

########################################################################################################

############
#What data needs/should be transformed
############

boxplotdata=RFLU[,c(6:68)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}

# Transformations: 
# Calculated summary stats to know if 0's were present, ect. summary(RFdata$PerDensC)

RFLU$Log_AREA_SQKM=log10(RFLU$AREA_SQKM)

#
#summary(RFLU$OE_TN)
RFLU$OE_TN_2=((RFLU$OE_TN+179.50)+1)
#boxplot(RFLU$OE_TN_2)
RFLU$OE_TN_3=log10(RFLU$OE_TN_2)
#par(mfrow=c(2,3))
#boxplot(RFLU$OE_TN_3)
#boxplot(RFLU$Log_OE_TN)
#boxplot(RFLU$OE_TN)
#hist(RFLU$OE_TN_3)
#hist(RFLU$Log_OE_TN)
#hist(RFLU$OE_TN)

#
#summary(RFLU$OE_TP)
RFLU$OE_TP_2=((RFLU$OE_TP+40.76)+1)
#boxplot(RFLU$OE_TP_2)
RFLU$OE_TP_3=log10(RFLU$OE_TP_2)
#boxplot(RFLU$OE_TP_3)
#boxplot(RFLU$Log_OE_TP)
#par(mfrow=c(2,3))
#boxplot(RFLU$OE_TP_3)
#boxplot(RFLU$Log_OE_TP)
#boxplot(RFLU$OE_TP)
#hist(RFLU$OE_TP_3)
#hist(RFLU$Log_OE_TP)
#hist(RFLU$OE_TP)
#

#Can't arcsin square root because there are values >1
RFLU$SR_Prop_YrPr=sqrt(RFLU$A_Prop_YrPr)
RFLU$SR_Prop_3YrPrPr=sqrt(RFLU$A_Prop_3YrPrPr)

#summary(RFLU$AG_WS)
RFLU$LGT_AG_WS=logit(RFLU$AG_WS+0.1)
RFLU$LGT_URBAN_WS=logit(RFLU$URBAN_WS+0.1)


boxplotdata=RFLU[,c(68:78)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}


colnames(RFLU)

########################################################################################################

####################
#Correlations
####################
# Indicators/Stressors, Grazing, Anthropogenic, and Natural variables 
RFLU_CorSubset=RFLU[,6:75]
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

#LUSub=RFLU[,c(63,6:14)]
#pairs(LUSub, lower.panel=panel.smooth, upper.panel=panel.cor)
#LUSub1=RFLU[,c(63,14:17)]
#pairs(LUSub1, lower.panel=panel.smooth, upper.panel=panel.cor)
#LUSub1=RFLU[,c(63,18:22)]
#pairs(LUSub1, lower.panel=panel.smooth, upper.panel=panel.cor)
#LUSub2=RFLU[,c(63,22:31)]
#pairs(LUSub2, lower.panel=panel.smooth, upper.panel=panel.cor)
#LUSub2=RFLU[,c(63,69:72)]
#pairs(LUSub2, lower.panel=panel.smooth, upper.panel=panel.cor)

########################################################################################################
########################################################################################################
SR_Prop_YrPr+SR_Prop_3YrPrPr+
OE_TN_2+OE_TN_3+OE_TP_2+OE_TP_3+
###################################################################
# Use anthropogenic and natural variables: TN_Prop
###################################################################
#1
TN_Prop=randomForest(OE_TN_3~A_Prop_YrPr+A_Prop_3YrPrPr+
                      Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                      SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                      Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                      UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                  data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_Prop
varImpPlot(TN_Prop)


#2
TN_Prop=randomForest(OE_TN_3~A_Prop_YrPr+A_Prop_3YrPrPr+
                       Density_RdCross+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+URBAN_WS+
                       SpDensity300m+StreamDens+IntDensC+Slope_WS+Log_AREA_SQKM+SumAve_P+
                       KFCT_AVE+PRMH_AVE+alru_dom+Volcanic_7,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_Prop
varImpPlot(TN_Prop)

#3 
TN_Prop=randomForest(OE_TN_3~A_Prop_YrPr+A_Prop_3YrPrPr+Density_RdCross+PctXclsr+Slope_WS+Log_AREA_SQKM,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_Prop
varImpPlot(TN_Prop)

#3.1 
TN_Prop=randomForest(OE_TN_3~A_Prop_3YrPrPr+Density_RdCross+Slope_WS+Log_AREA_SQKM,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_Prop
varImpPlot(TN_Prop)
par(mfrow=c(2,3))

partialPlot(TN_Prop, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TN_Prop, RFLU,Density_RdCross, cex.main=1)
partialPlot(TN_Prop, RFLU,Slope_WS, cex.main=1)
partialPlot(TN_Prop, RFLU,Log_AREA_SQKM, cex.main=1)


bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, A_Prop_3YrPrPr, Density_RdCross, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, A_Prop_3YrPrPr, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, A_Prop_3YrPrPr, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, Density_RdCross, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, Density_RdCross, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, Slope_WS, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors

#3.2 
TN_Prop=randomForest(OE_TN_3~A_Prop_3YrPrPr+Density_RdCross+Log_AREA_SQKM,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_Prop
varImpPlot(TN_Prop)
par(mfrow=c(2,3))

#4 
TN_Prop=randomForest(OE_TN_3~A_Prop_YrPr+A_Prop_3YrPrPr+Slope_WS,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_Prop
varImpPlot(TN_Prop)

par(mfrow=c(1,3))
partialPlot(TN_Prop, RFLU,A_Prop_YrPr, cex.main=1)
partialPlot(TN_Prop, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TN_Prop, RFLU,Slope_WS, cex.main=1)

bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, A_Prop_3YrPrPr, Slope_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, A_Prop_3YrPrPr, A_Prop_YrPr, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TN_Prop, RFLU, Slope_WS, A_Prop_YrPr, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors

#5
TN_Prop=randomForest(OE_TN_3~A_Prop_3YrPrPr+Slope_WS,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TN_Prop
varImpPlot(TN_Prop)



########################################################################################################
########################################################################################################

###################################################################
# Use anthropogenic variables: TPLU
###################################################################
#1
TP_Prop=randomForest(OE_TP_3~A_Prop_YrPr+A_Prop_3YrPrPr+
                       Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                       SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                       Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                       UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)

#2
TP_Prop=randomForest(OE_TP_3~A_Prop_YrPr+A_Prop_3YrPrPr+Density_RdCross+PctXclsr+DAMvol_Stand_WS+AG_WS+
                       SprgDensity_WS+A_SpDensity800m+StreamDens+StmOrd+
                       Slope_WS+Log_AREA_SQKM+ELEV_RANGE+TMAX_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+ArtPathDens,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)

#3
TP_Prop=randomForest(OE_TP_3~A_Prop_YrPr+A_Prop_3YrPrPr+Density_RdCross+DAMvol_Stand_WS+
                       StreamDens+StmOrd+Slope_WS+Log_AREA_SQKM+KFCT_AVE,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)


#4
TP_Prop=randomForest(OE_TP_3~A_Prop_3YrPrPr+Density_RdCross+StreamDens+Slope_WS+Log_AREA_SQKM,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)

par(mfrow=c(2,3))
partialPlot(TP_Prop, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TP_Prop, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP_Prop, RFLU,StreamDens, cex.main=1)
partialPlot(TP_Prop, RFLU,Slope_WS, cex.main=1)
partialPlot(TP_Prop, RFLU,Log_AREA_SQKM, cex.main=1)

#5
TP_Prop=randomForest(OE_TP_3~A_Prop_3YrPrPr+Density_RdCross+StreamDens+Log_AREA_SQKM,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)

par(mfrow=c(2,3))
partialPlot(TP_Prop, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TP_Prop, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP_Prop, RFLU,StreamDens, cex.main=1)
partialPlot(TP_Prop, RFLU,Log_AREA_SQKM, cex.main=1)

#6
TP_Prop=randomForest(OE_TP_3~A_Prop_3YrPrPr+Density_RdCross+StreamDens,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)

par(mfrow=c(2,3))
partialPlot(TP_Prop, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TP_Prop, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP_Prop, RFLU,StreamDens, cex.main=1)

bpp.out = bivarpartialPlot.randomForest(TP_Prop, RFLU, A_Prop_3YrPrPr, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP_Prop, RFLU, A_Prop_3YrPrPr, Density_RdCross, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(TP_Prop, RFLU, Density_RdCross, StreamDens, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors



#7
TP_Prop=randomForest(OE_TP_3~A_Prop_3YrPrPr+Density_RdCross+StreamDens+Slope_WS,
                       data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)

par(mfrow=c(2,3))
partialPlot(TP_Prop, RFLU,A_Prop_3YrPrPr, cex.main=1)
partialPlot(TP_Prop, RFLU,Density_RdCross, cex.main=1)
partialPlot(TP_Prop, RFLU,StreamDens, cex.main=1)
partialPlot(TP_Prop, RFLU,Slope_WS, cex.main=1)



#8
TP_Prop=randomForest(OE_TP_3~A_Prop_3YrPrPr+Density_RdCross,
                     data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
TP_Prop
varImpPlot(TP_Prop)

########################################################################################################
########################################################################################################

###################################################################
# : XCMGLU
###################################################################
#1) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~A_Prop_YrPr+A_Prop_3YrPrPr+
                      Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                      SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                      Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                      UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#2) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~A_Prop_YrPr+A_Prop_3YrPrPr+Density_RdCross+RdDensC+Percent_HMA+AG_WS+
                      PerDensC+IntDensC+StmOrd+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+MEANP_WS+TMAX_WS+
                      KFCT_AVE+PRMH_AVE+alru_dom+HYDR_WS+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#3) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~A_Prop_YrPr+A_Prop_3YrPrPr+Density_RdCross+Percent_HMA+AG_WS+
                      StmOrd+SITE_ELEV+ELEV_RANGE+TMAX_WS+PRMH_AVE+alru_dom+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)


#4) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~A_Prop_3YrPrPr+Density_RdCross+Percent_HMA+
                      StmOrd+ELEV_RANGE+TMAX_WS+PRMH_AVE+alru_dom+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#5) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~Percent_HMA+StmOrd+TMAX_WS+alru_dom+ELVmean_WS,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

#6) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~Percent_HMA+StmOrd+TMAX_WS+alru_dom,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

par(mfrow=c(2,3))
partialPlot(XCMGLU, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGLU, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGLU, RFLU,TMAX_WS, cex.main=1)
partialPlot(XCMGLU, RFLU,alru_dom, cex.main=1)


#7) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~Percent_HMA+StmOrd+alru_dom,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)
par(mfrow=c(2,3))
partialPlot(XCMGLU, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGLU, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGLU, RFLU,alru_dom, cex.main=1)


bpp.out = bivarpartialPlot.randomForest(XCMGLU, RFLU, Percent_HMA, StmOrd, ylab="rating", n1.pt=nump, n2.pt=nump, theta=300) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(XCMGLU, RFLU, Percent_HMA, alru_dom, ylab="rating", n1.pt=nump, n2.pt=nump, theta=210) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(XCMGLU, RFLU, StmOrd, alru_dom, ylab="rating", n1.pt=nump, n2.pt=nump, theta=130) #change theta on this one, can't use factors


####
#8) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~Density_RdCross+Percent_HMA+StmOrd+ELEV_RANGE+PRMH_AVE,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)
partialPlot(XCMGLU, RFLU,Density_RdCross, cex.main=1)
partialPlot(XCMGLU, RFLU,Percent_HMA, cex.main=1)
partialPlot(XCMGLU, RFLU,StmOrd, cex.main=1)
partialPlot(XCMGLU, RFLU,ELEV_RANGE, cex.main=1)
partialPlot(XCMGLU, RFLU,PRMH_AVE, cex.main=1)


#9) Run RF for XCMG response and all grazing, anthropogenic, variables
XCMGLU=randomForest(XCMG~Percent_HMA+StmOrd+PRMH_AVE,
                    data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
XCMGLU
varImpPlot(XCMGLU)

########################################################################################################
########################################################################################################

###################################################################
# : denLU
###################################################################
#1
denLU=randomForest(xcdenmid~A_Prop_YrPr+A_Prop_3YrPrPr+
                     Density_RdCross+RdDensC+Percent_HMA+PctXclsr+MINEden_WS+DAMden_WS+DAMvol_Stand_WS+AG_WS+URBAN_WS+
                     SprgDensity_WS+SpDensity300m+A_SpDensity800m+StreamDens+PerDensC+IntDensC+StmOrd+
                     Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+SumAve_P+MEANP_WS+TMAX_WS+TMIN_WS+
                     UCS_Mean+KFCT_AVE+PRMH_AVE+alru_dom+ArtPathDens+HYDR_WS+PCT_SEDIM+Volcanic_7+ELVmean_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)
#2
denLU=randomForest(xcdenmid~A_Prop_YrPr+PctXclsr+MINEden_WS+DAMvol_Stand_WS+URBAN_WS+
                     SprgDensity_WS+SpDensity300m+StreamDens+PerDensC+IntDensC+StmOrd+
                     Slope_WS+Log_AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmax_WS+MEANP_WS+TMAX_WS+TMIN_WS+
                     KFCT_AVE+PRMH_AVE+ArtPathDens+HYDR_WS+PCT_SEDIM+ELVmean_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#3
denLU=randomForest(xcdenmid~A_Prop_YrPr+PctXclsr+MINEden_WS+SprgDensity_WS+StreamDens+IntDensC+
                     SITE_ELEV+ELEV_RANGE+ELVmax_WS+TMAX_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+HYDR_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#4
denLU=randomForest(xcdenmid~A_Prop_YrPr+PctXclsr+MINEden_WS+SprgDensity_WS+IntDensC+
                     SITE_ELEV+ELVmax_WS+TMIN_WS+KFCT_AVE+PRMH_AVE+HYDR_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#5
denLU=randomForest(xcdenmid~PctXclsr+MINEden_WS+SprgDensity_WS+IntDensC+TMIN_WS+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)


#6
denLU=randomForest(xcdenmid~PctXclsr+MINEden_WS+IntDensC+TMIN_WS+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#7
denLU=randomForest(xcdenmid~IntDensC+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

#8
denLU=randomForest(xcdenmid~MINEden_WS+IntDensC+TMIN_WS+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)
par(mfrow=c(2,3))
partialPlot(denLU, RFLU,MINEden_WS, cex.main=1)
partialPlot(denLU, RFLU,IntDensC, cex.main=1)
partialPlot(denLU, RFLU,TMIN_WS, cex.main=1)
partialPlot(denLU, RFLU,PRMH_AVE, cex.main=1)


#9
denLU=randomForest(xcdenmid~IntDensC+TMIN_WS+PRMH_AVE,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

par(mfrow=c(2,3))
partialPlot(denLU, RFLU,IntDensC, cex.main=1)
partialPlot(denLU, RFLU,TMIN_WS, cex.main=1)
partialPlot(denLU, RFLU,PRMH_AVE, cex.main=1)

bpp.out = bivarpartialPlot.randomForest(denLU, RFLU, IntDensC, TMIN_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=30) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(denLU, RFLU, IntDensC, PRMH_AVE, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(denLU, RFLU, TMIN_WS, PRMH_AVE, ylab="rating", n1.pt=nump, n2.pt=nump, theta=245) #change theta on this one, can't use factors


#10
denLU=randomForest(xcdenmid~A_Prop_YrPr+IntDensC+SprgDensity_WS+TMIN_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

par(mfrow=c(2,3))
partialPlot(denLU, RFLU,A_Prop_YrPr, cex.main=1)
partialPlot(denLU, RFLU,IntDensC, cex.main=1)
partialPlot(denLU, RFLU,SprgDensity_WS, cex.main=1)
partialPlot(denLU, RFLU,TMIN_WS, cex.main=1)

#11
denLU=randomForest(xcdenmid~IntDensC+TMIN_WS,
                   data=RFLU, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
denLU
varImpPlot(denLU)

par(mfrow=c(1,3))
partialPlot(denLU, RFLU,IntDensC, cex.main=1)
partialPlot(denLU, RFLU,TMIN_WS, cex.main=1)

bpp.out = bivarpartialPlot.randomForest(denLU, RFLU, IntDensC, TMIN_WS, ylab="rating", n1.pt=nump, n2.pt=nump, theta=30) #change theta on this one, can't use factors

