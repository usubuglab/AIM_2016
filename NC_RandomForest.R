##############################################################################################################################################################################################
#Random Forest for Northern California to determine most important stressors to biological conditions
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
##load Bug File
#Bugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\NV_BugResults.csv")
#Load Indicator File
#RF_Indicators=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\Indicators_RFinput_5Jan2015.csv")

#Bugs, Indicators, and Natural Variables. 
RFdata=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\All_BugsIndicatorsNatural_11Jan2015.csv")

#Natural predictors alone
RF_NVnat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\NV_GISvariables_RFinput_11Jan2015.csv")
RF_CAnat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\CSCI_GISvariables_RFinput_11Jan2015.csv")
RF_LUnat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\NaturalVariables_RFinput_11Jan2015.csv")


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
IndicNatSubset=RFdata[,8:59]
IndicNatCor=cor(IndicNatSubset)
#write.csv(IndicNatCor,'\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\RandomForest\\CorrelationResults.csv')


####################
#Random Forest Models
####################

###################################################################
# NV model natural variables.
colnames(RF_NVnat)
#1) Run RF For natural preds from the NV MMI GIS input file.
NVnat=randomForest(NV_MMI~ELVmin_WS+ELVmax_WS+ELVmean_WS+SQ_KM+HYDR_WS+WDmax_WS+Pmax_WS+Pmin_WS+Tmax_WS+
                     BFI_WS+Tmax_PT+PrdCond+Slope_WS,data=RF_NVnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NVnat
varImpPlot(NVnat)
#2) Run RF For subset natural preds from the NV MMI GIS input file.
NVnat=randomForest(NV_MMI~ELVmin_WS+SQ_KM+Tmax_PT+Slope_WS,
                   data=RF_NVnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NVnat
varImpPlot(NVnat)

par(mfrow=c(2,2))
partialPlot(NVnat, RF_NVnat,ELVmin_WS, cex.main=1)
partialPlot(NVnat, RF_NVnat,SQ_KM, cex.main=1)
partialPlot(NVnat, RF_NVnat,Tmax_PT, cex.main=1)
partialPlot(NVnat, RF_NVnat,Slope_WS, cex.main=1)

###################################################################################
###################################################################################
#CA model Natural Variables ONLY
###################################################################################
colnames(RF_CAnat)
#1) Run RF For natural preds from the CSCI GIS input file.
CAnat=randomForest(NV_MMI~TEMP_00_09+SumAve_P+SITE_ELEV+S_Mean+PRMH_AVE+PPT_00_09+PCT_SEDIM+P_MEAN+New_Long+New_Lat+N_MEAN+
                     MgO_Mean+LPREM_mean+KFCT_AVE+ELEV_RANGE+CaO_Mean+BDH_AVE+AREA_SQKM,
                   data=RF_CAnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
CAnat
varImpPlot(CAnat)
#2) Run RF For subset natural preds from the CSCI GIS input file.
CAnat=randomForest(NV_MMI~ELEV_RANGE+CaO_Mean+AREA_SQKM,
                   data=RF_CAnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
CAnat
varImpPlot(CAnat)

###################################################################################
###################################################################################
# Natural Variables from Land Use excel file. This is a combination of many natural variables. 
###################################################################################
colnames(RF_LUnat)
#1) Run RF For natural preds from the LU data file.
LUnat=randomForest(NV_MMI~Year+DOY+SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+BFI_WS+HYDR_WS+GW_P_Sp_Mx+Slope_WS+
                     AREA_SQKM+PCT_SEDIM+Volcanic_7+Dom_Geol+SITE_ELEV+ELEV_RANGE+ELVcv_PT+ELVmax_WS+ELVmean_WS+ELVmin_WS+Pmax_WS+
                     Pmin_WS+SumAve_P+MEANP_WS+RH_WS+TMAX_WS+TMEAN_WS+TMIN_WS+UCS_Mean+CaO_Mean+MgO_Mean+S_Mean+Pct_Alfi+AWC_soil+BDH_AVE+
                     Db3rdbar+KFCT_AVE+PRMH_AVE+SOC+Awch_WS+alru_dom+Evergr_ave+EVI_AveAve+EVI_MaxAve+PerShrubC+PerHerbacC,
                   data=RF_LUnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LUnat
varImpPlot(LUnat)

#2) Run RF For subset natural preds from the LU data file.
LUnat=randomForest(NV_MMI~alru_dom+ELEV_RANGE+AREA_SQKM+StreamDens+S_Mean,data=RF_LUnat, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
LUnat
varImpPlot(LUnat)

###################################################################################
###################################################################################
# Run RF with Indicators ONLY for NV MMI
###################################################################################
colnames(RFdata)

###################################################################################
# Run RF with all indicators, but reducing Water Quality indicators
# 1
NV.WQ=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                      PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)

# All p-hab, only OE water chem
# 2
NV.WQ=randomForest(NV_MMI~OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                      PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+
                      QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)

# All p-hab, only measured water chem
# 3
NV.WQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+
                      BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)



# All p-hab, only pred water chem
# 4
NV.WQ=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+PH+BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+
                      PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.WQ
varImpPlot(NV.WQ)

###################################################################################
# Reducing p-hab indicators
NV.allphab=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+
                      BnkCover_BLM+BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#1
NV.allphab=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+
                      BnkStability_BLM+XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#2
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkCover_BLM+XFC_NAT+xcdenmid+PCT_SAFN+xcdenbk+XCMG+XCMGW+xbnk_h+
                      xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#3
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkCover_BLM+XFC_NAT+xcdenbk+XCMG+xbnk_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#4
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+XCMG+QR1, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#5
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+XCMG+QR1,
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

par(mfrow=c(2,3))
partialPlot(NV.allphab, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.allphab, RFdata,NTL, cex.main=1)
partialPlot(NV.allphab, RFdata,PTL, cex.main=1)
partialPlot(NV.allphab, RFdata,XCMG, cex.main=1)
partialPlot(NV.allphab, RFdata,QR1, cex.main=1)
#6
NV.allphab=randomForest(NV_MMI~CONDUCTIVITY+NTL+XCMG,
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.allphab
varImpPlot(NV.allphab)

#############################################################################
# Reducing p-hab indicators but starting with only BLM
#1
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkStability_BLM+XFC_NAT+xcdenmid+
                      LINCIS_H+PCT_SAFN+XCMG, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

#2
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+
                      PCT_SAFN+XCMG, data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

#3
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+XCMG, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

par(mfrow=c(2,3))
partialPlot(NV.BLM.indic, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,NTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,XCMG, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PH, cex.main=1)


#4
NV.BLM.indic=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+XCMG, 
                    data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)

# OE TRIAL RUN
NV.BLM.indic=randomForest(NV_MMI~OE_Conduct+OE_TN+OE_TP+PH+XCMG, 
                          data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
NV.BLM.indic
varImpPlot(NV.BLM.indic)
####################################################################################################

par(mfrow=c(2,3))
partialPlot(NV.BLM.indic, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,NTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PTL, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,XCMG, cex.main=1)
partialPlot(NV.BLM.indic, RFdata,PH, cex.main=1)




###################################################################################
###################################################################################
# Run RF with Indicators and Natural Preds for NV MMI
###################################################################################
colnames(RFdata)

###################################################################################
# Run with all natural and indicators
# 1
RFIndNat.all=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                        SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+StmOrd+HYDR_WS+GW_P_Sp_Mx+Slope_WS+AREA_SQKM+Volcanic_7+
                        SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce many natural variables.
# 2
RFIndNat.all=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                        SpNum800m+StreamDens+PerDensC+IntDensC+Slope_WS+AREA_SQKM+ELEV_RANGE+ELVmin_WS+CaO_Mean+S_Mean+alru_dom+Tmax_PT, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)


# Reduce indicators.
# 3
RFIndNat.all=randomForest(NV_MMI~PrdCond+CONDUCTIVITY+NTL+PTL+OE_TN+OE_TP+BnkStability_BLM+
                        xcdenmid+xcdenbk+XCMG+QR1+SpNum800m+IntDensC+Slope_WS+AREA_SQKM+
                        CaO_Mean+S_Mean+alru_dom+Tmax_PT, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce indicators more.
# 4
RFIndNat.all=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_TN+OE_TP+XCMG+QR1+SpNum800m+IntDensC+
                        Slope_WS+AREA_SQKM+CaO_Mean+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce indicators more.
# 5
RFIndNat.all=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+OE_TN+OE_TP+XCMG+
                        QR1+IntDensC+Slope_WS+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# Reduce more.
# 6
RFIndNat.all=randomForest(NV_MMI~NTL+PTL+OE_TN+OE_TP+XCMG+QR1+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# 7
RFIndNat.all=randomForest(NV_MMI~NTL+OE_TN+OE_TP+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# 8
RFIndNat.all=randomForest(NV_MMI~NTL+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)


par(mfrow=c(2,3))
partialPlot(RFIndNat.all, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFIndNat.all, RFdata,NTL, cex.main=1)
partialPlot(RFIndNat.all, RFdata,S_Mean, cex.main=1)
partialPlot(RFIndNat.all, RFdata,XCMG, cex.main=1)
partialPlot(RFIndNat.all, RFdata,IntDensC, cex.main=1)



#9
RFIndNat.all=randomForest(NV_MMI~NTL+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

##############################################################################################
# Run with BLM select indicators (measured water chem only, not modeled) and all natural
# 1
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
                        SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+
                        StmOrd+HYDR_WS+GW_P_Sp_Mx+Slope_WS+AREA_SQKM+Volcanic_7+
                        SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+
                        CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)
# Run with BLM select indicators (measured water chem only, not modeled) and subset natural
# 2
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+PH+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)

# Run with subset of BLM select indicators (measured water chem only, not modeled) and subset natural
# 3
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+
                        PH+BnkStability_BLM+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)

# Run with subset of BLM select indicators (measured water chem only, not modeled) and subset natural
# 4
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)

par(mfrow=c(2,4))
partialPlot(RFIndNat.BLM.MWQ, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,PTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,NTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,S_Mean, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,XCMG, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,IntDensC, cex.main=1)


# 5
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+IntDensC+AREA_SQKM+S_Mean, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)
################################################################################################
par(mfrow=c(2,4))
partialPlot(RFIndNat.BLM.MWQ, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,NTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,PTL, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,XCMG, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,IntDensC, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFIndNat.BLM.MWQ, RFdata,S_Mean, cex.main=1)


##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Trying some with OE
##############################################################################
#1
RFoe=randomForest(NV_OE0~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+
                        OE_Conduct+OE_TN+OE_TP+PH+BnkCover_BLM+BnkStability_BLM+
                        XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+
                        XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                        SprgNum_WS+SpNum300m+SpNum800m+StreamDens+PerDensC+IntDensC+
                        StmOrd+HYDR_WS+GW_P_Sp_Mx+Slope_WS+AREA_SQKM+Volcanic_7+
                        SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+
                        CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                      data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#2
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+xcdenmid+LINCIS_H+XEMBED+XCMG+SpNum300m+SpNum800m+IntDensC+
                    GW_P_Sp_Mx+Slope_WS+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmin_WS+TMAX_WS+PRMH_AVE+alru_dom+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#3
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+xcdenmid+XCMG+AREA_SQKM+SITE_ELEV+ELEV_RANGE+ELVmin_WS+alru_dom+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)
#4
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+xcdenmid+XCMG+AREA_SQKM+ELVmin_WS+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)
#5
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+PTL+XCMG+AREA_SQKM+ELVmin_WS+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#6
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+XCMG+AREA_SQKM+Tmax_PT, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)

#7
RFoe=randomForest(NV_OE0~CONDUCTIVITY+NTL+XCMG+AREA_SQKM, 
                  data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFoe
varImpPlot(RFoe)
##############################################################################
par(mfrow=c(2,3))
partialPlot(RFoe, RFdata,CONDUCTIVITY, cex.main=1)
partialPlot(RFoe, RFdata,NTL, cex.main=1)
#partialPlot(RFoe, RFdata,PTL, cex.main=1)
partialPlot(RFoe, RFdata,XCMG, cex.main=1)
#partialPlot(RFoe, RFdata,ELVmin_WS, cex.main=1)
partialPlot(RFoe, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFoe, RFdata,Tmax_PT, cex.main=1)
########################################################################################################
# Stop trying with OE
########################################################################################################
########################################################################################################
########################################################################################################

# Some initial trial and error: 

#Try to rerun some of the "best" model replacing measured WQ with OE WQ To better tell the story... 
### RESULTS: Once replace OE WQ results do not change the model results so use the OE results since it makes more sense.
#OE_Conduct+OE_TN+OE_TP

# 8 Original 
RFIndNat.all=randomForest(NV_MMI~NTL+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                          data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)

# 8 Rerun 
RFIndNat.all=randomForest(NV_MMI~OE_TN+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                          data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.all
varImpPlot(RFIndNat.all)


# Run with subset of BLM select indicators (measured water chem only, not modeled) and subset natural
# 4 Original
RFIndNat.BLM.MWQ=randomForest(NV_MMI~CONDUCTIVITY+NTL+PTL+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                              data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)

# Run with subset of BLM select indicators (measured water chem only, not modeled) and subset natural
# 4 Rerun
RFIndNat.BLM.OEWQ=randomForest(NV_MMI~OE_Conduct+OE_TN+OE_TP+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                              data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.OEWQ
varImpPlot(RFIndNat.BLM.OEWQ)

par(mfrow=c(2,4))
partialPlot(RFIndNat.BLM.OEWQ, RFdata,OE_Conduct, cex.main=1)
partialPlot(RFIndNat.BLM.OEWQ, RFdata,OE_TN, cex.main=1)
partialPlot(RFIndNat.BLM.OEWQ, RFdata,OE_TP, cex.main=1)
partialPlot(RFIndNat.BLM.OEWQ, RFdata,XCMG, cex.main=1)
partialPlot(RFIndNat.BLM.OEWQ, RFdata,IntDensC, cex.main=1)
partialPlot(RFIndNat.BLM.OEWQ, RFdata,AREA_SQKM, cex.main=1)
partialPlot(RFIndNat.BLM.OEWQ, RFdata,S_Mean, cex.main=1)

########################################################################################################
########################################################################################################
#What data needs/should be transformed

boxplotdata=RFdata[,c(6:59)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}

# Transformations: 
# If values have negatives need to assess how to Log (e.g., OR WQ)
# Calculated summary stats to know if ) were present, ect. summary(RFdata$PerDensC)
RFtransdata=RFdata
RFtransdata$Log_AREA_SQKM=log10(RFtransdata$AREA_SQKM)
RFtransdata$Log_OE_TN=log10(ifelse(RFtransdata$OE_TN<0,0,RFtransdata$OE_TN)+1)
RFtransdata$Log_OE_TP=log10(ifelse(RFtransdata$OE_TP<0,0,RFtransdata$OE_TP)+1)
RFtransdata$Log_alru_dom=log10(RFtransdata$alru_dom+1)
RFtransdata$Log_SprgNum_WS=log10(RFtransdata$SprgNum_WS+1)
RFtransdata$Log_PerDensC=log10(RFtransdata$PerDensC+1)
RFtransdata$Log_Slope_WS=log10(RFtransdata$Slope_WS)
RFtransdata$Log_HYDR_WS=log10(RFtransdata$HYDR_WS)
RFtransdata$Sqrt_BnkStability_BLM=sqrt(RFtransdata$BnkStability_BLM)

# Look at how transformations changed data
boxplotdata=RFtransdata[,c(6:68)]
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}

########################################################################################################
########################################################################################################

# Run RF with Transformed Variables. 
# 1 ALLL 
T_RFIndNat.all=randomForest(NV_MMI~PrdCond+Pred_TN+Pred_TP+CONDUCTIVITY+NTL+PTL+OE_Conduct+Log_OE_TN+Log_OE_TP+PH+BnkCover_BLM+Sqrt_BnkStability_BLM+
                            XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XEMBED+xcdenbk+XCMG+XCMGW+L_XCMGW+xbnk_h+xinc_h+EMAP_W1_HALL+NRSA_W1_HALL+QR1+
                            Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+IntDensC+StmOrd+Log_HYDR_WS+GW_P_Sp_Mx+Log_Slope_WS+Log_AREA_SQKM+Volcanic_7+
                            SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                          data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.all
varImpPlot(T_RFIndNat.all)


# 2 ALLL 
T_RFIndNat.all=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+
                              XCMG+xbnk_h+xinc_h+QR1+
                             SpNum800m+StreamDens+IntDensC+Log_Slope_WS+Log_AREA_SQKM+
                              ELVmin_WS+S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.all
varImpPlot(T_RFIndNat.all)

# 3 ALLL 
T_RFIndNat.all=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+
                              XCMG+QR1+
                              StreamDens+IntDensC+Log_AREA_SQKM+
                              S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.all
varImpPlot(T_RFIndNat.all)

# 4 ALLL 
T_RFIndNat.all=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+
                              XCMG+QR1+
                             IntDensC+Log_AREA_SQKM+
                              S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.all
varImpPlot(T_RFIndNat.all)


# 5 ALLL SAME MODEL AS WITHOUT TRANSFORMATIONS!!! 
T_RFIndNat.all=randomForest(NV_MMI~Log_OE_TN+
                              XCMG+
                              IntDensC+Log_AREA_SQKM+
                              S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.all
varImpPlot(T_RFIndNat.all)

par(mfrow=c(2,3))
partialPlot(T_RFIndNat.all, RFtransdata,Log_OE_TN, cex.main=1)
partialPlot(T_RFIndNat.all, RFtransdata,XCMG, cex.main=1)
partialPlot(T_RFIndNat.all, RFtransdata,IntDensC, cex.main=1)
partialPlot(T_RFIndNat.all, RFtransdata,Log_AREA_SQKM, cex.main=1)
partialPlot(T_RFIndNat.all, RFtransdata,S_Mean, cex.main=1)


# 6 ALLL SAME MODEL AS WITHOUT TRANSFORMATIONS!!! 
T_RFIndNat.all=randomForest(NV_MMI~Log_OE_TN+
                              Log_AREA_SQKM+
                              S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.all
varImpPlot(T_RFIndNat.all)
##########################

cor(RFtransdata[,c('Log_OE_TN','Log_AREA_SQKM','XCMG','IntDensC','S_Mean')])
########################################################################################################
########################################################################################################
# Run with Transformed data and JUST BLM indicators
#1
T_RFIndNat.BLM=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+PH+Sqrt_BnkStability_BLM+
                              XFC_NAT+xcdenmid+LINCIS_H+PCT_SAFN+XCMG+
                              Log_SprgNum_WS+SpNum300m+SpNum800m+StreamDens+Log_PerDensC+IntDensC+StmOrd+Log_HYDR_WS+GW_P_Sp_Mx+Log_Slope_WS+Log_AREA_SQKM+Volcanic_7+
                              SITE_ELEV+ELEV_RANGE+ELVmin_WS+Pmax_WS+TMAX_WS+UCS_Mean+CaO_Mean+S_Mean+KFCT_AVE+PRMH_AVE+Log_alru_dom+Evergr_ave+Tmax_PT+TEMP_00_09, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.BLM
varImpPlot(T_RFIndNat.BLM)

#2
T_RFIndNat.BLM=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+
                              XCMG+
                             IntDensC+Log_AREA_SQKM+
                              ELEV_RANGE+ELVmin_WS+UCS_Mean+S_Mean+PRMH_AVE+Tmax_PT, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.BLM
varImpPlot(T_RFIndNat.BLM)

#3
T_RFIndNat.BLM=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+
                              XCMG+
                              IntDensC+Log_AREA_SQKM+
                              S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.BLM
varImpPlot(T_RFIndNat.BLM)


#4
T_RFIndNat.BLM=randomForest(NV_MMI~Log_OE_TN+
                              XCMG+
                              IntDensC+Log_AREA_SQKM+
                              S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.BLM
varImpPlot(T_RFIndNat.BLM)

par(mfrow=c(2,3))
partialPlot(T_RFIndNat.BLM, RFtransdata,Log_OE_TN, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,XCMG, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,IntDensC, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,Log_AREA_SQKM, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,S_Mean, cex.main=1)


#5
T_RFIndNat.BLM=randomForest(NV_MMI~Log_OE_TN+
                              XCMG+
                              Log_AREA_SQKM, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.BLM
varImpPlot(T_RFIndNat.BLM)
#################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
#3-D plots (Can take a bit of time to run)
######################
#Original Model run #4
RFIndNat.BLM.MWQ=randomForest(NV_MMI~OE_Conduct+OE_TN+OE_TP+XCMG+IntDensC+AREA_SQKM+S_Mean, 
                              data=RFdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
RFIndNat.BLM.MWQ
varImpPlot(RFIndNat.BLM.MWQ)
# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,4))
nump = 15
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, AREA_SQKM, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_Conduct, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TN, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TP, AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, AREA_SQKM, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, AREA_SQKM, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_Conduct, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TN, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TP, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, IntDensC, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, IntDensC, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TN, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TP, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_Conduct, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_Conduct, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TN, OE_TP, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TN, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TP, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TP, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, OE_TN, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(RFIndNat.BLM.MWQ, RFtransdata, XCMG, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors




#Original Model run #4 WITH transformed data
T_RFIndNat.BLM.MWQ=randomForest(NV_MMI~OE_Conduct+Log_OE_TN+Log_OE_TP+XCMG+IntDensC+Log_AREA_SQKM+S_Mean, 
                              data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.BLM.MWQ
varImpPlot(T_RFIndNat.BLM.MWQ)
# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,4))
nump = 15
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, IntDensC, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, OE_Conduct, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TN, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TP, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_AREA_SQKM, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_AREA_SQKM, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, IntDensC,OE_Conduct,  ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TN, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TP, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, IntDensC, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, IntDensC, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TN, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TP, OE_Conduct, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, OE_Conduct, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, OE_Conduct, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TN, Log_OE_TP, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TN, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TP, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TP, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, Log_OE_TN, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM.MWQ, RFtransdata, XCMG, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors



######################

#3
T_RFIndNat.BLM=randomForest(NV_MMI~Log_OE_TN+Log_OE_TP+
                              XCMG+
                              IntDensC+Log_AREA_SQKM+
                              S_Mean, 
                            data=RFtransdata, importance=TRUE, proximity=TRUE, bias.corr=TRUE)
T_RFIndNat.BLM
varImpPlot(T_RFIndNat.BLM)



# 2-D plots
par(mfrow=c(2,3))
partialPlot(T_RFIndNat.BLM, RFtransdata,Log_OE_TN, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,Log_OE_TP, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,XCMG, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,IntDensC, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,Log_AREA_SQKM, cex.main=1)
partialPlot(T_RFIndNat.BLM, RFtransdata,S_Mean, cex.main=1)

# nump= Changes the number of "data points" used to make the graph
par(mfrow=c(2,3))
nump = 15
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, IntDensC, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TN, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TP, Log_AREA_SQKM, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_AREA_SQKM, XCMG, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_AREA_SQKM, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TN, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TP, IntDensC, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, IntDensC, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, IntDensC, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors

bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TN, Log_OE_TP, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TN, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TP, XCMG,ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TP, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, Log_OE_TN, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(T_RFIndNat.BLM, RFtransdata, XCMG, S_Mean, ylab="rating", n1.pt=nump, n2.pt=nump, theta=40) #change theta on this one, can't use factors





##

ab=c(1:5)
for(i in 1:length(ab)) {
  ifelse(ab[i]>2, print(2), print(1))
}


-#iteration example
  -list=c(1,2,4,5,6,7)
-for (i in 1:length(list)){
  -  if(list[i]<5){
    -    print(list[i] + 2)
    -  } else {print(list[i] *5 )}
  -}
-
  -#Nicole's Iteration ex
  -n.list=c(0,1,2,3,5,7,9)
-for(i in 1:length(n.list)){ 
  -  if(n.list[i]<5){
    -    print(n.list[i]+2) 
    -    } else{print(n.list[i]+7)}
  -}








