##loading package
library (randomForest)

##loading file
EC_All=read.csv("z:\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\Predicting_Stressors_OE\\Modeling_Categorical\\EC_All.csv")
EC_All=read.csv("z:\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\Predicting_Stressors_OE\\Modeling_Continuous\\EC_All.csv")
modelfile=read.csv("z:\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\Predicting_Stressors_OE\\modelfile3.csv")

##Data screening
summary(AREMP_OEcat3max_nat) ##summary statistics
str(EC_All) ## lists variable types
is.na(AREMP_OEminCat_Dist) ##flags NA values

##switching variable types
AREMP_OEminCat_Dist$Oerat=as.factor(AREMP_OEminCat_Dist$Oerat) 


#creating dataframes with only anthropogenic predictors
ECdisturb2=subset(EC_All,select= c(EC, Max_DamVol:PrivPct))# this will work even if all response variables are in 1 file
ECdisturb=subset(EC_All,select=EC:PrivPct)
ECdiffdisturb=subset(modelfile,select= c(EC_Diff, Max_DamVol:PrivPct))# this will work even if all response variables are in 1 file
TPdiffdisturb=subset(modelfile,select= c(TP_Diff, Max_DamVol:PrivPct))
TNdiffdisturb=subset(modelfile,select= c(TN_Diff, Max_DamVol:PrivPct))
ECdiffdisturbucs=subset(modelfile,select= c(EC_Diff, UCSMean_WS, Max_DamVol:PrivPct))

ECdiffdisturbpred=subset(modelfile,select= c(EC_Diff, EC_Pred, Max_DamVol:PrivPct))
TPdiffdisturbpred=subset(modelfile,select= c(TP_Diff, TP_Pred, Max_DamVol:PrivPct))
TNdiffdisturbpred=subset(modelfile,select= c(TN_Diff, TN_Pred, Max_DamVol:PrivPct))
OEdisturb=subset(modelfile,select= c(OE, Max_DamVol:PrivPct))

ECdisturb=subset(modelfile,select= c(EC, Max_DamVol:PrivPct))# this will work even if all response variables are in 1 file
TPdisturb=subset(modelfile,select= c(TP, Max_DamVol:PrivPct))
TNdisturb=subset(modelfile,select= c(TN, Max_DamVol:PrivPct))
ECdisturbpred=subset(modelfile,select= c(EC, EC_Pred, Max_DamVol:PrivPct))
TPdisturbpred=subset(modelfile,select= c(TP, TP_Pred, Max_DamVol:PrivPct))
TNdisturbpred=subset(modelfile,select= c(TN, TN_Pred, Max_DamVol:PrivPct))


#creating dataframes with anthro and natural predictors
ECdiffAll=subset(modelfile,select= c(EC_Diff, Pmax_WS:PrivPct))# this will work even if all response variables are in 1 file
TPdiffAll=subset(modelfile,select= c(TP_Diff, Pmax_WS:PrivPct))# this will work even if all response variables are in 1 file
TNdiffAll=subset(modelfile,select= c(TN_Diff, Pmax_WS:PrivPct))# this will work even if all response variables are in 1 file
OEAll=subset(modelfile,select= c(OE, Pmax_WS:PrivPct))
ECrtgAll=subset(modelfile,select= c(ECrtg, Pmax_WS:PrivPct))# this will work even if all response variables are in 1 file

#creating dataframes with instream stressors and bugs
OEinstream=subset(modelfile,select= c(OE, EC_Diff,TN_Diff,TP_Diff,AugST:AvgAngle))
OEinstream2=subset(modelfile,select= c(OE, EC,TN,TP, AugST:AvgAngle))
OEinstream3=subset(modelfile,select= c(OE, ECrtg,TNrtg,TPrtg,AugST:AvgAngle))
OEinstream4=subset(modelfile,select= c(OE, EC:AvgAngle))
OEinstream5=subset(modelfile,select= c(OE, EC_Diff,TN_Diff,TP_Diff,EC_Pred,TN_Pred,TP_Pred,AugST:AvgAngle))

#creating dataframes with other bug metrics
richnessdisturb=subset(modelfile,select= c(Richness, Max_DamVol:PrivPct))
EPTdisturb=subset(modelfile,select= c(num_EPT, Max_DamVol:PrivPct))
scraperdisturb=subset(modelfile,select= c(num_scraper, Max_DamVol:PrivPct))
clingerdisturb=subset(modelfile,select= c(num_clinger, Max_DamVol:PrivPct))
noninsectdisturb=subset(modelfile,select= c(num_noninsect, Max_DamVol:PrivPct))

richnessall=subset(modelfile,select= c(Richness, Pmax_WS:PrivPct))
EPTall=subset(modelfile,select= c(num_EPT, Pmax_WS:PrivPct))
scraperall=subset(modelfile,select= c(num_scraper, Pmax_WS:PrivPct))
clingerall=subset(modelfile,select= c(num_clinger, Pmax_WS:PrivPct))
noninsectall=subset(modelfile,select= c(num_noninsect, Pmax_WS:PrivPct))

# creating dataframes just with natural predictors
richnessnat=subset(modelfile,select= c(Richness, EC_Pred,TN_Pred,TP_Pred, SummerTemp_Predicted,Pmax_WS:Prmh_WS))
EPTnat=subset(modelfile,select= c(num_EPT, EC_Pred,TN_Pred,TP_Pred,SummerTemp_Predicted,Pmax_WS:Prmh_WS))
scrapernat=subset(modelfile,select= c(num_scraper, EC_Pred,TN_Pred,SummerTemp_Predicted,TP_Pred,Pmax_WS:Prmh_WS))
clingernat=subset(modelfile,select= c(num_clinger, EC_Pred,TN_Pred,SummerTemp_Predicted,TP_Pred,Pmax_WS:Prmh_WS))
noninsectnat=subset(modelfile,select= c(num_noninsect, EC_Pred,TN_Pred,TP_Pred,SummerTemp_Predicted,Pmax_WS:Prmh_WS))

#creating dataframes with residuals
richnessdisturb=subset(modelfile,select= c(Max_DamVol:PrivPct))
EPTdisturb=subset(modelfile,select= c(Max_DamVol:PrivPct))
scraperdisturb=subset(modelfile,select= c(Max_DamVol:PrivPct))
clingerdisturb=subset(modelfile,select= c(Max_DamVol:PrivPct))
noninsectdisturb=subset(modelfile,select= c(Max_DamVol:PrivPct))

richnessdisturb=subset(richnessdisturb,select= c(richnessresd,Max_DamVol:PrivPct))

richnessinstream=subset(modelfile,select= c(Richness, EC_Diff,TN_Diff,TP_Diff,AugST:AvgAngle))
EPTinstream=subset(modelfile,select= c(num_EPT, EC_Diff,TN_Diff,TP_Diff,AugST:AvgAngle))
scraperinstream=subset(modelfile,select= c(num_scraper, EC_Diff,TN_Diff,TP_Diff,AugST:AvgAngle))
clingerinstream=subset(modelfile,select= c(num_clinger, EC_Diff,TN_Diff,TP_Diff,AugST:AvgAngle))
noninsectinstream=subset(modelfile,select= c(num_noninsect, EC_Diff,TN_Diff,TP_Diff,AugST:AvgAngle))

richnessinstream=subset(modelfile,select= c(Richness, EC,TN,TP,AugST:AvgAngle))
EPTinstream=subset(modelfile,select= c(num_EPT, EC,TN,TP,AugST:AvgAngle))
scraperinstream=subset(modelfile,select= c(num_scraper, EC,TN,TP,AugST:AvgAngle))
clingerinstream=subset(modelfile,select= c(num_clinger, EC,TN,TP,AugST:AvgAngle))
noninsectinstream=subset(modelfile,select= c(num_noninsect, EC,TN,TP,AugST:AvgAngle))

Allbugmetrics=subset(modelfile,select= c(OE, Richness:num_noninsect))
cor(Allbugmetrics)

attach(ECdiffdisturb)
boxplot(EC_Diff)
detach(ECdiffdisturb)

attach(TNdiffdisturb)
boxplot(TN_Diff)
detach(TNdiffdisturb)

attach(TPdiffdisturb)
boxplot(TP_Diff)
detach(TPdiffdisturb)

attach(OEdisturb)
boxplot(OE)# don't transform OE
boxplot(log(OE))
detach(OEdisturb)

######################################################################
##EC_diff regression
ECdiffdisturb.rf = randomForest (EC_Diff ~., data = ECdiffdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdiffdisturb.rf
varImpPlot(ECdiffdisturb.rf)

ECdiffdisturbpred.rf = randomForest (EC_Diff ~., data = ECdiffdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdiffdisturbpred.rf
varImpPlot(ECdiffdisturbpred.rf)# predicted EC coming up as important here would indicate that there was an interaction between natural factors and anthro?

ECdiffdisturbpred.rf2 = randomForest (EC_Diff ~ AG_WS+OilGasCoun+HydrAlt_km, data = ECdiffdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdiffdisturbpred.rf2
varImpPlot(ECdiffdisturbpred.rf2)

ECdisturb.rf = randomForest (EC ~., data = ECdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdisturb.rf
varImpPlot(ECdisturb.rf)

ECdisturbpred.rf = randomForest (EC ~., data = ECdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdisturbpred.rf
varImpPlot(ECdisturbpred.rf) #predicted conduct can be used as a surrugate for natural factors and the top anthro factors were more important than the predicted EC

### Final most parsimonious
ECdiffdisturb.rf5 = randomForest (EC_Diff ~ AG_WS + OilGasCoun + HydrAlt_km, data = ECdiffdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdiffdisturb.rf5
varImpPlot(ECdiffdisturb.rf5)
###check correlations
attach(ECdiffdisturb)
tmp.cor = cbind(AG_WS,OilGasCoun,HydrAlt_km,AllotPct,OilFld_Pct,NPDES_Dist_m)
cor(tmp.cor)
detach(ECdiffdisturb)

####tried adding in UCS
ECdiffdisturbucs.rf = randomForest (EC_Diff ~ AG_WS + OilGasCoun + HydrAlt_km + SumBlmPct +OilFld_Pct+ NPDES_Dist_m +UCSMean_WS, data = ECdiffdisturbucs, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdiffdisturbucs.rf
varImpPlot(ECdiffdisturbucs.rf)

####All predictors
ECdiffAll.rf2 = randomForest (EC_Diff ~ AG_WS + OilGasCoun + HydrAlt_km + AllotPct +OilFld_Pct+ NPDES_Dist_m +UCSMean_WS +Kfct_WS, data = ECdiffAll, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECdiffAll.rf2
varImpPlot(ECdiffAll.rf2)

attach(ECdiffAll)
tmp.cor = cbind(AG_WS,OilGasCoun,HydrAlt_km,AllotPct,OilFld_Pct,NPDES_Dist_m, Kfct_WS, Pmax_WS,UCSMean_WS)
cor(tmp.cor)
cor(ECdiffAll)

ECrtgAll.rf2 = randomForest (ECrtg ~ AG_WS + OilGasCoun + HydrAlt_km + AllotPct +OilFld_Pct+ NPDES_Dist_m +UCSMean_WS +Kfct_WS, data = ECrtgAll, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
ECrtgAll.rf2
varImpPlot(ECrtgAll.rf2)


#########################################################################
##TN_diff regression
TNdiffdisturb.rf = randomForest (TN_Diff ~., data = TNdiffdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TNdiffdisturb.rf
varImpPlot(TNdiffdisturb.rf)

##TN_diff regression
TNdiffdisturbpred.rf = randomForest (TN_Diff ~., data = TNdiffdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TNdiffdisturbpred.rf
varImpPlot(TNdiffdisturbpred.rf)

##TN_diff regression
TNdiffdisturbpred.rf2 = randomForest (TN_Diff ~ HydrAlt_km+Sum_DamVol+OilGasCoun, data = TNdiffdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TNdiffdisturbpred.rf2
varImpPlot(TNdiffdisturbpred.rf2)

##TN regression
TNdisturbpred.rf = randomForest (TN ~., data = TNdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TNdisturbpred.rf
varImpPlot(TNdisturbpred.rf)

##Final most parsimonious
TNdiffdisturb.rf2 = randomForest (TN_Diff ~ HydrAlt_km + OilGasCoun +Sum_DamVol, data = TNdiffdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TNdiffdisturb.rf2
varImpPlot(TNdiffdisturb.rf2)
###check correlations
attach(ECdiffdisturb)
tmp.cor = cbind(Max_DamVol,OilGasCoun,HydrAlt_km)
cor(tmp.cor)
detach(ECdiffdisturb)

###3 variables that came out of Ryan Hill's code but % var explained only 17
TNdiffdisturb.rf3 = randomForest (TN_Diff ~ HydrAlt_km + NPDES_Dist_m +Max_DamVol, data = TNdiffdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TNdiffdisturb.rf3
varImpPlot(TNdiffdisturb.rf3)

####All predictors
TNdiffAll.rf = randomForest (TN_Diff ~., data = TNdiffAll, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TNdiffAll.rf
varImpPlot(TNdiffAll.rf)

##########################################################
##TP_diff regression
TPdiffdisturb.rf = randomForest (TP_Diff ~., data = TPdiffdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TPdiffdisturb.rf
varImpPlot(TPdiffdisturb.rf)

TPdiffdisturbpred.rf = randomForest (TP_Diff ~., data = TPdiffdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TPdiffdisturbpred.rf
varImpPlot(TPdiffdisturbpred.rf)


TPdisturbpred.rf = randomForest (TP ~., data = TPdisturbpred, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TPdisturbpred.rf
varImpPlot(TPdisturbpred.rf)


##Final most parsimonious
TPdiffdisturb.rf2 = randomForest (TP_Diff ~ Max_DamVol+HydrAlt_km+SumBlmPct +OilGasCoun, data = TPdiffdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TPdiffdisturb.rf2
varImpPlot(TPdiffdisturb.rf2)

##check correlations
attach(ECdiffdisturb)
tmp.cor = cbind(Sum_DamVol,OilGasCoun,PastPct,HydrAlt_km)
cor(tmp.cor)
detach(ECdiffdisturb)

####All predictors
TPdiffAll.rf = randomForest (TP_Diff ~., data = TPdiffAll, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
TPdiffAll.rf
varImpPlot(TPdiffAll.rf)

##################################################################
##OE regression
OEdisturb.rf = randomForest (OE ~., data = OEdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEdisturb.rf
varImpPlot(OEdisturb.rf)

OEdisturb.rf2 = randomForest (OE ~ MINEperSQKM+HydrAlt_km+URB_WS, data = OEdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEdisturb.rf2
varImpPlot(OEdisturb.rf2)

attach(ECdiffdisturb)
tmp.cor = cbind(MINEperSQKM,HydrAlt_km,URB_WS)
cor(tmp.cor)
detach(ECdiffdisturb)

#All predictors
OEAll.rf = randomForest (OE ~., data = OEAll, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEAll.rf
varImpPlot(OEAll.rf)


#Instream
OEinstream.rf2 = randomForest (OE ~., data = OEinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEinstream.rf2
varImpPlot(OEinstream.rf2)

OEinstream.rf3 = randomForest (OE ~ TP, data = OEinstream2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEinstream.rf3
varImpPlot(OEinstream.rf3)

OEinstream.rf4 = randomForest (OE ~., data = OEinstream3, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEinstream.rf4
varImpPlot(OEinstream.rf4)

OEinstream.rf5 = randomForest (OE ~., data = OEinstream4, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEinstream.rf5
varImpPlot(OEinstream.rf5)

OEinstream.rf6 = randomForest (OE ~., data = OEinstream5, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEinstream.rf6
varImpPlot(OEinstream.rf6)

OEinstream.rf61 = randomForest (OE ~MWMT+AugST+D16_woBed, data = OEinstream5, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
OEinstream.rf61
varImpPlot(OEinstream.rf61)


#####################################################################################################
####other metrics modeling
richnessdisturb.rf = randomForest (Richness ~., data = richnessdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessdisturb.rf
varImpPlot(richnessdisturb.rf)

richnessdisturb.rf2 = randomForest (Richness ~OilFld_Pct+AG_WS+HydrAlt_km+RPastPct, data = richnessdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessdisturb.rf2
varImpPlot(richnessdisturb.rf2)

EPTdisturb.rf = randomForest (num_EPT ~., data = EPTdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTdisturb.rf
varImpPlot(EPTdisturb.rf)

EPTdisturb.rf2 = randomForest (num_EPT ~OilGasCoun+ AG_WS+AllotPct, data = EPTdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTdisturb.rf2
varImpPlot(EPTdisturb.rf2)

scraperdisturb.rf = randomForest (num_scraper ~., data = scraperdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperdisturb.rf
varImpPlot(scraperdisturb.rf)

scraperdisturb.rf2 = randomForest (num_scraper ~ AG_WS+OilFld_Pct+HydrAlt_km, data = scraperdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperdisturb.rf2
varImpPlot(scraperdisturb.rf2)

clingerdisturb.rf = randomForest (num_clinger ~., data = clingerdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerdisturb.rf
varImpPlot(clingerdisturb.rf)

clingerdisturb.rf2 = randomForest (num_clinger ~OilFld_Pct+AllotPct+AG_WS, data = clingerdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerdisturb.rf2
varImpPlot(clingerdisturb.rf2)

noninsectdisturb.rf = randomForest (num_noninsect ~., data = noninsectdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectdisturb.rf
varImpPlot(noninsectdisturb.rf)

noninsectdisturb.rf2 = randomForest (num_noninsect ~BLMPct+OilGasCoun +RdDens, data = noninsectdisturb, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectdisturb.rf2
varImpPlot(noninsectdisturb.rf2)
####################################################################################################
##############models with natural and anthrogenic
richnessall.rf = randomForest (Richness ~., data = richnessall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessall.rf
varImpPlot(richnessall.rf)

richnessall.rf2 = randomForest (Richness ~WSA + BFI_WS + Pmax_WS, data = richnessall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessall.rf2
varImpPlot(richnessall.rf2)

EPTall.rf = randomForest (num_EPT ~., data = EPTall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTall.rf
varImpPlot(EPTall.rf)

EPTall.rf2 = randomForest (num_EPT ~Pmax_WS+BFI_WS+Kfct_WS+Pmin_WS+WSA+OilFld_Pct, data = EPTall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTall.rf2
varImpPlot(EPTall.rf2)

scraperall.rf = randomForest (num_scraper ~., data = scraperall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperall.rf
varImpPlot(scraperall.rf)

scraperall.rf2 = randomForest (num_scraper ~ AG_WS+OilFld_Pct+HydrAlt_km, data = scraperall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperall.rf2
varImpPlot(scraperall.rf2)

clingerall.rf = randomForest (num_clinger ~., data = clingerall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerall.rf
varImpPlot(clingerall.rf)

clingerall.rf2 = randomForest (num_clinger ~OilFld_Pct+AllotPct+AG_WS, data = clingerall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerall.rf2
varImpPlot(clingerall.rf2)

noninsectall.rf = randomForest (num_noninsect ~., data = noninsectall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectall.rf
varImpPlot(noninsectall.rf)

noninsectall.rf2 = randomForest (num_noninsect ~BLMPct+OilGasCoun +RdDens, data = noninsectall, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectall.rf2
varImpPlot(noninsectall.rf2)
##########################################################################################################
######models just with natural factors
richnessnat.rf = randomForest (Richness ~., data = richnessnat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessnat.rf
varImpPlot(richnessnat.rf)

richnessnat.rf3 = randomForest (richresd ~., data = richnessnat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessnat.rf3
varImpPlot(richnessnat.rf3)

richnessnat.rf2 = randomForest (Richness ~Pmax_WS+BFI_WS+WSA, data = richnessnat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessnat.rf2
varImpPlot(richnessnat.rf2)

EPTnat.rf = randomForest (num_EPT ~., data = EPTnat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTnat.rf
varImpPlot(EPTnat.rf)

EPTnat.rf2 = randomForest (num_EPT ~Pmax_WS+BFI_WS+Kfct_WS+Pmin_WS, data = EPTnat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTnat.rf2
varImpPlot(EPTnat.rf2)

scrapernat.rf = randomForest (num_scraper ~., data = scrapernat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scrapernat.rf
varImpPlot(scrapernat.rf)

scrapernat.rf2 = randomForest (num_scraper ~ Tmean_WS+Kfct_WS+Pmin_WS, data = scrapernat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scrapernat.rf2
varImpPlot(scrapernat.rf2)

clingernat.rf = randomForest (num_clinger ~., data = clingernat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingernat.rf
varImpPlot(clingernat.rf)

clingernat.rf2 = randomForest (num_clinger ~Pmax_WS+Awch_WS+Pmin_WS+Dom_Geol+MgOMean_WS, data = clingernat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingernat.rf2
varImpPlot(clingernat.rf2)

noninsectnat.rf = randomForest (num_noninsect ~., data = noninsectnat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectnat.rf
varImpPlot(noninsectnat.rf)

noninsectnat.rf2 = randomForest (num_noninsect ~TP_Pred + EC_Pred + Hydr_PT +Tmax_WS + SummerTemp_Predicted, data = noninsectnat, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectnat.rf2
varImpPlot(noninsectnat.rf2)
##############################################################
#with residuals of natural and only disturb
####other metrics modeling
richnessdisturb.rf = randomForest (richnessresd ~., data = richnessdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessdisturb.rf
varImpPlot(richnessdisturb.rf)

richnessdisturb.rf2 = randomForest (richnessresd ~ RdDens+OilGasCoun, data = richnessdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessdisturb.rf2
varImpPlot(richnessdisturb.rf2)

EPTdisturb.rf = randomForest (EPTresd ~., data = EPTdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTdisturb.rf
varImpPlot(EPTdisturb.rf)AllotPct+SumGrazing+NPDES_Dist_m+Sum_DamVol+OilFld_Pct

EPTdisturb.rf2 = randomForest (EPTresd ~NPDES_Dist_m+OilGasCoun, data = EPTdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTdisturb.rf2
varImpPlot(EPTdisturb.rf2)

scraperdisturb.rf = randomForest (scraperresd ~., data = scraperdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperdisturb.rf
varImpPlot(scraperdisturb.rf)

scraperdisturb.rf2 = randomForest (scraperresd ~ PrivPct+OilFld_Pct+AG_WS+NPDES_Dist_m, data = scraperdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperdisturb.rf2
varImpPlot(scraperdisturb.rf2)

clingerdisturb.rf = randomForest (clingerresd ~., data = clingerdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerdisturb.rf
varImpPlot(clingerdisturb.rf)

clingerdisturb.rf2 = randomForest (clingerresd ~NPDES_Dist_m+OilGasCoun, data = clingerdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerdisturb.rf2
varImpPlot(clingerdisturb.rf2)

noninsectdisturb.rf = randomForest (noninsectresd ~., data = noninsectdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectdisturb.rf
varImpPlot(noninsectdisturb.rf)

noninsectdisturb.rf2 = randomForest (noninsectresd ~OilFld_Pct+PrivPct, data = noninsectdisturb2, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectdisturb.rf2
varImpPlot(noninsectdisturb.rf2)

###############################################
###creating dataframes and variables with residuals
residuals=cbind(richnessnat.rf2$predicted,EPTnat.rf2$predicted,scrapernat.rf2$predicted,clingernat.rf2$predicted,noninsectnat.rf2$predicted)
richnessresd=richnessnat$Richness-richnessnat.rf2$predicted
EPTresd=EPTnat$num_EPT-EPTnat.rf2$predicted
scraperresd=scrapernat$num_scraper-scrapernat.rf2$predicted
clingerresd=clingernat$num_clinger-clingernat.rf2$predicted
noninsectresd=noninsectnat$num_noninsect-noninsectnat.rf2$predicted
richnessdisturb$richnessresd=richnessresd
EPTdisturb$EPTresd=EPTresd
scraperdisturb$scraperresd=scraperresd
clingerdisturb$clingerresd=clingerresd
noninsectdisturb$noninsectresd=noninsectresd

###reordering variables
richnessdisturb2=subset(richnessdisturb,select= c(richnessresd,Max_DamVol:PrivPct))
EPTdisturb2=subset(EPTdisturb,select= c(EPTresd,Max_DamVol:PrivPct))
scraperdisturb2=subset(scraperdisturb,select= c(scraperresd,Max_DamVol:PrivPct))
clingerdisturb2=subset(clingerdisturb,select= c(clingerresd,Max_DamVol:PrivPct))
noninsectdisturb2=subset(noninsectdisturb,select= c(noninsectresd,Max_DamVol:PrivPct))


#########################################################################################################
richnessinstream.rf = randomForest (Richness ~., data = richnessinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessinstream.rf
varImpPlot(richnessinstream.rf)

richnessinstream.rf2 = randomForest (Richness ~Stab2+PCT_SAFN+AvgAngle, data = richnessinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
richnessinstream.rf2
varImpPlot(richnessinstream.rf2)

EPTinstream.rf = randomForest (num_EPT ~., data = EPTinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTinstream.rf
varImpPlot(EPTinstream.rf)

EPTinstream.rf2 = randomForest (num_EPT ~MWMT+TN_Diff+AvgAngle, data = EPTinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTinstream.rf2
varImpPlot(EPTinstream.rf2)

EPTinstream.rf3 = randomForest (num_EPT ~EC+MWMT+TN+AvgAngle+AugST+Stab2, data = EPTinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EPTinstream.rf3
varImpPlot(EPTinstream.rf3)###top model when raw EC, TN, TP values used

scraperinstream.rf = randomForest (num_scraper ~., data = scraperinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperinstream.rf
varImpPlot(scraperinstream.rf)

scraperinstream.rf2 = randomForest (num_scraper ~EC_Diff+AvgAngle, data = scraperinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperinstream.rf2
varImpPlot(scraperinstream.rf2)

scraperinstream.rf3 = randomForest (num_scraper ~EC+AvgAngle+TP+MWMT+Stab2, data = scraperinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
scraperinstream.rf3
varImpPlot(scraperinstream.rf3)###top model when raw EC, TN, TP values used

clingerinstream.rf = randomForest (num_clinger ~., data = clingerinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerinstream.rf
varImpPlot(clingerinstream.rf)

clingerinstream.rf2 = randomForest (num_clinger ~MWMT+AvgAngle+AugST, data = clingerinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerinstream.rf2
varImpPlot(clingerinstream.rf2)

clingerinstream.rf3 = randomForest (num_clinger ~EC+MWMT+AvgAngle+AugST, data = clingerinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
clingerinstream.rf3
varImpPlot(clingerinstream.rf3)###top model when raw EC, TN, TP values used

noninsectinstream.rf = randomForest (num_noninsect ~., data = noninsectinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectinstream.rf
varImpPlot(noninsectinstream.rf)

noninsectinstream.rf2 = randomForest (num_noninsect ~PCT_SAFN+Stab2+EC_Diff, data = noninsectinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectinstream.rf2
varImpPlot(noninsectinstream.rf2)

noninsectinstream.rf3 = randomForest (num_noninsect ~PCT_SAFN+Stab2+EC, data = noninsectinstream, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
noninsectinstream.rf3
varImpPlot(noninsectinstream.rf3)
###################################################################preliminary trials#################################################
##randomforest modeling regression mode
EC_All_contin.rf = randomForest (EC ~., data = EC_All, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EC_All_contin.rf
varImpPlot(EC_All_contin.rf)

##randomforest modeling regression mode
EC_All_diff.rf = randomForest (EC_Diff ~., data = EC_All_diff, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
EC_All_diff.rf
varImpPlot(EC_All_diff.rf)

##randomforest modeling Prediction mode - OE/ALL
OE_All.rf = randomForest (OErtg ~., data = OE_All, importance = TRUE, ntree=2000) #, proximity = TRUE, bias.corr = TRUE)
OE_All.rf
varImpPlot(OE_All.rf)

##randomforest modeling Prediction mode - OE/Dist
OE_All_dist.rf = randomForest (OErtg ~., data = OE_All_dist, importance = TRUE, ntree=2000) #, proximity = TRUE, bias.corr = TRUE)
OE_All_dist.rf
varImpPlot(OE_All_dist.rf)

##randomforest modeling Prediction mode - OE/Dist/GP
OE_All_dist_GP.rf = randomForest (OErtg ~., data = OE_All_dist_GP, importance = TRUE, ntree=2000) #, proximity = TRUE, bias.corr = TRUE)
OE_All_dist_GP.rf
varImpPlot(OE_All_dist_GP.rf)

##randomforest modeling Prediction mode - EC/All
EC_All.rf = randomForest (ECrtg ~., data = EC_All, importance = TRUE, ntree=2000) #, proximity = TRUE, bias.corr = TRUE)
EC_All.rf
varImpPlot(EC_All.rf)


##randomforest modeling Prediction mode - EC/All/GP
EC_All_GP.rf = randomForest (ECrtg ~., data = EC_All_GP, importance = TRUE, ntree=2000) #, proximity = TRUE, bias.corr = TRUE)
EC_All_GP.rf
varImpPlot(EC_All_GP.rf)

##randomforest modeling Prediction mode - EC/Dist
EC_dist.rf = randomForest (ECrtg ~., data = EC_dist, importance = TRUE, ntree=2000) #, proximity = TRUE, bias.corr = TRUE)
EC_dist.rf
varImpPlot(EC_dist.rf)

##randomforest modeling Prediction mode - EC/Dist/GP
EC_dist_GP.rf = randomForest (ECrtg ~., data = EC_dist_GP, importance = TRUE, ntree=2000) #, proximity = TRUE, bias.corr = TRUE)
EC_dist_GP.rf
varImpPlot(EC_dist_GP.rf)
#####################################################################################################################################


##Ryan Hill's automated script for variable selection- ranks the variables by R squared
xname = names(ECdiffdisturb[,2:ncol(ECdiffdisturb)])# enter the column number for the first predictor variable
tmp.formula = "EC_Diff ~ "# enter response variable


counter = 1
for (x in xname){
  
  fmla=as.formula(paste(tmp.formula, paste(x, collapse="+")))
  
  tmp.rf = randomForest(fmla, data=ECdiffdisturb)
  rsq = median(tmp.rf$rsq)
  
  if(counter==1) var.perform=data.frame(x,rsq) 
  
  if(counter > 1) var.perform=rbind(var.perform,data.frame(x,rsq))
  
  counter = counter+1   
  
}
var.perform[order(var.perform$rsq, decreasing=T),]

###Ryan Hills code
xname = names(TNdiffdisturb[,2:ncol(ECdiffdisturb)])# enter the column number for the first predictor variable
tmp.formula = "TN_Diff ~ "# enter response variable


counter = 1
for (x in xname){
  
  fmla=as.formula(paste(tmp.formula, paste(x, collapse="+")))
  
  tmp.rf = randomForest(fmla, data=TNdiffdisturb)
  rsq = median(tmp.rf$rsq)
  
  if(counter==1) var.perform=data.frame(x,rsq) 
  
  if(counter > 1) var.perform=rbind(var.perform,data.frame(x,rsq))
  
  counter = counter+1   
  
}
var.perform[order(var.perform$rsq, decreasing=T),]


# partialplot
par(mai=c(2,2,2,2)) 
sapply(unique(AREMP_OEcat3max_nat$Oerat3max),function(grp){
partialPlot(AREMP_OEcat3max_nat.rf,pred.data=AREMP_OEcat3max_nat, x.var= Organic.matter, which.class="Good",main=paste("",""),xlab="Organic matter (%)",ylab="(logit Probability of good rating)/2", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)});



partialPlot(ECdiffdisturb.rf5,pred.data=ECdiffdisturb, x.var= AG_WS,main=paste("",""),xlab="AG_WS (%)",ylab="Observed EC-Expected EC", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(ECdiffdisturb.rf5,pred.data=ECdiffdisturb, x.var= OilGasCoun,main=paste("",""),ylab="Observed EC-Expected EC", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(ECdiffdisturb.rf5,pred.data=ECdiffdisturb, x.var= HydrAlt_km,main=paste("",""),,ylab="Observed EC-Expected EC", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(ECdiffdisturb.rf5,pred.data=ECdiffdisturb, x.var= AllotPct,main=paste("",""),,ylab="Observed EC-Expected EC", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(ECdiffdisturb.rf5,pred.data=ECdiffdisturb, x.var= OilFld_Pct,main=paste("",""),,ylab="Observed EC-Expected EC", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(ECdiffdisturb.rf5,pred.data=ECdiffdisturb, x.var= NPDES_Dist_m,main=paste("",""),,ylab="Observed EC-Expected EC", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)

##weird response for AllotPct so tried PastPct but range of variable very different
partialPlot(ECdiffdisturb.rf,pred.data=ECdiffdisturb, x.var= PastPct,main=paste("",""),,ylab="Observed EC-Expected EC", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)

partialPlot(TNdiffdisturb.rf2,pred.data=TNdiffdisturb, x.var= HydrAlt_km,main=paste("",""),,ylab="Observed TN-Expected TN", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(TNdiffdisturb.rf2,pred.data=TNdiffdisturb, x.var= OilGasCoun,main=paste("",""),,ylab="Observed TN-Expected TN", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(TNdiffdisturb.rf2,pred.data=TNdiffdisturb, x.var= Sum_DamVol,main=paste("",""),,ylab="Observed TN-Expected TN", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)

partialPlot(TPdiffdisturb.rf2,pred.data=TPdiffdisturb, x.var= Sum_DamVol,main=paste("",""),,ylab="Observed TP-Expected TP", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(TPdiffdisturb.rf2,pred.data=TPdiffdisturb, x.var= HydrAlt_km,main=paste("",""),,ylab="Observed TP-Expected TP", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(TPdiffdisturb.rf2,pred.data=TPdiffdisturb, x.var= PastPct,main=paste("",""),,ylab="Observed TP-Expected TP", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(TPdiffdisturb.rf2,pred.data=TPdiffdisturb, x.var= OilGasCoun,main=paste("",""),,ylab="Observed TP-Expected TP", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)

partialPlot(OEdisturb.rf2,pred.data=OEdisturb, x.var= MINEperSQKM,main=paste("",""),,ylab="OE", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(OEdisturb.rf2,pred.data=OEdisturb, x.var= HydrAlt_km,main=paste("",""),,ylab="OE", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(OEdisturb.rf2,pred.data=OEdisturb, x.var= URB_WS,main=paste("",""),,ylab="OE", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)

partialPlot(OEinstream.rf61,pred.data=OEinstream5, x.var= MWMT,main=paste("",""),,ylab="OE", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(OEinstream.rf61,pred.data=OEinstream5, x.var= AugST,main=paste("",""),,ylab="OE", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(OEinstream.rf61,pred.data=OEinstream5, x.var= D16_woBed,main=paste("",""),,ylab="OE", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)

partialPlot(richnessdisturb.rf2,pred.data=richnessdisturb2, x.var= RdDens,main=paste("",""),,ylab="Richness natural RF residuals", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(richnessdisturb.rf2,pred.data=richnessdisturb2, x.var= OilGasCoun,main=paste("",""),,ylab="Richness natural RF residuals", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)

partialPlot(scraperdisturb.rf2,pred.data=scraperdisturb2, x.var= PrivPct,main=paste("",""),,ylab="# of scraper taxa natural RF residuals", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(scraperdisturb.rf2,pred.data=scraperdisturb2, x.var= OilFld_Pct,main=paste("",""),,ylab="# of scraper taxa natural RF residuals", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(scraperdisturb.rf2,pred.data=scraperdisturb2, x.var= AG_WS,main=paste("",""),,ylab="# of scraper taxa natural RF residuals", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)
partialPlot(scraperdisturb.rf2,pred.data=scraperdisturb2, x.var= NPDES_Dist_m,main=paste("",""),,ylab="# of scraper taxa natural RF residuals", lwd=3, mai=c(0.5,5,0.5,0.5), cex.lab=2, cex.axis=1.5)



##random forest bivariate partial plot


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

## once you have defined the bivariate plot function (with above code), use this code to create one.  it takes 5-10 mins, so be patient:



nump = 10
bpp.out = bivarpartialPlot.randomForest(AREMP_OEcat3max_ALL.rf, AREMP_OEcat3max_ALL, Organic.matter, Road.Density,which.class="Good",ylab="Probability of good rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors

bpp.out4 = bivarpartialPlot.randomForest(ECdiffAll.rf2, ECdiffAll, UCSMean_WS, AG_WS, n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out5 = bivarpartialPlot.randomForest(ECdiffAll.rf2, ECdiffAll, UCSMean_WS, AllotPct, n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out = bivarpartialPlot.randomForest(ECrtgAll.rf2, ECrtgAll, UCSMean_WS, AG_WS,which.class="Good",ylab="Probability of good rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors

bpp.out1 = bivarpartialPlot.randomForest(ECrtgAll.rf2, ECrtgAll, Kfct_WS, AG_WS,which.class="Good",ylab="Probability of good rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out2 = bivarpartialPlot.randomForest(ECrtgAll.rf2, ECrtgAll, UCSMean_WS, AllotPct,which.class="Good",ylab="Probability of good rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors
bpp.out3 = bivarpartialPlot.randomForest(ECrtgAll.rf2, ECrtgAll, Kfct_WS, AllotPct,which.class="Good",ylab="Probability of good rating", n1.pt=nump, n2.pt=nump, theta=220) #change theta on this one, can't use factors

##exploratory plots of each variable # consider transforming response and OilGasCoun all others are too skewed for common transformations to help much
attach(ECdisturb)
boxplot(EC)
boxplot(log(EC))# consider log transforming variable
boxplot(Max_DamVol)
boxplot(Max_DamVol)
boxplot(sqrt(Max_DamVol))
boxplot((Max_DamVol)^2)
boxplot(Sum_DamVol)
boxplot(sqrt(Sum_DamVol))
boxplot(Sum_DamVol^2)
boxplot(MINEperSQKM)
boxplot(log(MINEperSQKM+1))
boxplot(MINEperSQKM)
boxplot(log(Sum_DamVol+1))
boxplot(log(Max_DamVol+1))
boxplot(OilGasCoun)
boxplot(log(OilGasCoun+1))#consider log +1 transforming variable
boxplot(OilFld_Pct)
boxplot(log(OilFld_Pct+1))
boxplot(AllotPct)
boxplot(PastPct)
boxplot(log(PastPct+1))
boxplot(RPastPct)
boxplot(log(RPastPct+1))
boxplot(HorsePct)
boxplot(log(HorsePct+1))
boxplot(AG_WS)
boxplot(log(AG_WS+1))
boxplot(URB_WS)
plot(NPDES_Dist_m)
boxplot(HydrAlt_km)
boxplot(log(HydrAlt_km+1))
