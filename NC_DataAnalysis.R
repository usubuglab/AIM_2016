#How to order data
New table name=data[order(data$column),]

#R studio only allows the first 1000 records and 100 fields to be viewed. With the below code I can choose which records I want to view. This is an example of viewing rows 3500-4500
#View(data[records or rows, fields or columns])
View(data[3500:4500,])

# Quick notes:
#### ! means negate
#### | means OR

#############################################################################

##############    Water quality condition determinations      ###############

#############################################################################

# First get field measured WQ data (conductivity, tn, tp) using code in NC_DataConsumption
# Read in predicted WQ results from the WQ models
PrdWQresults=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\Pred_WQresults_2014All.csv")

#Rename the siteCode column in predicted file to match the Site ID column in the data file so that the files can be merged
PrdWQresults$SITE_ID=PrdWQresults$SiteCode

#Merge the files and check that it worked correctly
#to use the merge function you must have a column in each file that match exactly. 
AllWQ=merge(PrdWQresults,WQfinal)

#Reorganize and remove unwanted columns by only choosing those you want, you can use column numbers or column names. I think column names are more intuitive but column numbers left as an example.
AllWQ2=AllWQ[,c('UID','SITE_ID','LOC_NAME','DATE_COL','PrdCond','Pred_TN','Pred_TP','CONDUCTIVITY',	'NTL',	'PTL')]
#Trial2=AllWQ[,c(7,1,13,11,6,5,4,8,9,10)]

#Now subtract Observe - expected to get the OE score to be classified as G F P. Then set thresholds for each indicator
#If o-e is > lower limit it is fair, if it is > upper limit its poor, if it is < lower limit it is good(all else is good)

###Conductivity
AllWQ2$OE_Cond = AllWQ2$CONDUCTIVITY - AllWQ2$PrdCond 
AllWQ2$Cond_condition=ifelse(AllWQ2$OE_Cond <=27.1,'Good',ifelse(AllWQ2$OE_Cond >53.7, 'Poor','Fair'))

###Total N
AllWQ2$OE_TN = AllWQ2$NTL - AllWQ2$Pred_TN 
AllWQ2$TN_condition=ifelse(AllWQ2$OE_TN <=52.1,'Good',ifelse(AllWQ2$OE_TN >114.7, 'Poor','Fair'))

###Total P
AllWQ2$OE_TP = AllWQ2$PTL - AllWQ2$Pred_TP 
AllWQ2$TP_condition=ifelse(AllWQ2$OE_TP <=9.9,'Good',ifelse(AllWQ2$OE_TP >21.3, 'Poor','Fair'))

View(AllWQ2)
rm(PrdWQresults,AllWQ)


#Write to a csv, but I would prefer not to do this... UID issue may cause problems in the future so it would be easier to just keep the data active and in R. 
#However I did write to a csv so there was a hardcopy of the results for a backup.
#write.csv(AllWQ2, "\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\WQconditions_2014All_8Oct2014.csv")



#############################################################################

##############     Invertebrate condition determinations      ###############

#############################################################################
NorCalBugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\ALL_BugModel_Results.csv")

#############################################################################
#######################     NV MODELS     ###################################
#############################################################################
#Scores below the impaired threshold imply degradation. Scores greater than or equal to the equivalence threshold imply reference condition. 
#Pc>0
#Impaired = 0.6020531
#Equivalence = 0.6864668

#Pc>=0.5
#Impaired = 0.6035492
#Equivalence = 0.701018

#Excel formula: =IF(N3> OR EQUAL TO 0.6864668,"Good",IF(N3<0.6020531,"Poor","Fair"))

NorCalBugs$NV_OE5_Cond=ifelse(NorCalBugs$NV_OE5 >=0.701018,'Good',ifelse(NorCalBugs$NV_OE5 <0.6035492, 'Poor','Fair'))

NorCalBugs$NV_OE0_Cond=ifelse(NorCalBugs$NV_OE0 >=0.6864668,'Good',ifelse(NorCalBugs$NV_OE0 <0.6020531, 'Poor','Fair'))


#############################################################################
#######################     CSCI MODELS     #################################
#############################################################################
#Uses 0.5 detection! Poor=(Mean-(1.65*SD)), good is .825*SD
#### Mean:  CSCI: 1.010739; O/E: 1.021478
#### StDv:  CSCI: 0.123775; O/E: 0.190266
### CSCI
# Good >=0.9086246
# Fair
# Poor <0.8067303

###O/E
# Good >=0.8645086
# Fair
# Poor <0.7075391

#Uses 0.5 detection!Poor=(Mean-(2*SD)), good is 1*SD
### CSCI
# Good >=0.886964
# Fair
# Poor < 0.763189

###O/E
# Good >=0.831212
# Fair
# Poor <0.640946

######Current recommendation: classify impairment (poor) as scores falling 1.65 SD  
######below the Mean (Mean-(1.65*SD));  This is still undergoing discussion in the 
######SWAMP group, so the traditional 2 SD approach can still be implemented. 

##########################

#Index   Likely intact        Possibly intact        Likely altered      Very likely altered
#          (≥0.30)            (0.10 to 0.30)         (0.01 to 0.10)           (<0.01)
#CSCI       ≥0.92              0.79 to 0.92	          0.63 to 0.79	       0.00 to 0.63
#pMMI	      ≥0.91	             0.77 to 0.91	          0.58 to 0.77	       0.00 to 0.58
#O/E        ≥0.90	             0.76 to 0.90	          0.56 to 0.76	       0.00 to 0.56

#            Good                              Fair                            Poor
#CSCI      >= 0.855                       <0.855 and >0.71                    < 0.71        	      
#pMMI	     >= 0.84	                      <0.84 and >0.675                    < 0.675	              
#O/E       >= 0.83	                      <0.83 and >0.66                     < 0.66	                


# My first attempt is to set thresholds based on the Mean and SD for OE and CSCI and then compare to the tabled approach. 
# However, I will using a three category approach rather than the 4 categories. To do this I will take the midpoint of 
# each middle category to create one middle category and expanding the thresholds of the "good" and "poor" categories.
# Column names: CSCI_OE  CSCI_MMI	CSCI_Hybrid

# Using the midpoint criteria 
NorCalBugs$CSCI_Hybrid_MidCond=ifelse(NorCalBugs$CSCI_Hybrid >=0.855,'Good',ifelse(NorCalBugs$CSCI_Hybrid <0.71, 'Poor','Fair'))
NorCalBugs$CSCI_MMI_MidCond=ifelse(NorCalBugs$CSCI_MMI >=0.84,'Good',ifelse(NorCalBugs$CSCI_MMI <0.675, 'Poor','Fair'))
NorCalBugs$CSCI_OE_MidCond=ifelse(NorCalBugs$CSCI_OE >=0.83,'Good',ifelse(NorCalBugs$CSCI_OE <0.66, 'Poor','Fair'))

#Using the two upper categories merged to create just 3 categories rather than 4
NorCalBugs$CSCI_Hybrid_UpperCond=ifelse(NorCalBugs$CSCI_Hybrid >=0.79,'Good',ifelse(NorCalBugs$CSCI_Hybrid <0.63, 'Poor','Fair'))
NorCalBugs$CSCI_MMI_UpperCond=ifelse(NorCalBugs$CSCI_MMI >=0.7,'Good',ifelse(NorCalBugs$CSCI_MMI <0.58, 'Poor','Fair'))
NorCalBugs$CSCI_OE_UpperCond=ifelse(NorCalBugs$CSCI_OE >=0.76,'Good',ifelse(NorCalBugs$CSCI_OE <0.56, 'Poor','Fair'))




Freq1=cbind(count(NorCalBugs,var='CSCI_Hybrid_MidCond'),count(NorCalBugs,var='CSCI_MMI_MidCond'),count(NorCalBugs,var='CSCI_OE_MidCond'),count(NorCalBugs,var='NV_OE0_Cond'),count(NorCalBugs,var='NV_OE5_Cond'),count(NorCalBugs,var='NV_MMI_Cond'),count(NorCalBugs,var='CSCI_OE_UpperCond'),count(NorCalBugs,var='CSCI_Hybrid_UpperCond'),count(NorCalBugs,var='CSCI_MMI_UpperCond'))


NorCalCHECK_Bugs=subset(NorCalBugs, SiteCode=='AR-SS-8066'|SiteCode=='SU-SS-8311'|SiteCode=='AR-LS-8018'|SiteCode=='AR-SS-8017'|SiteCode=='HC-SS-8456'|SiteCode=='HC-SS-8475')
NorCalCHECK_Bugs$NicoleJudge=c('G','P','P','P','G','G')
  



#plot O/E
plot(NorCalBugs$NV_OE5,NorCalBugs$CSCI_OE, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_OE~NorCalBugs$NV_OE5)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_OE,NorCalBugs$NV_OE5))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

#plot MMI
NorCalBugs$NV_MMI_2=NorCalBugs$NV_MMI/100
plot(NorCalBugs$NV_MMI_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$NV_MMI_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$NV_MMI_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

  
plot(NorCalBugs$NV_MMI_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$NV_MMI_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$NV_MMI_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)





#Second EXAMPLE TEXT IN GRAPH
#TEXT IN GRAPH
#plot(Weight~Length)
#lines(m1,col="red")
#text(locator(1), " Y=9e-07x^3.4198   R Squared = .92")
#############################################################################

##############   Physical Habitat condition determinations    ###############

#############################################################################

#First I need to combine Ecoregions to the sample sites. 
#Read in ecoregion to sample sitecode
NorCalSites_Ecoregions=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\UID_SiteID_Ecoregions.csv")
#Because UIDs get turned into different values when exported as a csv I was unable to use the merge function because UIDs did not match exactly. Be careful with using this method in the future... 
t1=NorCalSites_Ecoregions[order(NorCalSites_Ecoregions$UID, decreasing=FALSE),]
t2=AquametCheck[order(AquametCheck$UID, decreasing=FALSE),]
t3=cbind(t1,t2)
t3=t3[,-1] 
Indicators=t3[,c(4,1,2,3,5:12)]
rm(t1,t2,t3,NorCalSites_Ecoregions)

#Subset all by ecoregions... got to be a better way...
#ECO10
#Get a list of the indicators in the file to use for Subsetting
unique(Indicators$ECO10)

Ind_MT_PNW=subset(Indicators, ECO10=='MT-PNW')
Ind_XE_NORTH=subset(Indicators, ECO10=='XE-NORTH')


# MT-PNW for all indicators 
Ind_MT_PNW$XFC_NAT_COND=ifelse(Ind_MT_PNW$XFC_NAT_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_MT_PNW$XFC_NAT_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW','XFC_NAT_0.25']),"Good","Fair"))
Ind_MT_PNW$XCMG_COND=ifelse(Ind_MT_PNW$XCMG_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW', 'XCMG_0.05']),"Poor",ifelse(Ind_MT_PNW$XCMG_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW','XCMG_0.25']),"Good","Fair"))
Ind_MT_PNW$XCMGW_COND=ifelse(Ind_MT_PNW$XCMGW_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW', 'XCMGW_0.05']),"Poor",ifelse(Ind_MT_PNW$XCMGW_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW','XCMGW_0.25']),"Good","Fair"))
Ind_MT_PNW$XCDENMID_COND=ifelse(Ind_MT_PNW$xcdenmid_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW', 'XCDENMID_0.05']),"Poor",ifelse(Ind_MT_PNW$xcdenmid_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW','XCDENMID_0.25']),"Good","Fair"))
Ind_MT_PNW$LINCIS_H_COND=ifelse(Ind_MT_PNW$LINCIS_H_CHECK >= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_MT_PNW$LINCIS_H_CHECK<(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW','LINCIS_H_0.75']),"Good","Fair"))
Ind_MT_PNW$PCT_SAFN_COND=ifelse(Ind_MT_PNW$PCT_SAFN_CHECK>= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_MT_PNW$PCT_SAFN_CHECK<(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW','PCT_SAFN_0.75']),"Good","Fair"))
#Still need to create aqumet XEMBED metric
#Ind_MT_PNW$XEMBED_COND=ifelse(Ind_MT_PNW$XEMBED_CHECK >= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW', 'XEMBED_0.95']),"Poor",ifelse(Ind_MT_PNW$XEMBED_CHECK<(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='MT-PNW','XEMBED_0.75']),"Good","Fair"))


# XE-NORTH for all indicators 
Ind_XE_NORTH$XFC_NAT_COND=ifelse(Ind_XE_NORTH$XFC_NAT_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_XE_NORTH$XFC_NAT_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH','XFC_NAT_0.25']),"Good","Fair"))
Ind_XE_NORTH$XCMG_COND=ifelse(Ind_XE_NORTH$XCMG_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH', 'XCMG_0.05']),"Poor",ifelse(Ind_XE_NORTH$XCMG_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH','XCMG_0.25']),"Good","Fair"))
Ind_XE_NORTH$XCMGW_COND=ifelse(Ind_XE_NORTH$XCMGW_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH', 'XCMGW_0.05']),"Poor",ifelse(Ind_XE_NORTH$XCMGW_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH','XCMGW_0.25']),"Good","Fair"))
Ind_XE_NORTH$XCDENMID_COND=ifelse(Ind_XE_NORTH$xcdenmid_CHECK <= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH', 'XCDENMID_0.05']),"Poor",ifelse(Ind_XE_NORTH$xcdenmid_CHECK>(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH','XCDENMID_0.25']),"Good","Fair"))
Ind_XE_NORTH$LINCIS_H_COND=ifelse(Ind_XE_NORTH$LINCIS_H_CHECK >= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_XE_NORTH$LINCIS_H_CHECK<(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH','LINCIS_H_0.75']),"Good","Fair"))
Ind_XE_NORTH$PCT_SAFN_COND=ifelse(Ind_XE_NORTH$PCT_SAFN_CHECK >= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_XE_NORTH$PCT_SAFN_CHECK<(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH','PCT_SAFN_0.75']),"Good","Fair"))
#Still need to create aqumet XEMBED metric
#Ind_XE_NORTH$XEMBED_COND=ifelse(Ind_XE_NORTH$XEMBED_CHECK >= (Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH', 'XEMBED_0.95']),"Poor",ifelse(Ind_XE_NORTH$XEMBED_CHECK<(Thresholds_ECO10 [Thresholds_ECO10$ECO10=='XE-NORTH','XEMBED_0.75']),"Good","Fair"))

IndicatorCond_ECO10=merge(Ind_XE_NORTH,Ind_MT_PNW, all=TRUE)

#Ecoregion lvl III
unique(Indicators$ECO_LVL_3NAME)

Ind_CastFoot=subset(Indicators, ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills')
Ind_NorthBasin=subset(Indicators, ECO_LVL_3NAME=='Northern Basin and Range')
Ind_SierraNV=subset(Indicators, ECO_LVL_3NAME=='Sierra Nevada')

# Eastern Cascades Slopes and Foothills
Ind_CastFoot$XFC_NAT_COND=ifelse(Ind_CastFoot$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_CastFoot$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XFC_NAT_0.25']),"Good","Fair"))
Ind_CastFoot$XCMG_COND=ifelse(Ind_CastFoot$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCMG_0.05']),"Poor",ifelse(Ind_CastFoot$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCMG_0.25']),"Good","Fair"))
Ind_CastFoot$XCMGW_COND=ifelse(Ind_CastFoot$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCMGW_0.05']),"Poor",ifelse(Ind_CastFoot$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCMGW_0.25']),"Good","Fair"))
Ind_CastFoot$XCDENMID_COND=ifelse(Ind_CastFoot$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCDENMID_0.05']),"Poor",ifelse(Ind_CastFoot$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCDENMID_0.25']),"Good","Fair"))
Ind_CastFoot$LINCIS_H_COND=ifelse(Ind_CastFoot$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_CastFoot$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','LINCIS_H_0.75']),"Good","Fair"))
Ind_CastFoot$PCT_SAFN_COND=ifelse(Ind_CastFoot$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_CastFoot$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','PCT_SAFN_0.75']),"Good","Fair"))
#Still need to create aqumet XEMBED metric
#Ind_CastFoot$XEMBED_COND=ifelse(Ind_CastFoot$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XEMBED_0.95']),"Poor",ifelse(Ind_CastFoot$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XEMBED_0.75']),"Good","Fair"))

# Ind_NorthBasin
Ind_NorthBasin$XFC_NAT_COND=ifelse(Ind_NorthBasin$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_NorthBasin$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XFC_NAT_0.25']),"Good","Fair"))
Ind_NorthBasin$XCMG_COND=ifelse(Ind_NorthBasin$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCMG_0.05']),"Poor",ifelse(Ind_NorthBasin$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCMG_0.25']),"Good","Fair"))
Ind_NorthBasin$XCMGW_COND=ifelse(Ind_NorthBasin$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCMGW_0.05']),"Poor",ifelse(Ind_NorthBasin$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCMGW_0.25']),"Good","Fair"))
Ind_NorthBasin$XCDENMID_COND=ifelse(Ind_NorthBasin$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCDENMID_0.05']),"Poor",ifelse(Ind_NorthBasin$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCDENMID_0.25']),"Good","Fair"))
Ind_NorthBasin$LINCIS_H_COND=ifelse(Ind_NorthBasin$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_NorthBasin$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','LINCIS_H_0.75']),"Good","Fair"))
Ind_NorthBasin$PCT_SAFN_COND=ifelse(Ind_NorthBasin$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_NorthBasin$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','PCT_SAFN_0.75']),"Good","Fair"))
#Still need to create aqumet XEMBED metric
#Ind_NorthBasin$XEMBED_COND=ifelse(Ind_NorthBasin$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XEMBED_0.95']),"Poor",ifelse(Ind_NorthBasin$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XEMBED_0.75']),"Good","Fair"))


# Ind_SierraNV
Ind_SierraNV$XFC_NAT_COND=ifelse(Ind_SierraNV$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_SierraNV$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XFC_NAT_0.25']),"Good","Fair"))
Ind_SierraNV$XCMG_COND=ifelse(Ind_SierraNV$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCMG_0.05']),"Poor",ifelse(Ind_SierraNV$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCMG_0.25']),"Good","Fair"))
Ind_SierraNV$XCMGW_COND=ifelse(Ind_SierraNV$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCMGW_0.05']),"Poor",ifelse(Ind_SierraNV$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCMGW_0.25']),"Good","Fair"))
Ind_SierraNV$XCDENMID_COND=ifelse(Ind_SierraNV$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCDENMID_0.05']),"Poor",ifelse(Ind_SierraNV$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCDENMID_0.25']),"Good","Fair"))
Ind_SierraNV$LINCIS_H_COND=ifelse(Ind_SierraNV$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_SierraNV$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','LINCIS_H_0.75']),"Good","Fair"))
Ind_SierraNV$PCT_SAFN_COND=ifelse(Ind_SierraNV$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_SierraNV$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','PCT_SAFN_0.75']),"Good","Fair"))
#Still need to create aqumet XEMBED metric
#Ind_SierraNV$XEMBED_COND=ifelse(Ind_SierraNV$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XEMBED_0.95']),"Poor",ifelse(Ind_SierraNV$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XEMBED_0.75']),"Good","Fair"))


IndicatorCond_ECO_LVL_3NAME=rbind(Ind_SierraNV,Ind_NorthBasin,Ind_CastFoot)


rm(Ind_MT_PNW,Ind_XE_NORTH,Ind_CastFoot,Ind_NorthBasin,Ind_SierraNV)


Freq_ECOIII=cbind(count(IndicatorCond_ECO_LVL_3NAME,var='XFC_NAT_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCMG_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCMGW_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCDENMID_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='LINCIS_H_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='PCT_SAFN_COND'))
            
Freq_ECO10=cbind(count(IndicatorCond_ECO10,var='XFC_NAT_COND'),
            count(IndicatorCond_ECO10,var='XCMG_COND'),
            count(IndicatorCond_ECO10,var='XCMGW_COND'),
            count(IndicatorCond_ECO10,var='XCDENMID_COND'),
            count(IndicatorCond_ECO10,var='LINCIS_H_COND'),
            count(IndicatorCond_ECO10,var='PCT_SAFN_COND'))
            
            
            
            
            



#############################################################################

##############      Aquamet Indicator calculations check      ###############

#############################################################################

#XFC_NAT
###Get the approrpaite fish parameters from NC_DataConsumption
######'BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'
###### Change numeric categories into appropriate percentages, pivot to take the mean or each fish cover category at a site. 
######Then sum to categories of fish cover for each site to have the final results to compared to aquamet's xfc_nat
###### The way this is calculated causes NA's to be treated as blanks that do not count for or against the average. For example if only 1 NA for BOULDR then you would divide boulders by 10 transects instead of 11. See UID 11625 for an example.
fish$ResultsPer=ifelse(fish$RESULT == 1, 0.05,ifelse(fish$RESULT == 2, 0.25,ifelse(fish$RESULT == 3, 0.575,ifelse(fish$RESULT == 4, 0.875,ifelse(fish$RESULT ==0, 0, NA)))))
#fishpvt=cast(fish,'UID+TRANSECT~PARAMETER', value='RESULT')
fishpvt2=cast(fish,'UID~PARAMETER', value='ResultsPer',fun='mean')
fishpvt2$XFC_NAT_CHECK=rowSums(fishpvt2[,c(2,3,4,5,6,7)])

#xcdenmid
MidDensiom = subset(densiom, POINT == "CU"|POINT =="CD"|POINT == "CL"|POINT == "CR")
DensPvt=cast(MidDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
DensPvt$xcdenmid_CHECK=(DensPvt$DENSIOM/17)*100
#Trying to figure out what is going on with UID 11802.
#Dens_Pvt3=cast(MidDens3,'UID+TRANSECT~PARAMETER',value='RESULT',fun=mean)

#LINCIS_H

###############################################
##########   TROUBLESHOOTING START  ###########
##IncBnk2=cast(Incision,'UID~PARAMETER', value='RESULT', fun=mean)
##INCISED=Incision[,c("UID","TRANSECT","PARAMETER","RESULT")]
#IB=aggregate(Incision)
#IB=cast(Incision,'UID~PARAMETER',  value='RESULT', fun=aggregate)
#IBagg=aggregate(Incision,by='PARAMETER',value='RESULT', FUN=mean)

###LINCIS_h REMOVAL OF SIDE CHANNELS TO FIX PLROBLEM.. DIDN'T FIX THE ISSUE... 
#NoSide= subset(Incision, TRANSECT == "A"|TRANSECT == "B"|TRANSECT == "C"|TRANSECT == "D"|TRANSECT == "E"|TRANSECT == "F"|TRANSECT == "G"|TRANSECT == "H"|TRANSECT == "I"|TRANSECT == "J"|TRANSECT == "K")
#NS_IncBnk=cast(NoSide,'UID~PARAMETER', value='RESULT', fun=mean)
#NS_IncBnk$LINCIS_H_Check=log10(NS_InkBnk$INCISED-NS_IncBnk$BANKHT+0.1)
#########    TROUBLESHOOTING END   ############
###############################################

###First the max value of either the side channel or main channel needs to be chosen. To do this I changed all side channels (X-letter) to just the main letter (Sidechannel at A (XA) would be changed to just A).
###Then I subset the data so that missing values would not cause errors. 
###Then I pivoted by the max to chose the max transect value (If XA=5 and A=2 then the XA value would be chosen and the A value removed, note that it is no longer called XA so there would just be 2 A transects for a site with an A sidechannel)
### Then take the average at each site for bank height and incised height. Merge the data back together and then calculate LINCIS_H
Incision$TRANSECT=mapvalues(Incision$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
INCISED=subset(Incision, PARAMETER=="INCISED")
BANKHT=subset(Incision, PARAMETER=="BANKHT")
Inc=cast(INCISED,'UID+TRANSECT~PARAMETER', value='RESULT', fun=max)
Bnk=cast(BANKHT,'UID+TRANSECT~PARAMETER', value='RESULT', fun=max)
xIncht=setNames(aggregate(Inc$INCISED,list(UID=Inc$UID),mean),c("UID","xinc_h_CHECK"))
xBnkht=setNames(aggregate(Bnk$BANKHT,list(UID=Bnk$UID),mean),c("UID","xbnk_h_CHECK"))
IncBnk=merge(xBnkht,xIncht,all=TRUE)
IncBnk$LINCIS_H_CHECK=log10(IncBnk$xinc_h_CHECK-IncBnk$xbnk_h_CHECK+0.1)


#pct_safn
#First you have to take the percent of Sand and percent of fine separately and then add them together... NOPE YOU CAN DO IT EITHER WAY... Just do pct sa and fns works too...
#Correct...For 2013 data
################################
##This is ONLY for 2013 data
####Doing Sand and fines separately: This was a check to figure out how SAFN was calculated and is only needed if you want the individual metrics of Sand and Fines
#Sediment$SA_True=ifelse(Sediment$RESULT == "SA", 1, 0)
#pctsa=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='SA_True',fun='mean')),c("UID","PCT_SA_CHECK"))
#pctsa$PCT_SA_CHECK=pctsa$PCT_SA_CHECK*100

#Sediment$FN_True=ifelse(Sediment$RESULT == "FN", 1, 0)
#pctfn=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='FN_True',fun='mean')),c("UID","PCT_FN_CHECK"))
#pctfn$PCT_FN_CHECK=pctfn$PCT_FN_CHECK*100

#pctSAFN=merge(pctfn,pctsa, All=TRUE)
#pctSAFN$PCT_SAFN_CHECK=pctSAFN$PCT_FN_CHECK+pctSAFN$PCT_SA_CHECK


####Doing sand and fines together
Sediment$SAFN_True=ifelse(Sediment$RESULT == "SA", 1,ifelse(Sediment$RESULT == "FN", 1, 0))
pctsafn=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='SAFN_True',fun='mean')),c("UID","PCT_SAFN_CHECK"))
pctsafn$PCT_SAFN_CHECK=pctsafn$PCT_SAFN_CHECK*100


#Now for 2014 data... 

A_Sed2014=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')
##Checking how many records should be deleted by ordering and just looking at how many bank and na locations there are. 
#B_Sed2014=A_Sed2014[order(A_Sed2014$LOC),]
#View(B_Sed2014[2000:2588,])
#B_Sed2014=A_Sed2014[order(A_Sed2014$SIZE_NUM),]
#View(B_Sed2014[2000:2588,])

#Remove all sediment records that were collected on the "BANK"
C_Sed2014=A_Sed2014[!A_Sed2014$LOC== "BANK", ]
#Can't use na.omit because it omits all records with an NA in ANY field. I only want to remove NAs in the LOC and the SIZE_NUM (sediment result) field. 
E_Sed2014=C_Sed2014[complete.cases(C_Sed2014[,c("LOC","SIZE_NUM")]),]
E_Sed2014$SAFN_True=ifelse(E_Sed2014$SIZE_NUM == "1", 1, 0)
F_Sed2014=setNames(aggregate(E_Sed2014$SAFN_True,list(UID=E_Sed2014$UID),mean), c("UID","PCT_SAFN_CHECK"))
F_Sed2014$PCT_SAFN_CHECK=F_Sed2014$PCT_SAFN_CHECK*100


# Combine the two datasets for PCT_SAFN together so that I don't have multiple files for the same thing
PCT_SAFN_ALL=rbind(pctsafn,F_Sed2014)


#############
##TO check if bed and bank measurements were included I ran this code. This code does not distinguish between bed or bank just runs to get the mean of all 2014 particles, regardless of location. THis shows that Sarah's code is missing the BED/BANK determinations....
#WR_Sed2014$SAFN_True=ifelse(WR_Sed2014$RESULT == "1", 1, 0)
#WR1_Sed2014=cast(WR_Sed2014,'UID~PARAMETER', value='SAFN_True', fun=mean)


###############################################
##########   TROUBLESHOOTING START  ###########
#Wrong
#Sediment$SAFN_True=ifelse(Sediment$RESULT == "SA", 1,ifelse(Sediment$RESULT == "FN", 1, 0))
#pctsafn=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='SAFN_True',fun='mean')),c("UID","PCT_SAFN_CHECK"))
#pctsafn$PCT_SAFN_CHECK=pctsafn$PCT_SAFN_CHECK*100
#Wrong
###
#rawsed=read.csv("N:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\R-processing\\NorCal_Substrate_ACTIVE_INACTIVE_Aquamet_check_14Oct14.csv")
#rawsed$SAFN_True=ifelse(rawsed$RESULT == "SA", 1,ifelse(rawsed$RESULT == "FN", 1, 0))
#Bpctsafn=setNames((cast(rawsed,'UID~SAMPLE_TYPE', value='SAFN_True',fun='mean')),c("UID","PCT_SAFN_CHECK"))
#Bpctsafn$PCT_SAFN_CHECK=Bpctsafn$PCT_SAFN_CHECK*100
#Wrong
########
#Sedtry1$SAFN_True=ifelse(Sedtry1$RESULT == "SA", 1,ifelse(Sedtry1$RESULT == "FN", 1, 0))
#Sedtry12=cast(Sedtry1,'UID+TRANSECT~SAMPLE_TYPE', value='SAFN_True',fun='mean')
#Sedtry123=setNames(aggregate(Sedtry12$CROSSSECW,list(UID=Sedtry12$UID),mean),c("UID","pctsafn"))
#Sedtry12$PCT_SAFN_CHECK=Sedtry12$PCT_SAFN_CHECK*100
#pctSAFN$PCT_SAFN_CHECK=pctSAFN$PCT_FN_CHECK+pctSAFN$PCT_SA_CHECK
#Wrong
###############################
#Sedtry1$SA_True=ifelse(Sedtry1$RESULT == "SA", 1, 0)
#Sedtry12=cast(Sedtry1,'UID+TRANSECT~SAMPLE_TYPE', value='SA_True',fun='mean')
#Sedtry123=setNames(aggregate(Sedtry12$CROSSSECW,list(UID=Sedtry12$UID),mean),c("UID","pctsa"))
#########    TROUBLESHOOTING END   ############
###############################################



#xcmg
RipXCMG$ResultsPer=ifelse(RipXCMG$RESULT == 1, 0.05,ifelse(RipXCMG$RESULT == 2, 0.25,ifelse(RipXCMG$RESULT == 3, 0.575,ifelse(RipXCMG$RESULT == 4, 0.875,ifelse(RipXCMG$RESULT ==0, 0, NA)))))
XCMG_new=setNames(cast(RipXCMG,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',  'VALUE'))
XCMG_new1=setNames(aggregate(VALUE~UID,data=XCMG_new,FUN=mean),list("UID","XCMG_CHECK"))


#xcmgw
#xcmgw=XC+XMW+XGW: However, this is not how aquamet is calculating it, the order of operation would give different results if XC was caluclated and then added to XMW and XMG
#More true to aquamet calculation: XCMG=XCL+XCS+XMW+XGW 
#Need to just calculate it by transect side first then average at an entire site. 
##XC=XCL+XCS (Small Canopy trees (CANSTRE) + Large Canopy trees(CANBTRE))  
##XMW=Understory woody aka UNDWDY
##MGW= ground cover woody GCWDY
RipWW$ResultsPer=ifelse(RipWW$RESULT == 1, 0.05,ifelse(RipWW$RESULT == 2, 0.25,ifelse(RipWW$RESULT == 3, 0.575,ifelse(RipWW$RESULT == 4, 0.875,ifelse(RipWW$RESULT ==0, 0, NA)))))
XCMGW_new=setNames(cast(RipWW,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',	'VALUE'))
XCMGW_new1=setNames(aggregate(VALUE~UID,data=XCMGW_new,FUN=mean),list("UID","XCMGW_CHECK"))

###############################################
##########   TROUBLESHOOTING START  ###########
#Originals
#RipXCMG_Final=cast(RipXCMG,'UID~PARAMETER', value='ResultsPer',fun='mean')
#RipXCMG_Final$XCMG_CHECK=rowSums(RipXCMG_Final[,c(2,3,4,5,6,7)])
#RipXCMGW_Final=cast(RipWW,'UID~PARAMETER', value='ResultsPer',fun='mean')
#RipXCMGW_Final$XCMGW_CHECK=rowSums(RipXCMGW_Final[,c(2,3,4,5)])
#Redo.... agrfdnd
# WW6 has two values that do not match: 
####11626 has an XC (Canopy LG trees and Canopy Small trees metric does not match aquamets) problem
####12440 has a UNDWDY (understory woody does not match Aquamet) problem
#WW5=cast(RipWW,'UID+TRANSECT~PARAMETER', value='ResultsPer',fun='mean')
#WW5$XC_TRY=WW5$CANBTRE+WW5$CANSTRE
#WW6=setNames(aggregate(XC_TRY+GCWDY+UNDWDY~UID,data=WW5,FUN=mean),list("UID","XCMGW_CHECK2"))
#WW5$TW1=WW5$CANBTRE+WW5$CANSTRE+WW5$UNDWDY+WW5$GCWDY
#TW2=setNames(aggregate(TW1~UID,data=WW5,FUN=mean),list("UID","XCMGW_TW2"))
##################################################
#WW7=setNames(aggregate(XC_TRY~UID,data=WW5,FUN=mean),list("UID","XC_CHECK2"))
#WW8=setNames(aggregate(GCWDY~UID,data=WW5,FUN=mean),list("UID","GCW_CHECK2"))
#WW9=setNames(aggregate(UNDWDY~UID,data=WW5,FUN=mean),list("UID","UNDW_CHECK2"))
###############################################################
#RipXCMGW_Final$blurg2=WW7$XC_CHECK2+RipXCMGW_Final$UNDWDY+RipXCMGW_Final$GCWDY
#############################
#WWA=join_all(list(WW7,WW8,WW9),by="UID")
###############################
#RipXCMGW_Final$XC_CHECK2=WW7$XC_CHECK2
#RipXCMGW_Final$WWB3=rowSums(RipXCMG_Final[,c(4,5)])
#RipXCMGW_Final$WWB3=rowSums(RipXCMG_Final[,c(4,5)])
#View(RipXCMGW_Final)
#colnames(RipXCMGW_Final)
####################################
#RipXCMGW_Final$WWB5=RipXCMGW_Final$XC_CHECK2+RipXCMGW_Final$GCWDY+RipXCMGW_Final$UNDWDY
#BLAH=RipXCMGW_Final[,c('UID','WWB5','XCMGW_CHECK','XCMGW_CHECK2')]
#View(BLAH)
###############################
#WWredo=subset(RipWW, PARAMETER=="CANBTRE"|PARAMETER=="CANSTRE")
#WWredo2=cast(WWredo,'UID~SAMPLE_TYPE', value='ResultsPer',fun='mean')
#RipXCMGW_Final$XC=rowSums(RipXCMGW_Final[,c(2,3)])
#RipXCMGW_Final$XCMGW_CHECK2=rowSums(RipXCMGW_Final[,c(4,5,7)])
#############################################
#Didn't Work: WW6=aggregate(WW5,FUN=mean, by=list('UID'))
#View(RipWW[2000:3000,])
#########    TROUBLESHOOTING END   ############
###############################################


#Xembed: Embeddedness: 
#Good to go although this is not yet updated in the IndicatorInventory file, Update file and then delete this note.
XEMBED=setNames(cast(EMBED,'UID~PARAMETER', value='RESULT', fun='mean'), list('UID','XEMBED_CHECK'))



#W1_HALL
#Be careful, the documentation says to use P=0.667, but the aquamet code says 0.6667, if there ends up being a lot of P's in the data this makes a difference!!! 
Human_Influ$Weights=ifelse(Human_Influ$RESULT == "B", 1.5,ifelse(Human_Influ$RESULT == "C", 1.0, ifelse(Human_Influ$RESULT == "P", 0.6667, 0))) 
W1_HALL=cast(Human_Influ,'UID~PARAMETER', value='Weights',fun='mean')
W1_HALL$W1_HALL_CHECK=rowSums(W1_HALL[,c(2:12)])       


###xcdenbk: needed for QR1
BnkDensiom = subset(densiom, POINT == "LF"|POINT =="RT")
BnkDensPvt=cast(BnkDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
BnkDensPvt$xcdenbk_CHECK=(BnkDensPvt$DENSIOM/17)*100

#QR1
#QR1= {(QRVeg1) (QRVeg2) (QRDIST1)} ^ 0.333; 
#if XCMGW <=2.00, then QRVeg1=.1+(.9 (XCMGW/2.00))
#if XCMGW >2.00 then QRVeg1=1; 
#and QRVeg2=0.1 + [0.9(XCDENBK/100)]; 
#QRDIST1=1/(1+W1_Hall). 

QR1=join_all(list(XCMGW_new1,BnkDensPvt,W1_HALL), by='UID')
QR1=setNames(subset(QR1[,c('UID','XCMGW_CHECK','xcdenbk_CHECK','W1_HALL_CHECK')]),list('UID','XCMGW','xcdenbk','W1_HALL'))

#QRVeg1
QR1$QRveg1=ifelse(QR1$XCMGW<=2.00,.1+(.9*(QR1$XCMGW/2)),1)
#QRVeg2
QR1$QRVeg2=0.1 + (0.9*(QR1$xcdenbk/100))
#QRDIST1
QR1$QRDIST1=1/(1+QR1$W1_HALL)
#Final QR1 calculation
QR1$QR1_CHECK=(QR1$QRveg1*QR1$QRVeg2*QR1$QRDIST1)^0.333


#To get all calculated values together... Although some tables still have the metrics included.
AquametCheckJoin=join_all(list(fishpvt2,DensPvt,XCMGW_new1,XCMG_new1,IncBnk,PCT_SAFN_ALL,QR1),by="UID")
#To remove all of the metrics and only get the indicators subset by UID and all those columns ending in "CHECK". Hmm..not really sure what the $ is doing here, the code works without it, but all the examples I've looked at keep the $ so I kept it too... 
AquametCheck=AquametCheckJoin[,c("UID",grep("CHECK$", colnames(AquametCheckJoin),value=TRUE))]
#write.csv(AquametCheck,"C:\\Users\\Nicole\\Desktop\\AquametCheck2.csv")
#Remove all other data files as they are no longer needed
rm(densiom,RipXCMG,XCMG_new,XCMG_new1,RipWW,XCMGW_new,XCMGW_new1,AquametCheckJoin,fish,fishpvt2,MidDensiom,DensPvt,Incision,INCISED,BANKHT,Inc,Bnk,xIncht,xBnkht,IncBnk,Sediment,pctsafn,Sed2014,A_Sed2014,C_Sed2014,E_Sed2014,F_Sed2014,PCT_SAFN_ALL)

#AquametCheck10=join_all(list(fishpvt2,DensPvt,RipXCMGW_Final,RipXCMG_Final,IncBnk,PCT_SAFN_ALL),by="UID")
#AquametCheck11=AquametCheck10[,c("UID",grep("CHECK$", colnames(AquametCheck10),value=TRUE))]
#write.csv(AquametCheck11,"C:\\Users\\Nicole\\Desktop\\AquametCheck11.csv")






#############################################################################

##############             EPA EMAP and WRSA data             ###############

#############################################################################

#Read in EMAP and NRSA data
EMAP=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\EMAP.csv")
NRSA=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\NRSA.csv")

#Change all column names to uppercase because R is case sensitive
names(EMAP) = toupper(names(EMAP))
names(NRSA) = toupper(names(NRSA))

#Make a column to distinguish between which project the data belongs to
NRSA$PROJECT='NRSA'
EMAP$PROJECT='EMAP'

#Transpose data so that it can be merged. 
T_EMAP=t(EMAP)
T_NRSA=t(NRSA)

#Merge the two files
#Merged=merge(T_EMAP,T_NRSA)
Merged2=merge(T_EMAP,T_NRSA, by="row.names", all=TRUE)
#final1=t(Merged)
final1=t(Merged2)

#Yes! This works! To keep column 1 from the transposed data as column headings
Final2=setNames(data.frame(t(Merged2[,-1])), Merged2[,1])

#CLEAN UP DATA LIST AND WRITE OUT CSV!!!!!!!
rm(EMAP,NRSA,T_NRSA,T_EMAP,Merged2,final1)
#Write out because I cannot figure out how do deal with this in R....
#write.csv(Final2, "\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\Mass_Combination_NRSA_EMAP.csv")

#################################################################################
#############
#Sooo.. I needed Excel to clean up and do a few things, but now coming back to R
# to set thresholds for p-hab indicators
#############
#################################################################################

combined=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\Comb_21Oct2014_Rinput_DoNotAlter.csv")

#Use to get wadeable or boatable only, if this line isn't run both boatable and wadeable will be used. Change REALM== "" to specify 
combined=subset(combined, REALM == "WADEABLE")

#To subset for reference to use on RIPARIAN indicators.
#First subset the data to only include sites with R or S designations
RIP_RS_combined=subset(combined, RST_FRIP_AND_RMD_PHAB == "R"|RST_FRIP_AND_RMD_PHAB == "S")
#Now I need to remove duplicate sites, but choose the one with the most recent year. 
###So I order by if it was revisited, the site code, and then the year.  Check this with the view
RIP_RS_reorder=RIP_RS_combined[order(RIP_RS_combined$REVISITED_OVERLAP,RIP_RS_combined$DUPLICATE_ID, RIP_RS_combined$YEAR, decreasing=FALSE),]
#View(RIP_RS_reorder[1250:2041,])
###Now I remove the duplicate that is listed second.
RIP_RS_minusDup= RIP_RS_reorder[!duplicated(RIP_RS_reorder$DUPLICATE_ID),]
#View(RIP_RS_minusDup[1000:1999,])
RIP_RS_final=RIP_RS_minusDup[order(RIP_RS_minusDup$REVISITED_OVERLAP, decreasing=TRUE),]

#To subset for reference to use on SEDIMENT and INSTREAM indicators.
#First subset the data to only include sites with R or S designations
SED_RS_combined=subset(combined, RST_FSED_AND_RMD_PHAB == "R"|RST_FSED_AND_RMD_PHAB == "S")
#Now I need to remove duplicate sites, but choose the one with the most recent year. 
###So I order by if it was revisited, the site code, and then the year.  Check this with the view
SED_RS_reorder=SED_RS_combined[order(SED_RS_combined$REVISITED_OVERLAP,SED_RS_combined$DUPLICATE_ID, SED_RS_combined$YEAR, decreasing=FALSE),]
#View(SED_RS_reorder[1250:2041,])
###Now I remove the duplicate that is listed second.
SED_RS_minusDup= SED_RS_reorder[!duplicated(SED_RS_reorder$DUPLICATE_ID),]
#View(SED_RS_minusDup[1000:1999,])
SED_RS_final=SED_RS_minusDup[order(SED_RS_minusDup$REVISITED_OVERLAP, decreasing=TRUE),]


#To get Sample sizes used to set thresholds
##This does not count NAs (good) and was manually checked to determine if it was counting the correct information.
###FOR eco10
pvt1=aggregate(XCDENMID~ECO10,data=RIP_RS_final,FUN=length)
pvt2=aggregate(XCMG~ECO10,data=RIP_RS_final,FUN=length)
pvt3=aggregate(XCMGW~ECO10,data=RIP_RS_final,FUN=length)
pvt4=aggregate(PCT_SAFN~ECO10,data=SED_RS_final,FUN=length)
pvt5=aggregate(DPCT_SF~ECO10,data=SED_RS_final,FUN=length)
pvt6=aggregate(XFC_NAT~ECO10,data=SED_RS_final,FUN=length)
pvt7=aggregate(LINCIS_H~ECO10,data=SED_RS_final,FUN=length)
pvt8=aggregate(XEMBED~ECO10,data=SED_RS_final,FUN=length)
ECO10_SampSizes=join_all(list(pvt1,pvt2,pvt3,pvt4,pvt5, pvt6, pvt7, pvt8),by="ECO10")


#NorCal specific hybrid ecoregions
ECO10_SampSizes_NC = subset(ECO10_SampSizes, ECO10 == "XE-SOUTH"|ECO10 == "XE-NORTH"|ECO10 == "MT-PNW")

##I wish this would work so that the code was more adaptable, but it won't work with any additional ecoregions... why? 
#Desired_eco=list("XE-SOUTH","XE-NORTH")
#ECOtry = subset(ECO10_SampSizes, ECO10_SampSizes$ECO10 == Desired_eco)

###For ECO_LVL_3NAME 
pvt11=aggregate(XCDENMID~ECO_LVL_3NAME,data=RIP_RS_final,FUN=length)
pvt12=aggregate(XCMG~ECO_LVL_3NAME,data=RIP_RS_final,FUN=length)
pvt13=aggregate(XCMGW~ECO_LVL_3NAME,data=RIP_RS_final,FUN=length)
pvt14=aggregate(PCT_SAFN~ECO_LVL_3NAME,data=SED_RS_final,FUN=length)
pvt15=aggregate(DPCT_SF~ECO_LVL_3NAME,data=SED_RS_final,FUN=length)
pvt16=aggregate(XFC_NAT~ECO_LVL_3NAME,data=SED_RS_final,FUN=length)
pvt17=aggregate(LINCIS_H~ECO_LVL_3NAME,data=SED_RS_final,FUN=length)
pvt18=aggregate(XEMBED~ECO_LVL_3NAME,data=SED_RS_final,FUN=length)
ECOIII_SampSizes=join_all(list(pvt11,pvt12,pvt13,pvt14,pvt15, pvt16, pvt17, pvt18),by="ECO_LVL_3NAME")

#NorCal specific hybrid ecoregions
ECOIII_SampSizes_NC = subset(ECOIII_SampSizes, ECO_LVL_3NAME == "Central Basin and Range"|ECO_LVL_3NAME == "Eastern Cascades Slopes and Foothills"|ECO_LVL_3NAME == "Northern Basin and Range"|ECO_LVL_3NAME == "Sierra Nevada")

#Remove all working objects, leave files that will be used later in code, and open the final sample sizes. 
rm(RIP_RS_combined,RIP_RS_reorder,RIP_RS_minusDup,SED_RS_combined,SED_RS_reorder,SED_RS_minusDup,pvt1,pvt2,pvt3,pvt4,pvt5, pvt6, pvt7, pvt8,pvt11,pvt12,pvt13,pvt14,pvt15, pvt16, pvt17, pvt18)
View(ECO10_SampSizes)
View(ECO10_SampSizes_NC)
View(ECOIII_SampSizes)
View(ECOIII_SampSizes_NC)

###############################
# IN EXCEL... CHANGE .95 TO .75 OR .25 AND .05 AND CHANGE THE ECOREGION FOR EACH
#PROBLEM!!!!! Need excel 2010 to do a real percentile!
#=Percentile.exc
###############################

# Use riparian reference sites for: XCDENMID, XCMG, XCMGW, 

###

#Wish I could get this or the next line to work, but it won't repeat the ecoregion name and so I have to do it individually....
#RIP_Thres_XCDENMID = aggregate(Wade_RIP_Final$XCDENMID, by = list(Wade_RIP_Final$ECO10), FUN = function(x) quantile(x, probs = c(0.95,0.75,0.25,0.05),na.rm=TRUE))
#T4=aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = function(x) quantile(x,probs=0.95,na.rm=TRUE))
#trial2 = aggregate(Wade_RIP_Final$XCDENMID, by = list(Wade_RIP_Final$ECO10), FUN = quantile, probs= c(0.05,0.25,0.75,0.95),na.rm=TRUE)

#XCDENMID
T1=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XCDENMID_0.05"))
T2=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XCDENMID_0.25"))
T5=join_all(list(T1,T2), by="ECO10")
#XCMG
T1=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XCMG_0.05"))
T2=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XCMG_0.25"))
T6=join_all(list(T1,T2), by="ECO10")
#XCMGW
T1=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XCMGW_0.05"))
T2=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XCMGW_0.25"))
T7=join_all(list(T1,T2), by="ECO10")
##Combine all
RIP_THRESHOLDS_ECO10=join_all(list(T5,T6,T7),by="ECO10")

###################################################################################
###################################################################################


# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#SECONDARY INDICATORS: CVDPTH, LRP100, LDVRP100, C1WM100

#PCT_SAFN
T3=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","PCT_SAFN_0.75"))
T4=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","PCT_SAFN_0.95"))
T5=join_all(list(T3,T4), by="ECO10")
#DPCT_SF
#T1=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","DPCT_SF_0.05"))
#T2=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","DPCT_SF_0.25"))
#T3=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","DPCT_SF_0.75"))
#T4=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","DPCT_SF_0.95"))
#T6=join_all(list(T1,T2,T3,T4), by="ECO10")
#XFC_NAT
T1=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XFC_NAT_0.05"))
T2=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XFC_NAT_0.25"))
T7=join_all(list(T1,T2), by="ECO10")
#LINCIS_H
T3=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","LINCIS_H_0.75"))
T4=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","LINCIS_H_0.95"))
T8=join_all(list(T3,T4), by="ECO10")
#XEMBED
T3=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","XEMBED_0.75"))
T4=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","XEMBED_0.95"))
T9=join_all(list(T3,T4), by="ECO10")
##Combine all
SED_THRESHOLDS_ECO10=join_all(list(T5,T7,T8,T9),by="ECO10")


#Make one file for ECO 10 thresholds and remove unwanted working files

Thresholds_ECO10=merge(SED_THRESHOLDS_ECO10,RIP_THRESHOLDS_ECO10, all=TRUE)
rm(T1,T2,T3,T4,T5,T6,T7,T8,T9,RIP_THRESHOLDS_ECO10,SED_THRESHOLDS_ECO10)
# WHAT TO DO WITH: W1_HALL, QR1
# W1_HALL has set thresholds see \\share1.bluezone.usu.edu\miller\buglab\Research Projects\BLM_WRSA_Stream_Surveys\Analysis\Indicator_Scoping\NorCalIndicatorInventory24Oct2014 (Date may vary)

########################################################################################
###########################################################################################
########################################################################################
## For Ecoregion level III


#XCDENMID
T11=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.05"))
T12=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.25"))
T15=join_all(list(T11,T12), by="ECO_LVL_3NAME")
#XCMG
T11=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.05"))
T12=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.25"))
T16=join_all(list(T11,T12), by="ECO_LVL_3NAME")
#XCMGW
T11=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.05"))
T12=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.25"))
T17=join_all(list(T11,T12), by="ECO_LVL_3NAME")
##Combine all
RIP_THRESHOLDS_lvlIII=join_all(list(T15,T16,T17),by="ECO_LVL_3NAME")

###################################################################################
###################################################################################


# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#SECONDARY INDICATORS: CVDPTH, LRP100, LDVRP100, C1WM100

#PCT_SAFN

T13=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.75"))
T14=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.95"))
T15=join_all(list(T13,T14), by="ECO_LVL_3NAME")
#DPCT_SF 
#To add this back in you need to add to Join all line at bottom of indicator list and to the line joining all files. 
#T11=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.05"))
#T12=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.25"))
#T13=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.75"))
#T14=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.95"))
#T16=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
#XFC_NAT
T11=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.05"))
T12=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.25"))
T17=join_all(list(T11,T12), by="ECO_LVL_3NAME")
#LINCIS_H
T13=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.75"))
T14=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.95"))
T18=join_all(list(T13,T14), by="ECO_LVL_3NAME")
#XEMBED
T13=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.75"))
T14=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.95"))
T19=join_all(list(T13,T14), by="ECO_LVL_3NAME")
##Combine all
SED_THRESHOLDS_lvlIII=join_all(list(T15,T17,T18,T19),by="ECO_LVL_3NAME")

#Make one file for ECO 10 thresholds and remove unwanted working files
Thresholds_lvlIII=merge(RIP_THRESHOLDS_lvlIII,SED_THRESHOLDS_lvlIII, all=TRUE)
rm(T11,T12,T13,T14,T15,T16,T17,T18,T19,RIP_THRESHOLDS_lvlIII,SED_THRESHOLDS_lvlIII,RIP_RS_final,SED_RS_final)

#Make threshold files for NorCal specific ecoregions for ease of looking
Thresholds_lvlIII_NC=subset(Thresholds_lvlIII, ECO_LVL_3NAME == "Central Basin and Range"|ECO_LVL_3NAME == "Eastern Cascades Slopes and Foothills"|ECO_LVL_3NAME == "Northern Basin and Range"|ECO_LVL_3NAME == "Sierra Nevada")
Thresholds_ECO10_NC=subset(Thresholds_ECO10, ECO10 == "XE-SOUTH"|ECO10 == "XE-NORTH"|ECO10 == "MT-PNW")



