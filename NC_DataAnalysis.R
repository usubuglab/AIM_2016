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
#All raw model results
NorCalBugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\ALL_BugModel_Results.csv")

#############################################################################
#######################     NV MODELS     ###################################
#############################################################################
#Scores below the impaired threshold imply degradation. 
#Scores greater than or equal to the equivalence threshold imply reference condition. 
#A value between the two thresholds indicates that the score cannot be definitively determined to fall within or below reference condition. (Techinically this does not mean "fair")
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




#### Checking to make sure results make sense 
Freq1=cbind(count(NorCalBugs,var='CSCI_Hybrid_MidCond'),count(NorCalBugs,var='CSCI_MMI_MidCond'),count(NorCalBugs,var='CSCI_OE_MidCond'),count(NorCalBugs,var='NV_OE0_Cond'),count(NorCalBugs,var='NV_OE5_Cond'),count(NorCalBugs,var='NV_MMI_Cond'),count(NorCalBugs,var='CSCI_OE_UpperCond'),count(NorCalBugs,var='CSCI_Hybrid_UpperCond'),count(NorCalBugs,var='CSCI_MMI_UpperCond'))

NorCalCHECK_Bugs=subset(NorCalBugs, SiteCode=='AR-SS-8066'|SiteCode=='SU-SS-8311'|SiteCode=='AR-LS-8018'|SiteCode=='AR-SS-8017'|SiteCode=='HC-SS-8456'|SiteCode=='HC-SS-8475')
NorCalCHECK_Bugs$NicoleJudge=c('G','P','P','P','G','G')
  



####Plotting OE and MMI to compare
#Must get all scores on same scale so I've divided by the reference score Average
#CSCI MMI and NV OE Pc=0 both have an average of 1.00 so no adjustment is needed. 
NorCalBugs$NV_MMI_2=NorCalBugs$NV_MMI/56.04737
NorCalBugs$NV_OE5_2=NorCalBugs$NV_OE5/1.06
NorCalBugs$CSCI_OE_2=NorCalBugs$CSCI_OE/1.021478
NorCalBugs$CSCI_Hybrid_2=NorCalBugs$CSCI_Hybrid/1.010739

par(mfrow=c(1,2))

############  NV Models   ############
#Plot NV O/E (Pc=0) to NV MMI
plot(NorCalBugs$NV_OE0,NorCalBugs$NV_MMI_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$NV_MMI_2~NorCalBugs$NV_OE0)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$NV_MMI_2,NorCalBugs$NV_OE0))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)
#plot NV O/E (0.5) and NV MMI 
plot(NorCalBugs$NV_OE5_2,NorCalBugs$NV_MMI_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$NV_MMI_2~NorCalBugs$NV_OE5_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$NV_MMI_2,NorCalBugs$NV_OE5_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)
#Plot NV O/E (Pc=0) and NV O/E (0.5)
plot(NorCalBugs$NV_OE5_2,NorCalBugs$NV_OE0, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$NV_OE0~NorCalBugs$NV_OE5_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$NV_OE0,NorCalBugs$NV_OE5_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)


############  CSCI Models   ############
# Plot CSCI OE and CSCI MMI
plot(NorCalBugs$CSCI_OE_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.1), xlim=c(0,1.1),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$CSCI_OE_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$CSCI_OE_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)
# Plot CSCI OE and CSCI MMI
plot(NorCalBugs$CSCI_OE_2,NorCalBugs$CSCI_Hybrid_2, ylim=c(0,1.1), xlim=c(0,1.1),abline((lm(NorCalBugs$CSCI_Hybrid_2~NorCalBugs$CSCI_OE_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_Hybrid_2,NorCalBugs$CSCI_OE_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)
# Plot CSCI Hybrid and CSCI MMI
plot(NorCalBugs$CSCI_Hybrid_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.1), xlim=c(0,1.1),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$CSCI_Hybrid_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$CSCI_Hybrid_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)



############  NV and CSCI Models   ############
#plot NV O/E (0.5) and CSCI O/E (also a 0.5 prob of capture)
plot(NorCalBugs$NV_OE5_2,NorCalBugs$CSCI_OE_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_OE_2~NorCalBugs$NV_OE5_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_OE_2,NorCalBugs$NV_OE5_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

#plot NV O/E (0) and CSCI O/E (0.5 prob of capture)
plot(NorCalBugs$NV_OE0,NorCalBugs$CSCI_OE_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_OE_2~NorCalBugs$NV_OE0)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_OE_2,NorCalBugs$NV_OE0))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

#plot NV and CSCI MMI
plot(NorCalBugs$NV_MMI_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$NV_MMI_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$NV_MMI_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)





################# Confusions Matrix of results ##############################
# Could also use table() but this will not label rows vs columns as xtab does. 
#NV only
NorCalBugs$NV_MMI_Cond_2=ifelse(NorCalBugs$NV_MMI_Cond=="Reference","Good",ifelse(NorCalBugs$NV_MMI_Cond=="Impaired","Poor","Fair"))
xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$NV_MMI_Cond)

xtabs(~NorCalBugs$NV_OE0_Cond+NorCalBugs$NV_MMI_Cond_2)
xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$NV_OE0_Cond)

#CSCI only
xtabs(~NorCalBugs$CSCI_OE_MidCond+NorCalBugs$CSCI_MMI_MidCond)

xtabs(~NorCalBugs$CSCI_OE_MidCond+NorCalBugs$CSCI_Hybrid_MidCond)
xtabs(~NorCalBugs$CSCI_Hybrid_MidCond+NorCalBugs$CSCI_MMI_MidCond)

#CSCI to NV
xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$CSCI_OE_MidCond)
xtabs(~NorCalBugs$NV_MMI_Cond_2+NorCalBugs$CSCI_MMI_MidCond)

xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$CSCI_MMI_MidCond)
xtabs(~NorCalBugs$NV_MMI_Cond_2+NorCalBugs$CSCI_OE_MidCond)

xtabs(~NorCalBugs$NV_OE0_Cond+NorCalBugs$CSCI_OE_MidCond)



# If NV=CA in concordance, if in dicordance with Good and Fair them is gets Bad_Discord, but if it is just one step of discordance it is Med_Discord
# Checked with the count function to make sure the values were the same as the xtab tables above. 
NorCalBugs$MMI_ModAgreement=ifelse(NorCalBugs$NV_MMI_Cond_2==NorCalBugs$CSCI_MMI_MidCond,"Concord",
       ifelse(NorCalBugs$NV_MMI_Cond_2=="Good" & NorCalBugs$CSCI_MMI_MidCond=="Poor"|NorCalBugs$NV_MMI_Cond_2=="Poor" & NorCalBugs$CSCI_MMI_MidCond=="Good",'Bad_Discord',
              ifelse(NorCalBugs$NV_MMI_Cond_2=="Fair" & NorCalBugs$CSCI_MMI_MidCond=="Poor"|NorCalBugs$NV_MMI_Cond_2=="Fair" & NorCalBugs$CSCI_MMI_MidCond=="Good"|NorCalBugs$NV_MMI_Cond_2=="Poor" & NorCalBugs$CSCI_MMI_MidCond=="Fair"|NorCalBugs$NV_MMI_Cond_2=="Good" & NorCalBugs$CSCI_MMI_MidCond=="Fair",
                     "Med_Discord",0)))

NorCalBugs$OE5_ModAgreement=ifelse(NorCalBugs$NV_OE5_Cond==NorCalBugs$CSCI_OE_MidCond,"Concord",
                                   ifelse(NorCalBugs$NV_OE5_Cond=="Good" & NorCalBugs$CSCI_OE_MidCond=="Poor"|NorCalBugs$NV_OE5_Cond=="Poor" & NorCalBugs$CSCI_OE_MidCond=="Good",'Bad_Discord',
                                          ifelse(NorCalBugs$NV_OE5_Cond=="Fair" & NorCalBugs$CSCI_OE_MidCond=="Poor"|NorCalBugs$NV_OE5_Cond=="Fair" & NorCalBugs$CSCI_OE_MidCond=="Good"|NorCalBugs$NV_OE5_Cond=="Poor" & NorCalBugs$CSCI_OE_MidCond=="Fair"|NorCalBugs$NV_OE5_Cond=="Good" & NorCalBugs$CSCI_OE_MidCond=="Fair",
                                                 "Med_Discord",0)))





#Second EXAMPLE TEXT IN GRAPH
#TEXT IN GRAPH
#plot(Weight~Length)
#lines(m1,col="red")
#text(locator(1), " Y=9e-07x^3.4198   R Squared = .92")



#############################################################################

##############   Physical Habitat condition determinations    ###############

#############################################################################

###### BANKS stability and cover
IndicatorCheck$BnkStab_BLM_COND=ifelse(IndicatorCheck$BnkStability_BLM_CHECK>0.60,'Good',ifelse(IndicatorCheck$BnkStability_BLM_CHECK<0.40,'Poor','Fair'))

IndicatorCheck$BnkCover_BLM_COND=ifelse(IndicatorCheck$BnkCover_BLM_CHECK>0.60,'Good',ifelse(IndicatorCheck$BnkCover_BLM_CHECK<0.40,'Poor','Fair'))


#First I need to combine Ecoregions to the sampled sites. 
#Read in ecoregion to sample sitecode
NorCalSites_Ecoregions=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\UID_SiteID_Ecoregions.csv")

#Because UIDs get turned into different values when exported as a csv I was unable to use the merge function because UIDs did not match exactly. Be careful with using this method in the future... 
t1=NorCalSites_Ecoregions[order(NorCalSites_Ecoregions$UID, decreasing=FALSE),]
t2=IndicatorCheck[order(IndicatorCheck$UID, decreasing=FALSE),]
t3=cbind(t1,t2)
t3=t3[,-1] 
Indicators=t3[,c(4,1,2,3,5:23)]
rm(t1,t2,t3,NorCalSites_Ecoregions)


#Subset all by ecoregions... got to be a better way...
################################################################

#Level III ecoregions: Applying Thresholds

################################################################
unique(Indicators$ECO_LVL_3NAME)

Ind_CastFoot=subset(Indicators, ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills')
Ind_NorthBasin=subset(Indicators, ECO_LVL_3NAME=='Northern Basin and Range')
Ind_SierraNV=subset(Indicators, ECO_LVL_3NAME=='Sierra Nevada')

#Apply pH thresholds
Ind_CastFoot$PH1=ifelse(Ind_CastFoot$PH_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'PH_0.05'])
                        |Ind_CastFoot$PH_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'PH_0.95']),"Poor",
                        ifelse(Ind_CastFoot$PH_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','PH_0.25']) &
                                 Ind_CastFoot$PH_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','PH_0.75']),"Good","Fair"))

Ind_NorthBasin$PH1=ifelse(Ind_NorthBasin$PH_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'PH_0.05'])
                          |Ind_NorthBasin$PH_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'PH_0.95']),"Poor",
                          ifelse(Ind_NorthBasin$PH_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','PH_0.25']) &
                                   Ind_NorthBasin$PH_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','PH_0.75']),"Good","Fair"))

Ind_SierraNV$PH1=ifelse(Ind_SierraNV$PH_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'PH_0.05'])
                        |Ind_SierraNV$PH_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'PH_0.95']),"Poor",
                        ifelse(Ind_SierraNV$PH_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','PH_0.25']) &
                                 Ind_SierraNV$PH_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','PH_0.75']),"Good","Fair"))

# Eastern Cascades Slopes and Foothills
Ind_CastFoot$XFC_NAT_COND=ifelse(Ind_CastFoot$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_CastFoot$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XFC_NAT_0.25']),"Good","Fair"))
Ind_CastFoot$XCMG_COND=ifelse(Ind_CastFoot$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCMG_0.05']),"Poor",ifelse(Ind_CastFoot$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCMG_0.25']),"Good","Fair"))
Ind_CastFoot$XCMGW_COND=ifelse(Ind_CastFoot$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCMGW_0.05']),"Poor",ifelse(Ind_CastFoot$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCMGW_0.25']),"Good","Fair"))
Ind_CastFoot$XCDENMID_COND=ifelse(Ind_CastFoot$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCDENMID_0.05']),"Poor",ifelse(Ind_CastFoot$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCDENMID_0.25']),"Good","Fair"))
Ind_CastFoot$LINCIS_H_COND=ifelse(Ind_CastFoot$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_CastFoot$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','LINCIS_H_0.75']),"Good","Fair"))
Ind_CastFoot$PCT_SAFN_COND=ifelse(Ind_CastFoot$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_CastFoot$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','PCT_SAFN_0.75']),"Good","Fair"))
Ind_CastFoot$XEMBED_COND=ifelse(Ind_CastFoot$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XEMBED_0.95']),"Poor",ifelse(Ind_CastFoot$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XEMBED_0.75']),"Good","Fair"))

# Ind_NorthBasin
Ind_NorthBasin$XFC_NAT_COND=ifelse(Ind_NorthBasin$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_NorthBasin$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XFC_NAT_0.25']),"Good","Fair"))
Ind_NorthBasin$XCMG_COND=ifelse(Ind_NorthBasin$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCMG_0.05']),"Poor",ifelse(Ind_NorthBasin$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCMG_0.25']),"Good","Fair"))
Ind_NorthBasin$XCMGW_COND=ifelse(Ind_NorthBasin$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCMGW_0.05']),"Poor",ifelse(Ind_NorthBasin$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCMGW_0.25']),"Good","Fair"))
Ind_NorthBasin$XCDENMID_COND=ifelse(Ind_NorthBasin$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCDENMID_0.05']),"Poor",ifelse(Ind_NorthBasin$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCDENMID_0.25']),"Good","Fair"))
Ind_NorthBasin$LINCIS_H_COND=ifelse(Ind_NorthBasin$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_NorthBasin$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','LINCIS_H_0.75']),"Good","Fair"))
Ind_NorthBasin$PCT_SAFN_COND=ifelse(Ind_NorthBasin$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_NorthBasin$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','PCT_SAFN_0.75']),"Good","Fair"))
Ind_NorthBasin$XEMBED_COND=ifelse(Ind_NorthBasin$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XEMBED_0.95']),"Poor",ifelse(Ind_NorthBasin$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XEMBED_0.75']),"Good","Fair"))


# Ind_SierraNV
Ind_SierraNV$XFC_NAT_COND=ifelse(Ind_SierraNV$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XFC_NAT_0.05']),"Poor",ifelse(Ind_SierraNV$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XFC_NAT_0.25']),"Good","Fair"))
Ind_SierraNV$XCMG_COND=ifelse(Ind_SierraNV$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCMG_0.05']),"Poor",ifelse(Ind_SierraNV$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCMG_0.25']),"Good","Fair"))
Ind_SierraNV$XCMGW_COND=ifelse(Ind_SierraNV$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCMGW_0.05']),"Poor",ifelse(Ind_SierraNV$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCMGW_0.25']),"Good","Fair"))
Ind_SierraNV$XCDENMID_COND=ifelse(Ind_SierraNV$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCDENMID_0.05']),"Poor",ifelse(Ind_SierraNV$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCDENMID_0.25']),"Good","Fair"))
Ind_SierraNV$LINCIS_H_COND=ifelse(Ind_SierraNV$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'LINCIS_H_0.95']),"Poor",ifelse(Ind_SierraNV$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','LINCIS_H_0.75']),"Good","Fair"))
Ind_SierraNV$PCT_SAFN_COND=ifelse(Ind_SierraNV$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'PCT_SAFN_0.95']),"Poor",ifelse(Ind_SierraNV$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','PCT_SAFN_0.75']),"Good","Fair"))
Ind_SierraNV$XEMBED_COND=ifelse(Ind_SierraNV$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XEMBED_0.95']),"Poor",ifelse(Ind_SierraNV$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XEMBED_0.75']),"Good","Fair"))


IndicatorCond_ECO_LVL_3NAME=rbind(Ind_SierraNV,Ind_NorthBasin,Ind_CastFoot)

rm(Ind_CastFoot,Ind_NorthBasin,Ind_SierraNV)


#L_XCMGW thresholds
IndicatorCond_ECO_LVL_3NAME$L_XCMGW_COND=ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Sierra Nevada'|IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK<(-0.492),"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK>=-0.261, "Good","Fair")), 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Northern Basin and Range',
                                                       ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK<(-0.444),"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK>=-0.240, "Good","Fair")),"NA"))

#W1_HALL
#When using EMAP-West 
IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_COND=ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Sierra Nevada'|IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK>0.95,"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK<=0.35, "Good","Fair")), 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Northern Basin and Range',
                                                       ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK>0.9,"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK<=0.7, "Good","Fair")),"NA"))
#When using NRSA 
#If I am going to use this I MUST change the the way it is calculated
IndicatorCond_ECO_LVL_3NAME$NRSA_W1_HALL_COND=ifelse(IndicatorCond_ECO_LVL_3NAME$NRSA_W1_HALL_CHECK<0.33,"Good",ifelse(IndicatorCond_ECO_LVL_3NAME$NRSA_W1_HALL_CHECK>=1.5,"Poor","Fair"))


#QR1: Not sure yet......


#How many G, F, P values for each indicator... Not all indicators listed 
Freq_ECOIII=cbind(count(IndicatorCond_ECO_LVL_3NAME,var='XFC_NAT_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCMG_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCMGW_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='L_XCMGW_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XEMBED_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='EMAP_W1_HALL_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='NRSA_W1_HALL_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCDENMID_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='LINCIS_H_COND'),
             count(IndicatorCond_ECO_LVL_3NAME,var='PCT_SAFN_COND'))
            




################################################################

#ECO10: Applying Thresholds

################################################################
#Get a list of the indicators in the file to use for Subsetting
#pH is not listed here, see above for how to apply thresholds. 
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
rm(Ind_MT_PNW,Ind_XE_NORTH)

Freq_ECO10=cbind(count(IndicatorCond_ECO10,var='XFC_NAT_COND'),
            count(IndicatorCond_ECO10,var='XCMG_COND'),
            count(IndicatorCond_ECO10,var='XCMGW_COND'),
            count(IndicatorCond_ECO10,var='XCDENMID_COND'),
            count(IndicatorCond_ECO10,var='LINCIS_H_COND'),
            count(IndicatorCond_ECO10,var='PCT_SAFN_COND'))
           

