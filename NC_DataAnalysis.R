#How to order data
New table name=data[order(data$column),]

#R studio only allows the first 1000 records and 100 fields. With the below code I can choose which records I want. This is an example of viewing rown 3500-4500
#View(data[records or rows, fields or columns])
View(data[3500:4500,])

# Quick notes:
#### ! means negate
#### | means OR


##############    Water quality condition determinations      ###############
#############################################################################

# First get field measured WQ data (conductivity, tn, tp) using code in NC_DataConsumption
# Read in predicted WQ results from the WQ models
PrdWQresults=read.csv("N:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\Pred_WQresults_2014All.csv")

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


#Write to a csv, but I would prefer not to do this... UID issue may cause problems in the future so it would be easier to just keep the data active and in R. 
#However I did write to a csv so there was a hardcopy of the results for a backup.
write.csv(AllWQ2, "N:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\WQconditions_2014All_8Oct2014.csv")



##############      Aquamet Indicator calculations check      ###############
#############################################################################

#XFC_NAT
###Get the approrpaite fish metrics from NC_DataConsumption
######'BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'
###### Change numeric categories into appropriate percentages, pivot and take the mean or each fish cover category at a site. 
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
####Doing Sand and fines separately
Sediment$SA_True=ifelse(Sediment$RESULT == "SA", 1, 0)
pctsa=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='SA_True',fun='mean')),c("UID","PCT_SA_CHECK"))
pctsa$PCT_SA_CHECK=pctsa$PCT_SA_CHECK*100

Sediment$FN_True=ifelse(Sediment$RESULT == "FN", 1, 0)
pctfn=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='FN_True',fun='mean')),c("UID","PCT_FN_CHECK"))
pctfn$PCT_FN_CHECK=pctfn$PCT_FN_CHECK*100

pctSAFN=merge(pctfn,pctsa, All=TRUE)
pctSAFN$PCT_SAFN_CHECK=pctSAFN$PCT_FN_CHECK+pctSAFN$PCT_SA_CHECK


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


# Now I'd like to combine the two datasets for PCT_SAFN together so that I don't have multiple files for the same thing
PCT_SAFN_ALL=rbind(pctsafn,F_Sed2014)



#############
##TO check if bed and bank measurements were included I ran this code. This code does not distinguish between bed or bank just runs to get the mean of all 2014 particles, regardless of location
WR_Sed2014$SAFN_True=ifelse(WR_Sed2014$RESULT == "1", 1, 0)
WR1_Sed2014=cast(WR_Sed2014,'UID~PARAMETER', value='SAFN_True', fun=mean)


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
RipXCMG_Final=cast(RipXCMG,'UID~PARAMETER', value='ResultsPer',fun='mean')
RipXCMG_Final$XCMG_CHECK=rowSums(RipXCMG_Final[,c(2,3,4,5,6,7)])

#xcmgw
RipWW$ResultsPer=ifelse(RipWW$RESULT == 1, 0.05,ifelse(RipWW$RESULT == 2, 0.25,ifelse(RipWW$RESULT == 3, 0.575,ifelse(RipWW$RESULT == 4, 0.875,ifelse(RipWW$RESULT ==0, 0, NA)))))
RipXCMGW_Final=cast(RipWW,'UID~PARAMETER', value='ResultsPer',fun='mean')
RipXCMGW_Final$XCMGW_CHECK=rowSums(RipXCMGW_Final[,c(2,3,4,5)])












##############             EPA EMAP and WRSA data             ###############
#############################################################################



