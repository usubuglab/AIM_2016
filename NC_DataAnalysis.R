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
##TO check if bed and bank measurements were included I ran this code. This code does not distinguish between bed or bank just runs to get the mean of all 2014 particles, regardless of location. THis shows that Sarah's code is missing the BED/BANK determinations....
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
XCMG_new=setNames(cast(RipXCMG,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',  'VALUE'))
XCMG_new1=setNames(aggregate(VALUE~UID,data=XCMG_new,FUN=mean),list("UID","XCMG_CHECK"))


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




#To get all calculated values together... Although some tables still have the metrics included.
AquametCheckJoin=join_all(list(fishpvt2,DensPvt,XCMGW_new1,XCMG_new1,IncBnk,pctsa,pctfn,pctSAFN,pctsafn,PCT_SAFN_ALL),by="UID")
#To remove all of the metrics and only get the indicators subset by UID and all those columns ending in "CHECK". Hmm..not really sure what the $ is doing here, the code works without it, but all the examples I've looked at keep the $ so I kept it too... 
AquametCheck=AquametCheckJoin[,c("UID",grep("CHECK$", colnames(AquametCheckJoin),value=TRUE))]
write.csv(AquametCheck,"C:\\Users\\Nicole\\Desktop\\AquametCheck2.csv")

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

#Write out because I cannot figure out how do deal with this in R....
write.csv(Final2, "\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\Mass_Combination_NRSA_EMAP.csv")

#################################################################################
#############
#Sooo.. I needed Excel to clean up and do a few things, but now coming back to R
# to set thresholds for p-hab indicators
#############
#################################################################################

combined=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\comb_21Oct2014.csv")

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
T3=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","XCDENMID_0.75"))
T4=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","XCDENMID_0.95"))
T5=join_all(list(T1,T2,T3,T4), by="ECO10")
#XCMG
T1=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XCMG_0.05"))
T2=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XCMG_0.25"))
T3=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","XCMG_0.75"))
T4=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","XCMG_0.95"))
T6=join_all(list(T1,T2,T3,T4), by="ECO10")
#XCMGW
T1=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XCMGW_0.05"))
T2=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XCMGW_0.25"))
T3=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","XCMGW_0.75"))
T4=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","XCMGW_0.95"))
T7=join_all(list(T1,T2,T3,T4), by="ECO10")
##Combine all
RIP_THRESHOLDS=join_all(list(T5,T6,T7),by="ECO10")

###################################################################################
###################################################################################


# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#SECONDARY INDICATORS: CVDPTH, LRP100, LDVRP100, C1WM100

#PCT_SAFN
T1=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","PCT_SAFN_0.05"))
T2=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","PCT_SAFN_0.25"))
T3=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","PCT_SAFN_0.75"))
T4=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","PCT_SAFN_0.95"))
T5=join_all(list(T1,T2,T3,T4), by="ECO10")
#DPCT_SF
T1=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","DPCT_SF_0.05"))
T2=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","DPCT_SF_0.25"))
T3=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","DPCT_SF_0.75"))
T4=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","DPCT_SF_0.95"))
T6=join_all(list(T1,T2,T3,T4), by="ECO10")
#XFC_NAT
T1=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XFC_NAT_0.05"))
T2=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XFC_NAT_0.25"))
T3=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","XFC_NAT_0.75"))
T4=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","XFC_NAT_0.95"))
T7=join_all(list(T1,T2,T3,T4), by="ECO10")
#LINCIS_H
T1=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","LINCIS_H_0.05"))
T2=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","LINCIS_H_0.25"))
T3=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","LINCIS_H_0.75"))
T4=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","LINCIS_H_0.95"))
T8=join_all(list(T1,T2,T3,T4), by="ECO10")
#XEMBED
T1=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","XEMBED_0.05"))
T2=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","XEMBED_0.25"))
T3=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","XEMBED_0.75"))
T4=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","XEMBED_0.95"))
T9=join_all(list(T1,T2,T3,T4), by="ECO10")
##Combine all
SED_THRESHOLDS=join_all(list(T5,T6,T7,T8,T9),by="ECO10")


# WHAT TO DO WITH: W1_HALL, QR1

########################################################################################
###########################################################################################
########################################################################################
## For Ecoregion level III


#XCDENMID
T11=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.05"))
T12=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.25"))
T13=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.75"))
T14=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.95"))
T15=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
#XCMG
T11=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.05"))
T12=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.25"))
T13=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.75"))
T14=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.95"))
T16=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
#XCMGW
T11=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.05"))
T12=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.25"))
T13=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.75"))
T14=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.95"))
T17=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
##Combine all
RIP_THRESHOLDS_lvlIII=join_all(list(T15,T16,T17),by="ECO_LVL_3NAME")

###################################################################################
###################################################################################


# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#SECONDARY INDICATORS: CVDPTH, LRP100, LDVRP100, C1WM100

#PCT_SAFN
T11=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.05"))
T12=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.25"))
T13=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.75"))
T14=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.95"))
T15=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
#DPCT_SF
T11=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.05"))
T12=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.25"))
T13=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.75"))
T14=setNames(aggregate(SED_RS_final$DPCT_SF, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","DPCT_SF_0.95"))
T16=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
#XFC_NAT
T11=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.05"))
T12=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.25"))
T13=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.75"))
T14=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.95"))
T17=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
#LINCIS_H
T11=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.05"))
T12=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.25"))
T13=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.75"))
T14=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.95"))
T18=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
#XEMBED
T11=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.05"))
T12=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.25"))
T13=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.75"))
T14=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.95"))
T19=join_all(list(T11,T12,T13,T14), by="ECO_LVL_3NAME")
##Combine all
SED_THRESHOLDS_lvlIII=join_all(list(T15,T16,T17,T18,T19),by="ECO_LVL_3NAME")

