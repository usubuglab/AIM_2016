#############################################################################

##############        Merge EPA's EMAP and WRSA data          ###############

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

#CLEAN UP DATA LIST AND WRITE OUT CSV!!!!!!! #deleting indicators that weren't needed
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





###############################################################################################

# EPA data for level III ecoregions

###############################################################################################
#To get Sample sizes used to set thresholds
##This does not count NAs (good) and was manually checked to determine if it was counting the correct information.
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
rm(pvt11,pvt12,pvt13,pvt14,pvt15, pvt16, pvt17, pvt18)
#If you do not plan to run hybrid ecoregions (ECO10) run this line, If you do plan to use ECO10 DO NOT RUN THIS LINE
rm(RIP_RS_combined,RIP_RS_reorder,RIP_RS_minusDup,SED_RS_combined,SED_RS_reorder,SED_RS_minusDup)
###############################################################################################

# SETTING THRESHOLDS FOR level III ecoregions!!!

###############################################################################################
## For Ecoregion level III
# Use riparian reference sites for:  XCDENMID, XCMG, XCMGW, and PH!!
#Use riparian reference for PH because 
## 1) P-hab reference were selected using a variety of filters, since we do not have specific chemical reference sites we used p-hab reference
## 2) There are a few more reference sites for riparian than for sediment so Riparian was used over Sediment.  
#XCDENMID
T11=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.10"))
T12=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO_LVL_3NAME","XCDENMID_0.30"))
T15=join_all(list(T11,T12), by="ECO_LVL_3NAME")
#XCMG
T11=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.10"))
T12=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO_LVL_3NAME","XCMG_0.30"))
T16=join_all(list(T11,T12), by="ECO_LVL_3NAME")
#XCMGW
T11=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.10"))
T12=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO_LVL_3NAME","XCMGW_0.30"))
T17=join_all(list(T11,T12), by="ECO_LVL_3NAME")

#PH
#PH thresholds were determine to be too strict so we are using the national standards of <6.5 and >9.0 as Poor, >7 and <8.5 as Good.
#if EPA reference thresholds want to be used you must uncomment the below code and add "T18" to the liste of code to join it back into the RIP_THRESHOLDS_lvlIII object
#A few pH issues
#PH_EPA=RIP_RS_final[,1:25]
#Replace NAs with if statement to combine columns
#PH_EPA$PH=ifelse(is.na(PH_EPA$PHLAB),PH_EPA$PHSTVL,PH_EPA$PHLAB)
#thresholds
#T11=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO_LVL_3NAME), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO_LVL_3NAME","PH_0.10"))
#T12=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO_LVL_3NAME), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO_LVL_3NAME","PH_0.30"))
#T13=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO_LVL_3NAME), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO_LVL_3NAME","PH_0.90"))
#T14=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO_LVL_3NAME), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO_LVL_3NAME","PH_0.70"))
#T18=join_all(list(T11,T12,T14,T13), by="ECO_LVL_3NAME")


##Combine all
RIP_THRESHOLDS_lvlIII=join_all(list(T15,T16,T17),by="ECO_LVL_3NAME")


###################################################################################
# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#PCT_SAFN
T13=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.70"))
T14=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO_LVL_3NAME","PCT_SAFN_0.90"))
T15=join_all(list(T13,T14), by="ECO_LVL_3NAME")
#DPCT_SF
###Modeled indicator, does not use Ecoregion percentil thresholds as these others do.. 
#XFC_NAT
T11=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.10"))
T12=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO_LVL_3NAME","XFC_NAT_0.30"))
T17=join_all(list(T11,T12), by="ECO_LVL_3NAME")
#LINCIS_H
T13=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.70"))
T14=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO_LVL_3NAME","LINCIS_H_0.90"))
T18=join_all(list(T13,T14), by="ECO_LVL_3NAME")
#XEMBED
T13=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.70"))
T14=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO_LVL_3NAME","XEMBED_0.90"))
T19=join_all(list(T13,T14), by="ECO_LVL_3NAME")
##Combine all
SED_THRESHOLDS_lvlIII=join_all(list(T15,T17,T18,T19),by="ECO_LVL_3NAME")



###################################################################################
# Use set thresholds for:  L_XCMGW, W1_HALL, QR1
# Defined when thresholds are implemented and applied to field measured indicators.
###################################################################################


#Make one file for level III thresholds and remove unwanted working files
Thresholds_lvlIII=merge(RIP_THRESHOLDS_lvlIII,SED_THRESHOLDS_lvlIII, all=TRUE)
# Add pH thresholds
#Using National standards NOT EPA regional reference approach. See above for code to use EPA regional reference approach and notes on why this was a problem to use.
#Column names of 0.10, 0.30, 0.70, and 0.90 were used so the ConditionDeterminations code did not need to be updated regardless of if you were using National standards or EPA regional reference thresholds.
Thresholds_lvlIII$PH_0.10=6.5
Thresholds_lvlIII$PH_0.30=7
Thresholds_lvlIII$PH_0.70=8.5
Thresholds_lvlIII$PH_0.90=9


#Make threshold files for NorCal specific ecoregions for ease of looking
Thresholds_lvlIII_NC=subset(Thresholds_lvlIII, ECO_LVL_3NAME == "Central Basin and Range"|ECO_LVL_3NAME == "Eastern Cascades Slopes and Foothills"|ECO_LVL_3NAME == "Northern Basin and Range"|ECO_LVL_3NAME == "Sierra Nevada")


rm(T11,T12,T13,T14,T15,T16,T17,T18,T19,RIP_THRESHOLDS_lvlIII,SED_THRESHOLDS_lvlIII,RIP_RS_final,SED_RS_final)





###############################################################################################

# EPA data for ECO10

###############################################################################################
#May not include code for all indicators (e.g., pH) See ecoregion III for 
#To get Sample sizes used to set thresholds
##This does not count NAs (good) and was manually checked to determine if it was counting the correct information.
###FOR ECO10
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

#Remove all working objects, leave files that will be used later in code, and open the final sample sizes. 
rm(RIP_RS_combined,RIP_RS_reorder,RIP_RS_minusDup,SED_RS_combined,SED_RS_reorder,SED_RS_minusDup,pvt1,pvt2,pvt3,pvt4,pvt5, pvt6, pvt7, pvt8)
###############################################################################################

# SETTING THRESHOLDS FOR ECO10!!!

###############################################################################################
# Use riparian reference sites for:  XCDENMID, XCMG, XCMGW, and PH!!
#Use riparian reference for PH because 
## 1) P-hab reference were selected using a variety of filters, since we do not have specific chemical reference sites we used p-hab reference
## 2) There are a few more reference sites for riparian than for sediment so Riparian was used over Sediment.  
#XCDENMID
T1=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XCDENMID_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XCDENMID_0.30"))
T5=join_all(list(T1,T2), by="ECO10")
#XCMG
T1=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XCMG_0.10"))#changing alpha
T2=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XCMG_0.30"))#changing alpha
T6=join_all(list(T1,T2), by="ECO10")
#XCMGW
T1=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XCMGW_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XCMGW_0.30"))
T7=join_all(list(T1,T2), by="ECO10")

#PH
#PH thresholds were determine to be too strict so we are using the national standards of <6.5 and >9.0 as Poor, >7 and <8.5 as Good.
#if EPA reference thresholds want to be used you must uncomment the below code and add "T18" to the liste of code to join it back into the RIP_THRESHOLDS_lvlIII object
#A few pH issues
#PH_EPA=RIP_RS_final[,1:25]
#Replace NAs with if statement to combine columns
#PH_EPA$PH=ifelse(is.na(PH_EPA$PHLAB),PH_EPA$PHSTVL,PH_EPA$PHLAB)
#thresholds
#T11=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","PH_0.10"))
#T12=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","PH_0.30"))
#T13=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO10","PH_0.90"))
#T14=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO10","PH_0.70"))
#T18=join_all(list(T11,T12,T14,T13), by="ECO10")

##Combine all
RIP_THRESHOLDS_ECO10=join_all(list(T5,T6,T7,),by="ECO10")

###################################################################################
###################################################################################
# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#PCT_SAFN
T3=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO10","PCT_SAFN_0.70"))
T4=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO10","PCT_SAFN_0.90"))
T5=join_all(list(T3,T4), by="ECO10")
#DPCT_SF
###Modeled indicator, does not use Ecoregion percentil thresholds as these others do.. 
#XFC_NAT
T1=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XFC_NAT_0.10"))
T2=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XFC_NAT_0.30"))
T7=join_all(list(T1,T2), by="ECO10")
#LINCIS_H
T3=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO10","LINCIS_H_0.70"))
T4=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO10","LINCIS_H_0.90"))
T8=join_all(list(T3,T4), by="ECO10")
#XEMBED
T3=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO10","XEMBED_0.70"))
T4=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO10","XEMBED_0.90"))
T9=join_all(list(T3,T4), by="ECO10")
##Combine all
SED_THRESHOLDS_ECO10=join_all(list(T5,T7,T8,T9),by="ECO10")

###################################################################################
# Use set thresholds for:  L_XCMGW, W1_HALL, QR1
# Defined when thresholds are implemented and applied to field measured indicators.
###################################################################################

#Make one file for ECO 10 thresholds and remove unwanted working files
Thresholds_ECO10=merge(SED_THRESHOLDS_ECO10,RIP_THRESHOLDS_ECO10, all=TRUE)
# Add pH thresholds
#Using National standards NOT EPA regional reference approach. See above for code to use EPA regional reference approach and notes on why this was a problem to use.
#Column names of 0.10, 0.30, 0.70, and 0.90 were used so the ConditionDeterminations code did not need to be updated regardless of if you were using National standards or EPA regional reference thresholds.
Thresholds_ECO10$PH_0.10=6.5
Thresholds_ECO10$PH_0.30=7
Thresholds_ECO10$PH_0.70=8.5
Thresholds_ECO10$PH_0.90=9


rm(T1,T2,T3,T4,T5,T6,T7,T8,T9,RIP_THRESHOLDS_ECO10,SED_THRESHOLDS_ECO10)


#Make NorCal Specific ECO10 threshold files
Thresholds_ECO10_NC=subset(Thresholds_ECO10, ECO10 == "XE-SOUTH"|ECO10 == "XE-NORTH"|ECO10 == "MT-PNW")
