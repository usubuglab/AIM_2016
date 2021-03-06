#############################################################################

##############        Merge EPA's EMAP and WRSA data          ###############

#############################################################################

#Read in EMAP and NRSA data
EMAP=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\EMAP.csv")
NRSA=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\NRSA.csv")

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
#write.csv(Final2, "\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\Mass_Combination_NRSA_EMAP.csv")

#################################################################################
#############
#Sooo.. I needed Excel to clean up and do a few things, but now coming back to R
# to set thresholds for p-hab indicators
#############
#################################################################################

combined=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\Comb_21Oct2014_Rinput_DoNotAlter.csv")

#add back in a few columns from Mass_Combination_NRSA_EMAP
#key in combined file is SITE_ID
#as of March 31 2017 the above code to merge NRSA and EMAP data is throwing a row.names duplicate error because REALM is duplicated in the NRSA dataset. 
#I don't know why this wasn't an issue previously but I assume that the dataset in Mass_combination_NRSA_EMAP.csv is correct and would join needed fields back in from this csv rather than the code below.
Final2=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\Mass_Combination_NRSA_EMAP.csv")
Final2sub=subset(Final2,select=c(SITE_ID,XBKF_H,W1_HAG, W1H_CROP,W1H_WALL,PROJECT,XC,XM,XG,XSLOPE,XSLOPE_FIELD,XSLOPE_FRDATA,XSLOPE_FRREF,XSLOPE_MAP,XWD_RAT,XWIDTH,XWXD,XBKF_W,XDEPTH,V1W_MSQ,V1WM100,V2W_MSQ,V2WM100,V4W_MSQ,V4WM100,C1TM100,C1WM100,C2TM100,C2WM100,C4TM100,C4WM100,FLOWSITE))
combined2=join(combined,Final2sub,by='SITE_ID', type="left",match="all")

combined3=read.csv('trialreferencedata.csv')
combined3=join(combined3,Final2sub,by='SITE_ID', type="left",match="all")

#Use to get wadeable or boatable only, if this line isn't run both boatable and wadeable will be used. Change REALM== "" to specify 
#combined2=subset(combined2, REALM == "WADEABLE")
combined2$BNK_THRESH=ifelse(as.numeric(combined2$XBKF_W)>10,"LargeWadeable","SmallWadeable")
combined2$BNK_THRESH=ifelse(combined2$REALM=="WADEABLE",combined2$BNK_THRESH,combined2$REALM)
combined2$THRESH=paste(combined2$ECO10,combined2$BNK_THRESH, sep="_")

combined2$THRESH3=as.factor(combined2$THRESH)
levels(combined2$THRESH3) <- list( XE_SOUTH_SmallWadeable="XE_SOUTH_SmallWadeable",XE_SOUTH_LargeWadeable="XE_SOUTH_LargeWadeable", 
                                  MT_SWEST_SmallWadeable="MT_SWEST_SmallWadeable",MT_SWEST_LargeWadeable="MT_SWEST_LargeWadeable", 
                                  XE_EPLAT_SmallWadeable="XE_EPLAT_SmallWadeable",XE_EPLAT_LargeWadeable="XE_EPLAT_LargeWadeable", 
                                  MT_PNW_SmallWadeable="MT_PNW_SmallWadeable", MT_PNW_LargeWadeable="MT_PNW_LargeWadeable",MT_PNW_BOATABLE="MT_PNW_BOATABLE",  
                                  PL_NCULT_SmallWadeable="PL_NCULT_SmallWadeable", PL_NCULT_LargeWadeable="PL_NCULT_LargeWadeable",PL_NCULT_BOATABLE="PL_NCULT_BOATABLE", 
                                  PL_RANGE_SmallWadeable="PL_RANGE_SmallWadeable",PL_RANGE_LargeWadeable="PL_RANGE_LargeWadeable", PL_RANGE_BOATABLE="PL_RANGE_BOATABLE",
                                  MT_SROCK_SmallWadeable="MT_SROCK_SmallWadeable",MT_SROCK_LargeWadeable="MT_SROCK_LargeWadeable", 
                                  MT_NROCK_SmallWadeable="MT_NROCK_SmallWadeable", MT_NROCK_LargeWadeable="MT_NROCK_LargeWadeable",
                                  XE_NORTH_SmallWadeable="XE_NORTH_SmallWadeable",XE_NORTH_LargeWadeable="XE_NORTH_LargeWadeable",
                                  #Other=c("XE_CALIF_LargeWadeable","XE_CALIF_SmallWadeable","0_BOATABLE","XE_SOUTH_NA","MT_PNW_0","MT_NROCK_NA", "_SmallWadeable","_LargeWadeable","_BOATABLE"  ,"_NA", "MT_SROCK_NA" , "0_LargeWadeable" , "MT_SWEST_NA", "0_SmallWadeable", "XE_CALIF_BOATABLE","MT_SWEST_BOATABLE"),   
                                  MT_ROCK_BOATABLE=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),  
                                  XE_SEPLAT_BOATABLE=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
                                    )
combined2$THRESH2=combined2$THRESH
combined2$THRESH2=ifelse(combined2$THRESH2=="PL_RANGE_BOATABLE"|combined2$THRESH2=="PL_NCULT_BOATABLE"|combined2$THRESH2=="MT_PNW_BOATABLE"|combined2$THRESH2=="MT_NROCK_BOATABLE"|combined2$THRESH2=="MT_SROCK_BOATABLE"|combined2$THRESH2=="XE_NORTH_BOATABLE"|combined2$THRESH2=="XE_EPLAT_BOATABLE"|combined2$THRESH2=="XE_SOUTH_BOATABLE","ALL_BOATING",combined2$THRESH2)

#combined2=subset(combined2,BNK_THRESH=="LargeWadeable")
#combined2=subset(combined2,BNK_THRESH=="SmallWadeable")
#combined2=subset(combined2, REALM == "BOATABLE")
#To subset for reference to use on RIPARIAN indicators.
#First subset the data to only include sites with R or S designations
RIP_RS_combined=subset(combined2, RST_FRIP_AND_RMD_PHAB == "R"|RST_FRIP_AND_RMD_PHAB == "S")
#Now I need to remove duplicate sites, but choose to remove the one with the most recent year. 
#Remove the one with the most recent year because it may not still be in reference condition when it was sampled the second time.
###So I order by if it was revisited, the site code, and then the year.  Check this with the view
RIP_RS_reorder=RIP_RS_combined[order(RIP_RS_combined$REVISITED_OVERLAP,RIP_RS_combined$DUPLICATE_ID, RIP_RS_combined$YEAR, decreasing=FALSE),]
#View(RIP_RS_reorder[1250:2041,])
###Now I remove the duplicate that is listed second.
RIP_RS_minusDup= RIP_RS_reorder[!duplicated(RIP_RS_reorder$DUPLICATE_ID),]
#View(RIP_RS_minusDup[1000:1999,])
RIP_RS_final=RIP_RS_minusDup[order(RIP_RS_minusDup$REVISITED_OVERLAP, decreasing=TRUE),]



#To subset for reference to use on SEDIMENT and INSTREAM indicators.
#First subset the data to only include sites with R or S designations
SED_RS_combined=subset(combined2, RST_FSED_AND_RMD_PHAB == "R"|RST_FSED_AND_RMD_PHAB == "S")
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
pvt19=aggregate(XGB~ECO_LVL_3NAME,data=RIP_RS_final,FUN=length)
ECOIII_SampSizes=join_all(list(pvt11,pvt12,pvt13,pvt14,pvt15, pvt16, pvt17, pvt18, pvt19),by="ECO_LVL_3NAME")

#NorCal specific hybrid ecoregions
ECOIII_SampSizes_NC = subset(ECOIII_SampSizes, ECO_LVL_3NAME == "Central Basin and Range"|ECO_LVL_3NAME == "Eastern Cascades Slopes and Foothills"|ECO_LVL_3NAME == "Northern Basin and Range"|ECO_LVL_3NAME == "Sierra Nevada")

#Remove all working objects, leave files that will be used later in code, and open the final sample sizes. 
rm(pvt11,pvt12,pvt13,pvt14,pvt15, pvt16, pvt17, pvt18, pvt19)
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
#XGB Bare ground indictor TRIAL!
T11=setNames(aggregate(RIP_RS_final$XGB, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO_LVL_3NAME","XGB_0.90"))
T12=setNames(aggregate(RIP_RS_final$XGB, by = list(RIP_RS_final$ECO_LVL_3NAME), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO_LVL_3NAME","XGB_0.70"))
T19=join_all(list(T11,T12), by="ECO_LVL_3NAME")


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
RIP_THRESHOLDS_lvlIII=join_all(list(T15,T16,T17,T19),by="ECO_LVL_3NAME")


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
pvt2=aggregate(XCDENBK~ECO10,data=RIP_RS_final,FUN=length)
pvt3=aggregate(XCMG~ECO10,data=RIP_RS_final,FUN=length)
pvt4=aggregate(XCMGW~ECO10,data=RIP_RS_final,FUN=length)
pvt5=aggregate(PCT_SAFN~ECO10,data=SED_RS_final,FUN=length)
pvt6=aggregate(DPCT_SF~ECO10,data=SED_RS_final,FUN=length)
pvt7=aggregate(XFC_NAT~ECO10,data=SED_RS_final,FUN=length)
pvt8=aggregate(LINCIS_H~ECO10,data=SED_RS_final,FUN=length)
pvt9=aggregate(XEMBED~ECO10,data=SED_RS_final,FUN=length)
ECO10_SampSizes=join_all(list(pvt1,pvt2,pvt3,pvt4,pvt5, pvt6, pvt7,pvt8,pvt9),by="ECO10")

#boxplot(XCDENMID~ECO10+BNK_THRESH,data=RIP_RS_final)

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
# Use riparian reference sites for:  XCDENMID, XCDENBK, XCMG, XCMGW, and PH!!
#Use riparian reference for PH because 
## 1) P-hab reference were selected using a variety of filters, since we do not have specific chemical reference sites we used p-hab reference
## 2) There are a few more reference sites for riparian than for sediment so Riparian was used over Sediment.  
#XCDENMID
T1=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XCDENMID_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XCDENMID_0.30"))
T5=join_all(list(T1,T2), by="ECO10")
#XCDENBK
T1=setNames(aggregate(RIP_RS_final$XCDENBK, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XCDENBK_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCDENBK, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XCDENBK_0.30"))
T6=join_all(list(T1,T2), by="ECO10")
#XCMG
T1=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XCMG_0.10"))#changing alpha
T2=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XCMG_0.30"))#changing alpha
T7=join_all(list(T1,T2), by="ECO10")
#XCMGW
T1=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","XCMGW_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","XCMGW_0.30"))
T8=join_all(list(T1,T2), by="ECO10")

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
RIP_THRESHOLDS_ECO10=join_all(list(T5,T6,T7,T8),by="ECO10")

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
#RP100
T1=setNames(aggregate(SED_RS_final$RP100, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","RP100_0.10"))
T2=setNames(aggregate(SED_RS_final$RP100, by = list(SED_RS_final$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","RP100_0.30"))
T10=join_all(list(T1,T2), by="ECO10")



##Combine all
SED_THRESHOLDS_ECO10=join_all(list(T5,T7,T8,T9,T10),by="ECO10")

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

###############################################################################################

# SETTING THRESHOLDS FOR ECO10 by stream size!!!

###############################################################################################
#May not include code for all indicators (e.g., pH) See ecoregion III for 
#To get Sample sizes used to set thresholds
##This does not count NAs (good) and was manually checked to determine if it was counting the correct information.
###FOR ECO10
pvt1=aggregate(XCDENMID~THRESH3,data=RIP_RS_final,FUN=length)
pvt2=aggregate(XCDENBK~THRESH3,data=RIP_RS_final,FUN=length)
pvt3=aggregate(XCMG~THRESH3,data=RIP_RS_final,FUN=length)
pvt4=aggregate(XCMGW~THRESH3,data=RIP_RS_final,FUN=length)
pvt5=aggregate(PCT_SAFN~THRESH3,data=SED_RS_final,FUN=length)
pvt7=aggregate(XFC_NAT~THRESH3,data=SED_RS_final,FUN=length)
pvt8=aggregate(LINCIS_H~THRESH3,data=SED_RS_final,FUN=length)
pvt9=aggregate(XEMBED~THRESH3,data=SED_RS_final,FUN=length)
ECO10_SampSizes=join_all(list(pvt1,pvt2,pvt3,pvt4,pvt5, pvt7,pvt8,pvt9),by="THRESH3")

# Use riparian reference sites for:  XCDENMID, XCDENBK, XCMG, XCMGW, and PH!!
#Use riparian reference for PH because 
## 1) P-hab reference were selected using a variety of filters, since we do not have specific chemical reference sites we used p-hab reference
## 2) There are a few more reference sites for riparian than for sediment so Riparian was used over Sediment.  
#XCDENMID
T1=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCDENMID_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCDENMID, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCDENMID_0.30"))
T5=join_all(list(T1,T2), by="THRESH3")
#XCDENBK
T1=setNames(aggregate(RIP_RS_final$XCDENBK, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCDENBK_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCDENBK, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCDENBK_0.30"))
T6=join_all(list(T1,T2), by="THRESH3")
#XCMG
T1=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCMG_0.10"))#changing alpha
T2=setNames(aggregate(RIP_RS_final$XCMG, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCMG_0.30"))#changing alpha
T7=join_all(list(T1,T2), by="THRESH3")
#XCMGW
T1=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCMGW_0.10"))
T2=setNames(aggregate(RIP_RS_final$XCMGW, by = list(RIP_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCMGW_0.30"))
T8=join_all(list(T1,T2), by="THRESH3")

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
RIP_THRESHOLDS_ECO10=join_all(list(T5,T6,T7,T8),by="THRESH3")

###################################################################################
###################################################################################
# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#PCT_SAFN
T3=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","PCT_SAFN_0.70"))
T4=setNames(aggregate(SED_RS_final$PCT_SAFN, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","PCT_SAFN_0.90"))
T5=join_all(list(T3,T4), by="THRESH3")
#DPCT_SF
###Modeled indicator, does not use Ecoregion percentil THRESH3olds as these others do.. 
#XFC_NAT
T1=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XFC_NAT_0.10"))
T2=setNames(aggregate(SED_RS_final$XFC_NAT, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XFC_NAT_0.30"))
T7=join_all(list(T1,T2), by="THRESH3")
#LINCIS_H
T3=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","LINCIS_H_0.70"))
T4=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","LINCIS_H_0.90"))
T8=join_all(list(T3,T4), by="THRESH3")
#XEMBED
T3=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","XEMBED_0.70"))
T4=setNames(aggregate(SED_RS_final$XEMBED, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","XEMBED_0.90"))
T9=join_all(list(T3,T4), by="THRESH3")
#RP100
T1=setNames(aggregate(SED_RS_final$RP100, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","RP100_0.10"))
T2=setNames(aggregate(SED_RS_final$RP100, by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","RP100_0.30"))
T10=join_all(list(T1,T2), by="THRESH3")

#C1WM100
T1=setNames(aggregate(as.numeric(SED_RS_final$C1WM100), by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","C1WM100_0.10"))
T2=setNames(aggregate(as.numeric(SED_RS_final$C1WM100), by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","C1WM100_0.30"))
#V1WM100
T3=setNames(aggregate(as.numeric(SED_RS_final$V1WM100), by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","V1WM100_0.10"))
T4=setNames(aggregate(as.numeric(SED_RS_final$V1WM100), by = list(SED_RS_final$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","V1WM100_0.30"))

T10=join_all(list(T1,T2,T3,T4), by="THRESH3")

##Combine all
SED_THRESHOLDS_ECO10=join_all(list(T5,T7,T8,T9,T10),by="THRESH3")

###################################################################################
# Use set thresholds for:  L_XCMGW, W1_HALL, QR1
# Defined when thresholds are implemented and applied to field measured indicators.
###################################################################################

#Make one file for ECO 10 thresholds and remove unwanted working files
Thresholds_ECO10=merge(SED_THRESHOLDS_ECO10,RIP_THRESHOLDS_ECO10, all=TRUE)

####boating incision with all boating sites combined
#LINCIS_H
T3=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$THRESH2), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","LINCIS_H_0.70"))
T4=setNames(aggregate(SED_RS_final$LINCIS_H, by = list(SED_RS_final$THRESH2), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","LINCIS_H_0.90"))
T8=join_all(list(T3,T4), by="THRESH3")
SubT8=subset(T8,THRESH3=="ALL_BOATING")

Thresholds_Final=merge(Thresholds_ECO10,SubT8, all=TRUE)

# # Add pH thresholds
# #Using National standards NOT EPA regional reference approach. See above for code to use EPA regional reference approach and notes on why this was a problem to use.
# #Column names of 0.10, 0.30, 0.70, and 0.90 were used so the ConditionDeterminations code did not need to be updated regardless of if you were using National standards or EPA regional reference thresholds.
# Thresholds_ECO10$PH_0.10=6.5
# Thresholds_ECO10$PH_0.30=7
# Thresholds_ECO10$PH_0.70=8.5
# Thresholds_ECO10$PH_0.90=9


rm(T1,T2,T3,T4,T5,T6,T7,T8,T9,RIP_THRESHOLDS_ECO10,SED_THRESHOLDS_ECO10)

