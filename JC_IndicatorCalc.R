#only indicators with "_CHECK" added on the name are in the end output. Once this output is compared to Aquamet change "_CHECK" to  "_FINAL"
#To get the metadata table you must use the SQL code. 
#tblMETADATA= sqlQuery(wrsa1314, "select * from tblMETADATA")
#############################################################################
# FIRST Run the settup section of DataConsumption_WRSAdb.R
# SECOND Run filters for appropriate project, years, and protocols.
####projects=c('WRSA','NV','GSENM','COPLT','2015ProtocolOverlap','AKEFO','NORCAL')# most useful for separating NorCal and WRSA, note that abbreviations differ between Access and SQL/FM
####years=c('2013','2014','2015')#as character, not number
####protocols=c('NRSA13','WRSA14','BOAT14','AK14')#for separating differences in overall protocol, may not be relevant for some parameters
########Sitecode filter will likely be use to check data at one site or troubleshoot code during processing. Must add to code as needed as it is not built in to code below.
########sitecodes=c('OT-SS-7112')#c('EL-LS-8134','EL-SS-8127','MN-LS-1004','MN-SS-1104','MS-SS-3103','XE-RO-5086','XN-LS-4016','XN-SS-4128','XS-LS-6029' )#QAduplicateSites#c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')#sites for NorCalTesting

#############################################################################

######################    Getting all of the Data      ######################

#############################################################################

#To get WQ data for 3 parameters for all NorCal sites #add turbidity and temp
WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','NTL','PTL','TURBIDITY','TEMPERATURE','EC_PRED','TN_PRED','TP_PRED'),Projects=projects,Years=years,Protocols=protocols)
WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')


# #Get pH for NorCal
# PHtbl=tblRetrieve(Parameters=c('PH'),Projects=projects,Years=years,Protocols=protocols)
# PHpvt=cast(PHtbl,'UID~PARAMETER',value='RESULT')
# PHfinal=addKEYS(PHpvt,c('SITE_ID','DATE_COL','LOC_NAME'))
# PHfinal=PHfinal[,c(1,5,4,3,2)]
# rm(PHtbl,PHpvt)

#Get Site Code to UID, run the water quality lines above and just pull from that...This didn't work for what I initially needed, but is a good way to get UID/Sitecode
#UID_SiteCode=WQfinal[,c('UID','SITE_ID')]

#Getting Data for aquamet check of XFC_NAT
fish=tblRetrieve(Parameters=c('BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'),Projects=projects,Years=years,Protocols=protocols)
#unique(fish$RESULT)# check data structure
#fishpvt=cast(fish,'UID+TRANSECT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates


#Getting data for aquamet check of xcdenmid and xcdenbk
densiom=tblRetrieve(Parameters='DENSIOM',Projects=projects,Years=years,Protocols=protocols)
#unique(densiom$RESULT)
#densiompvt=cast(densiom,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(densiom$POINT)

#Getting data for aquamet check of LINCIS_H, I need bankfull height and incision height for this metric
Incision=tblRetrieve(Parameters=c('INCISED','BANKHT'),Projects=projects,Years=years,Protocols=protocols)
#min(Incision$RESULT);max(Incision$RESULT)
#incisionpvt=cast(Incision,'UID+TRANSECT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates

#Getting data for aquamet check of pct_safn.
Sediment=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS'),Projects=projects,Years=years,Protocols=protocols)
Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects=projects,Years=years,Protocols=protocols)
#unique(Sediment$RESULT)
#Sedimentpvt=cast(Sediment,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(Sedimentpvt$TRANSECT)# check data structure
#Sed2014pvt=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(Sed2014pvt$POINT)# check data structure
#unique(Sed2014pvt$LOC)
#min(Sed2014pvt$SIZE_NUM);max(Sed2014pvt$SIZE_NUM)
#WR_Sed2014=tblRetrieve(Parameters=c('SIZE_NUM'),Projects=projects,Years=years,Protocols=protocols)

#Getting data for aquamet check of XCMG
#RipALL=tblRetrieve(Parameters=c("BARE","CANBTRE","CANSTRE","CANVEG","GCNWDY","GCWDY","UNDERVEG","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols)
#unique(RipALL$RESULT)
RipXCMG=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCNWDY","GCWDY","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols)
#unique(RipXCMG$RESULT)
RipWW=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols)
RipGB=tblRetrieve(Parameters=c("BARE"),Projects=projects,Years=years,Protocols=protocols)
#RipAllpvt=cast(RipALL,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(RipAllpvt$TRANSECT)

#Getting data for aquamet check W1_HALL
#Figure out the differences... Human influence sample type...
Human_Influ=tblRetrieve(Parameters=c('BUILD','LOG','MINE','PARK','PAST','PAVE','PIPES','ROAD','ROW','TRASH','WALL'), Projects=projects,Years=years,Protocols=protocols)                       
#Human_Influpvt=cast(Human_Influ,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#unique(Human_Influ$RESULT)
#unique(Human_Influpvt$POINT)

#Getting data for aquamet check XEMBED
EMBED=tblRetrieve(Parameters='EMBED', Projects=projects,Years=years,Protocols=protocols)
#EMBEDpvt=cast(EMBED,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#unique(EMBED$RESULT)
#unique(EMBED$TRANSECT)
#unique(EMBED$POINT)

#To get data for aquamet check QR1, run densiom, Human_Influ, and RipWW to get the data. Code to calculate QR1 is in NC_DataAnalysis file

#Bank Stability
BankStab=tblRetrieve(Parameters=c('STABLE','EROSION','COVER'), Projects=projects,Years=years,Protocols=protocols)
#unique(BankStab$RESULT)
#BankStabpvt=cast(BankStab,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#unique(BankStab$POINT)
#unique(BankStab$TRANSECT)

#Angle-PIBO method only
Angle=tblRetrieve(Parameters=c('ANGLE180'),Projects=projects, Years=years,Protocols=protocols)
#Anglepvt=cast(Angle,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#unique(Anglepvt$TRANSECT)
#min(Angle$RESULT);max(Angle$RESULT)

#Slope
Slope_height=tblRetrieve(Parameters=c('SLOPE'), Projects=projects, Years=years,Protocols=protocols)
SlpReachLen=tblRetrieve(Parameters=c('SLPRCHLEN'), Projects=projects, Years=years,Protocols=protocols)
#Slope_heightpvt=cast(Slope_height,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#SlpReachLenpvt=cast(SlpReachLen,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')

#Thalweg
thalweg=tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols)
thalweg=subset(thalweg,SAMPLE_TYPE!='CROSSSECW')
#thalwegpvt=cast(thalweg,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#unique(thalwegpvt$POINT)
#max(thalwegpvt$DEPTH);min(thalwegpvt$DEPTH)

#Pools
pool_length=tblRetrieve(Parameters=c('LENGTH'),Projects=projects, Years=years,Protocols=protocols)
reach_length=tblRetrieve(Parameters=c('TRCHLEN'),Projects=projects, Years=years,Protocols=protocols)
PoolDepth=tblRetrieve(Parameters=c('PTAILDEP','MAXDEPTH'), Projects=projects, Years=years,Protocols=protocols)

#Sinuosity
StreamLen=tblRetrieve(Parameters=c('TRCHLEN'), Projects=projects, Years=years,Protocols=protocols)
BRTR=tblRetrieve(Parameters=c('LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR'),Projects=projects, Years=years,Protocols=protocols)
BRTR=cast(BRTR,'UID~PARAMETER',value='RESULT')

#Channel Dimensions
WetWid=tblRetrieve(Parameters=c('WETWIDTH'),Projects=projects, Years=years,Protocols=protocols)#Wetted widths from thalweg
WetWid2=tblRetrieve(Parameters=c('WETWID'),Projects=projects, Years=years,Protocols=protocols)#Wetted widths from main transects
BankWid=tblRetrieve(Parameters=c('BANKWID'),Projects=projects, Years=years,Protocols=protocols)
#WetWidpvt=cast(WetWid,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#WetWid2pvt=cast(WetWid2,'UID+TRANSECT~PARAMETER',value='RESULT')
#BankWidpvt=cast(BankWid,'UID+TRANSECT~PARAMETER',value='RESULT')

#METADATA
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR'),Projects=projects,Years=years,Protocols=protocols)
listsites=setNames(cast(listsites,'UID~PARAMETER',value='RESULT'),c("UID","DATE_COL_CHECK","LAT_DD_CHECK","LAT_DD_BR_CHECK","LAT_DD_TR_CHECK","LOC_NAME_CHECK","LON_DD_CHECK","LON_DD_BR_CHECK","LON_DD_TR_CHECK","PROJECT_CHECK","PROTOCOL_CHECK","SITE_ID_CHECK",'VALXSITE_CHECK'))
listsites=listsites[,c(1,12,6,2,3,7,10,13,11,5,9,4,8)]


#average # of pieces of wood?
LwdCatWet=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'W%'"))$PARAMETER
LwdCatDry=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'D%'"))$PARAMETER
LwdWet=tblRetrieve(Parameters=LwdCatWet,Projects=projects,Years=years,Protocols=protocols)
LwdDry=tblRetrieve(Parameters=LwdCatDry,Projects=projects,Years=years,Protocols=protocols)
TRCHLEN=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT'),Projects=projects,Years=years,Protocols=protocols)
TRCHLEN=cast(TRCHLEN,'UID~PARAMETER',value='RESULT')
#TRCHLEN is not the same as the reachlen used in Aquamet
#The reachlen is calc from mulitplying INCREMENT by the thalweg stations
#We need to estimate the intended number of wadeable thalweg stations at each transect
#which are considered sampled (even if there is no data) for the purposes of
#calculating residual pools and channel lengths.  The number of stations at
#a transect is calculated as the greater of either the number of stations
#occuring in the dataframe for that transect, or the most common count of
#stations (i.e. station mode) occuring at that site. 


### Getting Data to calculate Indicators Stops here


#############################################################################

################    BankStability and Cover Indicators##      ###############

#############################################################################
#Need to decide on removing depositional banks
#combine data from main channels and side channels
#BankStab$TRANSECT=mapvalues(BankStab$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
#Pivot data so that Each parameter has it's own column
Banks=cast(BankStab, 'UID+TRANSECT+POINT~PARAMETER', value='RESULT')#decided to average across all side channels because PIBO takes only the outside banks but can't determine which are the outside banks after the fact
#Banks=cast(BankStab, 'UID+TRANSECT+POINT~PARAMETER', value='RESULT', fun=max)#take whichever main or side channel is most unstable (max uses the highest alphbetically which is Erosional, uncovered, and slump--(eroding is least stable but since getting combined it doesn't matter))
#I want to calculate the percent of banks that are Covered.  
Banks$CoverValue=as.numeric(ifelse(Banks$COVER=='UC',"0",ifelse(Banks$COVER=='CV',"1","NA")))
#I want to calculate the percent of banks that are Stable (Absent) 
# Unstable==(Fracture, slump, slough, eroding)
Banks$StableValue=as.numeric(ifelse(Banks$STABLE=='SP'|Banks$STABLE=='ER'|Banks$STABLE=='LH'|Banks$STABLE=='FC',"0",ifelse(Banks$STABLE=='AB',"1","NA")))
#combined stability and cover
Banks$BnkCover_Stab=as.numeric(ifelse((Banks$CoverValue+Banks$StableValue)<2,0,1))

#only erosional banks
BanksErosional=subset(Banks, EROSION=='EL')
BnkCvrErosional=setNames(aggregate(CoverValue~UID,data=BanksErosional, FUN=mean), c('UID','BnkCover_Erosional_CHECK'))
BnkStbErosional=setNames(aggregate(StableValue~UID,data=BanksErosional, FUN=mean), c('UID','BnkStability_Erosional_CHECK'))
BnkCover_StabErosional=setNames(aggregate(BnkCover_Stab~UID,data=BanksErosional, FUN=mean), c('UID','BnkCover_StabErosional_CHECK'))
#samplesize
nBnkCover_StabErosional=setNames(aggregate(BnkCover_Stab~UID,data=BanksErosional, FUN=length), c('UID','nBnkCover_StabErosional_CHECK'))


#both erosional and depositional banks
BanksAll=Banks
BnkCvrAll=setNames(aggregate(CoverValue~UID,data=BanksAll, FUN=mean), c('UID','BnkCover_All_CHECK'))
BnkStbAll=setNames(aggregate(StableValue~UID,data=BanksAll, FUN=mean), c('UID','BnkStability_All_CHECK'))
BnkCover_StabAll=setNames(aggregate(BnkCover_Stab~UID,data=BanksAll, FUN=mean), c('UID','BnkCover_StabAll_CHECK'))
#samplesize
nBnkCover_StabAll=setNames(aggregate(BnkCover_Stab~UID,data=BanksAll, FUN=length), c('UID','nBnkCover_StabAll_CHECK'))# 2 banks at 21 transects should not be less than 21

#merge all bank files
BnkErosional=join_all(list(BnkCover_StabErosional,BnkCvrErosional,BnkStbErosional,nBnkCover_StabErosional), by="UID")
BnkAll=join_all(list(BnkCover_StabAll,BnkCvrAll,BnkStbAll,nBnkCover_StabAll,nBnkCover_StabAll), by="UID")



#remove cases with less than 50% of data #15 sites excluded?
#exclude=subset(BnkErosional,nBnkCover_StabErosional_CHECK<21)#15 excluded
BnkErosional$BnkCover_StabErosional_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<21,NA,BnkErosional$BnkCover_StabErosional_CHECK) 
BnkErosional$BnkCover_Erosional_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<21,NA,BnkErosional$BnkCover_Erosional_CHECK)  
BnkErosional$BnkStability_Erosional_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<21,NA,BnkErosional$BnkStability_Erosional_CHECK) 

BnkAll$BnkCover_StabAll_CHECK=ifelse(BnkAll$nBnkCover_StabAll_CHECK<21,NA,BnkAll$BnkCover_StabAll_CHECK) 
BnkAll$BnkCover_All_CHECK=ifelse(BnkAll$nBnkCover_StabAll_CHECK<21,NA,BnkAll$BnkCover_All_CHECK)  
BnkAll$BnkStability_All_CHECK=ifelse(BnkAll$nBnkCover_StabAll_CHECK<21,NA,BnkAll$BnkStability_All_CHECK) 

#############################################################################

##############         WQ Indicator calculations check        ###############

#############################################################################

#At the end, all columns with "Check" at the end are included in the main file
WQpvt$CONDUCTIVITY=round(WQpvt$CONDUCTIVITY,digits=2)
WQpvt$TN_PRED=round(WQpvt$TN_PRED,digits=3)
WQpvt$TP_PRED=round(WQpvt$TP_PRED,digits=1)
WQpvt$EC_PRED=round(WQpvt$EC_PRED,digits=2)
WQpvt$OE_EC=round(WQpvt$CONDUCTIVITY-WQpvt$EC_PRED,digits=2)
WQpvt$OE_TN=round(WQpvt$NTL-WQpvt$TN_PRED,digits=3)
WQpvt$OE_TP=round(WQpvt$PTL-WQpvt$TP_PRED,digits=1)
WQfinal=setNames(WQpvt,c("UID","CONDUCTIVITY_CHECK","EC_PRED_CHECK","NTL_CHECK","PH_CHECK","PTL_CHECK","TEMPERATURE_CHECK","TN_PRED_CHECK","TP_PRED_CHECK","TURBIDITY_CHECK","OE_EC_CHECK","OE_TN_CHECK","OE_TP_CHECK"))
WQfinal=WQfinal[,c(1,2,3,13,6,9,12,4,8,11,5,7,10)]


#############################################################################

##############      Aquamet Indicator calculations check      ###############

#############################################################################

#XFC_NAT
###Get the approrpaite fish parameters from NC_DataConsumption
######'BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'
###### Change numeric categories into appropriate percentages, pivot to take the mean or each fish cover category at a site. 
###### Then sum to categories of fish cover for each site to have the final results to compared to aquamet's xfc_nat
###### The way this is calculated causes NA's to be treated as blanks that do not count for or against the average. For example if only 1 NA for BOULDR then you would divide boulders by 10 transects instead of 11. See UID 11625 for an example.
fish$ResultsPer=ifelse(fish$RESULT == 1, 0.05,ifelse(fish$RESULT == 2, 0.25,ifelse(fish$RESULT == 3, 0.575,ifelse(fish$RESULT == 4, 0.875,ifelse(fish$RESULT ==0, 0, NA)))))
fishpvt2=cast(fish,'UID~PARAMETER', value='ResultsPer',fun='mean')
fishpvt2$XFC_NAT_CHECK=round(rowSums(fishpvt2[,c(2,3,4,5,6,7)]),digits=2)
#sample size
fishpvt3=cast(fish,'UID~PARAMETER', value='ResultsPer',fun='length')
fishpvt2$nXFC_NAT_CHECK=(fishpvt3$BOULDR+fishpvt3$BRUSH+fishpvt3$LVTREE+fishpvt3$OVRHNG+fishpvt3$UNDCUT+fishpvt3$WOODY)# 6 categories *11 transects 
#fishpvt5=subset(fishpvt2,nXFC_NAT_CHECK<33)#only 4 sites that this applies to and 28 was the lowest so decided to use all data 
fishpvt2$XFC_NAT_CHECK=ifelse(fishpvt2$nXFC_NAT_CHECK<33,NA,fishpvt2$XFC_NAT_CHECK)

#xcdenmid
MidDensiom = subset(densiom, POINT == "CU"|POINT =="CD"|POINT == "CL"|POINT == "CR")
DensPvt=cast(MidDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
DensPvt$XCDENMID_CHECK=round((DensPvt$DENSIOM/17)*100,digits=2)
#Trying to figure out what is going on with UID 11802.
#Dens_Pvt3=cast(MidDens3,'UID+TRANSECT~PARAMETER',value='RESULT',fun=mean)
nDensPvt=setNames(count(MidDensiom,"UID"),c("UID","nXCDENMID_CHECK"))#should be 4 locations at 11 transects=44 so half is 22
DensPvt=merge(nDensPvt,DensPvt,by="UID")
DensPvt$XCDENMID_CHECK=ifelse(DensPvt$nXCDENMID_CHECK<22,NA,DensPvt$XCDENMID_CHECK)#7 have values of 20

#xcdenbk
BnkDensiom = subset(densiom, POINT == "LF"|POINT =="RT")
BnkDensPvt=cast(BnkDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
BnkDensPvt$XCDENBK_CHECK=round((BnkDensPvt$DENSIOM/17)*100,digits=2)
nBnkDensPvt=setNames(count(BnkDensiom,"UID"),c("UID","nXCDENBK_CHECK"))# should be 2 locations at 11 transects=22 so half is 11
BnkDensPvt=merge(nBnkDensPvt,BnkDensPvt,by="UID")
BnkDensPvt$XCDENBK_CHECK=ifelse(BnkDensPvt$nXCDENBK_CHECK<11,NA,BnkDensPvt$XCDENBK_CHECK)#8 have n=8-10

#LINCIS_H
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

#sample sizes
nInc=setNames(count(Inc,"UID"),c("UID","nxinc_h_CHECK"))
nBnk=setNames(count(Bnk,"UID"),c("UID","nxbnk_h_CHECK"))   

IncBnk=join_all(list(xBnkht,xIncht,nInc, nBnk),by="UID")
IncBnk$xinc_h_CHECK=ifelse(IncBnk$nxinc_h_CHECK<5,NA,IncBnk$xinc_h_CHECK)#5 sites less than 5, 1-4
IncBnk$xbnk_h_CHECK=ifelse(IncBnk$nxbnk_h_CHECK<5,NA,IncBnk$xbnk_h_CHECK)#1 site
IncBnk$LINCIS_H_CHECK=round(log10(IncBnk$xinc_h_CHECK-IncBnk$xbnk_h_CHECK+0.1),digits=2)
IncBnk$xinc_h_CHECK=round(IncBnk$xinc_h_CHECK,digits=2)
IncBnk$xbnk_h_CHECK=round(IncBnk$xbnk_h_CHECK,digits=2)


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




#pct_safn
#First you have to take the percent of Sand and percent of fine separately and then add them together... NOPE YOU CAN DO IT EITHER WAY... Just do pct sa and fns works too...
#Correct...For 2013 data
################################
##This is ONLY for 2013 data
####Doing Sand and fines separately: This was a check to figure out how SAFN was calculated and is only needed if you want the individual metrics of Sand and Fines
#Sediment$SA_True=ifelse(Sediment$RESULT == "SA", 1, 0)
#pctsa=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='SA_True',fun='mean')),c("UID","PCT_SA_CHECK"))
#pctsa$PCT_SA_CHECK=pctsa$PCT_SA_CHECK*100
#####################################
#Sediment$FN_True=ifelse(Sediment$RESULT == "FN", 1, 0)
#pctfn=setNames((cast(Sediment,'UID~SAMPLE_TYPE', value='FN_True',fun='mean')),c("UID","PCT_FN_CHECK"))
#pctfn$PCT_FN_CHECK=pctfn$PCT_FN_CHECK*100
##################################
#pctSAFN=merge(pctfn,pctsa, All=TRUE)
#pctSAFN$PCT_SAFN_CHECK=pctSAFN$PCT_FN_CHECK+pctSAFN$PCT_SA_CHECK

#### 2013 data and Boating data which was also stored in Size_CLS
####Doing sand and fines together
Sediment$SAFN_True=ifelse(Sediment$RESULT == "SA", 1,ifelse(Sediment$RESULT == "FN", 1, 0))
pctsafn=setNames((aggregate(Sediment$SAFN_True,by=list(UID=Sediment$UID), data=Sediment, FUN='mean')),c("UID","PCT_SAFN_CHECK"))#had to remove NorCal code that casted by Sample_Type because of boating data
pctsafn$PCT_SAFN_CHECK=round(pctsafn$PCT_SAFN_CHECK*100,digits=1)
Sedimentpvt=cast(Sediment,'UID~PARAMETER',value='RESULT',fun=length)# number of pebbles collected at intermediate and main transects
Sedimentpvt$nPCT_SAFN_CHECK=(Sedimentpvt$SIZE_CLS+Sedimentpvt$XSIZE_CLS) # number of pebbles collected at all transects only 4 boating sites had less than 50
Sedimentpvtsub=subset(Sedimentpvt,select=c(UID,nPCT_SAFN_CHECK))
pctsafn_2013=merge(pctsafn,Sedimentpvtsub,by="UID")

#Now for 2014 data... 
A_Sed2014=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')
##Checking how many records should be deleted by ordering and just looking at how many bank and na locations there are. 
#B_Sed2014=A_Sed2014[order(A_Sed2014$LOC),]
#View(B_Sed2014[2000:2588,])
#B_Sed2014=A_Sed2014[order(A_Sed2014$SIZE_NUM),]
#View(B_Sed2014[2000:2588,])

#All pebbles 
# E_Sed2014=A_Sed2014[complete.cases(A_Sed2014[,c("LOC","SIZE_NUM")]),]
# E_Sed2014$SAFN_True=ifelse(E_Sed2014$SIZE_NUM == "1", 1, 0)
# F_Sed2014=setNames(aggregate(E_Sed2014$SAFN_True,list(UID=E_Sed2014$UID),mean), c("UID","PCT_SAFN_CHECK"))########this still counts particles with 0s in the particle count and as not fines; is this what we want to do?
# F_Sed2014$PCT_SAFN_CHECK=round(F_Sed2014$PCT_SAFN_CHECK*100,digits=1)


#only bed pebbles
#Remove all sediment records that were collected on the "BANK"
C_Sed2014=A_Sed2014[!A_Sed2014$LOC== "BANK", ]
#Can't use na.omit because it omits all records with an NA in ANY field. I only want to remove NAs in the LOC and the SIZE_NUM (sediment result) field. 
E_Sed2014=C_Sed2014[complete.cases(C_Sed2014[,c("LOC","SIZE_NUM")]),]
E_Sed2014$SAFN_True=ifelse(E_Sed2014$SIZE_NUM == "1", 1, 0)
F_Sed2014=setNames(aggregate(E_Sed2014$SAFN_True,list(UID=E_Sed2014$UID),mean), c("UID","PCT_SAFN_CHECK"))########this still counts particles with 0s in the particle count and as not fines; is this what we want to do?
F_Sed2014$PCT_SAFN_CHECK=round(F_Sed2014$PCT_SAFN_CHECK*100,digits=1)

#sample sizes
Nall_Sed2014pvt=setNames(cast(Sed2014,'UID~PARAMETER',value='RESULT',fun=length),c("UID","nLOC","nallPCT_SAFN_CHECK"))#number of all collected pebbles
Nall_Sed2014pvtsub=subset(Nall_Sed2014pvt, select=c(UID,nallPCT_SAFN_CHECK))
Nbed_Sed2014pvt=aggregate(.~UID, data=C_Sed2014, length)#number of bed pebbles
Nbed=setNames(subset(Nbed_Sed2014pvt, select=c(UID,SIZE_NUM)),c("UID","nPCT_SAFN_CHECK"))
# 7 sites from AK had right around 100 which they should according the the AK protocol
# 4 WRSA sites had right around 100 pebbles so these were included
# 2 WRSA sites had values between 60-70 but this is still more than half of pebbles collected in 2013 so this data was included and used
# 2 CO sites have values around 100 but OK
# 1 NV site at 59 and 1 NV at 100

G_Sed2014=merge(F_Sed2014,Nbed, by="UID")

# Combine the two datasets for PCT_SAFN together so that I don't have multiple files for the same thing
H_Sed=rbind(pctsafn_2013,G_Sed2014)
PCT_SAFN_ALL=join(H_Sed,Nall_Sed2014pvtsub,by="UID",type="left")
PCT_SAFN_sub=subset(PCT_SAFN_ALL,nPCT_SAFN_CHECK<50)
PCT_SAFN_ALL$PCT_SAFN_CHECK=ifelse(PCT_SAFN_ALL$nPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$PCT_SAFN_CHECK)

###################################################################################################################################
#other sediment metrics
Sed2014=bintranslate(Table='Sed2014',ST='CROSSSECW',PM='SIZE_NUM')


#build table of the min and max of each size class 
#make sure to uncomment HP if want it to be "NA" but subsetting only measurable classes for D50 at least anyway
tt <- textConnection(
                  "class min     max\n                    
                     RS 4000    8000\n                   
                     RR 4000    8000\n                    
                     RC 4000    8000\n                    
                     BH 4000    8000\n#boatable class?                    
                     XB 1000    4000\n                    
                     SB  250    1000\n                    
                     BL  250    4000\n#combined boulder class                   
                     CB   64     250\n                    
                     GC   16      64\n                    
                     GF    2      16\n                   
                     GR    2      64\n                    
                     SA    0.06    2\n                    
                     FN    0.001   0.06\n                   
                     HP 4000    8000\n
                    #HP   NA      NA\n                    
                     WD   NA      NA\n                    
                     OT   NA      NA\n"
                  )
subsInfo <- read.table(tt, header = TRUE, stringsAsFactors = FALSE)
close(tt)
#take the geometric mean for each size class
#create function for geometric mean
gmean <- function(x){exp(mean(log(x)))}
subsInfo$diam <- NA
for (s in 1:nrow(subsInfo)) {
  subsInfo[s, ]$diam = gmean(c(subsInfo[s, ]$min, subsInfo[s, 
                                                           ]$max))
}
#log geometric mean diameter and log of the min and max of size classes
subsInfo$lDiam <- log10(subsInfo$diam)
subsInfo$lmin = log10(subsInfo$min)
subsInfo$lmax = log10(subsInfo$max)

#lsub_dmm - this code replicates aquamet for all 2013 data but 
#list of substrate classes included in lsub_dmm
wadeableAllOneBoulderClass <- c("RS", "RR", "RC", "BL", "CB", 
                                "GC", "GF", "SA", "FN", "HP", "WD", "OT")
#calculating lsub_dmm by taking the mean of the log geometric mean diameter 
df1lb <- Sediment#input dataframe
df1lb$RESULT <- ifelse(df1lb$RESULT %in% c("XB", "SB"), 
                       "BL", df1lb$RESULT)#merging the small and large boulder classes
ldBuglb <- merge(df1lb, subset(subsInfo, class %in% wadeableAllOneBoulderClass, 
                               select = c(class, diam, lDiam)), by.x = "RESULT", 
                 by.y = "class", all.x = TRUE)#subsetting just the classes included in 1sub_dmm
ldBug11lb <- setNames(aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                       mean, na.rm = TRUE), c("UID","lsub_dmm_CHECK"))# calculating the mean which is "lsub_dmm"


#list of measureable classes included in D50
wadeableMeasurableTwoBoulderClasses <- c("XB", "SB", "CB", 
                                         "GC", "GF", "SA", "FN")
interpdata <- subset(Sediment, RESULT %in% wadeableMeasurableTwoBoulderClasses)
measurable <- setNames(subset(subsInfo, class %in% wadeableMeasurableTwoBoulderClasses, 
                            select = c(class, lmin, lmax)), c("CLASS", "min", "max"))#setNames is doing something different than the internal rename function that was originally in aquamet
#####################################################################################
# A single object matching ‘interpolatePercentile’ was found
# It was found in the following places
# package:aquamet
# namespace:aquamet
# with value
 
interpolatePercentile<-function (df, classVar, percentile, pctlVar, classBounds) 
{
  df <- subset(df, !is.na(classVar))
  classCounts <- aggregate(list(classCount = df[[classVar]]), 
                           list(UID = df$UID, CLASS = df[[classVar]]), count)#counting the number of each size class category per uid
  sampleSizes <- aggregate(list(totalCount = df[[classVar]]), 
                           list(UID = df$UID), count)# counting total number of pebbles per UID
  classPcts <- merge(classCounts, sampleSizes, by = "UID")
  classPcts$pct <- 100 * classPcts$classCount/classPcts$totalCount# percent each size class makes up of all the pebbles collected at a site
  classPcts <- merge(classPcts, classBounds, by = "CLASS", 
                     all.x = TRUE)# add the bounds for each size class to the table
  classPcts <- classPcts[order(classPcts$UID, classPcts$min), 
                         ]# sort by UID and the smallest susbtrate first
  classPcts$upperPct <- ave(classPcts$pct, classPcts$UID, FUN = cumsum)#cumulative sum of the percent of each size class per UID --example GF50+GF50+CB25+(GF50+CB25+SB25)
  classPcts <- first(classPcts, "UID", "start")#another internal aquamet function using function "lag"
  classPcts <- lag(classPcts, "upperPct", "lowerPct")
  classPcts[classPcts$start, ]$lowerPct <- 0
  tt <- subset(classPcts, lowerPct < percentile & percentile <= 
                 upperPct) # percentile will not always fall on a size class so need to determine which two size classes it is in between and then interpolate using line below
  tt[pctlVar] <- with(tt, min + (max - min) * (percentile - 
                                                 lowerPct)/(upperPct - lowerPct))# linear interpolation equation 2.15 in bunte and abte 2001 pg 41
  tt <- tt[c("UID", pctlVar)]
  return(tt)
}
#################################################################################
A single object matching ‘first’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (df, v, first.v) 
{
  df <- lag(df, v, "..vLag", offset = 1)
  df[first.v] <- as.vector(ifelse(is.na(df[v]) | is.na(df["..vLag"]), 
                                  is.na(df[v]) != is.na(df["..vLag"]), df[v] != df["..vLag"]))
  df[1, first.v] <- TRUE
  df["..vLag"] <- NULL
  return(df)
}
##################################################################################



c16 <- interpolatePercentile(interpdata, "RESULT", 16, 
                             "lsub2d16inor", measurable)
c50 <- interpolatePercentile(interpdata, "RESULT", 50, 
                             "lsub2d50inor", measurable)
c84 <- interpolatePercentile(interpdata, "RESULT", 84, 
                             "lsub2d84inor", measurable)
c16$d16 <- 10^(c16$lsub2d16inor)
c50$d50 <- 10^(c50$lsub2d50inor)
c84$d84 <- 10^(c84$lsub2d84inor)
calcs <- merge(c16, merge(c50, c84, by = "UID", all = TRUE), 
               by = "UID", all = TRUE)

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
#sample sizes
nXCMG_new=setNames(count(RipXCMG,"UID"),c("UID","nXCMG_CHECK"))#6 strata *2 banks*11 transects=132 so half data=66
XCMG_new1=merge(nXCMG_new,XCMG_new1, by="UID")
XCMG_new1$XCMG_CHECK=ifelse(XCMG_new1$nXCMG_CHECK<66,NA,XCMG_new1$XCMG_CHECK)#5 sites 3 WRSA 54-60

#RipGB
RipGB$ResultsPer=ifelse(RipGB$RESULT == 1, 0.05,ifelse(RipGB$RESULT == 2, 0.25,ifelse(RipGB$RESULT == 3, 0.575,ifelse(RipGB$RESULT == 4, 0.875,ifelse(RipGB$RESULT ==0, 0, NA)))))
XGB_new=setNames(cast(RipGB,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',  'VALUE'))
XGB_new1=setNames(aggregate(VALUE~UID,data=XGB_new,FUN=mean),list("UID","XGB_CHECK"))


#xcmgw
#xcmgw=XC+XMW+XGW: However, this is not how aquamet is calculating it, the order of operation would give different results if XC was caluclated and then added to XMW and XMG
#More true to aquamet calculation: XCMG=XCL+XCS+XMW+XGW 
#Need to just calculate it by transect side first then average at an entire site. 
##XC=XCL+XCS (Small Canopy trees (CANSTRE) + Large Canopy trees(CANBTRE))  
##XMW=Understory woody aka UNDWDY
##MGW= ground cover woody GCWDY
RipWW$ResultsPer=ifelse(RipWW$RESULT == 1, 0.05,ifelse(RipWW$RESULT == 2, 0.25,ifelse(RipWW$RESULT == 3, 0.575,ifelse(RipWW$RESULT == 4, 0.875,ifelse(RipWW$RESULT ==0, 0, NA)))))
XCMGW_new=setNames(cast(RipWW,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',  'VALUE'))
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

#L_XCMGW
XCMGW_new1$L_XCMGW_CHECK=log10(XCMGW_new1$XCMGW_CHECK)

#Xembed
XEMBED=setNames(cast(EMBED,'UID~PARAMETER', value='RESULT', fun='mean'), list('UID','XEMBED_CHECK'))
nXEMBED=setNames(count(EMBED,"UID"),c("UID","nXEMBED"))
XEMBED=merge(XEMBED,nXEMBED,by="UID")

#W1_HALL
#Be careful, the documentation says to use P=0.667, but the aquamet code says 0.6667, if there ends up being a lot of P's in the data this makes a difference!!! 
#EMAP_WEST
Human_Influ$EMAP_Weights=ifelse(Human_Influ$RESULT == "B", 1.5,ifelse(Human_Influ$RESULT == "C", 1.0, ifelse(Human_Influ$RESULT == "P", 0.6667, 0))) 
W1_HALL=cast(Human_Influ,'UID~PARAMETER', value='EMAP_Weights',fun='mean')
W1_HALL$EMAP_W1_HALL_CHECK=rowSums(W1_HALL[,c(2:12)])     

# NRSA
Human_Influ$NRSA_Weights=ifelse(Human_Influ$RESULT == "B", 1.5,ifelse(Human_Influ$RESULT == "C", 1.0, ifelse(Human_Influ$RESULT == "P", 0.5, 0))) 
W1_HALL_NRSA=cast(Human_Influ,'UID~PARAMETER', value='NRSA_Weights',fun='mean')
W1_HALL$NRSA_W1_HALL_CHECK=rowSums(W1_HALL_NRSA[,c(2:12)])     


###xcdenbk: needed for QR1

#QR1
#QR1= {(QRVeg1) (QRVeg2) (QRDIST1)} ^ 0.333; 
#if XCMGW <=2.00, then QRVeg1=.1+(.9 (XCMGW/2.00))
#if XCMGW >2.00 then QRVeg1=1; 
#and QRVeg2=0.1 + [0.9(XCDENBK/100)]; 
#QRDIST1=1/(1+W1_Hall). 
QR1=join_all(list(XCMGW_new1,BnkDensPvt,W1_HALL), by='UID')
QR1=setNames(subset(QR1[,c('UID','XCMGW_CHECK','xcdenbk_CHECK','EMAP_W1_HALL_CHECK')]),list('UID','XCMGW','xcdenbk','W1_HALL'))

#QRVeg1
QR1$QRveg1=ifelse(QR1$XCMGW<=2.00,.1+(.9*(QR1$XCMGW/2)),1)
#QRVeg2
QR1$QRVeg2=0.1 + (0.9*(QR1$xcdenbk/100))
#QRDIST1
QR1$QRDIST1=1/(1+QR1$W1_HALL)
#Final QR1 calculation
QR1$QR1_CHECK=(QR1$QRveg1*QR1$QRVeg2*QR1$QRDIST1)^0.333

###########################################

#######JC ADDED Metrics/Indicators#########

###########################################
#Angle
Angle$RESULT=ifelse(Angle$RESULT<45,45,Angle$RESULT)
MeanAngle=setNames(round(cast(Angle,'UID~PARAMETER',value='RESULT',fun=mean),digits=0),c("UID","ANGLE180_CHECK"))                                             
#sample size
nAngle=setNames(count(Angle,"UID"),c("UID","nANGLE180_CHECK"))# 2 banks* 11 transects=22
MeanAngle=merge(nAngle,MeanAngle, by="UID")
MeanAngle$ANGLE180_CHECK=ifelse(MeanAngle$nANGLE180_CHECK<11,NA,MeanAngle$ANGLE180_CHECK)#8 site 5 WRSA

#Slope
Slope_height=cast(Slope_height, 'UID~PARAMETER',value='RESULT',fun=sum)
SlpReachLen=cast(SlpReachLen,'UID~PARAMETER',value='RESULT')
Slope_Per=merge(Slope_height,SlpReachLen, by=c('UID'), all=T)
Slope_Per$SlopePct=round(((Slope_Per$SLOPE/100)/(Slope_Per$SLPRCHLEN))*100,digits=2)
Slope_Per=setNames(Slope_Per,c("UID","SLOPE_CHECK","SLPRCHLEN_CHECK","SlopePct_CHECK"))

#Thalweg                                                                                                                                                                    
ThalwegMean=round(cast(thalweg,'UID~PARAMETER',value='RESULT',fun=mean),digits=1)
ThalwegMean=setNames(ThalwegMean, c("UID","XDEPTH_CHECK"))
ThalwegSD=cast(thalweg,'UID~PARAMETER',value='RESULT',fun=sd)
ThalwegSD=setNames(ThalwegSD,c("UID","SDDEPTH"))
Thalweg=merge(ThalwegMean,ThalwegSD,by=c('UID'), all=T)
Thalweg$CVDEPTH_CHECK=Thalweg$SDDEPTH/Thalweg$XDEPTH

###########################################
cdData <- tblRetrieve(Parameter=c("ACTRANSP", "DISTANCE", "INCREMNT", "SIDCHN", "OFF_CHAN", "REACHLENGTH"), Projects=projects,Years=years,Protocols=protocols)
metsoutGeneral <- metsGeneral(thalweg, channelgeometry)#working, but odd values coming out for reachlength still >> trouble shoot thalweg$STATION (currently set to numeric); seems to be good >> next up:  cdData data subset driving the station count which results in 0 for many NRSA sites which don't include all the "No"s for side channel, etc. >> cdData <- subset(indat, PARAMETER %in% c("ACTRANSP", "DISTANCE", "INCREMNT", "SIDCHN", "OFF_CHAN", "REACHLENGTH"))
rlen=subset(metsoutGeneral,METRIC=='reachlen'); rlen$VALUEl=rlen$RESULT-(rlen$RESULT*.1); rlen$VALUEh=rlen$RESULT+(rlen$RESULT*.1)#10% bounds
thalCheck=tblRetrieve(Parameters='TRCHLEN',UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
thalCheck=merge(rlen,thalCheck,intersect(setdiff(colnames(metsoutGeneral),'RESULT'),colnames(thalCheck))),all.x=T)
thalCheck$check=ifelse(thalCheck$RESULT>thalCheck$VALUEl & thalCheck$RESULT<thalCheck$VALUEh,'OK','X');thalCheck=subset(thalCheck,check=='X')
if(nrow(thalCheck)>0){print("WARNING! Calculated reach length (METRIC=='reachlen') significantly different from field recorded Reach Length (PARAMETER=='TRCHLEN'). Confirm correct INCREMENT parameter.")}


#LWD
#C1WM100- (Cummulative count of LWD in bankfull channel across all size classes)/(Reach Length) units are pieces/100m
LWD=setNames(aggregate(RESULT~UID,data=LwdWet,FUN=sum),c("UID","C1W"))# count of all LWD pieces per site
LWD=merge(LWD,TRCHLEN,by=c('UID'), all=T)
LWD$C1WM100_CHECK=round((LWD$C1W/LWD$TRCHLEN)*100,digits=1)
#V1WM100
LWDtt <- textConnection(
  "LWD_Cat  DIAMETER	LENGTH
  WLDLL	0.6	15
  WLDML	0.6	5
  WLDSL	0.6	1.5
  WMDLL	0.3	15
  WMDML	0.3	5
  WMDSL	0.3	1.5
  WSDLL	0.1	15
  WSDML	0.1	5
  WSDSL	0.1	1.5
  WXDLL	0.8	15
  WXDML	0.8	5
  WXDSL	0.8	1.5
  WLDSML  0.6	1.5
  WMDSML	0.3	1.5
  WSDSML	0.1	1.5
  WXDSML	0.8	1.5
  WSDSSL	0.1	1.5
  WXDSSL	0.8	1.5
  WMDSSL	0.3	1.5
  WLDSSL	0.6	1.5
  #WLDSML	0.6	3
  #WMDSML	0.3	3
  #WSDSML	0.1	3
  #WXDSML	0.8	3"
  )
LWD_sizes <- read.table(LWDtt, header = TRUE, stringsAsFactors = FALSE)
close(LWDtt)
LWD_sizes$VOLUME=pi*((1.33*(LWD_sizes$DIAMETER/2)^2)*(1.33*LWD_sizes$LENGTH))#pg 31 of Kauffman 1999 and he cites Robison 1998

# XwalkUnion=Xwalk(XwalkName='Aquamet1',Table='XwalkUnion',Source='R',XwalkDirection='')#!need to formally omit unused parameters and track down unknowns to see how they are used in aquamet (i.e. Assessment, etc)
# #collapse LWD
# XwalkLWDsub=subset(XwalkUnion,PARAMETER %in% c('DXDSL','DSDSL','DMDSL','DLDSL','WXDSL','WSDSL','WMDSL','WLDSL'));XwalkLWDsub$RESULT=as.numeric(XwalkLWDsub$RESULT);
# XwalkLWDagg=data.frame(cast(XwalkLWDsub,'UID+TRANSECT+POINT+TABLE+SAMPLE_TYPE+PARAMETER ~ .',value='RESULT',fun.aggregate=sum));XwalkLWDagg$RESULT=XwalkLWDagg$X.all.;XwalkLWDagg=ColCheck(XwalkLWDagg,colnames(XwalkUnion))#  228284433826712128 B
# XwalkNOlwd=subset(XwalkUnion,(IND %in% XwalkLWDsub$IND)==FALSE)
# XwalkUnion=rbind(XwalkLWDagg,XwalkNOlwd); rm(XwalkLWDsub); rm(XwalkLWDagg);rm(XwalkNOlwd)#XwalkUnionclean=XwalkUnion



#Pools
#PIBO METHOD
#Percent pools
pvtpools1=cast(pool_length,'UID~PARAMETER',value='RESULT',fun=sum) 
pvtpools2=cast(reach_length,'UID~PARAMETER',value='RESULT') 
poolsmerge=merge(pvtpools1,pvtpools2,by=c('UID'),all=T)
poolsmerge$PoolPct=round((poolsmerge$LENGTH/poolsmerge$TRCHLEN)*100,digits=0)
#residual pool depth
PoolDepth=cast(PoolDepth,'UID+POINT~PARAMETER',value='RESULT')
PoolDepth$RPD=PoolDepth$MAXDEPTH-PoolDepth$PTAILDEP
RPD=setNames(round(aggregate(PoolDepth$RPD,list(UID=PoolDepth$UID),mean)/100,digits=3),c("UID","RPD"))#converted to m
#pool frequency
count=setNames(count(PoolDepth,"UID"),c("UID","NumPools"))
poolmerge2=join_all(list(poolsmerge,count,RPD), by="UID")
poolmerge2$PoolFrq=round((poolmerge2$NumPools/poolmerge2$TRCHLEN)*10000,digits=0)###need to consider what reach length to use here #may need shorted lengths for parial reaches
Pools=setNames(subset(poolmerge2,select=c(UID,PoolPct,RPD,PoolFrq,NumPools)),c("UID","PoolPct_CHECK","RPD_CHECK","PoolFrq_CHECK","NumPools_CHECK"))

#Channel dimensions
#note the EPA combines all side channel, main and intermediate transects and then takes the max wetted width for a total of 10 values (excluding transect K?) instead of 21. This is likely an artifact of them adding intermediate transects halway through the project
#We are deviating from the EPA's method of calculating wetted width. We take the max value for main vs. side channels (should in theory always be main channel); then we average across all 21 values (main and intermediate)
#Because of this deviation, in the future we should remove the variable differences of wetwid and wetwidth and make them the same variable
WetWid2$TRANSECT=mapvalues(WetWid2$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))#change all side channels to normal transects
WetWid2=cast(WetWid2,'UID+TRANSECT~PARAMETER', value='RESULT', fun=max)#take the maximum wetted width among side channels
WetWid=subset(WetWid,POINT!=0)#remove duplicate wetted widths 
WetWid=setNames(cast(WetWid,'UID+TRANSECT+POINT~PARAMETER', value='RESULT'), list ("UID","TRANSECT","POINT","WETWID"))
WetWidSub=WetWid[,c(1,2,4)]# delete the point column
WetWidAll=rbind(WetWidSub,WetWid2)#merge main transects and intermediate transects together
WetWidFinal=setNames(round(aggregate(WETWID~UID,data=WetWidAll,FUN=mean),digits=2),list("UID","XWIDTH_CHECK"))#average across all transects
nWetWid=setNames(count(WetWidAll,"UID"),c("UID","nXWIDTH_CHECK"))#21 transects
WetWidFinal=merge(nWetWid,WetWidFinal)
WetWidFinal$XWIDTH_CHECK=ifelse(WetWidFinal$XWIDTH_CHECK<10,NA,WetWidFinal$XWIDTH_CHECK)#9 ranging from 4-11 not an indicator and just for context so lean towards leaving all measurements

##checking for interupted flow sites
# checkzero=rbind(WetWid,WetWid2)
# checkzero$PARAMETER=ifelse(checkzero$PARAMETER=='WETWIDTH','WETWID',checkzero$PARAMETER)
# checkzero_pvt=cast(checkzero,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')
# checkzerosubset=subset(checkzero_pvt,WETWID==0)
# checkzerosubset=subset(checkzerosubset,POINT=!0)

BankWid$TRANSECT=mapvalues(BankWid$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))#change all side channels to normal transects
BankWidpvt=cast(BankWid,'UID+TRANSECT~PARAMETER', value='RESULT', fun=sum)#sum across side channels and main transects
BankWidpvt=setNames(round(aggregate(BankWidpvt$BANKWID,list(UID=BankWidpvt$UID),mean),digits=2),c("UID","XBKF_W_CHECK"))#average all transects
nBankWid=setNames(count(BankWid,"UID"),c("UID","nXBKF_W_CHECK"))# should have 11 transects
BankWidFinal=merge(nBankWid,BankWidpvt)
BankWidFinal$XBXK_W_CHECK=ifelse(BankWidFinal$nXBKF_W_CHECK<5,NA,BankWidFinal$XBKF_W_CHECK)# one site with 4 leave in

#bank=BankWid$XBKF_W_CHECK
#quantile(bank,0.15)
#wet=WetWid$XWIDTH_CHECK
#quantile(wet,0.15)
                      

####################################################################################################################################                     
#To get all calculated values together... Although some tables still have the metrics included.
#IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkErosional,BnkAll,fishpvt2,DensPvt,BnkDensPvt,XCMGW_new1,XCMG_new1,XGB_new1,IncBnk,BankWid,WetWid,XEMBED,PCT_SAFN_ALL,W1_HALL,QR1,MeanAngle,Slope_Per,Thalweg,Pools),by="UID")
IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkErosional,BnkAll,fishpvt2,DensPvt,BnkDensPvt,XCMG_new1,IncBnk,BankWidFinal,WetWidFinal,XEMBED,PCT_SAFN_ALL,MeanAngle,Slope_Per,Thalweg,Pools,LWD),by="UID")

#To remove all of the metrics and only get the indicators subset by UID and all those columns ending in "CHECK". Hmm..not really sure what the $ is doing here, the code works without it, but all the examples I've looked at keep the $ so I kept it too... 
IndicatorCheck=IndicatorCheckJoin[,c("UID",grep("CHECK$", colnames(IndicatorCheckJoin),value=TRUE))]
#write.csv(IndicatorCheck,"C:\\Users\\Nicole\\Desktop\\IndicatorCheck2.csv")
#Remove all other data files as they are no longer needed
IndicatorCheck=subset(IndicatorCheck,PROTOCOL_CHECK=="BOAT14")
write.csv(IndicatorCheck,"IndicatorCheck_29April2016.csv")
rm(PHfinal,XGB_new,XGB_new1,BankStab,Banks,RipGB,EMBED,Human_Influ,W1_HALL,W1_HALL_NRSA,QR1,XEMBED,BnkDensPvt,BnkDensiom,densiom,RipXCMG,XCMG_new,XCMG_new1,RipWW,XCMGW_new,XCMGW_new1,IndicatorCheckJoin,fish,fishpvt2,
   MidDensiom,DensPvt,Incision,INCISED,BANKHT,Inc,Bnk,xIncht,xBnkht,IncBnk,Sediment,pctsafn,Sed2014,A_Sed2014,C_Sed2014,E_Sed2014,F_Sed2014,PCT_SAFN_ALL)
