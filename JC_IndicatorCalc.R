#To get the metadata table you must use the SQL code. 
#tblMETADATA= sqlQuery(wrsa1314, "select * from tblMETADATA")

# FIRST Run the settup section of DataConsumption_WRSAdb.R

#############################################################################

######################    Getting all of the Data      ######################

#############################################################################

#To get WQ data for 3 parameters for all NorCal sites #add turbidity and temp
WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','NTL','PTL','TURBIDITY','TEMPERATURE'),Projects=projects,Years=years,Protocols=protocols)
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

#Getting data for aquamet check of xcdenmid
densiom=tblRetrieve(Parameters='DENSIOM',Projects=projects,Years=years,Protocols=protocols)

#Getting data for aquamet check of LINCIS_H, I need bankfull height and incision height for this metric
Incision=tblRetrieve(Parameters=c('INCISED','BANKHT'),Projects=projects,Years=years,Protocols=protocols)

#Getting data for aquamet check of pct_safn.
Sediment=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS'),Projects=projects,Years=years,Protocols=protocols)
Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects=projects,Years=years,Protocols=protocols)

#WR_Sed2014=tblRetrieve(Parameters=c('SIZE_NUM'),Projects=projects,Years=years,Protocols=protocols)

#Getting data for aquamet check of XCMG
#RipALL=tblRetrieve(Parameters=c("BARE","CANBTRE","CANSTRE","CANVEG","GCNWDY","GCWDY","UNDERVEG","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols)
RipXCMG=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCNWDY","GCWDY","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols)
RipWW=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols)
RipGB=tblRetrieve(Parameters=c("BARE"),Projects=projects,Years=years,Protocols=protocols)

#Getting data for aquamet check W1_HALL
#Figure out the differences... Human influence sample type...
Human_Influ=tblRetrieve(Parameters=c('BUILD','LOG','MINE','PARK','PAST','PAVE','PIPES','ROAD','ROW','TRASH','WALL'), Projects=projects,Years=years,Protocols=protocols)                       

#Getting data for aquamet check XEMBED
EMBED=tblRetrieve(Parameters='EMBED', Projects=projects,Years=years,Protocols=protocols)

#To get data for aquamet check QR1, run densiom, Human_Influ, and RipWW to get the data. Code to calculate QR1 is in NC_DataAnalysis file

#Bank Stability
BankStab=tblRetrieve(Parameters=c('STABLE','EROSION','COVER'), Projects=projects,Years=years,Protocols=protocols)

#Angle-PIBO method only
Angle=tblRetrieve(Parameters=c('ANGLE180'),Projects=projects, Years=years,Protocols=protocols)

#Slope
Slope_height=tblRetrieve(Parameters=c('SLOPE'), Projects=projects, Years=years,Protocols=protocols)
SlpReachLen=tblRetrieve(Parameters=c('SLPRCHLEN'), Projects=projects, Years=years,Protocols=protocols)

#Thalweg
thalweg=tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols)
thalweg=subset(thalweg,SAMPLE_TYPE!='CROSSSECW')

#Pools
pool_length<-tblRetrieve(Parameters=c('LENGTH'),Projects=projects, Years=years,Protocols=protocols)
reach_length=tblRetrieve(Parameters=c('TRCHLEN'),Projects=projects, Years=years,Protocols=protocols)
PoolDepth=tblRetrieve(Parameters=c('PTAILDEP','MAXDEPTH'), Projects=projects, Years=years,Protocols=protocols)

#Sinuosity
StreamLen=tblRetrieve(Parameters=c('TRCHLEN'), Projects=projects, Years=years,Protocols=protocols)
BRTR=tblRetrieve(Parameters=c('LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR'),Projects=projects, Years=years,Protocols=protocols)

#METADATA
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL'),Projects=projects,Years=years,Protocols=protocols)
listsites=setNames(cast(listsites,'UID~PARAMETER',value='RESULT'),c("UID","DATE_COL_CHECK","LAT_DD_CHECK","LOC_NAME_CHECK","LON_DD_CHECK","PROJECT_CHECK","PROTOCOL_CHECK","SITE_ID_CHECK"))
listsites=listsites[,c(1,8,4,2,3,5,6,7)]
#average # of pieces of wood?
                                                                                                                                                                    
### Getting Data to calculate Indicators Stops here


#############################################################################

################    BankStability and Cover Indicators##      ###############

#############################################################################
#Pivot data so that Each parameter has it's own column
Banks=cast(BankStab, 'UID+TRANSECT+POINT~PARAMETER', value='RESULT', fun=max)
#I want to calculate the percent of banks that are Covered.  
Banks$CoverValue=as.numeric(ifelse(Banks$COVER=='UC',"0",ifelse(Banks$COVER=='CV',"1","NA")))
BnkCvr=setNames(aggregate(CoverValue~UID,data=Banks, FUN=mean), c('UID','BnkCover_BLM_CHECK'))

#I want to calculate the percent of banks that are Stable (Absent) 
# Unstable==(Fracture, slump, slough, eroding)

Banks$StableValue=as.numeric(ifelse(Banks$STABLE=='SP'|Banks$STABLE=='ER'|Banks$STABLE=='LH'|Banks$STABLE=='FC',"0",ifelse(Banks$STABLE=='AB',"1","NA")))
BnkStb=setNames(aggregate(StableValue~UID,data=Banks, FUN=mean), c('UID','BnkStability_BLM_CHECK'))

#############################################################################

##############         WQ Indicator calculations check        ###############

#############################################################################

#At the end, all columns with "Check" at the end are included in the main file

WQfinal=setNames(WQpvt,c("UID","CONDUCTIVITY_CHECK","NTL_CHECK","PH_CHECK","PTL_CHECK","TEMPERATURE_CHECK","TURBIDITY_CHECK"))
WQfinal=WQfinal[,c(1,4,2,3,5,6,7)]


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
fishpvt2$XFC_NAT_CHECK=rowSums(fishpvt2[,c(2,3,4,5,6,7)])

#xcdenmid
MidDensiom = subset(densiom, POINT == "CU"|POINT =="CD"|POINT == "CL"|POINT == "CR")
DensPvt=cast(MidDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
DensPvt$xcdenmid_CHECK=(DensPvt$DENSIOM/17)*100
#Trying to figure out what is going on with UID 11802.
#Dens_Pvt3=cast(MidDens3,'UID+TRANSECT~PARAMETER',value='RESULT',fun=mean)

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
IncBnk=merge(xBnkht,xIncht,all=TRUE)
IncBnk$LINCIS_H_CHECK=log10(IncBnk$xinc_h_CHECK-IncBnk$xbnk_h_CHECK+0.1)


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
MeanAngle=setNames(cast(Angle,'UID~PARAMETER',value='RESULT',fun=mean),c("UID","ANGLE180_CHECK")  )                                             

#Slope
Slope_height=cast(Slope_height, 'UID~PARAMETER',value='RESULT',fun=sum)
SlpReachLen=cast(SlpReachLen,'UID~PARAMETER',value='RESULT')
Slope_Per=merge(Slope_height,SlpReachLen, by=c('UID'), all=T)
Slope_Per$SlopePct=round(((Slope_Per$SLOPE/100)/(Slope_Per$SLPRCHLEN))*100,digits=2)
Slope_Per=setNames(Slope_Per,c("UID","SLOPE_CHECK","SLPRCHLEN_CHECK","SlopePct_CHECK"))

#Thalweg                                                                                                                                                                    
ThalwegMean=round(cast(thalweg,'UID~PARAMETER',value='RESULT',fun=mean),digits=1)
ThalwegMean=setNames(ThalwegMean, c("UID","MeanDepth_CHECK"))
ThalwegSD=cast(thalweg,'UID~PARAMETER',value='RESULT',fun=sd)
ThalwegSD=setNames(ThalwegSD,c("UID","SDDepth_CHECK"))

                   
#Pools
#Percent pools
pvtpools1=cast(pool_length,'UID~PARAMETER',value='RESULT',fun=sum) 
pvtpools2=cast(reach_length,'UID~PARAMETER',value='RESULT') 
poolsmerge=merge(pvtpools1,pvtpools2,by=c('UID'),all=T)
poolsmerge$PoolPct=round((poolsmerge$LENGTH/poolsmerge$TRCHLEN)*100,digits=0)
#residual pool depth
PoolDepth=cast(PoolDepth,'UID+POINT~PARAMETER',value='RESULT')
PoolDepth$RPD=PoolDepth$MAXDEPTH-PoolDepth$PTAILDEP
RPD=setNames(round(aggregate(PoolDepth$RPD,list(UID=PoolDepth$UID),mean),digits=1),c("UID","RPD"))
#pool frequency
count=setNames(count(PoolDepth,"UID"),c("UID","NumPools"))
poolmerge2=join_all(list(poolsmerge,count,RPD), by="UID")
poolmerge2$PoolFrq=round((poolmerge2$NumPools/poolmerge2$TRCHLEN)*10000,digits=0)###need to consider what reach length to use here #may need shorted lengths for parial reaches
Pools=setNames(subset(poolmerge2,select=c(UID,PoolPct,RPD,PoolFrq,NumPools)),c("UID","PoolPct_CHECK","RPD_CHECK","PoolFrq_CHECK","NumPools_CHECK"))

####################################################################################################################################                     
#To get all calculated values together... Although some tables still have the metrics included.
IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkCvr,BnkStb,fishpvt2,DensPvt,XCMGW_new1,XCMG_new1,XGB_new1,IncBnk,XEMBED,BnkDensPvt,W1_HALL,QR1,MeanAngle,Slope_Per,ThalwegMean,ThalwegSD,Pools),by="UID"),PCT_SAFN_ALL
#To remove all of the metrics and only get the indicators subset by UID and all those columns ending in "CHECK". Hmm..not really sure what the $ is doing here, the code works without it, but all the examples I've looked at keep the $ so I kept it too... 
IndicatorCheck=IndicatorCheckJoin[,c("UID",grep("CHECK$", colnames(IndicatorCheckJoin),value=TRUE))]
#write.csv(IndicatorCheck,"C:\\Users\\Nicole\\Desktop\\IndicatorCheck2.csv")
#Remove all other data files as they are no longer needed

rm(PHfinal,XGB_new,XGB_new1,BankStab,Banks,RipGB,EMBED,Human_Influ,W1_HALL,W1_HALL_NRSA,QR1,XEMBED,BnkDensPvt,BnkDensiom,densiom,RipXCMG,XCMG_new,XCMG_new1,RipWW,XCMGW_new,XCMGW_new1,IndicatorCheckJoin,fish,fishpvt2,
   MidDensiom,DensPvt,Incision,INCISED,BANKHT,Inc,Bnk,xIncht,xBnkht,IncBnk,Sediment,pctsafn,Sed2014,A_Sed2014,C_Sed2014,E_Sed2014,F_Sed2014,PCT_SAFN_ALL)
