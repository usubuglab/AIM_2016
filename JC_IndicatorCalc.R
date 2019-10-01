###################################################################################################################

##############                         Set Up                         #################

###################################################################################################################
#only indicators with "_CHECK" added on the name are in the end output. Once this output is compared to Aquamet change "_CHECK" to  "_FINAL"
#To get the metadata table you must use the SQL code. 
#tblMETADATA= sqlQuery(wrsa1314, "select * from tblMETADATA")
# #############################################################################
# # FIRST Run the settup section of DataConsumption_WRSAdb.R
# # SECOND Run filters for appropriate project, years, protocols, sitecodes, and insertion. For all filter options from all years see DataConsumption_WRSAdb.R
# ####
# #2018 filters
# projects=c('AKEFO','AK_AN_BSWI_2017','AK_CY_PLANUNIT_2017','AK_CY_UTILITYCORRIDOR_2016','AK_GL_STANDARD_2016','CO_FR_STANDARD_2016','CO_SW_STANDARD_2016','CO_NW_STANDARD_2016','NM_SONM_STANDARD_2018','OR_PR_WSR_2018','UT_GR_WSP_2018','WY_RA_STANDARD_2016','UT_CY_STANDARD_2017','UT_WD_SHEEPROCK_2017','WY_HD_STANDARD_2017')
# 
# years=c('2018')#as character, not number
# 
# protocols=c('WADE2016','BOAT2016')
# 
# sitecodes=c('')
# 
# insertion=c('22','23','24')
# 

###################################################################################################################
###################################################################################################################
###################################################################################################################

##############                         Getting All Data                         #################

###################################################################################################################
###################################################################################################################
###################################################################################################################

########################################################################################
##### Site Descriptors #####
########################################################################################
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR','DEWATER','BEAVER_FLOW_MOD','BEAVER_SIGN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#2017+
listsites=setNames(cast(listsites,'UID~PARAMETER',value='RESULT'),c("UID","BEAVER_FLOW_MOD_CHECK","BEAVER_SIGN_CHECK","DATE_COL_CHECK","WATER_WITHDRAWAL_CHECK","LAT_DD_CHECK","LAT_DD_BR_CHECK","LAT_DD_TR_CHECK","LOC_NAME_CHECK","LON_DD_CHECK","LON_DD_BR_CHECK","LON_DD_TR_CHECK","PROJECT_CHECK","PROTOCOL_CHECK","SITE_ID_CHECK",'VALXSITE_CHECK'))
listsites=listsites[,c(15,9,1,4,6,10,13,14,16,7,11,8,12,2,3,5)]
# #listsites=setNames(cast(listsites,'UID~PARAMETER',value='RESULT'),c("UID","DATE_COL_CHECK","LAT_DD_CHECK","LAT_DD_BR_CHECK","LAT_DD_TR_CHECK","LON_DD_CHECK","LON_DD_BR_CHECK","LON_DD_TR_CHECK","PROJECT_CHECK","PROTOCOL_CHECK","SITE_ID_CHECK",'VALXSITE_CHECK'))
# #don't run the last 2 lines for 2016 data because different # of columns...LOC_NAME not in 2016 data...should I join it back in?
# listsites=setNames(cast(listsites,'UID~PARAMETER',value='RESULT'),c("UID","BEAVER_FLOW_MOD_CHECK","BEAVER_SIGN_CHECK","DATE_COL_CHECK","LAT_DD_CHECK","LAT_DD_BR_CHECK","LAT_DD_TR_CHECK","LOC_NAME_CHECK","LON_DD_CHECK","LON_DD_BR_CHECK","LON_DD_TR_CHECK","PROJECT_CHECK","PROTOCOL_CHECK","SITE_ID_CHECK",'VALXSITE_CHECK'))
# #any data with 2016
# listsites=setNames(cast(listsites,'UID~PARAMETER',value='RESULT'),c("UID","BEAVER_FLOW_MOD_CHECK","BEAVER_SIGN_CHECK","DATE_COL_CHECK","WATER_WITHDRAWAL_CHECK","LAT_DD_CHECK","LAT_DD_BR_CHECK","LAT_DD_TR_CHECK","LOC_NAME_CHECK","LON_DD_CHECK","LON_DD_BR_CHECK","LON_DD_TR_CHECK","PROJECT_CHECK","PROTOCOL_CHECK","SITE_ID_CHECK",'VALXSITE_CHECK'))
listsites$PROTOCOL2_CHECK=ifelse(listsites$PROTOCOL=="BOAT14"|listsites$PROTOCOL=="BOAT2016","BOATABLE","WADEABLE")
listsites$FieldStatus_CHECK=ifelse(listsites$VALXSITE_CHECK=="WADEABLE"|listsites$VALXSITE_CHECK=="BOATABLE","Sampled - Full Reach",
                             ifelse(listsites$VALXSITE_CHECK=="PARBYWADE"|listsites$VALXSITE_CHECK=="PARBYBOAT","Sampled - Partial Reach",
                                    ifelse(listsites$VALXSITE_CHECK=="INTWADE","Sampled - Interrupted Flow",
                                    ifelse(listsites$VALXSITE_CHECK=="SUBSETWADE","Sampled - Core Subset",listsites$VALXSITE_CHECK ))))
listsites$IndicatorsCollected_CHECK=listsitesFieldStatus_CHECK=ifelse(listsites$VALXSITE_CHECK=="SUBSETWADE","Core Subset","Core" )

# #listsites=listsites[,c(1,12,6,2,3,7,10,13,11,5,9,4,8)]
# #run list sites and TRCHLEN below to get sinuosity data
TRCHLEN1=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)#not using TRCHLEN

#Side channel info
side=tblRetrieve(Parameters=c('SIDCHN','SIDCH_TYPE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
sidecount=setNames(addKEYS(cast(side,'UID~PARAMETER',value='RESULT'),c('SITE_ID','DATE_COL','PROJECT')),c('UID','NumSideChn_CHECK','NumTran','DATE_COL','PROJECT','SITE_ID'))#count is default function
sidecount$SIDCHN_PRES_CHECK=ifelse(sidecount$NumSideChn_CHECK>0,"PRESENT","ABSENT")


##########################################################################################
###### Biodiversity and Riparian Habitat Quality ######
##########################################################################################
#xcdenmid and xcdenbk - canopy cover
densiom=tblRetrieve(Parameters='DENSIOM',Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#unique(densiom$RESULT)
#densiompvt=cast(densiom,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(densiom$POINT)

#XCMG - vegetative complexity
#RipALL=tblRetrieve(Parameters=c("BARE","CANBTRE","CANSTRE","CANVEG","GCNWDY","GCWDY","UNDERVEG","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#unique(RipALL$RESULT)
RipXCMG=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCNWDY","GCWDY","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#unique(RipXCMG$RESULT)
RipWW=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
RipGB=tblRetrieve(Parameters=c("BARE"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#RipAllpvt=cast(RipALL,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(RipAllpvt$TRANSECT)

#BLM riparian cover and frequency
RipBLM=tblRetrieve(Parameters=c('CANRIPW','UNRIPW','GCRIP','INVASW', 'NATIVW','INVASH','NATIVH','SEGRUSH','INVASAQ'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#pvtRipBLM=cast(RipBLM,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')

#Bug data
#bug metrics are calculated outside of this script and joined back in
#bug metric data can be found at "Z:\\buglab\\Research Projects\\AIM\\Analysis\\WQ_Bug_Model_GIS_stats_and_results\\final_bug_scores_R_input_join_to_all_other_metrics_statebasedscores.xlsx"
#an SQL query to get the presence or absence of invasives can be found at "Z:\buglab\Research Projects\BLM_WRSA_Stream_Surveys\Results and Reports\AIM_2011_2015_results\invasives_query.sql"



###############################################################################################
###### Water Quality ######
###############################################################################################
#All WQ data
WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','NTL','PTL','TURBIDITY','TEMPERATURE','EC_PRED','TN_PRED','TP_PRED'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')
#WQfinal=WQpvt



###############################################################################################
##### Watershed Function and Instream Habitat Quality ######
###############################################################################################
#Pools
pool_length=tblRetrieve(Parameters=c('LENGTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
reach_length=tblRetrieve(Parameters=c('POOLRCHLEN'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
PoolDepth=tblRetrieve(Parameters=c('PTAILDEP','MAXDEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
poolcollect=tblRetrieve(Parameters='POOL_COLLECT',Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)

#LWD- should query wadeable and boatable wood because boatable wood stored under wadeable?
LwdCatWet=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'W%'"))$PARAMETER
LwdCatDry=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'D%'"))$PARAMETER
LwdWet=addKEYS(tblRetrieve(Parameters=LwdCatWet,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','DATE_COL'))
LwdDry=tblRetrieve(Parameters=LwdCatDry,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#pvtLwdWet=cast(LwdWet, 'UID~TRANSECT+PARAMETER',value='RESULT')
#pvtLwdDry=cast(LwdDry,'UID~TRANSECT+PARAMETER',value='RESULT')
#pvtLwd=merge(pvtLwdWet,pvtLwdDry, by='UID')
TRCHLEN=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)#not using TRCHLEN
TRCHLEN1=cast(TRCHLEN,'UID~PARAMETER',value='RESULT')
#TRCHLEN is not the same as the reachlen used in Aquamet
#The reachlen is calc from mulitplying INCREMENT by the thalweg stations
#We need to estimate the intended number of wadeable thalweg stations at each transect
#which are considered sampled (even if there is no data) for the purposes of
#calculating residual pools and channel lengths.  The number of stations at
#a transect is calculated as the greater of either the number of stations
#occuring in the dataframe for that transect, or the most common count of
#stations (i.e. station mode) occuring at that site. 

#pct_safn and other sediment metrics
Sediment=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#unique(Sediment$RESULT)
#Sedimentpvt=cast(Sediment,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(Sedimentpvt$TRANSECT)# check data structure
#Sed2014pvt=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
#unique(Sed2014pvt$POINT)# check data structure
#unique(Sed2014pvt$LOC)
#min(Sed2014pvt$SIZE_NUM);max(Sed2014pvt$SIZE_NUM)
#WR_Sed2014=tblRetrieve(Parameters=c('SIZE_NUM'),Projects=projects,Years=years,Protocols=protocols)

#Pool Tail Fines
PoolFines=tblRetrieve(Parameters=c('POOLFINES2','POOLFINES6','POOLNOMEAS'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)

#Bank Stability
BankStab=tblRetrieve(Parameters=c('STABLE','EROSION','COVER_FOLIAR','COVER_BASAL','BNK_VEG_FOLIAR','BNK_VEG_BASAL','BNK_COBBLE','BNK_LWD','BNK_BEDROCK'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
SideBank=tblRetrieve(Parameters=c('SIDCHN_BNK'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#unique(BankStab$RESULT)
#BankStabpvt=addKEYS(cast(BankStab,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
#unique(BankStab$POINT)
#unique(BankStab$TRANSECT)
#BankStabCoverClass=tblRetrieve(Parameters=c('BNK_VEG','BNK_COBBLE','BNK_LWD','BNK_BEDROCK'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#pvtBankStabCoverClass=addKEYS(cast(BankStabCoverClass,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))

#LINCIS_H - floodplain connectivity
Incision=tblRetrieve(Parameters=c('INCISED','BANKHT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#min(Incision$RESULT);max(Incision$RESULT)
#incisionpvt=cast(Incision,'UID+TRANSECT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates

#XFC_NAT- fish cover
fish=tblRetrieve(Parameters=c('BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#unique(fish$RESULT)# check data structure
#fishpvt=cast(fish,'UID+TRANSECT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates

#Angle-PIBO method only
Angle=tblRetrieve(Parameters=c('ANGLE180'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#Anglepvt=cast(Angle,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#unique(Anglepvt$TRANSECT)
#min(Angle$RESULT);max(Angle$RESULT)

#Thalweg mean , CV, and pct dry
thalweg=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROTOCOL'))
thalweg=subset(thalweg,SAMPLE_TYPE!='CROSSSECW')
#thalwegpvt=cast(thalweg,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#unique(thalwegpvt$POINT)
#max(thalwegpvt$DEPTH);min(thalwegpvt$DEPTH)

#RP100 and other EPA pool metrics - see seperate EPA pools script


#########################################################################################################
###### Covariates/ Other ######
#########################################################################################################
#Channel Dimensions
WetWid=tblRetrieve(Parameters=c('WETWIDTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)#Wetted widths from thalweg
WetWid2=tblRetrieve(Parameters=c('WETWID'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)#Wetted widths from main transects
BankWid=tblRetrieve(Parameters=c('BANKWID'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
#WetWidpvt=cast(WetWid,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#WetWid2pvt=cast(WetWid2,'UID+TRANSECT~PARAMETER',value='RESULT')
#BankWidpvt=cast(BankWid,'UID+TRANSECT~PARAMETER',value='RESULT')

#Floodprone width
#pre2017
#FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
#2017 plus
FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID','FLOOD_BFWIDTH'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
#FloodWidthpvt=cast(FloodWidth,'UID+TRANSECT~PARAMETER',value='RESULT')
#Slope
#2017
Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','PCT_GRADE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)                 

# Slope_height=tblRetrieve(Parameters=c('SLOPE'), Projects=projects, Years=c('2013','2014','2015'),Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# SlpReachLen=tblRetrieve(Parameters=c('SLPRCHLEN'), Projects=projects, Years=c('2013','2014','2015'),Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# #Slope_heightpvt=cast(Slope_height,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# #SlpReachLenpvt=cast(SlpReachLen,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','PCT_GRADE'),Projects=projects, Years=c('2016'),Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)                 

#run site descriptors for sinuosity data



##########################################################################################################
##### Other metrics still being worked on or old metrics no longer used #####
##########################################################################################################
# #W1_HALL- human influence
# #Figure out the differences... Human influence sample type...
# Human_Influ=tblRetrieve(Parameters=c('BUILD','LOG','MINE','PARK','PAST','PAVE','PIPES','ROAD','ROW','TRASH','WALL'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)                       
# #Human_Influpvt=cast(Human_Influ,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# #unique(Human_Influ$RESULT)
# #unique(Human_Influpvt$POINT)
# 
# #QR1
# #run densiom, Human_Influ, and RipWW to get the data. Code to calculate QR1 is in NC_DataAnalysis file
# 
# #XEMBED - embeddedness
# EMBED=tblRetrieve(Parameters='EMBED', Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
# #EMBEDpvt=cast(EMBED,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# #unique(EMBED$RESULT)
# #unique(EMBED$TRANSECT)
# #unique(EMBED$POINT)
# 

### Getting Data to calculate Indicators Stops here

###################################################################################################################
###################################################################################################################
###################################################################################################################

##############          Biodiversity and Riparian Habitat Quality       #################

###################################################################################################################
###################################################################################################################
###################################################################################################################

############################################

#          Canopy Cover Indicators        #

############################################

#xcdenmid -pct overhead cover
##just averages across side channels
MidDensiom = subset(densiom, POINT == "CU"|POINT =="CD"|POINT == "CL"|POINT == "CR")
DensPvt=cast(MidDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
DensPvt$XCDENMID_CHECK=round((DensPvt$DENSIOM/17)*100,digits=2)
#Trying to figure out what is going on with UID 11802.
#Dens_Pvt3=cast(MidDens3,'UID+TRANSECT~PARAMETER',value='RESULT',fun=mean)
nDensPvt=setNames(plyr::count(MidDensiom,"UID"),c("UID","nXCDENMID_CHECK"))#should be 4 locations at 11 transects=44 so half is 4 * 5transects
DensPvt=merge(nDensPvt,DensPvt,by="UID")
DensPvt$XCDENMID_CHECK=ifelse(DensPvt$nXCDENMID_CHECK<20,NA,DensPvt$XCDENMID_CHECK)
DensPvt$XCDENMID_CHECK=round(DensPvt$XCDENMID_CHECK,digits=1)

#xcdenbk -bank overhead cover ######changed calculation of boating sites in 2017 to include all 4 measurements taken at a bank
##just averages across sidechannels
BnkDensiom = subset(densiom, POINT == "LF"|POINT =="RT"|POINT=="UP"|POINT=="DN")
BnkDensPvt=cast(BnkDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
BnkDensPvt$XCDENBK_CHECK=round((BnkDensPvt$DENSIOM/17)*100,digits=2)
nBnkDensPvt=setNames(plyr::count(BnkDensiom,"UID"),c("UID","nXCDENBK_CHECK"))# should be 2 locations at 11 transects=22 so half is 2 * 5 transects
BnkDensPvt=merge(nBnkDensPvt,BnkDensPvt,by="UID")
BnkDensPvt$XCDENBK_CHECK=ifelse(BnkDensPvt$nXCDENBK_CHECK<10,NA,BnkDensPvt$XCDENBK_CHECK)
BnkDensPvt$XCDENBK_CHECK=round(BnkDensPvt$XCDENBK_CHECK,digits=1)

#########################################################

#          Veg and Riparian Complexity Indicators       #

#########################################################

# xcmg - vegative complexity
RipXCMG$ResultsPer=ifelse(RipXCMG$RESULT == 1, 0.05,ifelse(RipXCMG$RESULT == 2, 0.25,ifelse(RipXCMG$RESULT == 3, 0.575,ifelse(RipXCMG$RESULT == 4, 0.875,ifelse(RipXCMG$RESULT ==0, 0, NA)))))
XCMG_new=setNames(cast(RipXCMG,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',  'VALUE'))
XCMG_new1=setNames(aggregate(VALUE~UID,data=XCMG_new,FUN=mean),list("UID","XCMG_CHECK"))
XCMG_new1$XCMG_CHECK=round(XCMG_new1$XCMG_CHECK,digits=2)
#sample sizes
nXCMG_new=setNames(plyr::count(RipXCMG,"UID"),c("UID","nXCMG_CHECK"))#6 strata(canopy-big trees, small trees,understory- woody, nonwoody,groundcove- woody, nonwoody) *2 banks*11 transects=132 so half data = 6*2* 5 transects=60
XCMG_new1=merge(nXCMG_new,XCMG_new1, by="UID")
XCMG_new1$XCMG_CHECK=ifelse(XCMG_new1$nXCMG_CHECK<60,NA,XCMG_new1$XCMG_CHECK)#5 sites 3 WRSA 54-60


######## other EPA veg complexity variations ###########
# #RipGB
# RipGB$ResultsPer=ifelse(RipGB$RESULT == 1, 0.05,ifelse(RipGB$RESULT == 2, 0.25,ifelse(RipGB$RESULT == 3, 0.575,ifelse(RipGB$RESULT == 4, 0.875,ifelse(RipGB$RESULT ==0, 0, NA)))))
# XGB_new=setNames(cast(RipGB,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',  'VALUE'))
# XGB_new1=setNames(aggregate(VALUE~UID,data=XGB_new,FUN=mean),list("UID","XGB_CHECK"))
# 
# #xcmgw
# #xcmgw=XC+XMW+XGW: However, this is not how aquamet is calculating it, the order of operation would give different results if XC was caluclated and then added to XMW and XMG
# #More true to aquamet calculation: XCMG=XCL+XCS+XMW+XGW 
# #Need to just calculate it by transect side first then average at an entire site. 
# ##XC=XCL+XCS (Small Canopy trees (CANSTRE) + Large Canopy trees(CANBTRE))  
# ##XMW=Understory woody aka UNDWDY
# ##MGW= ground cover woody GCWDY
# RipWW$ResultsPer=ifelse(RipWW$RESULT == 1, 0.05,ifelse(RipWW$RESULT == 2, 0.25,ifelse(RipWW$RESULT == 3, 0.575,ifelse(RipWW$RESULT == 4, 0.875,ifelse(RipWW$RESULT ==0, 0, NA)))))
# XCMGW_new=setNames(cast(RipWW,'UID+TRANSECT+POINT~ACTIVE', value='ResultsPer',fun='sum'),list('UID',  'TRANSECT',  'POINT',  'VALUE'))
# XCMGW_new1=setNames(aggregate(VALUE~UID,data=XCMGW_new,FUN=mean),list("UID","XCMGW_CHECK"))
# 
# #L_XCMGW
# XCMGW_new1$L_XCMGW_CHECK=log10(XCMGW_new1$XCMGW_CHECK)
#########################################################

##BLM riparian cover and frequency##
#Riparian vegetation cover
RIP_VEG=subset(RipBLM, PARAMETER == 'CANRIPW'|PARAMETER == 'UNRIPW'|PARAMETER == 'GCRIP')
RIP_VEG$ResultsPer=ifelse(RIP_VEG$RESULT == 1, 0.05,ifelse(RIP_VEG$RESULT == 2, 0.25,ifelse(RIP_VEG$RESULT == 3, 0.575,ifelse(RIP_VEG$RESULT == 4, 0.875,ifelse(RIP_VEG$RESULT ==0, 0, NA)))))
nRIP_VEG=setNames(plyr::count(RIP_VEG,c("UID",'PARAMETER')),c("UID",'PARAMETER',"nRIP_VEG_CHECK"))#
nRIP_VEG=setNames(cast(nRIP_VEG,"UID~PARAMETER",value="nRIP_VEG_CHECK",fun="sum"),c('UID','nCANRIPW_CHECK','nUNRIPW_CHECK', 'nGCRIP_CHECK'))
RIP_VEG=setNames(cast(RIP_VEG,'UID~PARAMETER', value='ResultsPer',fun='mean'),c('UID','CANRIPW_CHECK','UNRIPW_CHECK', 'GCRIP_CHECK'))
RIP_VEG=merge(nRIP_VEG,RIP_VEG,by="UID")
RIP_VEG$CANRIPW_CHECK=round(RIP_VEG$CANRIPW_CHECK,digits=2)
RIP_VEG$UNRIPW_CHECK=round(RIP_VEG$UNRIPW_CHECK,digits=2)
RIP_VEG$GCRIP_CHECK=round(RIP_VEG$GCRIP_CHECK,digits=2)
RIP_VEG$CANRIPW_CHECK=ifelse(RIP_VEG$nCANRIPW_CHECK<10,NA,RIP_VEG$CANRIPW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
RIP_VEG$UNRIPW_CHECK=ifelse(RIP_VEG$nUNRIPW_CHECK<10,NA,RIP_VEG$UNRIPW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
RIP_VEG$GCRIP_CHECK=ifelse(RIP_VEG$nGCRIP_CHECK<10,NA,RIP_VEG$GCRIP_CHECK)#total=22,but collected at 5 transects=10, so min N=10

#Riparian vegetation frequency
# #Works!!!Try to take NA out of quotes and you may not need as.numeric
# #pre2019 data
# FQCY_VEG=subset(RipBLM, PARAMETER == 'INVASW'|PARAMETER == 'NATIVW'|PARAMETER == 'INVASH'|PARAMETER == 'NATIVH'|
#                   PARAMETER == 'SEGRUSH')
# nFQCY_VEG=setNames(plyr::count(FQCY_VEG,c("UID",'PARAMETER')),c("UID",'PARAMETER',"nFQCY_VEG_CHECK"))#total=22,but collected at 5 transects=10, so min N=10
# nFQCY_VEG=cast(nFQCY_VEG,"UID~PARAMETER",value="nFQCY_VEG_CHECK",fun="sum")
# FQCY_VEG$RESULT_A=as.numeric(ifelse(FQCY_VEG$RESULT == 'N', 0,ifelse(FQCY_VEG$RESULT == 'Y', 100,"NA")))
# FQCY_VEG=cast(FQCY_VEG,'UID~PARAMETER',value='RESULT_A',fun=mean)
# FQCY_VEG=setNames(merge(nFQCY_VEG,FQCY_VEG,by="UID"),c("UID","nINVASH_CHECK","nINVASW_CHECK","nNATIVH_CHECK","nNATIVW_CHECK","nSEGRUSH_CHECK","INVASH_CHECK","INVASW_CHECK","NATIVH_CHECK","NATIVW_CHECK","SEGRUSH_CHECK"))                  
# FQCY_VEG$INVASW_CHECK=round(FQCY_VEG$INVASW_CHECK,digits=0)
# FQCY_VEG$INVASH_CHECK=round(FQCY_VEG$INVASH_CHECK,digits=0)
# FQCY_VEG$NATIVH_CHECK=round(FQCY_VEG$NATIVH_CHECK,digits=0)
# FQCY_VEG$NATIVW_CHECK=round(FQCY_VEG$NATIVW_CHECK,digits=0)
# FQCY_VEG$SEGRUSH_CHECK=round(FQCY_VEG$SEGRUSH_CHECK,digits=0)
# FQCY_VEG$INVASW_CHECK=ifelse(FQCY_VEG$nINVASW_CHECK<10,NA,FQCY_VEG$INVASW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
# FQCY_VEG$INVASH_CHECK=ifelse(FQCY_VEG$nINVASH_CHECK<10,NA,FQCY_VEG$INVASH_CHECK)#total=22,but collected at 5 transects=10, so min N=10
# FQCY_VEG$NATIVH_CHECK=ifelse(FQCY_VEG$nNATIVH_CHECK<10,NA,FQCY_VEG$NATIVH_CHECK)#total=22,but collected at 5 transects=10, so min N=10
# FQCY_VEG$NATIVW_CHECK=ifelse(FQCY_VEG$nNATIVW_CHECK<10,NA,FQCY_VEG$NATIVW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
# FQCY_VEG$SEGRUSH_CHECK=ifelse(FQCY_VEG$nSEGRUSH_CHECK<10,NA,FQCY_VEG$SEGRUSH_CHECK)#total=22,but collected at 5 transects=10, so min N=10

#2019 data
FQCY_VEG=subset(RipBLM, PARAMETER == 'INVASW'|PARAMETER == 'NATIVW'|PARAMETER == 'INVASH'|PARAMETER == 'NATIVH'|
                  PARAMETER == 'SEGRUSH'|PARAMETER=='INVASAQ')
nFQCY_VEG=setNames(plyr::count(FQCY_VEG,c("UID",'PARAMETER')),c("UID",'PARAMETER',"nFQCY_VEG_CHECK"))#total=22,but collected at 5 transects=10, so min N=10
nFQCY_VEG=cast(nFQCY_VEG,"UID~PARAMETER",value="nFQCY_VEG_CHECK",fun="sum")
FQCY_VEG$RESULT_A=as.numeric(ifelse(FQCY_VEG$RESULT == 'N', 0,ifelse(FQCY_VEG$RESULT == 'Y', 100,"NA")))
FQCY_VEG=cast(FQCY_VEG,'UID~PARAMETER',value='RESULT_A',fun=mean)
FQCY_VEG=setNames(merge(nFQCY_VEG,FQCY_VEG,by="UID"),c("UID","nINVASAQ_CHECK","nINVASH_CHECK","nINVASW_CHECK","nNATIVH_CHECK","nNATIVW_CHECK","nSEGRUSH_CHECK","INVASAQ_CHECK","INVASH_CHECK","INVASW_CHECK","NATIVH_CHECK","NATIVW_CHECK","SEGRUSH_CHECK"))                  
FQCY_VEG$INVASAQ_CHECK=round(FQCY_VEG$INVASAQ_CHECK,digits=0)
FQCY_VEG$INVASW_CHECK=round(FQCY_VEG$INVASW_CHECK,digits=0)
FQCY_VEG$INVASH_CHECK=round(FQCY_VEG$INVASH_CHECK,digits=0)
FQCY_VEG$NATIVH_CHECK=round(FQCY_VEG$NATIVH_CHECK,digits=0)
FQCY_VEG$NATIVW_CHECK=round(FQCY_VEG$NATIVW_CHECK,digits=0)
FQCY_VEG$SEGRUSH_CHECK=round(FQCY_VEG$SEGRUSH_CHECK,digits=0)
FQCY_VEG$INVASAQ_CHECK=ifelse(FQCY_VEG$nINVASAQ_CHECK<10,NA,FQCY_VEG$INVASAQ_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$INVASW_CHECK=ifelse(FQCY_VEG$nINVASW_CHECK<10,NA,FQCY_VEG$INVASW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$INVASH_CHECK=ifelse(FQCY_VEG$nINVASH_CHECK<10,NA,FQCY_VEG$INVASH_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$NATIVH_CHECK=ifelse(FQCY_VEG$nNATIVH_CHECK<10,NA,FQCY_VEG$NATIVH_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$NATIVW_CHECK=ifelse(FQCY_VEG$nNATIVW_CHECK<10,NA,FQCY_VEG$NATIVW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$SEGRUSH_CHECK=ifelse(FQCY_VEG$nSEGRUSH_CHECK<10,NA,FQCY_VEG$SEGRUSH_CHECK)#total=22,but collected at 5 transects=10, so min N=10


###################################################################################################################
###################################################################################################################
###################################################################################################################

##############               Water   Quality             #################

###################################################################################################################
###################################################################################################################
###################################################################################################################

#At the end, all columns with "Check" at the end are included in the main file
WQpvt$CONDUCTIVITY_CHECK=round(WQpvt$CONDUCTIVITY,digits=2)
WQpvt$PTL_CHECK=round(WQpvt$PTL,digits=1)
WQpvt$NTL_CHECK=round(WQpvt$NTL,digits=1)
WQpvt$TN_PRED_CHECK=round(WQpvt$TN_PRED,digits=1)
WQpvt$TP_PRED_CHECK=round(WQpvt$TP_PRED,digits=1)
WQpvt$EC_PRED_CHECK=round(WQpvt$EC_PRED,digits=1)
WQpvt$OE_EC_CHECK=round(WQpvt$CONDUCTIVITY-WQpvt$EC_PRED,digits=2)
WQpvt$OE_TN_CHECK=round(WQpvt$NTL-WQpvt$TN_PRED,digits=1)
WQpvt$OE_TP_CHECK=round(WQpvt$PTL-WQpvt$TP_PRED,digits=1)
WQpvt$TURBIDITY_CHECK=round(WQpvt$TURBIDITY,digits=2)
WQpvt$PH_CHECK=WQpvt$PH
WQpvt$TEMPERATURE_CHECK=WQpvt$TEMPERATURE
WQfinal=WQpvt
#WQfinal=setNames(WQpvt,c("UID","CONDUCTIVITY_CHECK","EC_PRED_CHECK","NTL_CHECK","PH_CHECK","PTL_CHECK","TEMPERATURE_CHECK","TN_PRED_CHECK","TP_PRED_CHECK","OE_EC_CHECK","OE_TN_CHECK","OE_TP_CHECK","TURBIDITY_CHECK"))
#WQfinal=setNames(WQpvt,c("UID","CONDUCTIVITY_CHECK","EC_PRED_CHECK","NTL_CHECK","PH_CHECK","PTL_CHECK","TEMPERATURE_CHECK","TN_PRED_CHECK","TP_PRED_CHECK","OE_EC_CHECK","OE_TN_CHECK","OE_TP_CHECK"),"TURBIDITY_CHECK")                              
#WQfinal=WQfinal[,c(1,2,3,11,6,9,13,4,8,12,5,7,10)]
#If no turbidity data, the sitecode will appear in the TURBIDITY_CHECK column because SiteCode was pulled when the data was pulled from the database. 
#WQfinal=WQfinal[,c(1,12,13,11,14,10,15)]
###################################################################################################################
###################################################################################################################
###################################################################################################################

##############               Watershed Function and Instream Habitat Quality             #################

###################################################################################################################
###################################################################################################################
###################################################################################################################

#########################################################

#                 PIBO Pool Indicators                  #

#########################################################

#Percent pools
pool_length$RESULT=as.numeric(pool_length$RESULT)
pvtpools1=cast(pool_length,'UID~PARAMETER',value='RESULT',fun=sum) 
pvtpools2=cast(reach_length,'UID~PARAMETER',value='RESULT') 
poolsmerge=merge(pvtpools1,pvtpools2,by=c('UID'),all=T)
poolsmerge$PoolPct=round((poolsmerge$LENGTH/poolsmerge$POOLRCHLEN)*100,digits=2)

#residual pool depth
PoolDepth=cast(PoolDepth,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
PoolDepth$RPD=(PoolDepth$MAXDEPTH-PoolDepth$PTAILDEP)/100# convert from cm to m
RPD=setNames(aggregate(PoolDepth$RPD,list(UID=PoolDepth$UID),mean),c("UID","RPD"))#converted to m
RPD$RPD=round(RPD$RPD,digits=2)

#combine all pool metrics and add pool frequency
count=setNames(plyr::count(PoolDepth,"UID"),c("UID","NumPools"))
poolmerge2=join_all(list(poolsmerge,count,RPD), by="UID")
poolmerge2$PoolFrq=round((poolmerge2$NumPools/poolmerge2$POOLRCHLEN)*1000,digits=1)###need to consider what reach length to use here #may need shorted lengths for parial reaches#change to use new parameter POOLRCHLEN

#properly 0 out data if there were no pools and properly designate pools as NA if no flow
pvtpoolcollect=cast(poolcollect,'UID~PARAMETER',value='RESULT')
poolmerge2=merge(poolmerge2,pvtpoolcollect,by=c('UID'),all=T)
poolmerge2$PoolPct=ifelse(poolmerge2$POOL_COLLECT=='NP',0,ifelse(poolmerge2$POOL_COLLECT=='NF',NA,poolmerge2$PoolPct))
poolmerge2$PoolFrq=ifelse(poolmerge2$POOL_COLLECT=='NP',0,ifelse(poolmerge2$POOL_COLLECT=='NF',NA,poolmerge2$PoolFrq))
poolmerge2$RPD=ifelse(poolmerge2$POOL_COLLECT=='NP',NA,ifelse(poolmerge2$POOL_COLLECT=='NF',NA,poolmerge2$RPD)) 
poolmerge2$NumPools=ifelse(poolmerge2$POOL_COLLECT=='NP',0,ifelse(poolmerge2$POOL_COLLECT=='NF',NA,poolmerge2$NumPools)) 
Pools=setNames(subset(poolmerge2,select=c(UID,PoolPct,RPD,PoolFrq,NumPools)),c("UID","PoolPct_CHECK","RPD_CHECK","PoolFrq_CHECK","NumPools_CHECK"))


#########################################################

#                    LWD Indicators                     #

#########################################################
#C1WM100- (Cummulative count of LWD in bankfull channel across all size classes)/(Reach Length) units are pieces/100m
LwdWet$TRANSECT=mapvalues(LwdWet$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
LWD_test=setNames(aggregate(RESULT~UID+TRANSECT,data=LwdWet,FUN=sum),c("UID","TRANSECT","C1W"))# count of all LWD pieces per site
LWD_test2=setNames(plyr::count(LWD_test,"UID"),c("UID","NUMTRAN"))# count of the number of transects that wood was collected for #may require package plyr which requires R version > 3????? but I have also gotten count to work in other situations with other version of R required for aquamet
LWD=setNames(aggregate(RESULT~UID,data=LwdWet,FUN=sum),c("UID","C1W"))# count of all LWD pieces per site
LWD=merge(LWD_test2,LWD,by=c('UID'),all=T)
LWD=merge(LWD,TRCHLEN1,by=c('UID'), all=T)
#To get the reach length for which LWD was accesssed divide the total reach length by 10 to get the transect spacing and then multiply times the number of LWD transects sampled
#This is different than the EPA's reach length. The EPA determines reach length by approximating the number of intended thalweg stations
#They take the greater of either the max number of stations occurring at each transect or the station "mode" occurring at a site
#This seems overly complex....particularly the mode part why not get an accurate number of stations per transect? 
#Our method could over estimate wood though...crews may have evaluated sections of the thalweg but not recorded a wood value because they forgot or something else
#However not all crews collecting thalweg in future and may not be able to get thalweg depths where you could get wood...we probably could use another parameter that is collected at all thalweg stations (side channel presence- but did not collect this in 2016) 
LWD$LWD_RCHLEN=(LWD$TRCHLEN/10)*LWD$NUMTRAN 
LWD$C1WM100_CHECK=round((LWD$C1W/LWD$LWD_RCHLEN)*100,digits=3)# get the pieces/100m
#to exclude data that has less than 50% run volume code and code below


#V1WM100
#MUST run C1WM100 code prior to this code so that the RCHLEN (reach length) code works. 
#Duplications of size classes below are so that the table works with sites sampled with the 2013 protocol as well as the 2014 and later protocol 
#Because the Boatable and Wadeable sites have the same PARAMETER name but different size classes, To distinquish I've added "B_" in front of the boatable site PARAMETER. 
#All categories match our metadata table. 
#Based on Jawson Law's Code and communication with Phil Kauffman I have assigned a max or upper end value to those categories which were >some number

LWDtt <- textConnection(
  "PARAMETER  Min_DIAMETER  Max_DIAMETER	Min_LENGTH	Max_LENGTH
  B_WLDLL	0.8	1	30	75
  B_WLDML	0.8	1	15	30
  B_WLDSL	0.8	1	5	15
  B_WMDLL	0.6	0.8	30	75
  B_WMDML	0.6	0.8	15	30
  B_WMDSL	0.6	0.8	5	15
  B_WSDLL	0.3	0.6	30	75
  B_WSDML	0.3	0.6	15	30
  B_WSDSL	0.3	0.6	5	15
  B_WXDLL	1	2	30	75
  B_WXDML	1	2	15	30
  B_WXDSL	1	2	5	15
  WLDLL	0.6	0.8	15	30
  WLDML	0.6	0.8	5	15
  WLDSL	0.6	0.8	1.5	5
  WMDLL	0.3	0.6	15	30
  WMDML	0.3	0.6	5	15
  WMDSL	0.3	0.6	1.5	5
  WSDLL	0.1	0.3	15	30
  WSDML	0.1	0.3	5	15
  WSDSL	0.1	0.3	1.5	5
  WXDLL	0.8	2	15	30
  WXDML	0.8	2	5	15
  WXDSL	0.8	2	1.5	5
  WLDSML	0.6	0.8	3	5
  WMDSML	0.3	0.6	3	5
  WSDSML	0.1	0.3	3	5
  WXDSML	0.8	2	3	5
  WSDSSL	0.1	0.3	1.5	3
  WXDSSL	0.8	2	1.5	3
  WMDSSL	0.3	0.6	1.5	3
  WLDSSL	0.6	0.8	1.5	3"
)


#Creates a table of the volume value that corresponds to the size category.
LWD_sizes <- read.table(LWDtt, header = TRUE, stringsAsFactors = FALSE)
close(LWDtt)
#Equation found in Kauffman 99 (pg 31 of Kauffman 1999 and he cites Robison 1998) is wrong, first noticed because it does not produce values equal to those found in aquamet. Jason Law has the correct code, confirmed by Phil Kauffman and also produces the same values found in Aquamet. Aquamet code only uses calculated values from the equation but does not contain the equation itself
LWD_sizes$VOLUME=pi*((0.5*(LWD_sizes$Min_DIAMETER+((LWD_sizes$Max_DIAMETER-LWD_sizes$Min_DIAMETER)/3)))^2)*(LWD_sizes$Min_LENGTH+((LWD_sizes$Max_LENGTH-LWD_sizes$Min_LENGTH)/3))

LwdWet$PARAMETER=ifelse(LwdWet$SAMPLE_TYPE=="LWDB",sprintf('%s%s',"B_",LwdWet$PARAMETER),LwdWet$PARAMETER)#Adds a "B_" to all boatable parameters so that we can run the same code for everything. 
LWDvol1=join(LwdWet,LWD_sizes, by='PARAMETER') #Appends the correct volume value to size class in the table of data
LWDvol1$VOLcalc=LWDvol1$VOLUME*LWDvol1$RESULT #Get the overall volume of wood for each size class
LWDvolume=setNames(cast(LWDvol1,"UID+DATE_COL+SAMPLE_TYPE~ACTIVE", value="VOLcalc", fun="sum"),c('UID','DATE_COL','SAMPLE_TYPE','LWDvol')) #Pivots table to get the overall volume of wood for the entire reach. Keep Date so we can set different sample size requirements for different years
LWDvolume=merge(LWDvolume,subset(LWD, select=c(UID,LWD_RCHLEN),all=TRUE), by="UID",all=TRUE)#Need to run code within lwd count code before this will work
LWDvolume$V1WM100=(LWDvolume$LWDvol/LWDvolume$LWD_RCHLEN)*100
nLWD=setNames(plyr::count(LwdWet,"UID"),c("UID","nLWD"))#Calculates sample size for each site
LWDvolume$YEAR=format(as.Date(LWDvolume$DATE_COL,'%m/%d/%Y'),'%Y') #Separates year from the date as an easy way to apply apply the sample size needed code below since we had different samplke sizes in different years.
LWDvolume=merge(nLWD,LWDvolume,by="UID",all=TRUE) #Merge sample sizes to data file by UID
#Excludes all site values if they do not meet the minumum sample size required:
LWDvolume$V1WM100_CHECK=ifelse(LWDvolume$nLWD>=64&LWDvolume$YEAR>='2014'&LWDvolume$SAMPLE_TYPE!='LWDB',LWDvolume$V1WM100,# For wadeable sites collected in 2014 or later, total data points for entire reach=160 (16 per transect, 10 transects), 5 transects of data would be 80, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 64
                               ifelse(LWDvolume$nLWD>=48&LWDvolume$YEAR<'2014'&LWDvolume$SAMPLE_TYPE!='LWDB',LWDvolume$V1WM100,# For wadeable sites collected in 2013, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
                                      ifelse(LWDvolume$nLWD>=48&LWDvolume$SAMPLE_TYPE=='LWDB',LWDvolume$V1WM100,NA))) # For boatable sites collected in any year, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48


#merging all LWD stuff and excluding lwd count that dont have 50% of data
LWD=merge(LWD,LWDvolume,by="UID")
LWD$C1WM100_CHECK=ifelse(LWD$nLWD>=64&LWD$YEAR>='2014'&LWD$SAMPLE_TYPE!='LWDB',LWD$C1WM100,# For wadeable sites collected in 2014 or later, total data points for entire reach=160 (16 per transect, 10 transects), 5 transects of data would be 80, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 64
                         ifelse(LWD$nLWD>=48&LWD$YEAR<'2014'&LWD$SAMPLE_TYPE!='LWDB',LWD$C1WM100,# For wadeable sites collected in 2013, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
                                ifelse(LWD$nLWD>=48&LWD$SAMPLE_TYPE=='LWDB',LWD$C1WM100,NA))) # For boatable sites collected in any year, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
LWD$V1WM100_CHECK=round(LWD$V1WM100_CHECK,digits=3)

#The if statement above was not working, so I started developing this code, in the process I fixed the if statement above....
#LWDvolumeBoat=subset(LWDvolume,SAMPLE_TYPE=='LWDB')
#LWDvolume2014beyond=subset((LWDvolume,SAMPLE_TYPE=='LWDW'&YEAR>=2014))
#LWDvolume2013before=subset((LWDvolume,SAMPLE_TYPE=='LWDW'&YEAR<2014))

#LWDvolumeBoat$V1WM100_CHECK= ifelse(LWDvolumeBoat$nLWD>=48,LWDvolumeBoat$V1WM100,'NA')
#LWDvolume2014beyond$V1WM100_CHECK=ifelse(LWDvolume2014beyond$nLWD>=64,LWDvolumeBoat$V1WM100,'NA')
#LWDvolume2013before$V1WM100_CHECK=ifelse(LWDvolume2013before$nLWD>=48,LWDvolumeBoat$V1WM100,'NA')

#LWDvolume=rbind(LWDvolumeBoat,LWDvolume2014beyond,LWDvolume2013before)

# XwalkUnion=Xwalk(XwalkName='Aquamet1',Table='XwalkUnion',Source='R',XwalkDirection='')#!need to formally omit unused parameters and track down unknowns to see how they are used in aquamet (i.e. Assessment, etc)
# #collapse LWD
# XwalkLWDsub=subset(XwalkUnion,PARAMETER %in% c('DXDSL','DSDSL','DMDSL','DLDSL','WXDSL','WSDSL','WMDSL','WLDSL'));XwalkLWDsub$RESULT=as.numeric(XwalkLWDsub$RESULT);
# XwalkLWDagg=data.frame(cast(XwalkLWDsub,'UID+TRANSECT+POINT+TABLE+SAMPLE_TYPE+PARAMETER ~ .',value='RESULT',fun.aggregate=sum));XwalkLWDagg$RESULT=XwalkLWDagg$X.all.;XwalkLWDagg=ColCheck(XwalkLWDagg,colnames(XwalkUnion))#  228284433826712128 B
# XwalkNOlwd=subset(XwalkUnion,(IND %in% XwalkLWDsub$IND)==FALSE)
# XwalkUnion=rbind(XwalkLWDagg,XwalkNOlwd); rm(XwalkLWDsub); rm(XwalkLWDagg);rm(XwalkNOlwd)#XwalkUnionclean=XwalkUnion



#########################################################

#        PCT_SAFN and Other Sediment Indicators         #

#########################################################

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

#### 2013 data and Boating data which was also stored in Size_CLS. 2013 field protocol only collected Bed sediment, unlike 2014 and beyond which collected bed and bank sediment. 
####Doing sand and fines together
Sediment=subset(Sediment,RESULT!="OT"& RESULT!="WD") #removing all other or wood particles
Sediment$SAFN_True=ifelse(Sediment$RESULT == "SA", 1,ifelse(Sediment$RESULT == "FN", 1, 0))
pctsafn=setNames((aggregate(Sediment$SAFN_True,by=list(UID=Sediment$UID), data=Sediment, FUN='mean')),c("UID","bedPCT_SAFN2_CHECK"))#had to remove NorCal code that casted by Sample_Type because of boating data
pctsafn$bedPCT_SAFN2_CHECK=round(pctsafn$bedPCT_SAFN2_CHECK*100,digits=1)
Sedimentpvt=cast(Sediment,'UID~PARAMETER',value='RESULT',fun=length)# number of pebbles collected at intermediate and main transects
#Sedimentpvt$nbedPCT_SAFN_CHECK=(Sedimentpvt$SIZE_CLS+Sedimentpvt$XSIZE_CLS) # number of pebbles collected at all transects only 4 boating sites had less than 50
Sedimentpvt$nbedPCT_SAFN_CHECK=(Sedimentpvt$SIZE_CLS) # number of pebbles collected at all transects only 4 boating sites had less than 50
Sedimentpvtsub=subset(Sedimentpvt,select=c(UID,nbedPCT_SAFN_CHECK))
pctsafn_2013=merge(pctsafn,Sedimentpvtsub,by="UID")
pctsafn_2013$bedPCT_SAFN6_CHECK<-NA
#pctsafn_2013$nallPCT_SAFN_CHECK=pctsafn_2013$nbedPCT_SAFN_CHECK#duplicate bed data into a variable for all particles so that the data comes over in one column when combined with 2014+ data but this data should be used with caution because of protocol differences!
#pctsafn_2013$allPCT_SAFN_CHECK=pctsafn_2013$bedPCT_SAFN_CHECK#duplicate bed data into a variable for all particles so that the data comes over in one column when combined with 2014+ data but this data should be used with caution because of protocol differences!


#Now for 2014 data... 
Sed2014=subset(Sed2014,RESULT!='0')# removing all "Other" particles so that only inorganic particles are included in the PctSAFN calc
A_Sed2014=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')
##Checking how many records should be deleted by ordering and just looking at how many bank and na locations there are. 
#B_Sed2014=A_Sed2014[order(A_Sed2014$LOC),]
#View(B_Sed2014[2000:2588,])
#B_Sed2014=A_Sed2014[order(A_Sed2014$SIZE_NUM),]
#View(B_Sed2014[2000:2588,])

#only bed pebbles
#Remove all sediment records that were collected on the "BANK"
C_Sed2014=A_Sed2014[!A_Sed2014$LOC== "BANK", ]
#Can't use na.omit because it omits all records with an NA in ANY field. I only want to remove NAs in the LOC and the SIZE_NUM (sediment result) field. 
E_Sed2014=C_Sed2014[complete.cases(C_Sed2014[,c("LOC","SIZE_NUM")]),]
E_Sed2014$SAFN2_True=ifelse(E_Sed2014$SIZE_NUM == "1"| E_Sed2014$SIZE_NUM =="2", 1, 0)#changed to include 2 for 2016 data because we switched back to seperating out sands and fines
E_Sed2014$SAFN6_True=ifelse(as.numeric(E_Sed2014$SIZE_NUM)<=6, 1, 0)
F_Sed2014_2=setNames(aggregate(E_Sed2014$SAFN2_True,list(UID=E_Sed2014$UID),mean), c("UID","bedPCT_SAFN2_CHECK"))########this still counts particles with 0s in the particle count and as not fines; is this what we want to do?
F_Sed2014_6=setNames(aggregate(E_Sed2014$SAFN6_True,list(UID=E_Sed2014$UID),mean), c("UID","bedPCT_SAFN6_CHECK"))########this still counts particles with 0s in the particle count and as not fines; is this what we want to do?
F_Sed2014=merge(F_Sed2014_2,F_Sed2014_6, by='UID')
F_Sed2014$bedPCT_SAFN2_CHECK=round(F_Sed2014$bedPCT_SAFN2_CHECK*100,digits=1)
F_Sed2014$bedPCT_SAFN6_CHECK=round(F_Sed2014$bedPCT_SAFN6_CHECK*100,digits=1)

#All pebbles 
I_Sed2014=A_Sed2014[complete.cases(A_Sed2014[,c("LOC","SIZE_NUM")]),]
#I_Sed2014$SAFN_True=ifelse(I_Sed2014$SIZE_NUM == "1", 1, 0)
I_Sed2014$SAFN2_True=ifelse(I_Sed2014$SIZE_NUM == "1"| I_Sed2014$SIZE_NUM =="2", 1, 0)#changed to include 2 for 2016 data because we switched back to seperating out sands and fines
I_Sed2014$SAFN6_True=ifelse(as.numeric(I_Sed2014$SIZE_NUM)<=6 , 1, 0) 
J_Sed2014_2=setNames(aggregate(I_Sed2014$SAFN2_True,list(UID=I_Sed2014$UID),mean), c("UID","allPCT_SAFN2_CHECK"))
J_Sed2014_6=setNames(aggregate(I_Sed2014$SAFN6_True,list(UID=I_Sed2014$UID),mean), c("UID","allPCT_SAFN6_CHECK"))
J_Sed2014=merge(J_Sed2014_2,J_Sed2014_6, by='UID')
J_Sed2014$allPCT_SAFN2_CHECK=round(J_Sed2014$allPCT_SAFN2_CHECK*100,digits=1)
J_Sed2014$allPCT_SAFN6_CHECK=round(J_Sed2014$allPCT_SAFN6_CHECK*100,digits=1)

#sample sizes
Nall_Sed2014pvt=setNames(cast(Sed2014,'UID~PARAMETER',value='RESULT',fun=length),c("UID","nLOC","nallPCT_SAFN_CHECK"))#number of all collected pebbles
Nall_Sed2014pvtsub=subset(Nall_Sed2014pvt, select=c(UID,nallPCT_SAFN_CHECK))
Nbed_Sed2014pvt=aggregate(.~UID, data=C_Sed2014, length)#number of bed pebbles
Nbed=setNames(subset(Nbed_Sed2014pvt, select=c(UID,SIZE_NUM)),c("UID","nbedPCT_SAFN_CHECK"))
# 7 sites from AK had right around 100 which they should according the the AK protocol
# 4 WRSA sites had right around 100 pebbles so these were included
# 2 WRSA sites had values between 60-70 but this is still more than half of pebbles collected in 2013 so this data was included and used
# 2 CO sites have values around 100 but OK
# 1 NV site at 59 and 1 NV at 100

G_Sed2014=merge(F_Sed2014,Nbed, by="UID")
K_Sed2014=merge(J_Sed2014,Nall_Sed2014pvtsub, by="UID")

# Combine the two datasets for PCT_SAFN together so that I don't have multiple files for the same thing
#combining 2013 and 2014 data
H_Sed=rbind(pctsafn_2013,G_Sed2014)
if(exists("H_Sed")==TRUE){
  PCT_SAFN_ALL=join(H_Sed,K_Sed2014,by="UID",type="left")
  }else {PCT_SAFN_ALL=join(G_Sed2014,K_Sed2014,by="UID",type="left")}
#PCT_SAFN_sub=subset(PCT_SAFN_ALL,nallPCT_SAFN_CHECK<50)
PCT_SAFN_ALL$allPCT_SAFN2_CHECK=ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$allPCT_SAFN2_CHECK)
PCT_SAFN_ALL$bedPCT_SAFN2_CHECK=ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$bedPCT_SAFN2_CHECK)
PCT_SAFN_ALL$allPCT_SAFN6_CHECK=ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$allPCT_SAFN6_CHECK)
PCT_SAFN_ALL$bedPCT_SAFN6_CHECK=ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$bedPCT_SAFN6_CHECK)


####### other sediment metrics ##########
Sed2014_MEAS=subset(Sed2014,RESULT!=0 & PARAMETER=='SIZE_NUM')
Sed2014_MEAS$RESULT=as.numeric(Sed2014_MEAS$RESULT)
pvtSed2014_D=setNames(cast(Sed2014_MEAS,'UID~PARAMETER',value='RESULT',function(x) quantile(x,c(0.50,0.16,0.84))),c('UID','D50_CHECK','D16_CHECK','D84_CHECK'))
pvtSed2014_GM=setNames(cast(Sed2014_MEAS,'UID~PARAMETER',value='RESULT',function(x) exp(mean(log10(x)))),c('UID','GEOMEAN_CHECK'))
pvtSed2014_D$D50_CHECK=round(pvtSed2014_D$D50_CHECK,digit=0)
pvtSed2014_D$D16_CHECK=round(pvtSed2014_D$D16_CHECK,digit=0)
pvtSed2014_D$D84_CHECK=round(pvtSed2014_D$D84_CHECK,digit=0)
pvtSed2014_GM$GEOMEAN_CHECK=round(pvtSed2014_GM$GEOMEAN_CHECK,digit=0)
ALLSED=join_all(list(PCT_SAFN_ALL,pvtSed2014_D,pvtSed2014_GM),by='UID')
ALLSED$GEOMEAN_CHECK=ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$GEOMEAN_CHECK)
ALLSED$D50_CHECK=ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D50_CHECK)
ALLSED$D16_CHECK=ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D16_CHECK)
ALLSED$D84_CHECK=ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D84_CHECK)


#########################################################

#                Pool Tail Fines Indicators             #

#########################################################
pvtPoolFines=cast(PoolFines,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')#need to pivot to create the pctPoolFInes variable
pvtPoolFines$PctPoolFines2_CHECK=pvtPoolFines$POOLFINES2/(50-pvtPoolFines$POOLNOMEAS)*100
pvtPoolFines$PctPoolFines6_CHECK=pvtPoolFines$POOLFINES6/(50-pvtPoolFines$POOLNOMEAS)*100
aggpvt1PoolFines=aggregate(PctPoolFines2_CHECK~UID+TRANSECT,data=pvtPoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt2PoolFines=aggregate(PctPoolFines6_CHECK~UID+TRANSECT,data=pvtPoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt3PoolFines=aggregate(PctPoolFines2_CHECK~UID,data=aggpvt1PoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt4PoolFines=aggregate(PctPoolFines6_CHECK~UID,data=aggpvt2PoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt5PoolFines=setNames(aggregate(PctPoolFines2_CHECK~UID,data=aggpvt1PoolFines, FUN='sd'),c("UID","PctPoolFines2SD"))#average pool fines at a pool first # note these exclude NAs
aggpvt6PoolFines=setNames(aggregate(PctPoolFines6_CHECK~UID,data=aggpvt2PoolFines, FUN='sd'),c("UID","PctPoolFines6SD"))#average pool fines at a pool first # note these exclude NAs
FinalpvtPoolFines=join_all(list(aggpvt3PoolFines,aggpvt4PoolFines,aggpvt5PoolFines,aggpvt6PoolFines),by=c('UID'))
#FinalpvtPoolFines=join_all(list(aggpvt3PoolFines,aggpvt5PoolFines),by=c('UID'))
FinalpvtPoolFines$PctPoolFines2_CHECK=round(FinalpvtPoolFines$PctPoolFines2_CHECK,digits=0)
FinalpvtPoolFines$PctPoolFines6_CHECK=round(FinalpvtPoolFines$PctPoolFines6_CHECK,digits=0)
#calc CV for PIBO QC check
FinalpvtPoolFines$PctPoolFines2CV=FinalpvtPoolFines$PctPoolFines2SD/FinalpvtPoolFines$PctPoolFines2_CHECK#sites with CV >1.414 should be QCed 
FinalpvtPoolFines$PctPoolFines6CV=FinalpvtPoolFines$PctPoolFines6SD/FinalpvtPoolFines$PctPoolFines6_CHECK#sites with CV >1.414 should be QCed 
subQC=subset(FinalpvtPoolFines,FinalpvtPoolFines$PctPoolFines6CV>=1.41|FinalpvtPoolFines$PctPoolFines2CV>=1.41)#sites with CV >1.414 should be QCed 
#sample sizes were checked on raw data- if only 1 replicate in a pool data was inactivated and if more than 50% of remaining data missing data all data for site inactivated


##########################################################

#          BankStability and Cover Indicators            #

##########################################################
#Still need to decide on removing depositional banks
#Pivot data so that Each parameter has it's own column
#This averages across all side channel and main channel banks.
#PIBO takes only the outside banks but for pre-2016 data we can't determine which are the outside banks after the fact so we averaged across all banks that were collected.
Banks=cast(BankStab, 'UID+TRANSECT+POINT~PARAMETER', value='RESULT')#run this for all years data and then if 2016 data run subsection below

###################################################################
#run this section for 2016+ data
#for 2016 and beyond take only data from outside banks... run above code plus the code below for 2016 data
#get which side of the main channel the side channel was on to determine outside banks
#if side channel on right bank need to use right bank for X transect data and use left bank data for main transect
#if side channel on left bank need to use left bank for X transect data and use right bank data for main transect
pvtSideBank1=cast(SideBank,'UID+TRANSECT~PARAMETER',value='RESULT')
pvtSideBank2=pvtSideBank1
pvtSideBank2$TRANSECT=sub("^","X",pvtSideBank1$TRANSECT)
#need to get data to be opposite for main channel
pvtSideBank1$SIDCHN_BNK=ifelse(pvtSideBank1$SIDCHN_BNK=='LF','RT',ifelse(pvtSideBank1$SIDCHN_BNK=='RT','LF',pvtSideBank1$SIDCHN_BNK))
pvtSideBank3=rbind(pvtSideBank1,pvtSideBank2)
Banks=merge(Banks,pvtSideBank3,by=c('UID','TRANSECT'), all=T)
Banks$SIDCHN_BNK=ifelse(is.na(Banks$SIDCHN_BNK)==T,Banks$POINT,Banks$SIDCHN_BNK)
Banks=subset(Banks,Banks$SIDCHN_BNK==Banks$POINT)
#####################################################################

####this section is for all years
#I want to calculate the percent of banks that are Covered.  
Banks$CoverValueBasal=as.numeric(ifelse(Banks$COVER_BASAL=='UC',"0",ifelse(Banks$COVER_BASAL=='CV',"1","NA")))
Banks$CoverValueFoliar=as.numeric(ifelse(Banks$COVER_FOLIAR=='UC',"0",ifelse(Banks$COVER_FOLIAR=='CV',"1","NA")))
#I want to calculate the percent of banks that are Stable (Absent) 
# Unstable==(Fracture, slump, slough, eroding)
Banks$StableValue=as.numeric(ifelse(Banks$STABLE=='SP'|Banks$STABLE=='ER'|Banks$STABLE=='LH'|Banks$STABLE=='FC',"0",ifelse(Banks$STABLE=='AB',"1","NA")))
#combined stability and cover
Banks$BnkCoverFoliar_Stab=as.numeric(ifelse((Banks$CoverValueFoliar+Banks$StableValue)<2,0,1))

#only erosional banks
BanksErosional=subset(Banks, EROSION=='EL')
BnkCvrBasalErosional=setNames(aggregate(CoverValueBasal~UID,data=BanksErosional, FUN=mean), c('UID','BnkCoverBasal_Erosional_CHECK'))
BnkCvrFoliarErosional=setNames(aggregate(CoverValueFoliar~UID,data=BanksErosional, FUN=mean), c('UID','BnkCoverFoliar_Erosional_CHECK'))
BnkStbErosional=setNames(aggregate(StableValue~UID,data=BanksErosional, FUN=mean), c('UID','BnkStability_Erosional_CHECK'))
BnkCoverFoliar_StabErosional=setNames(aggregate(BnkCoverFoliar_Stab~UID,data=BanksErosional, FUN=mean), c('UID','BnkCoverFoliar_StabErosional_CHECK'))
BNK_BEDROCK=setNames(aggregate(as.numeric(BNK_BEDROCK)~UID,data=BanksErosional, FUN=mean),c('UID','BNK_BEDROCK_CHECK'))
BNK_COBBLE=setNames(aggregate(as.numeric(BNK_COBBLE)~UID,data=BanksErosional, FUN=mean),c('UID','BNK_COBBLE_CHECK'))
BNK_LWD=setNames(aggregate(as.numeric(BNK_LWD)~UID,data=BanksErosional, FUN=mean),c('UID','BNK_LWD_CHECK'))
BNK_VEG_BASAL=setNames(aggregate(as.numeric(BNK_VEG_BASAL)~UID,data=BanksErosional, FUN=mean),c('UID','BNK_VEG_BASAL_CHECK'))
BNK_VEG_FOLIAR=setNames(aggregate(as.numeric(BNK_VEG_FOLIAR)~UID,data=BanksErosional, FUN=mean),c('UID','BNK_VEG_FOLIAR_CHECK'))



#samplesize
nBnkCover_StabErosional=setNames(aggregate(STABLE~UID,data=BanksErosional, FUN=length), c('UID','nBnkCover_StabErosional_CHECK'))


#both erosional and depositional banks
BanksAll=Banks
BnkCvrFoliarAll=setNames(aggregate(CoverValueFoliar~UID,data=BanksAll, FUN=mean), c('UID','BnkCoverFoliar_All_CHECK'))
BnkStbAll=setNames(aggregate(StableValue~UID,data=BanksAll, FUN=mean), c('UID','BnkStability_All_CHECK'))
BnkCoverFoliar_StabAll=setNames(aggregate(BnkCoverFoliar_Stab~UID,data=BanksAll, FUN=mean), c('UID','BnkCoverFoliar_StabAll_CHECK'))
#samplesize
nBnkCoverFoliar_StabAll=setNames(aggregate(BnkCoverFoliar_Stab~UID,data=BanksAll, FUN=length), c('UID','nBnkCoverFoliar_StabAll_CHECK'))# 2 banks at 21 transects should not be less than 21 except boatable----which could be 11

#merge all bank files
BnkErosional=join_all(list(BnkCoverFoliar_StabErosional,BnkCvrFoliarErosional,BnkCvrBasalErosional,BnkStbErosional,nBnkCover_StabErosional,BNK_BEDROCK,BNK_COBBLE,BNK_LWD,BNK_VEG_FOLIAR, BNK_VEG_BASAL), by="UID",type ="full")
BnkAll=join_all(list(BnkCoverFoliar_StabAll,BnkCvrFoliarAll,BnkStbAll,nBnkCoverFoliar_StabAll), by="UID",type="full")
#convert to percent
BnkErosional$BnkCoverFoliar_Erosional_CHECK=round(BnkErosional$BnkCoverFoliar_Erosional_CHECK*100,digits=0)
BnkErosional$BnkCoverBasal_Erosional_CHECK=round(BnkErosional$BnkCoverBasal_Erosional_CHECK*100,digits=0)
BnkErosional$BnkStability_Erosional_CHECK=round(BnkErosional$BnkStability_Erosional_CHECK*100,digits=0)
BnkErosional$BnkCoverFoliar_StabErosional_CHECK=round(BnkErosional$BnkCoverFoliar_StabErosional_CHECK*100,digits=0)
BnkErosional$BNK_BEDROCK_CHECK=round(BnkErosional$BNK_BEDROCK_CHECK,digits=0)
BnkErosional$BNK_COBBLE_CHECK=round(BnkErosional$BNK_COBBLE_CHECK,digits=0)
BnkErosional$BNK_LWD_CHECK=round(BnkErosional$BNK_LWD_CHECK,digits=0)
BnkErosional$BNK_VEG_FOLIAR_CHECK=round(BnkErosional$BNK_VEG_FOLIAR_CHECK,digits=0)
BnkErosional$BNK_VEG_BASAL_CHECK=round(BnkErosional$BNK_VEG_BASAL_CHECK,digits=0)
BnkAll$BnkCoverFoliar_All_CHECK=round(BnkAll$BnkCoverFoliar_All_CHECK*100,digits=0)
BnkAll$BnkStability_All_CHECK=round(BnkAll$BnkStability_All_CHECK*100,digits=0)
BnkAll$BnkCoverFoliar_StabAll_CHECK=round(BnkAll$BnkCoverFoliar_StabAll_CHECK*100,digits=0)

#remove cases with less than 50% of data----2 banks at 21 transects should not be less than 21 except boatable----which could be 11
#exclude=subset(BnkErosional,nBnkCover_StabErosional_CHECK<11)#15 excluded
BnkErosional$BnkCoverFoliar_StabErosional_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BnkCoverFoliar_StabErosional_CHECK) 
BnkErosional$BnkCoverFoliar_Erosional_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BnkCoverFoliar_Erosional_CHECK)  
BnkErosional$BnkCoverBasal_Erosional_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BnkCoverBasal_Erosional_CHECK)  
BnkErosional$BnkStability_Erosional_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BnkStability_Erosional_CHECK) 
BnkErosional$BNK_BEDROCK_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BNK_BEDROCK_CHECK)
BnkErosional$BNK_COBBLE_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BNK_COBBLE_CHECK)
BnkErosional$BNK_LWD_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BNK_LWD_CHECK)
BnkErosional$BNK_VEG_FOLIAR_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BNK_VEG_FOLIAR_CHECK)
BnkErosional$BNK_VEG_BASAL_CHECK=ifelse(BnkErosional$nBnkCover_StabErosional_CHECK<11,NA,BnkErosional$BNK_VEG_BASAL_CHECK)

BnkAll$BnkCoverFoliar_StabAll_CHECK=ifelse(BnkAll$nBnkCoverFoliar_StabAll_CHECK<11,NA,BnkAll$BnkCoverFoliar_StabAll_CHECK)
BnkAll$BnkCoverFoliar_All_CHECK=ifelse(BnkAll$nBnkCoverFoliar_StabAll_CHECK<11,NA,BnkAll$BnkCoverFoliar_All_CHECK)  
BnkAll$BnkStability_All_CHECK=ifelse(BnkAll$nBnkCoverFoliar_StabAll_CHECK<11,NA,BnkAll$BnkStability_All_CHECK) 


# #new Bank Stability Cover Classes
# BankStabCoverClass$RESULT=as.numeric(BankStabCoverClass$RESULT)
# meanBankStabCoverClass=setNames(cast(BankStabCoverClass,'UID~PARAMETER',value='RESULT',fun=mean),c('UID','BNK_BEDROCK_CHECK','BNK_COBBLE_CHECK','BNK_LWD_CHECK','BNK_VEG_CHECK'))
# meanBankStabCoverClass$BNK_BEDROCK_CHECK=round(meanBankStabCoverClass$BNK_BEDROCK_CHECK,digits=0)
# meanBankStabCoverClass$BNK_COBBLE_CHECK=round(meanBankStabCoverClass$BNK_COBBLE_CHECK,digits=0)
# meanBankStabCoverClass$BNK_LWD_CHECK=round(meanBankStabCoverClass$BNK_LWD_CHECK,digits=0)
# meanBankStabCoverClass$BNK_VEG_CHECK=round(meanBankStabCoverClass$BNK_VEG_CHECK,digits=0)


##########################################################

#          Floodplain Connectivty Indicators            #

##########################################################
#LINCIS_H
###First the max value of either the side channel or main channel needs to be chosen. To do this I changed all side channels (X-letter) to just the main letter (Sidechannel at A (XA) would be changed to just A).
###Then I subset the data so that missing values would not cause errors. 
###Then I pivoted by the max to chose the max transect value (If XA=5 and A=2 then the XA value would be chosen and the A value removed, note that it is no longer called XA so there would just be 2 A transects for a site with an A sidechannel)
### Then take the average at each site for bank height and incised height. Merge the data back together and then calculate LINCIS_H
#note that mathematically it does not matter whether you take the mean bankfull height and subtract it from the mean incision height or if you take the paired bankfull height and incision height differences and then calculate a mean if N is same
Incision$TRANSECT=mapvalues(Incision$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
INCISED=subset(Incision, PARAMETER=="INCISED")
BANKHT=subset(Incision, PARAMETER=="BANKHT")
Inc=cast(INCISED,'UID+TRANSECT~PARAMETER', value='RESULT', fun=min)#devating from EPA and taking the min rather than the max because protocl is to take the lowest of the 2 sides
Bnk=cast(BANKHT,'UID+TRANSECT~PARAMETER', value='RESULT', fun=min)#devating from EPA and taking the min rather than the max because protocl is to take the lowest of the 2 sides
xIncht=setNames(aggregate(Inc$INCISED,list(UID=Inc$UID),mean),c("UID","xinc_h_CHECK"))
xBnkht=setNames(aggregate(Bnk$BANKHT,list(UID=Bnk$UID),mean),c("UID","xbnk_h_CHECK"))

#sample sizes
nInc=setNames(plyr::count(Inc,"UID"),c("UID","nxinc_h_CHECK"))
nBnk=setNames(plyr::count(Bnk,"UID"),c("UID","nxbnk_h_CHECK"))   

IncBnk=join_all(list(xBnkht,xIncht,nInc, nBnk),by="UID")
IncBnk$xinc_h_CHECK=ifelse(IncBnk$nxinc_h_CHECK<5,NA,IncBnk$xinc_h_CHECK)#5 sites less than 5, 1-4
IncBnk$xbnk_h_CHECK=ifelse(IncBnk$nxbnk_h_CHECK<5,NA,IncBnk$xbnk_h_CHECK)#1 site
IncBnk$LINCIS_H_CHECK=round(log10(IncBnk$xinc_h_CHECK-IncBnk$xbnk_h_CHECK+0.1),digits=2)
IncBnk$xinc_h_CHECK=round(IncBnk$xinc_h_CHECK,digits=2)
IncBnk$xbnk_h_CHECK=round(IncBnk$xbnk_h_CHECK,digits=2)


#BNK_HT_RATIO
#get thawleg info and fix differences in protocols and data storage among years
#thawleg was stored differently in 2013-2015 than in 2016 on. Points differ among years by one so one needs added to older data
##UNCOMMENT THIS SECTION IF DATA OLDER THAN 2017
# thalweg_ratio=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=c('2013','2014','2015'),Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROTOCOL'))
# thalweg_ratio=subset(thalweg_ratio,SAMPLE_TYPE!='CROSSSECW')
# thalweg_ratio$POINT=as.numeric(thalweg_ratio$POINT)+1
# thalweg_ratio2=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=c('2016'),Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROTOCOL'))
# thalweg_ratio3=rbind(thalweg_ratio,thalweg_ratio2)
# thalweg_ratio3=subset(thalweg_ratio3,RESULT!=0)#starting in 2017 bankfull and incision heights measured from thalweg at dry transect but any data before that needs omitted if thalweg=0
# thalweg_ratio4=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=c('2017'),Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROTOCOL'))
# thalweg_ratio5=rbind(thalweg_ratio3,thalweg_ratio4)
thalweg_ratio5=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROTOCOL'))
depth=subset(thalweg_ratio5,POINT==1)
depth$RESULT=depth$RESULT/100
#get bank info
BnkRatio=tblRetrieve(Parameters=c('INCISED','BANKHT'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
BnkRatiopvt=cast(BnkRatio,'UID+TRANSECT~PARAMETER',value='RESULT')
#join thalweg and bank info and calc ratio
BnkRatiopvt=join(BnkRatiopvt,depth, by=c('UID','TRANSECT'))
BnkRatiopvt=subset(BnkRatiopvt,is.na(RESULT)==FALSE)
BnkRatiopvt$BANKHT_DEPTH=BnkRatiopvt$BANKHT+BnkRatiopvt$RESULT
BnkRatiopvt$INCISED_DEPTH=BnkRatiopvt$INCISED+BnkRatiopvt$RESULT
BnkRatiopvt$Ratio=BnkRatiopvt$INCISED_DEPTH/BnkRatiopvt$BANKHT_DEPTH
BnkRatiopvt=subset(BnkRatiopvt,is.na(Ratio)==FALSE)
BnkRatioAvg=setNames(aggregate(BnkRatiopvt$Ratio,list(UID=BnkRatiopvt$UID),mean),c("UID","BNK_HT_RATIO_CHECK"))
nBnkRatiopvt=setNames(plyr::count(BnkRatiopvt,"UID"),c("UID","nBNK_HT_RATIO_CHECK"))
BnkRatioAvg=join(BnkRatioAvg,nBnkRatiopvt,by="UID")
BnkRatioAvg$BNK_HT_RATIO_CHECK=ifelse(BnkRatioAvg$nBNK_HT_RATIO_CHECK<5,NA,BnkRatioAvg$BNK_HT_RATIO_CHECK)
BnkRatioAvg$BNK_HT_RATIO_CHECK=round(BnkRatioAvg$BNK_HT_RATIO_CHECK,digits=2)




##########################################################################################################
#                           Contingent Indicators                                                        #
##########################################################################################################

##########################################################

#                    Fish Cover Indicator              #

##########################################################
#XFC_NAT
###Get the approrpaite fish parameters from NC_DataConsumption
######'BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'
###### Change numeric categories into appropriate percentages, pivot to take the mean or each fish cover category at a site. 
###### Then sum to categories of fish cover for each site to have the final results to compared to aquamet's xfc_nat
###### The way this is calculated causes NA's to be treated as blanks that do not count for or against the average. For example if only 1 NA for BOULDR then you would divide boulders by 10 transects instead of 11. See UID 11625 for an example.
###### I believe this method averages across side channel data
fish$ResultsPer=ifelse(fish$RESULT == 1, 0.05,ifelse(fish$RESULT == 2, 0.25,ifelse(fish$RESULT == 3, 0.575,ifelse(fish$RESULT == 4, 0.875,ifelse(fish$RESULT ==0, 0, NA)))))
fishpvt2=cast(fish,'UID~PARAMETER', value='ResultsPer',fun='mean')
fishpvt2$XFC_NAT_CHECK=rowSums(fishpvt2[,c(2,3,4,5,6,7)])
#sample size
fishpvt3=cast(fish,'UID~PARAMETER', value='ResultsPer',fun='length')
fishpvt2$nXFC_NAT_CHECK=(fishpvt3$BOULDR+fishpvt3$BRUSH+fishpvt3$LVTREE+fishpvt3$OVRHNG+fishpvt3$UNDCUT+fishpvt3$WOODY)# 6 categories *11 transects half data is 6*transects
#fishpvt5=subset(fishpvt2,nXFC_NAT_CHECK<33)#only 4 sites that this applies to and 28 was the lowest so decided to use all data 
fishpvt2$XFC_NAT_CHECK=ifelse(fishpvt2$nXFC_NAT_CHECK<30,NA,fishpvt2$XFC_NAT_CHECK)
fishpvt2$XFC_NAT_CHECK=round(fishpvt2$XFC_NAT_CHECK,digits=2)

##########################################################

#                    Bank Angle  Indicator              #

##########################################################
#Angle
Angle$RESULT=ifelse(Angle$RESULT<45,45,Angle$RESULT)
Angle$RESULT=as.numeric(Angle$RESULT)

#2016+ data
#need to treat side channels the same as with banks stability and only use the angles from the outer banks
#run the side channel section of the bank stability prior to running this to get pvtSideBank3
Angle=merge(Angle,pvtSideBank3, by=c('UID','TRANSECT'),all=T)
Angle$SIDCHN_BNK=ifelse(is.na(Angle$SIDCHN_BNK)==T,Angle$POINT,Angle$SIDCHN_BNK)
Angle=subset(Angle,Angle$SIDCHN_BNK==Angle$POINT)

#all years
MeanAngle=setNames(cast(Angle,'UID~PARAMETER',value='RESULT',fun=mean),c("UID","ANGLE180_CHECK"))

#sample size
nAngle=setNames(plyr::count(Angle,"UID"),c("UID","nANGLE180_CHECK"))# 2 banks* 11 transects=22
MeanAngle=merge(nAngle,MeanAngle, by="UID")
MeanAngle$ANGLE180_CHECK=ifelse(MeanAngle$nANGLE180_CHECK<10,NA,MeanAngle$ANGLE180_CHECK)#8 site 5 WRSA
MeanAngle$ANGLE180_CHECK=round(MeanAngle$ANGLE180_CHECK,digits=0)


##########################################################

#         Thalweg Mean, CV and Pct Dry  Indicators       #

##########################################################
#Thalweg                                                                                                                                                                    
#fixing issues with boating and wading in different units
thalweg$RESULT=thalweg$RESULT/100
Thalweg=cast(thalweg,'UID~PARAMETER',value='RESULT',fun=mean)
Thalweg=setNames(Thalweg, c("UID","XDEPTH_CHECK"))
Thalweg$XDEPTH_CHECK=round(Thalweg$XDEPTH_CHECK,digits=2) 
ThalwegSD=cast(thalweg,'UID~PARAMETER',value='RESULT',fun=sd)
ThalwegSD=setNames(ThalwegSD,c("UID","SDDEPTH"))
Thalweg=merge(Thalweg,ThalwegSD,by=c('UID'), all=T)
Thalweg$CVDEPTH_CHECK=round(Thalweg$SDDEPTH/Thalweg$XDEPTH,digits=2)

##Percent Dry
#decided to use number of thalweg depth=0 because more spatially explict...this means this indicator can't be calc if thalweg not collected
CountThalweg=setNames(plyr::count(thalweg,"UID"),c('UID','nDEPTH'))
Dry=subset(thalweg,RESULT=='0')
Dry=setNames(plyr::count(Dry,"UID"),c('UID','nDRY'))
PctDry=merge(CountThalweg,Dry,by=c('UID'),all=TRUE)
PctDry$PctDry_CHECK=ifelse(is.na(PctDry$nDRY/PctDry$nDEPTH*100)==TRUE,0,PctDry$nDRY/PctDry$nDEPTH*100)        
PctDry$PctDry_CHECK=round(PctDry$PctDry_CHECK,digits=1)              

###alternative ways of looking at flow and dryness              
# #percent dry based on # of dry transects
#dry=tblRetrieve(Parameters=c('TRANDRY'),Projects=projects, Years=years,Protocols=protocols)
#pvtdry=cast(dry,'UID+TRANSECT~PARAMETER',value='RESULT')
#countdry=count(pvtdry,c("UID","TRANDRY"))            
##flow for thalweg
#flow=tblRetrieve(Parameters=c('FLOW'),Projects=projects, Years=years,Protocols=protocols)
#countflow=count(flow,c("UID","RESULT")) 


Thalweg=merge(Thalweg,PctDry, by=c('UID'),all=TRUE)


#omit cases with incomplete thalweg 2016+ data 
tbl=tblRetrieve(Parameters=c('DEPTH'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
tbl.2=tblRetrieve(Parameters=c('NUM_THALWEG'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
tbl3=cast(tbl.2,'UID~PARAMETER',value='RESULT',mean)
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT',length),c('SITE_ID'))# count is default
thalweg.missing=merge(tbl.PVT,tbl3, by='UID')
thalweg.missing$pctcomplete=thalweg.missing$DEPTH/(thalweg.missing$NUM_THALWEG*10)*100
Thalweg=merge(Thalweg,thalweg.missing, by=c('UID'))
Thalweg$XDEPTH_CHECK=ifelse(Thalweg$pctcomplete<100,NA,Thalweg$XDEPTH_CHECK)
Thalweg$CVDEPTH_CHECK=ifelse(Thalweg$pctcomplete<100,NA,Thalweg$CVDEPTH_CHECK)  
Thalweg$CVDEPTH_CHECK=ifelse(Thalweg$PctDry_CHECK>0,NA,Thalweg$CVDEPTH_CHECK)# omit cases with dry thalwegs
Thalweg$PctDry_CHECK=ifelse(Thalweg$pctcomplete<100,NA,Thalweg$PctDry_CHECK)  



# #omit cases with incomplete thalweg 2014-2015
# thalweg.missing2014=sqlQuery(wrsa1314,sprintf("select Station.UID, StationCNT,DepthCNT from 
#                                               (select distinct UID,
#                                               CASE 
#                                               WHEN parameter='sub_5_7' and RESULT='5' THEN cast((RESULT*2)*10 as numeric)
#                                               WHEN parameter='sub_5_7' and RESULT='7' THEN cast((RESULT*2+1)*10 as numeric)
#                                               WHEN parameter='sub_5_7' and RESULT='14' THEN cast ((RESULT*2+2)*10 as numeric)
#                                               ELSE 'ISSUE'
#                                               END as StationCNT from tbltransect where parameter='SUB_5_7' and ACTIVE='true') as station
#                                               join
#                                               (select UID,  count(point) as DepthCNT from tblpoint where parameter='DEPTH' and SAMPLE_TYPE='THALW'  and ACTIVE='true' group by UID) as depth
#                                               on station.uid=depth.uid
#                                               --- where StationCNT > DepthCNT 
#                                               order by Station.UID"))
# thalweg.missing2014$pctcomplete=thalweg.missing2014$DepthCNT/thalweg.missing2014$StationCNT*100
# #omit cases with incomplete thalweg 2013
# thalweg.missing2013=sqlQuery(wrsa1314,sprintf("select Station.UID, StationDUPLICATES,StationCNT,DepthCNT from 
#                                               (select distinct UID,
#                                               CASE 
#                                               WHEN RESULT='5' THEN cast(RESULT*20 as numeric)
#                                               WHEN RESULT='7' THEN cast(RESULT*20+10 as numeric)
#                                               WHEN RESULT='14' THEN cast (RESULT*20+20 as numeric)
#                                               ELSE 'ISSUE'
#                                               END as StationCNT from tblpoint where parameter='SUB_5_7' and ACTIVE='true') as station
#                                               join
#                                               (select UID,count(result) as StationDUPLICATES from (select distinct UID, result from tblpoint where parameter='SUB_5_7' and ACTIVE='true') as stcnt group by UID) as stationcount
#                                               on station.uid=stationcount.uid
#                                               join 
#                                               (select UID, count(point) as DepthCNT from tblpoint where parameter='DEPTH' and POINT not in('CT','LC','LF','RC','RT') and ACTIVE='true' group by UID) as depth
#                                               on station.uid=depth.uid
#                                               --where StationCNT > DepthCNT or stationDUPLICATES>1
#                                               order by Station.UID"))
# thalweg.missing2013$pctcomplete=thalweg.missing2013$DepthCNT/thalweg.missing2013$StationCNT*100                             
# Thalweg2014=merge(Thalweg,thalweg.missing2014, by=c('UID'))
# Thalweg2013=merge(Thalweg,thalweg.missing2013, by=c('UID') ) 
# Thalweg_2013_2014=merge(Thalweg2013,Thalweg2014,by=c('UID'),all=TRUE)# mannually exclude data because such a mess
# # Thalweg_2013_2014$XDEPTH_CHECK=ifelse(Thalweg2014$pctcomplete<100,NA,Thalweg2014$XDEPTH_CHECK)
# # Thalweg_2013_2014$CVDEPTH_CHECK=ifelse(Thalweg2014$pctcomplete<100,NA,Thalweg2014$CVDEPTH_CHECK)              
# # Thalweg2013$XDEPTH_CHECK=ifelse(Thalweg2013$pctcomplete<100,NA,Thalweg2013$XDEPTH_CHECK)
# # Thalweg2013$CVDEPTH_CHECK=ifelse(Thalweg2013$pctcomplete<100,NA,Thalweg2013$CVDEPTH_CHECK)  
# 

             
###################################################################################################################
###################################################################################################################
###################################################################################################################

##############                     Covariates                    #################

###################################################################################################################
###################################################################################################################
###################################################################################################################

##########################################################

#                      Slope                            #

##########################################################
# #pre 2016 data
# Slope_height=cast(Slope_height, 'UID~PARAMETER',value='RESULT',fun=sum)
# SlpReachLen=cast(SlpReachLen,'UID~PARAMETER',value='RESULT')
# Slope_Per=merge(Slope_height,SlpReachLen, by=c('UID'), all=T)
# Slope_Per$PCT_GRADE=round(((Slope_Per$SLOPE/100)/(Slope_Per$SLPRCHLEN))*100,digits=2)
# Slope_Per=setNames(Slope_Per,c("UID","AVGSLOPE_CHECK","SLPRCHLEN_CHECK","PCT_GRADE_CHECK"))
#2016+ data
Slope_Per1=cast(Slope,'UID~PARAMETER',value='RESULT')                
Slope_Per1$PCT_GRADE=round(as.numeric(Slope_Per1$PCT_GRADE),digits=2)
Slope_Per1=setNames(Slope_Per1,c("UID","AVGSLOPE_CHECK","PCT_GRADE_CHECK","SLPRCHLEN_CHECK"))
#Slope_Per=rbind(Slope_Per,Slope_Per1)
Slope_Per=Slope_Per1
##########################################################

#                Channel Dimensions                     #

##########################################################
#note the EPA combines all side channel, main and intermediate transects and then takes the max wetted width for a total of 10 values (excluding transect K?) instead of 21. This is likely an artifact of them adding intermediate transects halway through the project
#We are deviating from the EPA's method of calculating wetted width. We take the max value for main vs. side channels (should in theory always be main channel); then we average across all 21 values (main and intermediate)
#Because of this deviation, in the future we should remove the variable differences of wetwid and wetwidth and make them the same variable
WetWid2$TRANSECT=mapvalues(WetWid2$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))#change all side channels to normal transects
WetWid2=cast(WetWid2,'UID+TRANSECT~PARAMETER', value='RESULT', fun=max)#take the maximum wetted width among side channels
WetWid=subset(WetWid,POINT!=0)#remove duplicate wetted widths 
WetWid=setNames(cast(WetWid,'UID+TRANSECT+POINT~PARAMETER', value='RESULT'), list ("UID","TRANSECT","POINT","WETWID"))
WetWidSub=WetWid[,c(1,2,4)]# delete the point column
WetWidAll=rbind(WetWidSub,WetWid2)#merge main transects and intermediate transects together
WetWidFinal=setNames(aggregate(as.numeric(WETWID)~UID,data=WetWidAll,FUN=mean),list("UID","XWIDTH_CHECK"))#average across all transects
nWetWid=setNames(plyr::count(WetWidAll,"UID"),c("UID","nXWIDTH_CHECK"))#21 transects
WetWidFinal=merge(nWetWid,WetWidFinal)
WetWidFinal$XWIDTH_CHECK=ifelse(WetWidFinal$nXWIDTH_CHECK<10,NA,WetWidFinal$XWIDTH_CHECK)#9 ranging from 4-11 not an indicator and just for context so lean towards leaving all measurements
WetWidFinal$XWIDTH_CHECK=round(WetWidFinal$XWIDTH_CHECK,digits=2)

##checking for interupted flow sites
# checkzero=rbind(WetWid,WetWid2)
# checkzero$PARAMETER=ifelse(checkzero$PARAMETER=='WETWIDTH','WETWID',checkzero$PARAMETER)
# checkzero_pvt=cast(checkzero,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')
# checkzerosubset=subset(checkzero_pvt,WETWID==0)
# checkzerosubset=subset(checkzerosubset,POINT=!0)

BankWid$TRANSECT=mapvalues(BankWid$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))#change all side channels to normal transects
BankWid$RESULT=as.numeric(BankWid$RESULT)
BankWidpvt=cast(BankWid,'UID+TRANSECT~PARAMETER', value='RESULT', fun=sum)#sum across side channels and main transects
BankWidpvt=setNames(aggregate(BankWidpvt$BANKWID,list(UID=BankWidpvt$UID),mean),c("UID","XBKF_W_CHECK"))#average all transects
nBankWid=setNames(plyr::count(BankWid,"UID"),c("UID","nXBKF_W_CHECK"))# should have 11 transects
BankWidFinal=merge(nBankWid,BankWidpvt)
BankWidFinal$XBKF_W_CHECK=ifelse(BankWidFinal$nXBKF_W_CHECK<5,NA,BankWidFinal$XBKF_W_CHECK)# one site with 4 leave in
BankWidFinal$XBKF_W_CHECK=round(BankWidFinal$XBKF_W_CHECK,digits=2)

##########################################################

#                Flood Prone Width                     #

##########################################################
#Flood Prone Width
FloodWidthCount=setNames(plyr::count(FloodWidth,"UID"),c('UID','nFloodWidth_CHECK'))#count doesn't appear to be working
FloodWidth$RESULT=as.numeric(FloodWidth$RESULT)
avgFloodWidth=setNames(cast(FloodWidth,'UID~PARAMETER',value='RESULT', fun=mean),c("UID","BNK_WT_CHECK","FLD_WT_CHECK"))
avgFloodWidth$FLD_WT_CHECK=round(avgFloodWidth$FLD_WT_CHECK,digits=2)
avgFloodWidth=merge(avgFloodWidth,FloodWidthCount,by=c('UID'))



#Entrenchment
#pre2017
#avgFloodWidth=merge(avgFloodWidth,BankWidFinal,by=c('UID'))
#avgFloodWidth$ENTRENCH_CHECK=round(avgFloodWidth$FLD_WT_CHECK/avgFloodWidth$XBKF_W_CHECK,digits=2)
#2017+
FloodWidthpvt=cast(FloodWidth,'UID+TRANSECT~PARAMETER',value='RESULT')
FloodWidthpvt$ENTRENCHpvt=FloodWidthpvt$FLOOD_WID/FloodWidthpvt$FLOOD_BFWIDTH
entrench=setNames(aggregate(as.numeric(ENTRENCHpvt)~UID,data=FloodWidthpvt,FUN=mean),c("UID","ENTRENCH_CHECK"))
entrench$ENTRENCH_CHECK=round(entrench$ENTRENCH_CHECK,digits=2)
entrench$ENTRENCH_CHECK=ifelse(entrench$ENTRENCH_CHECK<1,1,ifelse(entrench$ENTRENCH_CHECK>3,3,entrench$ENTRENCH_CHECK))


##########################################################

#                    Sinuosity                           #

##########################################################
#Data from site descriptors 
listsites$straightline=acos(sin(as.numeric(listsites$LAT_DD_BR_CHECK)*3.141593/180)*sin(as.numeric(listsites$LAT_DD_TR_CHECK)*3.141593/180) + cos(as.numeric(listsites$LAT_DD_BR_CHECK)*3.141593/180)*cos(as.numeric(listsites$LAT_DD_TR_CHECK)*3.141593/180)*cos(as.numeric(listsites$LON_DD_TR_CHECK)*3.141593/180-as.numeric(listsites$LON_DD_BR_CHECK)*3.141593/180)) * 6371000
TRCHLEN=setNames(cast(TRCHLEN,'UID~PARAMETER',value='RESULT'),c('UID','INCREMENT','TRCHLEN_CHECK'))
listsites=merge(listsites,TRCHLEN, by='UID', all=T)
listsites$SINUOSITY_CHECK=round(as.numeric(listsites$TRCHLEN_CHECK)/as.numeric(listsites$straightline),digits=2)
listsites$SINUOSITY_CHECK=ifelse(listsites$SINUOSITY_CHECK<1,NA,listsites$SINUOSITY_CHECK)
#still need to remove data from partial sites
listsites$SINUOSITY_CHECK=ifelse(listsites$VALXSITE_CHECK=="PARBYWADE"|listsites$VALXSITE_CHECK=="PARBYBOAT",NA,listsites$SINUOSITY_CHECK)

###################################################################################################################
###################################################################################################################
###################################################################################################################

##############    Other metrics still being worked on or old metrics no longer used       #################

###################################################################################################################
###################################################################################################################
###################################################################################################################

# #Xembed - embeddedness
# XEMBED=setNames(cast(EMBED,'UID~PARAMETER', value='RESULT', fun='mean'), list('UID','XEMBED_CHECK'))
# nXEMBED=setNames(count(EMBED,"UID"),c("UID","nXEMBED"))
# XEMBED=merge(XEMBED,nXEMBED,by="UID")
# XEMBED$XEMBED_CHECK=round(XEMBED$XEMBED_CHECK,digits=0)
# 
# #W1_HALL- human influence
# #Be careful, the documentation says to use P=0.667, but the aquamet code says 0.6667, if there ends up being a lot of P's in the data this makes a difference!!! 
# #EMAP_WEST
# Human_Influ$EMAP_Weights=ifelse(Human_Influ$RESULT == "B", 1.5,ifelse(Human_Influ$RESULT == "C", 1.0, ifelse(Human_Influ$RESULT == "P", 0.6667, 0))) 
# W1_HALL=cast(Human_Influ,'UID~PARAMETER', value='EMAP_Weights',fun='mean')
# W1_HALL$EMAP_W1_HALL_CHECK=rowSums(W1_HALL[,c(2:12)])     
# #NRSA
# Human_Influ$NRSA_Weights=ifelse(Human_Influ$RESULT == "B", 1.5,ifelse(Human_Influ$RESULT == "C", 1.0, ifelse(Human_Influ$RESULT == "P", 0.5, 0))) 
# W1_HALL_NRSA=cast(Human_Influ,'UID~PARAMETER', value='NRSA_Weights',fun='mean')
# W1_HALL$NRSA_W1_HALL_CHECK=rowSums(W1_HALL_NRSA[,c(2:12)])     
# 
# #QR1
# #xcdenbk: needed for QR1
# #QR1
# #QR1= {(QRVeg1) (QRVeg2) (QRDIST1)} ^ 0.333; 
# #if XCMGW <=2.00, then QRVeg1=.1+(.9 (XCMGW/2.00))
# #if XCMGW >2.00 then QRVeg1=1; 
# #and QRVeg2=0.1 + [0.9(XCDENBK/100)]; 
# #QRDIST1=1/(1+W1_Hall). 
# QR1=join_all(list(XCMGW_new1,BnkDensPvt,W1_HALL), by='UID')
# QR1=setNames(subset(QR1[,c('UID','XCMGW_CHECK','xcdenbk_CHECK','EMAP_W1_HALL_CHECK')]),list('UID','XCMGW','xcdenbk','W1_HALL'))
# 
# #QRVeg1
# QR1$QRveg1=ifelse(QR1$XCMGW<=2.00,.1+(.9*(QR1$XCMGW/2)),1)
# #QRVeg2
# QR1$QRVeg2=0.1 + (0.9*(QR1$xcdenbk/100))
# #QRDIST1
# QR1$QRDIST1=1/(1+QR1$W1_HALL)
# #Final QR1 calculation
# QR1$QR1_CHECK=(QR1$QRveg1*QR1$QRVeg2*QR1$QRDIST1)^0.333
# 

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

##############                         Combine All Indicators Into One File                 #################

###################################################################################################################
###################################################################################################################
###################################################################################################################
# ###combine all objects that exist in workspace that contain indicator values####
IndicatorCheckJoin=listsites
IndicatorList=c("DensPvt","BnkDensPvt","XCMG_new1", "RIP_VEG","FQCY_VEG","WQfinal","Pools","LWD","ALLSED","FinalpvtPoolFines","BnkErosional","BnkAll","IncBnk","BnkRatioAvg","fishpvt2","MeanAngle","Thalweg","BankWidFinal","WetWidFinal","avgFloodWidth","entrench","Slope_Per","sidecount")
for (s in 1:length(IndicatorList)) {
if(exists(IndicatorList[s])==TRUE){IndicatorCheckJoin=join_all(list(IndicatorCheckJoin,as.data.frame(get(IndicatorList[s]))),by="UID")}
  else {IndicatorCheckJoin=IndicatorCheckJoin}
}
#####OLD CODE###### 
#To get all calculated values together... Although some tables still have the metrics included.
#IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkErosional,BnkAll,fishpvt2,DensPvt,BnkDensPvt,XCMGW_new1,XCMG_new1,XGB_new1,IncBnk,BankWid,WetWid,XEMBED,PCT_SAFN_ALL,W1_HALL,QR1,MeanAngle,Slope_Per,Thalweg,Pools),by="UID")
# #choose correct line to run below based on the indicators that you have run and want included
# IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkErosional,BnkAll,fishpvt2,DensPvt,BnkDensPvt,XCMG_new1,IncBnk,BankWidFinal,WetWidFinal,XEMBED,PCT_SAFN_ALL,MeanAngle,Slope_Per,Thalweg,Pools,LWD),by="UID")
# IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkErosional,BnkAll,DensPvt,BnkDensPvt,XCMG_new1,IncBnk,BankWidFinal,WetWidFinal,PCT_SAFN_ALL,MeanAngle,Thalweg,Pools,LWD,avgFloodWidth),by="UID")
# IndicatorCheckJoin=join_all(list(listsites,BnkErosional,BnkAll,fishpvt2,DensPvt,BnkDensPvt,XCMG_new1,IncBnk,BankWidFinal,WetWidFinal,XEMBED,PCT_SAFN_ALL,MeanAngle,Slope_Per,Thalweg,LWD),by="UID")
#2016,2017
#with pool tail fines
#IndicatorCheckJoin=join_all(list(listsites,DensPvt,BnkDensPvt,XCMG_new1, RIP_VEG,FQCY_VEG,WQfinal,Pools,LWD,ALLSED,FinalpvtPoolFines,BnkErosional,BnkAll,IncBnk,fishpvt2,MeanAngle,Thalweg,PctDry,BankWidFinal,WetWidFinal,avgFloodWidth,entrench,Slope_Per),by="UID")
#without pool tail fines
#IndicatorCheckJoin=join_all(list(listsites,DensPvt,BnkDensPvt,XCMG_new1, RIP_VEG,FQCY_VEG,WQfinal,Pools,LWD,ALLSED,BnkErosional,BnkAll,IncBnk,fishpvt2,MeanAngle,Thalweg,PctDry,BankWidFinal,WetWidFinal,avgFloodWidth,entrench,Slope_Per),by="UID")

# #2013,2014,2015
# IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkErosional,BnkAll,fishpvt2,DensPvt,BnkDensPvt,XCMG_new1,IncBnk,BankWidFinal,WetWidFinal,ALLSED,MeanAngle,Thalweg,PctDry,Pools,FinalpvtPoolFines,LWD,Slope_Per),by="UID")
# IndicatorCheckJoin=join_all(list(listsites,WQfinal,BnkErosional,BnkAll,fishpvt2,DensPvt,BnkDensPvt,XCMG_new1,IncBnk,BankWidFinal,WetWidFinal,ALLSED,MeanAngle,Thalweg,PctDry,Pools,LWD,Slope_Per),by="UID")
# 
#################################################################################


#remove QC sites
IndicatorCheckJoin=subset(IndicatorCheckJoin,!(UID %in% c('12457','12422','12714','13550','11787','13527','9779832504','13518','13539','8497901114','2772740176','3833994365','7194282454','9846034316','7977571143','4943503766','6152206654','6964535047','7746712455','2956707014','4324237804','4197418344','8537408400','4116634326','2109978745','41CD7AF9-B7EF-40AE-AF4E-5B28F77F7819','BC23CBBE-CA03-4AB8-985D-567DCFA49FA6','82EB06AD-705F-40B5-87F0-6EBFE4220D92','437DF621-C56F-4647-A5AB-A23A35DB726B','B0DB3C45-2D14-42F4-9722-7A5F3E66F355','C88896E2-FA3F-4D7D-AAD2-21C11E698C9B','A3378CF0-EA53-4C46-B858-4B580D001E9A','2D41FBB7-398F-43DD-A22A-93569E76493A','D0DF3643-D231-4925-9A99-9317EA79A785','FEC8303F-9604-46F8-BBC0-DDD151D5B597','39DFF320-179A-43C7-AEC1-677F4EC871F7','1E4A0F45-3584-4C48-9FD0-1AC2D156CB1A','33A60797-0B44-4A10-8030-3AFC75F027BC','2E3E82A1-CC26-4F50-ACD2-10B7C6157CED','ADFD9643-3ADE-482A-936F-B938A9C70DB5','5210AEB7-AEE9-43D8-876E-5A71F74E0C4E','9F83617B-5192-4AC6-99A4-9126AADE0699','942B415A-BD49-41EB-A9DD-2D7F8D59E87D','30846500-7DB4-4A35-B00B-764193AD1A83','8134492B-CE67-43D3-AB69-DB0C2EC47DDF','59D2716A-91E1-41F9-930F-4FE5B7DFBA8F','1730B528-5317-47E3-B83D-FFE5EB0C0D38','00C5DDB3-BC6B-43BD-AA27-EAAD160A1F3F','CD1D4C49-FBE5-4A34-BE4B-EB85218D5E10','21985428-7940-457A-82F4-74D82923FD1C','EC330DD5-C092-427F-9AC9-42C5C44F4179','C79C0FF8-E461-4C21-B9D6-150FBEE01C5A','1371C53E-AE90-41E8-9FB7-620424D79CF7','7B46F302-0E19-45E0-8658-230D6FA1EADB','E5B057BC-9219-49A8-A801-555C3CD2BCCA','D1E43428-901C-401E-81A8-29857CFD521F','1185793A-3DA2-4812-BC91-F68C1E5C4298','3743C97A-F5F0-4F33-880A-414DA4FA46D5','6B275A46-FE74-4078-A9E0-37B8C607DF9D','9E20708F-6303-4CDA-A7C4-A37B81201A9E','31B9E787-E769-4C2D-B348-4032D8B40925','38D0FEAF-1201-4E0C-A58C-0355B40D5260')))

#To remove all of the metrics and only get the indicators subset by UID and all those columns ending in "CHECK". Hmm..not really sure what the $ is doing here, the code works without it, but all the examples I've looked at keep the $ so I kept it too... 
IndicatorCheck=IndicatorCheckJoin[,c("UID",grep("CHECK$", colnames(IndicatorCheckJoin),value=TRUE))]
#write.csv(IndicatorCheck,"C:\\Users\\Nicole\\Desktop\\IndicatorCheck2.csv")
#Remove all other data files as they are no longer needed
#IndicatorCheck=subset(IndicatorCheck,PROTOCOL_CHECK=="BOAT14")
write.csv(IndicatorCheck,paste0('IndicatorCheck',Sys.Date(),'.csv'))

#rm(PHfinal,XGB_new,XGB_new1,BankStab,Banks,RipGB,EMBED,Human_Influ,W1_HALL,W1_HALL_NRSA,QR1,XEMBED,BnkDensPvt,BnkDensiom,densiom,RipXCMG,XCMG_new,XCMG_new1,RipWW,XCMGW_new,XCMGW_new1,IndicatorCheckJoin,fish,fishpvt2,
 #  MidDensiom,DensPvt,Incision,INCISED,BANKHT,Inc,Bnk,xIncht,xBnkht,IncBnk,Sediment,pctsafn,Sed2014,A_Sed2014,C_Sed2014,E_Sed2014,F_Sed2014,PCT_SAFN_ALL)


# ###################################################################################################################
# ########################################################################################################################
# ########################################################################################################################
# 
# #                                       Notes and Troubleshooting Start                                      #
# 
# ########################################################################################################################
# ########################################################################################################################
# ###################################################################################################################
# 
# #development of 2013 sediment indicators and boatable sediment indicators that were binned into categories
# Sed2014=bintranslate(Table='Sed2014',ST='CROSSSECW',PM='SIZE_NUM')
# 
# #build table of the min and max of each size class 
# #make sure to uncomment HP if want it to be "NA" but subsetting only measurable classes for D50 at least anyway
# tt <- textConnection(
#   "class min     max\n                    
#   RS 4000    8000\n                   
#   RR 4000    8000\n                    
#   RC 4000    8000\n                    
#   BH 4000    8000\n#boatable class?                    
#   XB 1000    4000\n                    
#   SB  250    1000\n                    
#   BL  250    4000\n#combined boulder class                   
#   CB   64     250\n                    
#   GC   16      64\n                    
#   GF    2      16\n                   
#   GR    2      64\n                    
#   SA    0.06    2\n                    
#   FN    0.001   0.06\n                   
#   HP 4000    8000\n
#   #HP   NA      NA\n                    
#   WD   NA      NA\n                    
#   OT   NA      NA\n"
# )
# subsInfo <- read.table(tt, header = TRUE, stringsAsFactors = FALSE)
# close(tt)
# #take the geometric mean for each size class
# #create function for geometric mean
# gmean <- function(x){exp(mean(log(x)))}
# subsInfo$diam <- NA
# for (s in 1:nrow(subsInfo)) {
#   subsInfo[s, ]$diam = gmean(c(subsInfo[s, ]$min, subsInfo[s, 
#                                                            ]$max))
# }
# #log geometric mean diameter and log of the min and max of size classes
# subsInfo$lDiam <- log10(subsInfo$diam)
# subsInfo$lmin = log10(subsInfo$min)
# subsInfo$lmax = log10(subsInfo$max)
# 
# #lsub_dmm - this code replicates aquamet for all 2013 data but 
# #list of substrate classes included in lsub_dmm
# wadeableAllOneBoulderClass <- c("RS", "RR", "RC", "BL", "CB", 
#                                 "GC", "GF", "SA", "FN", "HP", "WD", "OT")
# #calculating lsub_dmm by taking the mean of the log geometric mean diameter 
# df1lb <- Sediment#input dataframe
# df1lb$RESULT <- ifelse(df1lb$RESULT %in% c("XB", "SB"), 
#                        "BL", df1lb$RESULT)#merging the small and large boulder classes
# ldBuglb <- merge(df1lb, subset(subsInfo, class %in% wadeableAllOneBoulderClass, 
#                                select = c(class, diam, lDiam)), by.x = "RESULT", 
#                  by.y = "class", all.x = TRUE)#subsetting just the classes included in 1sub_dmm
# ldBug11lb <- setNames(aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
#                                 mean, na.rm = TRUE), c("UID","lsub_dmm_CHECK"))# calculating the mean which is "lsub_dmm"
# 
# 
# #list of measureable classes included in D50
# wadeableMeasurableTwoBoulderClasses <- c("XB", "SB", "CB", 
#                                          "GC", "GF", "SA", "FN")
# interpdata <- subset(Sediment, RESULT %in% wadeableMeasurableTwoBoulderClasses)
# measurable <- setNames(subset(subsInfo, class %in% wadeableMeasurableTwoBoulderClasses, 
#                               select = c(class, lmin, lmax)), c("CLASS", "min", "max"))#setNames is doing something different than the internal rename function that was originally in aquamet
# #####################################################################################
# # A single object matching ‘interpolatePercentile’ was found
# # It was found in the following places
# # package:aquamet
# # namespace:aquamet
# # with value
# 
# interpolatePercentile<-function (df, classVar, percentile, pctlVar, classBounds) 
# {
#   df <- subset(df, !is.na(classVar))
#   classCounts <- aggregate(list(classCount = df[[classVar]]), 
#                            list(UID = df$UID, CLASS = df[[classVar]]), count)#counting the number of each size class category per uid
#   sampleSizes <- aggregate(list(totalCount = df[[classVar]]), 
#                            list(UID = df$UID), count)# counting total number of pebbles per UID
#   classPcts <- merge(classCounts, sampleSizes, by = "UID")
#   classPcts$pct <- 100 * classPcts$classCount/classPcts$totalCount# percent each size class makes up of all the pebbles collected at a site
#   classPcts <- merge(classPcts, classBounds, by = "CLASS", 
#                      all.x = TRUE)# add the bounds for each size class to the table
#   classPcts <- classPcts[order(classPcts$UID, classPcts$min), 
#                          ]# sort by UID and the smallest susbtrate first
#   classPcts$upperPct <- ave(classPcts$pct, classPcts$UID, FUN = cumsum)#cumulative sum of the percent of each size class per UID --example GF50+GF50+CB25+(GF50+CB25+SB25)
#   classPcts <- first(classPcts, "UID", "start")#another internal aquamet function using function "lag"
#   classPcts <- lag(classPcts, "upperPct", "lowerPct")
#   classPcts[classPcts$start, ]$lowerPct <- 0
#   tt <- subset(classPcts, lowerPct < percentile & percentile <= 
#                  upperPct) # percentile will not always fall on a size class so need to determine which two size classes it is in between and then interpolate using line below
#   tt[pctlVar] <- with(tt, min + (max - min) * (percentile - 
#                                                  lowerPct)/(upperPct - lowerPct))# linear interpolation equation 2.15 in bunte and abte 2001 pg 41
#   tt <- tt[c("UID", pctlVar)]
#   return(tt)
# }
# #################################################################################
# A single object matching ‘first’ was found
# It was found in the following places
# package:aquamet
# namespace:aquamet
# with value
# 
# function (df, v, first.v) 
# {
#   df <- lag(df, v, "..vLag", offset = 1)
#   df[first.v] <- as.vector(ifelse(is.na(df[v]) | is.na(df["..vLag"]), 
#                                   is.na(df[v]) != is.na(df["..vLag"]), df[v] != df["..vLag"]))
#   df[1, first.v] <- TRUE
#   df["..vLag"] <- NULL
#   return(df)
# }
# ##################################################################################
# 
# 
# 
# c16 <- interpolatePercentile(interpdata, "RESULT", 16, 
#                              "lsub2d16inor", measurable)
# c50 <- interpolatePercentile(interpdata, "RESULT", 50, 
#                              "lsub2d50inor", measurable)
# c84 <- interpolatePercentile(interpdata, "RESULT", 84, 
#                              "lsub2d84inor", measurable)
# c16$d16 <- 10^(c16$lsub2d16inor)
# c50$d50 <- 10^(c50$lsub2d50inor)
# c84$d84 <- 10^(c84$lsub2d84inor)
# calcs <- merge(c16, merge(c50, c84, by = "UID", all = TRUE), 
#                by = "UID", all = TRUE)
# 
# 

#Get pH for NorCal
# PHtbl=tblRetrieve(Parameters=c('PH'),Projects=projects,Years=years,Protocols=protocols)
# PHpvt=cast(PHtbl,'UID~PARAMETER',value='RESULT')
# PHfinal=addKEYS(PHpvt,c('SITE_ID','DATE_COL','LOC_NAME'))
# PHfinal=PHfinal[,c(1,5,4,3,2)]
# rm(PHtbl,PHpvt)

#Get Site Code to UID, run the water quality lines above and just pull from that...This didn't work for what I initially needed, but is a good way to get UID/Sitecode
#UID_SiteCode=WQfinal[,c('UID','SITE_ID')]

#bank stability side channel variations
#combine data from main channels and side channels
#BankStab$TRANSECT=mapvalues(BankStab$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))# no longer do this!!!!!!!!!!!!!!!!!!!!!!!!
#Banks=cast(BankStab, 'UID+TRANSECT+POINT~PARAMETER', value='RESULT', fun=max)#take whichever main or side channel is most unstable (max uses the highest alphbetically which is Erosional, uncovered, and slump--(eroding is least stable but since getting combined it doesn't matter)) #no longer do this!!!!!!!!!!!

#Riparian Complexity TROUBLESHOOTING 
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

#pct_SAFN troubleshooting
##TO check if bed and bank measurements were included I ran this code. This code does not distinguish between bed or bank just runs to get the mean of all 2014 particles, regardless of location. THis shows that Sarah's code is missing the BED/BANK determinations....
#WR_Sed2014$SAFN_True=ifelse(WR_Sed2014$RESULT == "1", 1, 0)
#WR1_Sed2014=cast(WR_Sed2014,'UID~PARAMETER', value='SAFN_True', fun=mean)
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

#Floodplain connectivity troubleshooting
##IncBnk2=cast(Incision,'UID~PARAMETER', value='RESULT', fun=mean)
##INCISED=Incision[,c("UID","TRANSECT","PARAMETER","RESULT")]
#IB=aggregate(Incision)
#IB=cast(Incision,'UID~PARAMETER',  value='RESULT', fun=aggregate)
#IBagg=aggregate(Incision,by='PARAMETER',value='RESULT', FUN=mean)
###LINCIS_h REMOVAL OF SIDE CHANNELS TO FIX PLROBLEM.. DIDN'T FIX THE ISSUE... 
#NoSide= subset(Incision, TRANSECT == "A"|TRANSECT == "B"|TRANSECT == "C"|TRANSECT == "D"|TRANSECT == "E"|TRANSECT == "F"|TRANSECT == "G"|TRANSECT == "H"|TRANSECT == "I"|TRANSECT == "J"|TRANSECT == "K")
#NS_IncBnk=cast(NoSide,'UID~PARAMETER', value='RESULT', fun=mean)
#NS_IncBnk$LINCIS_H_Check=log10(NS_InkBnk$INCISED-NS_IncBnk$BANKHT+0.1)

# pool fail fines troubleshooting 
####trying to figure out how to do averages all in one step but it averages transects and points ect or addes extra UID columns and Transect columns in
# agg2pvtPoolFines=aggregate(.~UID,data=pvtPoolFines, FUN='mean')#average pool fines at a pool first#note this method excludes NAs...I think that is OK though
# agg2pvtPoolFines$PctPoolFines2_CHECK=round(agg2pvtPoolFines$PctPoolFines2_CHECK,digits=2)
# agg2pvtPoolFines$PctPoolFines6_CHECK=round(agg2pvtPoolFines$PctPoolFines6_CHECK,digits=2)
# agg3pvtPoolFines=setNames(aggregate(.~UID,data=pvtPoolFines, FUN='sd'),c("UID","TRANSECT","POOLFINES2SD","POOLFINES6SD","POOLFINES#average pool fines at a pool first#note this method excludes NAs...I think that is OK though
####Alternative method that dones't exclude NAs but it somehow adds multiple UID columns...melt is yet another option but I deleted that section of code
#aggpvtPoolFines=aggregate(pvtPoolFines,by=list(UID=pvtPoolFines$UID,TRANSECT=pvtPoolFines$TRANSECT), FUN='mean')#average pool fines at a pool first
#agg2pvtPoolFines=aggregate(aggpvtPoolFines,by=list(UID=aggpvtPoolFines$UID), FUN='mean')#then average across pools
#agg3pvtPoolFines=aggregate(aggpvtPoolFines,by=list(UID=aggpvtPoolFines$UID), FUN='sd')#then average across pools
#####cast and melt options
# pvtPoolFines=cast(PoolFines,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')#need to pivot to create the pctPoolFInes variable 
# pvtPoolFines$PctPoolFines2_CHECK=pvtPoolFines$POOLFINES2/(50-pvtPoolFines$POOLNOMEAS)*100
# pvtPoolFines$PctPoolFines6_CHECK=pvtPoolFines$POOLFINES6/(50-pvtPoolFines$POOLNOMEAS)*100
# PoolFinesMelt=melt.data.frame(pvtPoolFines,id=c('UID','TRANSECT','POINT'))# make flat again so we can use the cast function to take the average ...more elegant solution would be writing a loop but rest of indicators all use cast so stuck with this approach
# pvt2PoolFines=cast(PoolFinesMelt,'UID+TRANSECT~variable',value='value',fun='mean')#average 3 replicates per pool first
# PoolFinesMelt2=melt.data.frame(pvt2PoolFines,id=c('UID','TRANSECT')) # make flat again so we can use cast function to take mean of all pools
# pvt3PoolFines=cast(PoolFinesMelt2,'UID~variable',value='value',fun='mean')#take average of average pool tail fines at each pool

# #EPA Reach Length
# cdData <- tblRetrieve(Parameter=c("ACTRANSP", "DISTANCE", "INCREMNT", "SIDCHN", "OFF_CHAN", "REACHLENGTH"), Projects=projects,Years=years,Protocols=protocols)
# metsoutGeneral <- metsGeneral(thalweg, channelgeometry)#working, but odd values coming out for reachlength still >> trouble shoot thalweg$STATION (currently set to numeric); seems to be good >> next up:  cdData data subset driving the station count which results in 0 for many NRSA sites which don't include all the "No"s for side channel, etc. >> cdData <- subset(indat, PARAMETER %in% c("ACTRANSP", "DISTANCE", "INCREMNT", "SIDCHN", "OFF_CHAN", "REACHLENGTH"))
# rlen=subset(metsoutGeneral,METRIC=='reachlen'); rlen$VALUEl=rlen$RESULT-(rlen$RESULT*.1); rlen$VALUEh=rlen$RESULT+(rlen$RESULT*.1)#10% bounds
# thalCheck=tblRetrieve(Parameters='TRCHLEN',UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
# thalCheck=merge(rlen,thalCheck,intersect(setdiff(colnames(metsoutGeneral),'RESULT'),colnames(thalCheck))),all.x=T)
# thalCheck$check=ifelse(thalCheck$RESULT>thalCheck$VALUEl & thalCheck$RESULT<thalCheck$VALUEh,'OK','X');thalCheck=subset(thalCheck,check=='X')
# if(nrow(thalCheck)>0){print("WARNING! Calculated reach length (METRIC=='reachlen') significantly different from field recorded Reach Length (PARAMETER=='TRCHLEN'). Confirm correct INCREMENT parameter.")}
