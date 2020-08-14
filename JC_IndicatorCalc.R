###################################################################################################################

##############                         Set Up                         #################

###################################################################################################################
#only indicators with "_CHECK" added on the name are in the end output.

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
                                    ifelse(listsites$VALXSITE_CHECK=="SUBSETWADE","Sampled - Core Subset",
                                    ifelse(listsites$VALXSITE_CHECK=="INTPARBYWADE","Sampled - Interrupted Flow and Partial Reach",listsites$VALXSITE_CHECK )
                                    ))))
listsites$IndicatorsCollected_CHECK=listsitesFieldStatus_CHECK=ifelse(listsites$VALXSITE_CHECK=="SUBSETWADE","Core Subset","Core" )

# #listsites=listsites[,c(1,12,6,2,3,7,10,13,11,5,9,4,8)]
# #run list sites and TRCHLEN below to get sinuosity data
TRCHLEN1=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)#not using TRCHLEN
TRCHLEN1$RESULT=as.numeric(TRCHLEN1$RESULT)

#Side channel info
side=tblRetrieve(Parameters=c('SIDCHN','SIDCH_TYPE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
sidecount=setNames(addKEYS(cast(side,'UID~PARAMETER',value='RESULT'),c('SITE_ID','DATE_COL','PROJECT')),c('UID','NumSideChn_CHECK','NumTran','DATE_COL','PROJECT','SITE_ID'))#count is default function
sidecount$SIDCHN_PRES_CHECK=ifelse(sidecount$NumSideChn_CHECK>0,"PRESENT","ABSENT")


##########################################################################################
###### Biodiversity and Riparian Habitat Quality ######
##########################################################################################
#xcdenmid and xcdenbk - canopy cover
densiom=tblRetrieve(Parameters='DENSIOM',Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
densiom$RESULT=as.numeric(densiom$RESULT)

#XCMG - vegetative complexity
#RipALL=tblRetrieve(Parameters=c("BARE","CANBTRE","CANSTRE","CANVEG","GCNWDY","GCWDY","UNDERVEG","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
RipXCMG=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCNWDY","GCWDY","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
RipXCMG$RESULT=as.numeric(RipXCMG$RESULT)
RipWW=tblRetrieve(Parameters=c("CANBTRE","CANSTRE","GCWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
RipWW$RESULT=as.numeric(RipWW$RESULT)
RipGB=tblRetrieve(Parameters=c("BARE"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
RipGB$RESULT=as.numeric(RipGB$RESULT)

#BLM riparian cover and frequency
RipBLM=tblRetrieve(Parameters=c('CANRIPW','UNRIPW','GCRIP','INVASW', 'NATIVW','INVASH','NATIVH','SEGRUSH','INVASAQ'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)

#Bug data
#bug metrics are calculated outside of this script and joined back in
#bug metric data can be found at "Z:\\buglab\\Research Projects\\AIM\\Analysis\\WQ_Bug_Model_GIS_stats_and_results\\final_bug_scores_R_input_join_to_all_other_metrics_statebasedscores.xlsx"
#an SQL query to get the presence or absence of invasives can be found at "Z:\buglab\Research Projects\BLM_WRSA_Stream_Surveys\Results and Reports\AIM_2011_2015_results\invasives_query.sql"



###############################################################################################
###### Water Quality ######
###############################################################################################
#All WQ data
WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','NTL','PTL','TURBIDITY','TEMPERATURE','EC_PRED','TN_PRED','TP_PRED'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
WQtbl$RESULT=as.numeric(WQtbl$RESULT)
WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')




###############################################################################################
##### Watershed Function and Instream Habitat Quality ######
###############################################################################################
#Pools
pool_length=tblRetrieve(Parameters=c('LENGTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
pool_length$RESULT=as.numeric(pool_length$RESULT)
reach_length=tblRetrieve(Parameters=c('POOLRCHLEN'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
reach_length$RESULT=as.numeric(reach_length$RESULT)
PoolDepth=tblRetrieve(Parameters=c('PTAILDEP','MAXDEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
PoolDepth$RESULT=as.numeric(PoolDepth$RESULT)
poolcollect=tblRetrieve(Parameters='POOL_COLLECT',Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)

#LWD- should query wadeable and boatable wood because boatable wood stored under wadeable?
LwdCatWet=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'W%'"))$PARAMETER
LwdCatDry=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'D%'"))$PARAMETER
LwdWet=addKEYS(tblRetrieve(Parameters=LwdCatWet,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','DATE_COL'))
LwdWet$RESULT=as.numeric(LwdWet$RESULT)
LwdDry=tblRetrieve(Parameters=LwdCatDry,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
LwdDry$RESULT=as.numeric(LwdDry$RESULT)
TRCHLEN=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)#not using TRCHLEN
TRCHLEN$RESULT=as.numeric(TRCHLEN$RESULT)
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

#Pool Tail Fines
PoolFines=tblRetrieve(Parameters=c('POOLFINES2','POOLFINES6','POOLNOMEAS'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
PoolFines$RESULT=as.numeric(PoolFines$RESULT)

#Bank Stability
BankStab=tblRetrieve(Parameters=c('STABLE','EROSION','COVER_FOLIAR','COVER_BASAL','BNK_VEG_FOLIAR','BNK_VEG_BASAL','BNK_COBBLE','BNK_LWD','BNK_BEDROCK'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
SideBank=tblRetrieve(Parameters=c('SIDCHN_BNK'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)

#LINCIS_H - floodplain connectivity
Incision=tblRetrieve(Parameters=c('INCISED','BANKHT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
Incision$RESULT=as.numeric(Incision$RESULT)

#XFC_NAT- fish cover
fish=tblRetrieve(Parameters=c('BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
fish$RESULT=as.numeric(fish$RESULT)

#Angle-PIBO method only
Angle=tblRetrieve(Parameters=c('ANGLE180'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
Angle$RESULT=as.numeric(Angle$RESULT)

#Thalweg mean , CV, and pct dry
thalweg=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROTOCOL'))
thalweg$RESULT=as.numeric(thalweg$RESULT)
thalweg=subset(thalweg,SAMPLE_TYPE!='CROSSSECW')

#RP100 and other EPA pool metrics 


#########################################################################################################
###### Covariates/ Other ######
#########################################################################################################
#Channel Dimensions
WetWid=tblRetrieve(Parameters=c('WETWIDTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)#Wetted widths from thalweg
WetWid$RESULT=as.numeric(WetWid$RESULT)
WetWid2=tblRetrieve(Parameters=c('WETWID'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)#Wetted widths from main transects
WetWid2$RESULT=as.numeric(WetWid2$RESULT)
BankWid=tblRetrieve(Parameters=c('BANKWID'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
BankWid$RESULT=as.numeric(BankWid$RESULT)

#Floodprone width
#pre2017
#FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
#2017 plus
FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID','FLOOD_BFWIDTH'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
FloodWidth$RESULT=as.numeric(FloodWidth$RESULT)
#FloodWidthpvt=cast(FloodWidth,'UID+TRANSECT~PARAMETER',value='RESULT')
#Slope
#2017
Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','PCT_GRADE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)                 
Slope$RESULT=as.numeric(Slope$RESULT)

#run site descriptors for sinuosity data



##########################################################################################################
##### Other metrics still being worked on or old metrics no longer used #####
##########################################################################################################
#W1_HALL- human influence
#Figure out the differences... Human influence sample type...
Human_Influ=tblRetrieve(Parameters=c('BUILD','LOG','MINE','PARK','PAST','PAVE','PIPES','ROAD','ROW','TRASH','WALL'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)



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



################################## these are not included in the end exported file but are things that we might include eventually #################
#####layer by layer#####
RipXCMG$ResultsPer=ifelse(RipXCMG$RESULT == 1, 0.05,ifelse(RipXCMG$RESULT == 2, 0.25,ifelse(RipXCMG$RESULT == 3, 0.575,ifelse(RipXCMG$RESULT == 4, 0.875,ifelse(RipXCMG$RESULT ==0, 0, NA)))))
layers=cast(RipXCMG,'UID~PARAMETER', value='ResultsPer',fun='mean')

######## other EPA veg complexity variations ###########
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


###################################################################################################################################################

##BLM riparian cover and frequency##
#Riparian vegetation cover
RIP_VEG=subset(RipBLM, PARAMETER == 'CANRIPW'|PARAMETER == 'UNRIPW'|PARAMETER == 'GCRIP')
RIP_VEG$RESULT=as.numeric(RIP_VEG$RESULT)
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
# #pre2019 data doesnt have INVASAQ and derived indicators
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


#########################################################

#        PctFinesLessThan 2mm and 6mm and Other Sediment Indicators         #

#########################################################

#pct_safn

#### 2013 data and Boating data which was also stored in Size_CLS. 2013 field protocol only collected Bed sediment, unlike 2014 and beyond which collected bed and bank sediment. 
####Doing sand and fines together
Sediment=subset(Sediment,RESULT!="OT"& RESULT!="WD") #removing all other or wood particles
Sediment$SAFN_True=ifelse(Sediment$RESULT == "SA", 1,ifelse(Sediment$RESULT == "FN", 1, 0))
pctsafn=setNames((aggregate(Sediment$SAFN_True,by=list(UID=Sediment$UID), data=Sediment, FUN='mean')),c("UID","bedPCT_SAFN2_CHECK"))#had to remove NorCal code that casted by Sample_Type because of boating data
pctsafn$bedPCT_SAFN2_CHECK=round(pctsafn$bedPCT_SAFN2_CHECK*100,digits=1)
Sedimentpvt=cast(Sediment,'UID~PARAMETER',value='RESULT',fun=length)# number of pebbles collected at intermediate and main transects
#Sedimentpvt$nbedPCT_SAFN_CHECK=(Sedimentpvt$SIZE_CLS+Sedimentpvt$XSIZE_CLS) # old data has data stored in both these parameters newer data only has size_cls
Sedimentpvt$nbedPCT_SAFN_CHECK=(Sedimentpvt$SIZE_CLS) # number of pebbles collected at all transects only 4 boating sites had less than 50
Sedimentpvtsub=subset(Sedimentpvt,select=c(UID,nbedPCT_SAFN_CHECK))
pctsafn_2013=merge(pctsafn,Sedimentpvtsub,by="UID")
pctsafn_2013$bedPCT_SAFN6_CHECK<-NA
#need to uncomment these lines for boatable data!!
#pctsafn_2013$nallPCT_SAFN_CHECK=pctsafn_2013$nbedPCT_SAFN_CHECK#duplicate bed data into a variable for all particles so that the data comes over in one column when combined with 2014+ data but this data should be used with caution because of protocol differences!
#pctsafn_2013$allPCT_SAFN_CHECK=pctsafn_2013$bedPCT_SAFN_CHECK#duplicate bed data into a variable for all particles so that the data comes over in one column when combined with 2014+ data but this data should be used with caution because of protocol differences!


#Now for 2014 data... 
Sed2014=subset(Sed2014,RESULT!='0')# removing all "Other" particles so that only inorganic particles are included in the PctSAFN calc
A_Sed2014=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')

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
PCT_SAFN_ALL=addKEYS(PCT_SAFN_ALL,c('PROTOCOL','PROJECT'))
PCT_SAFN_ALL$allPCT_SAFN2_CHECK=ifelse(grepl("^AK", PCT_SAFN_ALL$PROJECT)==TRUE, ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$allPCT_SAFN2_CHECK),
                                       ifelse(grepl("^BOAT",PCT_SAFN_ALL$PROTOCOL)==TRUE,ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$allPCT_SAFN2_CHECK),
                                              ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<100,NA,PCT_SAFN_ALL$allPCT_SAFN2_CHECK)))
PCT_SAFN_ALL$bedPCT_SAFN2_CHECK=ifelse(grepl("^AK", PCT_SAFN_ALL$PROJECT)==TRUE, ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$bedPCT_SAFN2_CHECK),
                                       ifelse(grepl("^BOAT",PCT_SAFN_ALL$PROTOCOL)==TRUE,ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$bedPCT_SAFN2_CHECK),
                                              ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<100,NA,PCT_SAFN_ALL$bedPCT_SAFN2_CHECK)))
PCT_SAFN_ALL$allPCT_SAFN6_CHECK=ifelse(grepl("^AK", PCT_SAFN_ALL$PROJECT)==TRUE, ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$allPCT_SAFN6_CHECK),
                                       ifelse(grepl("^BOAT",PCT_SAFN_ALL$PROTOCOL)==TRUE,ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$allPCT_SAFN6_CHECK),
                                              ifelse(PCT_SAFN_ALL$nallPCT_SAFN_CHECK<100,NA,PCT_SAFN_ALL$allPCT_SAFN6_CHECK)))
PCT_SAFN_ALL$bedPCT_SAFN6_CHECK=ifelse(grepl("^AK", PCT_SAFN_ALL$PROJECT)==TRUE, ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$bedPCT_SAFN6_CHECK),
                                       ifelse(grepl("^BOAT",PCT_SAFN_ALL$PROTOCOL)==TRUE,ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<50,NA,PCT_SAFN_ALL$bedPCT_SAFN6_CHECK),
                                              ifelse(PCT_SAFN_ALL$nbedPCT_SAFN_CHECK<100,NA,PCT_SAFN_ALL$bedPCT_SAFN6_CHECK)))
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
ALLSED$GEOMEAN_CHECK=ifelse(grepl("^AK", ALLSED$PROJECT)==TRUE, ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$GEOMEAN_CHECK),
                            ifelse(grepl("^BOAT",ALLSED$PROTOCOL)==TRUE,ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$GEOMEAN_CHECK),
                                   ifelse(ALLSED$nallPCT_SAFN_CHECK<100,NA,ALLSED$GEOMEAN_CHECK)))
ALLSED$D50_CHECK=ifelse(grepl("^AK", ALLSED$PROJECT)==TRUE, ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D50_CHECK),
                        ifelse(grepl("^BOAT",ALLSED$PROTOCOL)==TRUE,ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D50_CHECK),
                               ifelse(ALLSED$nallPCT_SAFN_CHECK<100,NA,ALLSED$D50_CHECK)))
ALLSED$D16_CHECK=ifelse(grepl("^AK", ALLSED$PROJECT)==TRUE, ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D16_CHECK),
                        ifelse(grepl("^BOAT",ALLSED$PROTOCOL)==TRUE,ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D16_CHECK),
                               ifelse(ALLSED$nallPCT_SAFN_CHECK<100,NA,ALLSED$D16_CHECK)))
ALLSED$D84_CHECK=ifelse(grepl("^AK", ALLSED$PROJECT)==TRUE, ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D84_CHECK),
                        ifelse(grepl("^BOAT",ALLSED$PROTOCOL)==TRUE,ifelse(ALLSED$nallPCT_SAFN_CHECK<50,NA,ALLSED$D84_CHECK),
                               ifelse(ALLSED$nallPCT_SAFN_CHECK<100,NA,ALLSED$D84_CHECK)))

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
#Decided in 2019 not to remove depositonal banks!
#Pivot data so that Each parameter has it's own column
#changed in 2019 NOT to averages across all side channel and main channel banks.
##PIBO takes only the outside banks but for pre-2016 data we can't determine which are the outside banks after the fact so we averaged across all banks that were collected.
Banks=cast(BankStab, 'UID+TRANSECT+POINT~PARAMETER', value='RESULT')#run this for all years data and then if 2016 data run subsection below

###################################################################
#run this section for 2016+ data
#for 2016 and beyond take only data from outside banks... run above code plus the code below for 2016 data
#get which side of the main channel the side channel was on to determine outside banks
#if side channel on right bank need to use right bank for X transect data and use left bank data for main transect
#if side channel on left bank need to use left bank for X transect data and use right bank data for main transect
pvtSideBank1=cast(SideBank,'UID+TRANSECT~PARAMETER',value='RESULT')
pvtSideBank1$TRANSECT=sub("^","X",pvtSideBank1$TRANSECT)
#need to only get side channel data that has corresponding bank stability data, before 2019 or 2018? we did not collect data at all kinds of side channels but did note their presence
sidechanneltran=join(Banks,pvtSideBank1, by=c('UID','TRANSECT'))
sidechanneltransub=subset(sidechanneltran,is.na(sidechanneltran$SIDCHN_BNK)==FALSE)
sidechanneltransub=subset(sidechanneltransub, SIDCHN_BNK==POINT)
pvtSideBank1=sidechanneltransub[,c("UID","TRANSECT","SIDCHN_BNK")]

#need to get data to be opposite for main channel
pvtSideBank2=pvtSideBank1
pvtSideBank2$TRANSECT=sub("^X","",pvtSideBank1$TRANSECT)
#need to get data to be opposite for main channel
pvtSideBank2$SIDCHN_BNK=ifelse(pvtSideBank2$SIDCHN_BNK=='LF','RT',ifelse(pvtSideBank2$SIDCHN_BNK=='RT','LF',pvtSideBank2$SIDCHN_BNK))
pvtSideBank3=rbind(pvtSideBank1,pvtSideBank2)
Banks=merge(Banks,pvtSideBank3,by=c('UID','TRANSECT'), all=T)
Banks$SIDCHN_BNK=ifelse(is.na(Banks$SIDCHN_BNK)==T,Banks$POINT,Banks$SIDCHN_BNK)
Banks=subset(Banks,Banks$SIDCHN_BNK==Banks$POINT)

#####################################################################
####this section is for all years
#I want to calculate the percent of banks that are Covered.  
#have to uncomment basal lines to get that indicator. Not collected in 2020 so commented out to get code to work
#Banks$CoverValueBasal=as.numeric(ifelse(Banks$COVER_BASAL=='UC',"0",ifelse(Banks$COVER_BASAL=='CV',"1",NA)))
Banks$CoverValueFoliar=as.numeric(ifelse(Banks$COVER_FOLIAR=='UC',"0",ifelse(Banks$COVER_FOLIAR=='CV',"1",NA)))
#I want to calculate the percent of banks that are Stable (Absent) 
# Unstable==(Fracture, slump, slough, eroding)
BanksNA=subset(Banks,is.na(EROSION)==TRUE)
BanksNA$StableValue=NA
BanksDepositionalFoliar=subset(Banks,EROSION=='DP'&is.na(Banks$CoverValueFoliar)==FALSE)
#BanksDepositionalBasal=subset(Banks,EROSION=='DP'&is.na(Banks$CoverValueFoliar)==TRUE)
BanksErosional=subset(Banks,EROSION=='EL')  
BanksDepositionalFoliar$StableValue=as.numeric(ifelse(BanksDepositionalFoliar$CoverValueFoliar=='1',"1",
                                           ifelse(BanksDepositionalFoliar$CoverValueFoliar=='0',"0","NA")))

#BanksDepositionalBasal$StableValue=as.numeric(ifelse(BanksDepositionalBasal$CoverValueBasal=='1', "1",
#                                                              ifelse(BanksDepositionalBasal$CoverValueBasal=='0',"0","NA")))
BanksErosional$StableValue=as.numeric(ifelse(BanksErosional$STABLE %in% c('SP','ER','LH','FC'),"0",
                                             ifelse(BanksErosional$STABLE=='AB',"1","NA")))
#Banks=rbind(BanksDepositionalBasal,BanksDepositionalFoliar,BanksErosional,BanksNA)
Banks=rbind(BanksDepositionalFoliar,BanksErosional,BanksNA)


#combined stability and cover
Banks$BnkCoverFoliar_Stab=as.numeric(ifelse((Banks$CoverValueFoliar+Banks$StableValue)<2,0,1))
#Banks$BnkCoverBasal_Stab=as.numeric(ifelse((Banks$CoverValueBasal+Banks$StableValue)<2,0,1))

#changed in 2019 to only compute indicators to match MIM which includes all banks (erosional and depositional)
BanksAll=Banks
#BnkCvrBasalAll=setNames(aggregate(CoverValueBasal~UID,data=BanksAll, FUN=mean), c('UID','BnkCoverBasal_All_CHECK'))
BnkCvrFoliarAll=setNames(aggregate(CoverValueFoliar~UID,data=BanksAll, FUN=mean), c('UID','BnkCoverFoliar_All_CHECK'))
BnkStbAll=setNames(aggregate(StableValue~UID,data=BanksAll, FUN=mean), c('UID','BnkStability_All_CHECK'))
BnkCoverFoliar_StabAll=setNames(aggregate(BnkCoverFoliar_Stab~UID,data=BanksAll, FUN=mean), c('UID','BnkCoverFoliar_StabAll_CHECK'))
#BnkCoverBasal_StabAll=setNames(aggregate(BnkCoverBasal_Stab~UID,data=BanksAll, FUN=mean), c('UID','BnkCoverBasal_StabAll_CHECK'))
BNK_BEDROCK=setNames(aggregate(as.numeric(BNK_BEDROCK)~UID,data=BanksAll, FUN=mean),c('UID','BNK_BEDROCK_CHECK'))
BNK_COBBLE=setNames(aggregate(as.numeric(BNK_COBBLE)~UID,data=BanksAll, FUN=mean),c('UID','BNK_COBBLE_CHECK'))
BNK_LWD=setNames(aggregate(as.numeric(BNK_LWD)~UID,data=BanksAll, FUN=mean),c('UID','BNK_LWD_CHECK'))
#BNK_VEG_BASAL=setNames(aggregate(as.numeric(BNK_VEG_BASAL)~UID,data=BanksAll, FUN=mean),c('UID','BNK_VEG_BASAL_CHECK'))
BNK_VEG_FOLIAR=setNames(aggregate(as.numeric(BNK_VEG_FOLIAR)~UID,data=BanksAll, FUN=mean),c('UID','BNK_VEG_FOLIAR_CHECK'))

#samplesize
nBnkCover_StabAll=setNames(aggregate(STABLE~UID,data=BanksAll, FUN=length), c('UID','nBnkCover_StabAll_CHECK'))

#join all Bank Files
#BnkAll=join_all(list(BnkCoverFoliar_StabAll,BnkCoverBasal_StabAll,BnkCvrFoliarAll,BnkCvrBasalAll, BnkStbAll,BNK_BEDROCK,BNK_COBBLE,BNK_LWD,BNK_VEG_BASAL,BNK_VEG_FOLIAR,nBnkCover_StabAll), by="UID",type="full")
BnkAll=join_all(list(BnkCoverFoliar_StabAll,BnkCvrFoliarAll, BnkStbAll,BNK_BEDROCK,BNK_COBBLE,BNK_LWD,BNK_VEG_FOLIAR,nBnkCover_StabAll), by="UID",type="full")

#convert to percent
BnkAll$BnkCoverFoliar_All_CHECK=round(BnkAll$BnkCoverFoliar_All_CHECK*100,digits=0)
#BnkAll$BnkCoverBasal_All_CHECK=round(BnkAll$BnkCoverBasal_All_CHECK*100,digits=0)
BnkAll$BnkStability_All_CHECK=round(BnkAll$BnkStability_All_CHECK*100,digits=0)
BnkAll$BnkCoverFoliar_StabAll_CHECK=round(BnkAll$BnkCoverFoliar_StabAll_CHECK*100,digits=0)
#BnkAll$BnkCoverBasal_StabAll_CHECK=round(BnkAll$BnkCoverBasal_StabAll_CHECK*100,digits=0)
BnkAll$BNK_BEDROCK_CHECK=round(BnkAll$BNK_BEDROCK_CHECK,digits=0)
BnkAll$BNK_COBBLE_CHECK=round(BnkAll$BNK_COBBLE_CHECK,digits=0)
BnkAll$BNK_LWD_CHECK=round(BnkAll$BNK_LWD_CHECK,digits=0)
BnkAll$BNK_VEG_FOLIAR_CHECK=round(BnkAll$BNK_VEG_FOLIAR_CHECK,digits=0)
#BnkAll$BNK_VEG_BASAL_CHECK=round(BnkAll$BNK_VEG_BASAL_CHECK,digits=0)


#remove cases with less than 45% of data- boatable 5 transects 2 banks=10
#42 total=2 banks at 21 transects (11 main and 10 intermediate)= tell them 5 main and 4 intermediate transects=9*2 banks=18
#changed in 2019 to require 21 for wadeable
#exclude=subset(BnkAll,nBnkCover_StabAll_CHECK<11)#15 excluded
BnkAll=addKEYS(BnkAll,c('PROTOCOL'))
BnkAllBoatable=subset(BnkAll,BnkAll$PROTOCOL %in% c('BOAT2016','BOAT14','BOAT2020'))
BnkAllWadeable=subset(BnkAll,BnkAll$PROTOCOL %in% c('NRSA13','WRSA14','AK14','WADE2016','WADE2020'))

BnkAllBoatable$BnkCoverFoliar_StabAll_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BnkCoverFoliar_StabAll_CHECK) 
#BnkAllBoatable$BnkCoverBasal_StabAll_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BnkCoverBasal_StabAll_CHECK) 
BnkAllBoatable$BnkCoverFoliar_All_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BnkCoverFoliar_All_CHECK)  
#BnkAllBoatable$BnkCoverBasal_All_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BnkCoverBasal_All_CHECK)  
BnkAllBoatable$BnkStability_All_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BnkStability_All_CHECK) 
BnkAllBoatable$BNK_BEDROCK_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BNK_BEDROCK_CHECK)
BnkAllBoatable$BNK_COBBLE_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BNK_COBBLE_CHECK)
BnkAllBoatable$BNK_LWD_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BNK_LWD_CHECK)
BnkAllBoatable$BNK_VEG_FOLIAR_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BNK_VEG_FOLIAR_CHECK)
#BnkAllBoatable$BNK_VEG_BASAL_CHECK=ifelse(BnkAllBoatable$nBnkCover_StabAll_CHECK<10,NA,BnkAllBoatable$BNK_VEG_BASAL_CHECK)


BnkAllWadeable$BnkCoverFoliar_StabAll_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BnkCoverFoliar_StabAll_CHECK) 
#BnkAllWadeable$BnkCoverBasal_StabAll_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BnkCoverBasal_StabAll_CHECK)
BnkAllWadeable$BnkCoverFoliar_All_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BnkCoverFoliar_All_CHECK)  
#BnkAllWadeable$BnkCoverBasal_All_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BnkCoverBasal_All_CHECK)  
BnkAllWadeable$BnkStability_All_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BnkStability_All_CHECK) 
BnkAllWadeable$BNK_BEDROCK_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BNK_BEDROCK_CHECK)
BnkAllWadeable$BNK_COBBLE_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BNK_COBBLE_CHECK)
BnkAllWadeable$BNK_LWD_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BNK_LWD_CHECK)
BnkAllWadeable$BNK_VEG_FOLIAR_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BNK_VEG_FOLIAR_CHECK)
#BnkAllWadeable$BNK_VEG_BASAL_CHECK=ifelse(BnkAllWadeable$nBnkCover_StabAll_CHECK<18,NA,BnkAllWadeable$BNK_VEG_BASAL_CHECK)


if(exists('BnkAllBoatable')==TRUE) {BnkAll=rbind(BnkAllWadeable,BnkAllBoatable)} else {BnkAll=BnkAllWadeable}


##########################################################

#          Floodplain Connectivty Indicators            #

##########################################################
#LINCIS_H
#2019 changed to not collect this data at side channels in 2019 so need to remove historic side channel data from calculations
### Then take the average at each site for bank height and incised height. Merge the data back together and then calculate LINCIS_H
#note that mathematically it does not matter whether you take the mean bankfull height and subtract it from the mean incision height or if you take the paired bankfull height and incision height differences and then calculate a mean if N is same
Incision=Incision[!(row.names(Incision) %in% grep("^X",Incision$TRANSECT)),]
INCISED=subset(Incision, PARAMETER=="INCISED")
BANKHT=subset(Incision, PARAMETER=="BANKHT")
Inc=cast(INCISED,'UID+TRANSECT~PARAMETER', value='RESULT')#changed this in 2019 from taking the lowest of the two sides to removing side channel data altogether , note EPA takes max
Bnk=cast(BANKHT,'UID+TRANSECT~PARAMETER', value='RESULT')#changed this in 2019 from taking the lowest of the two sides to removing side channel data altogether , note EPA takes max
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
thalweg_ratio5$RESULT=as.numeric(thalweg_ratio5$RESULT)
depth=subset(thalweg_ratio5,POINT==1)
depth$RESULT=depth$RESULT/100
#get bank info
BnkRatio=tblRetrieve(Parameters=c('INCISED','BANKHT'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
BnkRatio$RESULT=as.numeric(BnkRatio$RESULT)
#side channels don't need removed because thalweg not collected on side channels so side channel data not used anyway
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
fishpvt2$XFC_NAT_CHECK=ifelse(fishpvt2$nXFC_NAT_CHECK<30,NA,fishpvt2$XFC_NAT_CHECK) #half data is 5 transects * 6 categories
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



Thalweg=merge(Thalweg,PctDry, by=c('UID'),all=TRUE)

#omit cases with incomplete thalweg 2016+ data 
tbl=tblRetrieve(Parameters=c('DEPTH'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
tbl$RESULT=as.numeric(tbl$RESULT)
tbl.2=tblRetrieve(Parameters=c('NUM_THALWEG'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
tbl.2$RESULT=as.numeric(tbl.2$RESULT)
tbl3=cast(tbl.2,'UID~PARAMETER',value='RESULT',mean)
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT',length),c('SITE_ID'))# count is default
thalweg.missing=merge(tbl.PVT,tbl3, by='UID')
thalweg.missing$pctcomplete=thalweg.missing$DEPTH/(thalweg.missing$NUM_THALWEG*10)*100
Thalweg=merge(Thalweg,thalweg.missing, by=c('UID'))
Thalweg=addKEYS(Thalweg,c('VALXSITE'))
Thalweg$XDEPTH_CHECK=ifelse(Thalweg$VALXSITE %in% c('PARBYWADE','PARBYBOAT','INTPARBYWADE'),ifelse(Thalweg$pctcomplete<40,NA,Thalweg$XDEPTH_CHECK),ifelse(Thalweg$pctcomplete<80,NA,Thalweg$XDEPTH_CHECK))#changed from 100 to 80 in 2019
Thalweg$CVDEPTH_CHECK=ifelse(Thalweg$VALXSITE %in% c('PARBYWADE','PARBYBOAT','INTPARBYWADE'),ifelse(Thalweg$pctcomplete<40,NA,Thalweg$CVDEPTH_CHECK),ifelse(Thalweg$pctcomplete<80,NA,Thalweg$CVDEPTH_CHECK))#changed from 100 to 80 in 2019
Thalweg$CVDEPTH_CHECK=ifelse(Thalweg$PctDry_CHECK>0,NA,Thalweg$CVDEPTH_CHECK)# omit cases with dry thalwegs
Thalweg$PctDry_CHECK=ifelse(Thalweg$VALXSITE %in% c('PARBYWADE','PARBYBOAT','INTPARBYWADE'),ifelse(Thalweg$pctcomplete<40,NA,Thalweg$PctDry_CHECK),ifelse(Thalweg$pctcomplete<80,NA,Thalweg$PctDry_CHECK))#changed from 100 to 80 in 2019 #changed from 100 to 80 in 2019 



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
WetWid2=cast(WetWid2,'UID+TRANSECT~PARAMETER', value='RESULT', fun=sum)#changed in 2019 to take sum rather than max
WetWid=subset(WetWid,POINT!=0)#remove duplicate wetted widths 
WetWid=setNames(cast(WetWid,'UID+TRANSECT+POINT~PARAMETER', value='RESULT'), list ("UID","TRANSECT","POINT","WETWID"))
WetWidSub=WetWid[,c(1,2,4)]# delete the point column
WetWidAll=rbind(WetWidSub,WetWid2)#merge main transects and intermediate transects together
WetWidFinal=setNames(aggregate(as.numeric(WETWID)~UID,data=WetWidAll,FUN=mean),list("UID","XWIDTH_CHECK"))#average across all transects
nWetWid=setNames(plyr::count(WetWidAll,"UID"),c("UID","nXWIDTH_CHECK"))#21 transects
WetWidFinal=merge(nWetWid,WetWidFinal)
WetWidFinal$XWIDTH_CHECK=ifelse(WetWidFinal$nXWIDTH_CHECK<9,NA,WetWidFinal$XWIDTH_CHECK)# changed from 10 to 9 in 2019 because 5 main and 4 intermediate=9
WetWidFinal$XWIDTH_CHECK=round(WetWidFinal$XWIDTH_CHECK,digits=2)


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
listsites$SINUOSITY_CHECK=ifelse(listsites$SINUOSITY_CHECK<1,1,listsites$SINUOSITY_CHECK)# changed from excluding to rounding to 1 in 2019
#remove data from partial sites
listsites$SINUOSITY_CHECK=ifelse(listsites$VALXSITE_CHECK=="PARBYWADE"|listsites$VALXSITE_CHECK=="PARBYBOAT"|listsites$VALXSITE_CHECK=="INTPARBYWADE",NA,listsites$SINUOSITY_CHECK)

###################################################################################################################
###################################################################################################################
###################################################################################################################

##############    Other metrics still being worked on      #################

###################################################################################################################
###################################################################################################################
###################################################################################################################

# #W1_HALL- human influence
# #Be careful, the documentation says to use P=0.667, but the aquamet code says 0.6667, if there ends up being a lot of P's in the data this makes a difference!!! 
# #EMAP_WEST
# Human_Influ$EMAP_Weights=ifelse(Human_Influ$RESULT == "B", 1.5,ifelse(Human_Influ$RESULT == "C", 1.0, ifelse(Human_Influ$RESULT == "P", 0.6667, 0))) 
# W1_HALL=cast(Human_Influ,'UID~PARAMETER', value='EMAP_Weights',fun='mean')
# W1_HALL$EMAP_W1_HALL_CHECK=rowSums(W1_HALL[,c(2:12)])     
# #NRSA
Human_Influ$NRSA_Weights=ifelse(Human_Influ$RESULT == "B", 1.5,ifelse(Human_Influ$RESULT == "C", 1.0, ifelse(Human_Influ$RESULT == "P", 0.5, 0)))
W1_HALL_NRSA=cast(Human_Influ,'UID~PARAMETER', value='NRSA_Weights',fun='mean')
W1_HALL$NRSA_W1_HALL_CHECK=rowSums(W1_HALL_NRSA[,c(2:12)])


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
IndicatorList=c("DensPvt","BnkDensPvt","XCMG_new1", "RIP_VEG","FQCY_VEG","WQfinal","Pools","LWD","ALLSED","FinalpvtPoolFines","BnkAll","IncBnk","BnkRatioAvg","fishpvt2","MeanAngle","Thalweg","BankWidFinal","WetWidFinal","avgFloodWidth","entrench","Slope_Per","sidecount")
for (s in 1:length(IndicatorList)) {
if(exists(IndicatorList[s])==TRUE){IndicatorCheckJoin=join_all(list(IndicatorCheckJoin,as.data.frame(get(IndicatorList[s]))),by="UID")}
  else {IndicatorCheckJoin=IndicatorCheckJoin}
}
#################################################################################

# #remove QC sites- moving forward we will not be removing these and indicators need calculated and added to computed indicator table
# IndicatorCheckJoin=subset(IndicatorCheckJoin,!(UID %in% c('12457','12422','12714','13550','11787','13527','9779832504','13518','13539','8497901114','2772740176','3833994365','7194282454','9846034316','7977571143','4943503766','6152206654','6964535047','7746712455','2956707014','4324237804','4197418344','8537408400','4116634326','2109978745','41CD7AF9-B7EF-40AE-AF4E-5B28F77F7819','BC23CBBE-CA03-4AB8-985D-567DCFA49FA6','82EB06AD-705F-40B5-87F0-6EBFE4220D92','437DF621-C56F-4647-A5AB-A23A35DB726B','B0DB3C45-2D14-42F4-9722-7A5F3E66F355','C88896E2-FA3F-4D7D-AAD2-21C11E698C9B','A3378CF0-EA53-4C46-B858-4B580D001E9A','2D41FBB7-398F-43DD-A22A-93569E76493A','D0DF3643-D231-4925-9A99-9317EA79A785','FEC8303F-9604-46F8-BBC0-DDD151D5B597','39DFF320-179A-43C7-AEC1-677F4EC871F7','1E4A0F45-3584-4C48-9FD0-1AC2D156CB1A','33A60797-0B44-4A10-8030-3AFC75F027BC','2E3E82A1-CC26-4F50-ACD2-10B7C6157CED','ADFD9643-3ADE-482A-936F-B938A9C70DB5','5210AEB7-AEE9-43D8-876E-5A71F74E0C4E','9F83617B-5192-4AC6-99A4-9126AADE0699','942B415A-BD49-41EB-A9DD-2D7F8D59E87D','30846500-7DB4-4A35-B00B-764193AD1A83','8134492B-CE67-43D3-AB69-DB0C2EC47DDF','59D2716A-91E1-41F9-930F-4FE5B7DFBA8F','1730B528-5317-47E3-B83D-FFE5EB0C0D38','00C5DDB3-BC6B-43BD-AA27-EAAD160A1F3F','CD1D4C49-FBE5-4A34-BE4B-EB85218D5E10','21985428-7940-457A-82F4-74D82923FD1C','EC330DD5-C092-427F-9AC9-42C5C44F4179','C79C0FF8-E461-4C21-B9D6-150FBEE01C5A','1371C53E-AE90-41E8-9FB7-620424D79CF7','7B46F302-0E19-45E0-8658-230D6FA1EADB','E5B057BC-9219-49A8-A801-555C3CD2BCCA','D1E43428-901C-401E-81A8-29857CFD521F','1185793A-3DA2-4812-BC91-F68C1E5C4298','3743C97A-F5F0-4F33-880A-414DA4FA46D5','6B275A46-FE74-4078-A9E0-37B8C607DF9D','9E20708F-6303-4CDA-A7C4-A37B81201A9E','31B9E787-E769-4C2D-B348-4032D8B40925','38D0FEAF-1201-4E0C-A58C-0355B40D5260')))

#To remove all of the metrics and only get the indicators subset by UID and all those columns ending in "CHECK". Hmm..not really sure what the $ is doing here, the code works without it, but all the examples I've looked at keep the $ so I kept it too... 
IndicatorCheck=IndicatorCheckJoin[,c("UID",grep("CHECK$", colnames(IndicatorCheckJoin),value=TRUE))]
write.csv(IndicatorCheck,paste0('IndicatorCheck',Sys.Date(),'.csv'))

