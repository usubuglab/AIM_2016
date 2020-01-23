#Created by Jennifer Courtwright
#March 28 2017
#run filter parameters from data consumption to get desired sites
#run this script to export all raw data for desired sites.
#this script can also be used to QC data to make sure that the pivots using the cast function dont aggregate data.
##if you get an error that data was aggregated that means that there are duplicate values in the database that need to be removed prior to indicator computation

#####Reach
#Metadata
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR','TRCHLEN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
pvtlistsites=cast(listsites,'UID~PARAMETER',value='RESULT')
#WQ
WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','NTL','PTL','TURBIDITY','TEMPERATURE','EC_PRED','TN_PRED','TP_PRED'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')
#Bugs
Bugs=tblRetrieve(Parameters=c('JAR_NO','ACTUAL_DATE','AREA_SAMP','TRAN_NUM','SAMPLER','AREA','BUG_METHOD'),Years=years, Projects=projects,Protocol=protocols,SiteCodes=sitecodes)
Bugspvt=cast(Bugs,'UID~SAMPLE_TYPE+PARAMETER',value='RESULT')
#combine
reach=join_all(list(pvtlistsites,WQpvt,Bugspvt),by='UID')

#####Transect
#Fish cover
fish=tblRetrieve(Parameters=c('BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY','ALGAE','MACPHY'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
unique(fish$RESULT)# check data structure
fishpvt=addKEYS(cast(fish,'UID+TRANSECT~PARAMETER',value='RESULT'),c('SITE_ID'))# check data structure to make sure no duplicates

#Channel dimensions
ChannelDimensions=tblRetrieve(Parameters=c('INCISED','BANKHT','BANKWID','WETWID','WETWIDTH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
ChannelDimensionspvt=addKEYS(cast(ChannelDimensions,'UID+TRANSECT~PARAMETER',value='RESULT'),c('SITE_ID'))# check data structure to make sure no duplicates

#Floodwidth
FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID','FLOOD_HEIGHT','FLOOD_BFHEIGHT','FLOOD_BFWIDTH','FLOOD_MAXDEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
pvtFloodWidth=addKEYS(cast(FloodWidth,'UID+TRANSECT~PARAMETER',value='RESULT'),c('SITE_ID'))

#Wood
LwdCatWet=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'W%'"))$PARAMETER
LwdCatDry=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'D%'"))$PARAMETER
LwdWet=addKEYS(tblRetrieve(Parameters=LwdCatWet,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes),c('SITE_ID'))
LwdDry=addKEYS(tblRetrieve(Parameters=LwdCatDry,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes),c('SITE_ID'))
pvtLwdWet=cast(LwdWet, 'UID+TRANSECT~PARAMETER',value='RESULT')
pvtLwdDry=cast(LwdDry,'UID+TRANSECT~PARAMETER',value='RESULT')
pvtLwd=addKEYS(merge(pvtLwdWet,pvtLwdDry, by=c('UID','TRANSECT')),c('SITE_ID'))

#######Point
#Bank Stability
BankStab=tblRetrieve(Parameters=c('STABLE','EROSION','COVER_FOLIAR','COVER_BASAL','BNK_VEG_BASAL','BNK_VEG_FOLIAR','BNK_COBBLE','BNK_LWD','BNK_BEDROCK'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
unique(BankStab$RESULT)
BankStabpvt=addKEYS(cast(BankStab,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
unique(BankStab$POINT)
unique(BankStab$TRANSECT)

#Angle-PIBO method only
Angle=tblRetrieve(Parameters=c('ANGLE180'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
Anglepvt=cast(Angle,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
unique(Anglepvt$TRANSECT)

#Sediment
Sediment=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
unique(Sediment$RESULT)
Sedimentpvt=addKEYS(cast(Sediment,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))# check data structure to make sure no duplicates
unique(Sedimentpvt$TRANSECT)# check data structure
Sed2014pvt=addKEYS(cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))# check data structure to make sure no duplicates
unique(Sed2014pvt$POINT)# check data structure
unique(Sed2014pvt$LOC)

#Canopy cover
densiom=tblRetrieve(Parameters='DENSIOM',Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
unique(densiom$RESULT)
densiompvt=addKEYS(cast(densiom,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))# check data structure to make sure no duplicates
unique(densiom$POINT)

#Riparian
RipALL=tblRetrieve(Parameters=c("BARE","CANBTRE","CANSTRE","CANVEG","GCNWDY","GCWDY","UNDERVEG","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
unique(RipALL$RESULT)
RipAllpvt=cast(RipALL,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
unique(RipAllpvt$TRANSECT)
RipBLM=tblRetrieve(Parameters=c('CANRIPW','UNRIPW','GCRIP','INVASW', 'NATIVW','INVASH','NATIVH','SEGRUSH'),Projects=projects,Years=years,Protocols=protocols, SiteCodes=sitecodes)
pvtRipBLM=addKEYS(cast(RipBLM,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
pvtRiparian=merge(RipAllpvt,pvtRipBLM, by=c('UID','TRANSECT','POINT'))

#HumanInfluence
Human_Influ=tblRetrieve(Parameters=c('BUILD','LOG','MINE','PARK','PAST','PAVE','PIPES','ROAD','ROW','TRASH','WALL'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)                       
Human_Influpvt=addKEYS(cast(Human_Influ,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
unique(Human_Influ$RESULT)
unique(Human_Influpvt$POINT)

#Slope
Slope_height=tblRetrieve(Parameters=c('SLOPE'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
SlpReachLen=tblRetrieve(Parameters=c('SLPRCHLEN'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
Slope_heightpvt=cast(Slope_height,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
SlpReachLenpvt=cast(SlpReachLen,'UID~PARAMETER',value='RESULT')
Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','TRCHLEN','PARTIAL_RCHLEN','POOLRCHLEN','SLOPE_COLLECT','PCT_GRADE','VALXSITE'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)                 
pvtSlope=addKEYS(cast(Slope,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))

#Thalweg
thalweg=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes),c('PROTOCOL'))
thalweg=subset(thalweg,SAMPLE_TYPE!='CROSSSECW')
thalwegpvt=addKEYS(cast(thalweg,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
unique(thalwegpvt$POINT)

#Pools
pool=tblRetrieve(Parameters=c('LENGTH','PTAILDEP','MAXDEPTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
poolreach=tblRetrieve(Parameters=c('POOLRCHLEN','POOL_COLLECT'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
poolpvt=cast(pool,'UID+TRANSECT~PARAMETER',value='RESULT')
poolreachpvt=addKEYS(cast(poolreach,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))
poolfinal=merge(poolpvt,poolreachpvt,by='UID')

#Pool Tail Fines
PoolFines=tblRetrieve(Parameters=c('POOLFINES2','POOLFINES6','POOLNOMEAS'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
pvtPoolFines=addKEYS(cast(PoolFines,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))

#Comments
comments=addKEYS(tblRetrieve(Table='tblcomments', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes),c('SITE_ID','PROJECT'))
comments=comments[,c(13,14,1:12)]

#Metadata
metadata=(sqlQuery(wrsa1314,"select * from tblmetadata"))

          
#combine all files into an excel file and save
#runs out of memory around sediment if try to run all 2016 sites... need to do subsets of data to prevent this...
#still need to cut comments to desired sites and clean up comments before sharing
wb = createWorkbook()
sheet = createSheet(wb, "Metadata")
addDataFrame(metadata, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"SiteInfoWQBugs")  
addDataFrame(reach, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"FishCover")  
addDataFrame(fishpvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"ChannelDimensions")  
addDataFrame(ChannelDimensionspvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"FloodproneWidth")  
addDataFrame(pvtFloodWidth, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"LWD")  
addDataFrame(pvtLwd, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"BankStability")  
addDataFrame(BankStabpvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"BankAngle")  
addDataFrame(Anglepvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"Sediment")  
addDataFrame(Sed2014pvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"CanopyCover")  
addDataFrame(densiompvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"Riparian")  
addDataFrame(pvtRiparian, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"HumanInfluence")  
addDataFrame(Human_Influpvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet = createSheet(wb, "Slope")
addDataFrame(Slope_heightpvt, sheet=sheet, startColumn=1, row.names=FALSE)
addDataFrame(pvtSlope, sheet=sheet, startColumn=15, row.names=FALSE)
sheet = createSheet(wb, "Thalweg")
addDataFrame(thalwegpvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet = createSheet(wb, "Pools")
addDataFrame(poolfinal, sheet=sheet, startColumn=1, row.names=FALSE)        
sheet = createSheet(wb, "PoolFines")
addDataFrame(pvtPoolFines, sheet=sheet, startColumn=1, row.names=FALSE)
sheet = createSheet(wb, "Comments")
addDataFrame(comments, sheet=sheet, startColumn=1, row.names=FALSE)       
saveWorkbook(wb, "RawData.xlsx")