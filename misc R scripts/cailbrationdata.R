#Created by Jennifer Courtwright
#March 28 2017
#run filter parameters from data consumption to get desired sites
#run this script to export all raw data for desired sites.
#this script can also be used to QC data to make sure that the pivots using the cast function dont aggregate data.
##if you get an error that data was aggregated that means that there are duplicate values in the database that need to be removed prior to indicator computation


#Channel dimensions
ChannelDimensions=tblRetrieve(Parameters=c('INCISED','BANKHT','BANKWID'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
ChannelDimensionspvt=addKEYS(cast(ChannelDimensions,'UID+TRANSECT~PARAMETER',value='RESULT'),c('SITE_ID'))# check data structure to make sure no duplicates

#Floodwidth
FloodWidth=tblRetrieve(Parameters=c('FLOODWID'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
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
BankStab=tblRetrieve(Parameters=c('STABLE','EROSION','COVER','BNK_VEG','BNK_COBBLE','BNK_LWD','BNK_BEDROCK'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
unique(BankStab$RESULT)
BankStabpvt=addKEYS(cast(BankStab,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
unique(BankStab$POINT)
unique(BankStab$TRANSECT)

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
Human_Influ=tblRetrieve(Parameters=c('BUILD','LOG','MINE','PARK','PAST','PAVE','PIPES','ROAD','ROW','TRASH','WALL','RECR','GRAZ','HYDR','LIVE','CROP'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)                       
Human_Influpvt=addKEYS(cast(Human_Influ,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
unique(Human_Influ$RESULT)
unique(Human_Influpvt$POINT)

#Slope
Slope_height=tblRetrieve(Parameters=c('SLOPE'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
SlpReachLen=tblRetrieve(Parameters=c('SLPRCHLEN'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
Slope_heightpvt=cast(Slope_height,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
SlpReachLenpvt=cast(SlpReachLen,'UID~PARAMETER',value='RESULT')
Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','PCT_GRADE'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)                 
pvtSlope=addKEYS(cast(Slope,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))
IndividualSlope=tblRetrieve(Parameters=c('SLOPE','STARTHEIGHT','ENDHEIGHT'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
pvtIndividualSlope=addKEYS(cast(IndividualSlope,'UID+TRANSECT~PARAMETER',value='RESULT', fun=sum),c('SITE_ID','CREW_LEADER'))#note Transect=Pass



#Pools
pool=tblRetrieve(Parameters=c('LENGTH','PTAILDEP','MAXDEPTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
poolreach=tblRetrieve(Parameters=c('POOLRCHLEN','POOL_COLLECT'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
poolpvt=cast(pool,'UID+TRANSECT~PARAMETER',value='RESULT')
poolreachpvt=addKEYS(cast(poolreach,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))
poolfinal=merge(poolpvt,poolreachpvt,by='UID')



#combine all files into an excel file and save
#runs out of memory around sediment if try to run all 2016 sites... need to do subsets of data to prevent this...
#still need to cut comments to desired sites and clean up comments before sharing
wb = createWorkbook()
sheet =createSheet(wb,"ChannelDimensions")  
addDataFrame(ChannelDimensionspvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"FloodproneWidth")  
addDataFrame(pvtFloodWidth, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"LWD")  
addDataFrame(pvtLwd, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"BankStability")  
addDataFrame(BankStabpvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"CanopyCover")  
addDataFrame(densiompvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"Riparian")  
addDataFrame(pvtRiparian, sheet=sheet, startColumn=1, row.names=FALSE)
sheet =createSheet(wb,"HumanInfluence")  
addDataFrame(Human_Influpvt, sheet=sheet, startColumn=1, row.names=FALSE)
sheet = createSheet(wb, "Slope")
addDataFrame(pvtSlope, sheet=sheet, startColumn=1, row.names=FALSE)
addDataFrame(pvtIndividualSlope, sheet=sheet, startColumn=15, row.names=FALSE)
sheet = createSheet(wb, "Pools")
addDataFrame(poolfinal, sheet=sheet, startColumn=1, row.names=FALSE)             
saveWorkbook(wb, "RawData.xlsx")