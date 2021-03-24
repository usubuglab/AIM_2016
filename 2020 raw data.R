WaterQualityBugs_Wadeable_Boatable=tblRetrieve(Parameters=c('BUG_METHOD',	'SAMPLER',	'AREA_SAMP',	'TRAN_NUM',	'AREA',	'JAR_NO',	'CAL_INST_DATE',	'CAL_INST_ID',	'CAL_INST_MODEL',	'TIME_COLLECT_FIELD',	'CONDUCTIVITY',	'CORRECTED',	'PH',	'TEMPERATURE',	'TURBIDITY',	'NOT_COLLECTED',	'ACTUAL_DATE_CHEM',	'TIME_COLLECT_CHEM',	'BLANK_DUPLICATE',	'FROZEN_STATUS',	'TIME_UNFROZEN',	'SAMPLEID'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
WaterQualityBugs_Wadeable_Boatablepvt=addKEYS(cast(WaterQualityBugs_Wadeable_Boatable,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))

Bank_Wadeable=tblRetrieve(Parameters=c('ANGLE180','SLANT','EROSION','COVER_BASAL','COVER_FOLIAR','BNK_BEDROCK', 'BNK_COBBLE','BNK_LWD','BNK_VEG_FOLIAR','BNK_VEG_BASAL','STABLE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
Bank_Wadeablepvt=addKEYS(cast(Bank_Wadeable,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(Bank_Wadeablepvt,'Bank_Wadeable.csv',na='')

Bank_Boatable=tblRetrieve(Parameters=c('EROSION','COVER_BASAL','COVER_FOLIAR', 'STABLE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
Bank_Boatablepvt=addKEYS(cast(Bank_Boatable,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(Bank_Boatablepvt,'Bank_Boatable.csv',na='')

BankAngle_Boatable=tblRetrieve(Parameters=c('BKANGLE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
BankAngle_Boatable=cast(BankAngle_Boatable,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
Chosen=tblRetrieve(Parameters=c('CHOSEN_BANK'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
Chosen=cast(Chosen,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
ChosenSub=subset(Chosen,POINT==CHOSEN_BANK)
ChosenSub=ChosenSub[,c('UID','TRANSECT','CHOSEN_BANK')]
BankAnglejoin=join(BankAngle_Boatable,ChosenSub, type='left',by=c('UID','TRANSECT'))
BankAnglejoin$POINT2=ifelse(BankAnglejoin$POINT=='',BankAnglejoin$CHOSEN_BANK,BankAnglejoin$POINT)
BankAnglejoin=BankAnglejoin[,c('UID','TRANSECT','POINT2','BKANGLE')]
BankAnglejoin$POINT=BankAnglejoin$POINT2

Bank_Boatable2=join(Bank_Boatablepvt,BankAnglejoin, type='full',by=c('UID','TRANSECT','POINT'))
Bank_Boatable2=join(Bank_Boatable2,Chosen,type='full',by=c('UID','TRANSECT'))
write.csv(Bank_Boatable2,'Bank_Boatable2.csv',na='')

CANOPY_COVER=tblRetrieve(Parameters=c('DENSIOM'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
CANOPY_COVERpvt=addKEYS(cast(CANOPY_COVER,'UID+TRANSECT~PARAMETER+POINT',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(CANOPY_COVERpvt,'CANOPY_COVERpvt.csv',na='')

ChannelDimmensions_Wadeable=tblRetrieve(Parameters=c('WETWID',	'BARWID',	'TRANDRY',	'BANKWID',	'BANKHT',	'INCISED',	'SIDCHN',	'SIDCH_TYPE',	'SIDCHN_BNK',	'SIDCHN_FLOW'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
ChannelDimmensions_Wadeable=subset(ChannelDimmensions_Wadeable,SAMPLE_TYPE=='BANKW')
ChannelDimmensions_Wadeablepvt=addKEYS(cast(ChannelDimmensions_Wadeable,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(ChannelDimmensions_Wadeablepvt,'ChannelDimmensions_Wadeablepvt.csv',na='')

ChannelDimmensions_Boatable=tblRetrieve(Parameters=c('WETWID','BARWID','BANKWID','BANKHT','INCISED','CONSTRNT_TRAN','SEEOVRBK','SHOR2RIP'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
ChannelDimmensions_Boatablepvt=addKEYS(cast(ChannelDimmensions_Boatable,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(ChannelDimmensions_Boatablepvt,'ChannelDimmensions_Boatablepvt.csv',na='')

fish=tblRetrieve(Parameters=c('BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY','ALGAE','MACPHY','STRUCT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
fishpvt=addKEYS(cast(fish,'UID+TRANSECT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))# check data structure to make sure no duplicates
write.csv(fishpvt,'fishpvt.csv',na='')

#Floodwidth
FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID','FLOOD_HEIGHT','FLOOD_BFHEIGHT','FLOOD_BFWIDTH','FLOOD_MAXDEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
pvtFloodWidth=addKEYS(cast(FloodWidth,'UID+TRANSECT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(pvtFloodWidth,'pvtFloodWidth.csv',na='')

Human_Influ=tblRetrieve(Parameters=c('BUILD','LOG','MINE','PARK','PAST','PAVE','PIPES','ROAD','ROW','TRASH','WALL','GRAZ','HYDR','LIVE','RECR','RESTORATION'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)                       
Human_Influpvt=addKEYS(cast(Human_Influ,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(Human_Influpvt,'Human_Influpvt.csv',na='')


LittoralDepth=tblRetrieve(Parameters=c('DEPTH'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)             
LittoralDepth=subset(LittoralDepth, SAMPLE_TYPE=='LITTORALB')
LittoralDepthpvt=addKEYS(cast(LittoralDepth,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(LittoralDepthpvt,'LittoralDepthpvt.csv',na='')

LittoralSubstrate=tblRetrieve(Parameters=c('BOTTOMDOM','BOTTOMSEC','SHOREDOM','SHORESEC'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)             
LittoralSubstratepvt=addKEYS(cast(LittoralSubstrate,'UID+TRANSECT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(LittoralSubstratepvt,'LittoralSubstratepvt.csv',na='')

#wood location!!!
LWD=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' "))$PARAMETER
LWD2=addKEYS(tblRetrieve(Parameters=LWD,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes),c('SITE_ID'))
LWD3=addKEYS(cast(LWD2,'UID+TRANSECT~PARAMETER',value='RESULT'),c('SITE_ID'))
write.csv(LWD3,'LWD.csv',na='')

#Pools
pool=tblRetrieve(Parameters=c('HABTYPE','LENGTH','PTAILDEP','MAXDEPTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
poolpvt=addKEYS(cast(pool,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
write.csv(poolpvt,'poolpvt.csv',na='')

poolreach=tblRetrieve(Parameters=c('POOLRCHLEN','POOL_COLLECT'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
poolreachpvt=addKEYS(cast(poolreach,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))
poolfinal=merge(poolpvt,poolreachpvt,by='UID')

#Pool Tail Fines
PoolFines=tblRetrieve(Parameters=c('POOLFINES2','POOLFINES6','POOLNOMEAS',"POOLFINES6_512"),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes)
pvtPoolFines=addKEYS(cast(PoolFines,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
write.csv(pvtPoolFines,'pvtPoolFines.csv',na='')

thalweg_wade=addKEYS(tblRetrieve(Parameters=c('DEPTH','FLOW','TOODEEP','DEPTH_ANGLE','SIDCHN','BACKWATER','BAR_PRES'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes),c('PROTOCOL'))
thalweg_wade=subset(thalweg_wade,SAMPLE_TYPE!='CROSSSECW')
thalweg_wade=subset(thalweg_wade,SAMPLE_TYPE!='LITTORALB')
thalweg_wade=subset(thalweg_wade,SAMPLE_TYPE!='BANKW')
thalweg_wadepvt=addKEYS(cast(thalweg_wade,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(thalweg_wadepvt,'thalweg_wadepvt.csv',na='')

thalweg_boat=addKEYS(tblRetrieve(Parameters=c('DEPTH','DEPTH_METHOD','OFF_CHAN','SNAG','SIZE_CLS'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes),c('PROTOCOL'))
thalweg_boat=subset(thalweg_boat,SAMPLE_TYPE!='CROSSSECW')
thalweg_boat=subset(thalweg_boat,SAMPLE_TYPE!='LITTORALB')
thalweg_boatpvt=addKEYS(cast(thalweg_boat,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(thalweg_boatpvt,'thalweg_boatpvt.csv',na='')

StreambedParticles=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS','SIZE_NUM','LOC','DIST_LB','SIZE_CLS_CAT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
StreambedParticlespvt=addKEYS(cast(StreambedParticles,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(StreambedParticlespvt,'StreambedParticlespvt.csv',na='')
                              
#Slope
Slope_height=tblRetrieve(Parameters=c('STARTHEIGHT','ENDHEIGHT','SLOPE','STARTTRAN','ENDTRAN','METHOD'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
Slope_heightpvt=addKEYS(cast(Slope_height,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(Slope_heightpvt,'Slope.csv',na='')

SlopePool=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','TRCHLEN','POOLRCHLEN','POOL_COLLECT','SLOPE_COLLECT','PCT_GRADE','Z_SLOPEPASSQA'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)                 
pvtSlopePool=addKEYS(cast(SlopePool,'UID~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','PROTOCOL','VALXSITE','CREW_LEADER'))
write.csv(pvtSlopePool,'SlopePool.csv',na='')


RipALL=tblRetrieve(Parameters=c("BARE","CANBTRE","CANSTRE","CANVEG","GCNWDY","GCWDY","UNDERVEG","UNDNWDY","UNDWDY",'CANRIPW','UNRIPW','GCRIP'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
RipAllpvt=cast(RipALL,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
write.csv(RipAllpvt,'RipAllpvt.csv',na='')


Presence=tblRetrieve(Parameters=c('INVASW', 'NATIVW','INVASH','NATIVH','INVASAQ'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
presencepvt=cast(Presence,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
presencepvt$INVASW=ifelse(presencepvt$INVASW=='N','NoNoxiousWoody',presencepvt$INVASW)
presencepvt$INVASH=ifelse(presencepvt$INVASH=='N','NoNoxiousHerbaceous',presencepvt$INVASH)
presencepvt$NATIVW=ifelse(presencepvt$NATIVW=='N','NoNativeWoody',presencepvt$NATIVW)
presencepvt$NATIVH=ifelse(presencepvt$NATIVH=='N','NoNativeHerbaceous',presencepvt$NATIVH)
presencepvt$INVASAQ=ifelse(presencepvt$INVASAQ=='N','NoNoxiousAquatic',presencepvt$INVASAQ)
presence2=melt(presencepvt,id.vars=c('UID','TRANSECT','POINT'))
presence2=subset(presence2,value!='Y')
write.csv(presence2,'presence2.csv',na='')

Species=tblRetrieve(Parameters=c('INVAS_COMMON_NAME'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
write.csv(Species,'Species.csv',na='')
SegRush=tblRetrieve(Parameters=c('SEGRUSH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
SegRush$RESULT=ifelse(SegRush$RESULT=='Y','SedgeRush',ifelse(SegRush$RESULT=='N','NoSedgeRush',SegRush$RESULT))
write.csv(SegRush,'SegRush.csv',na='')

#add monument lat long, accuracy of coordinates typical widths, weather conditions, waterwidtdrawl, data collection org
siteinfo=tblRetrieve(Parameters=c('ACC_BR',	'ACC_TR',	'AGENCY',	'BEAVER_FLOW_MOD',	'BEAVER_SIGN',	'CREW_LEADER',	'DEWATER',	'ELEVATION',	'LAT_DD',	'LON_DD',	'LAT_DD_BR',	'LAT_DD_TR',	'LOC_NAME',	'LON_DD_BR',	'LON_DD_TR',	'MERGE',	'NAME1',	'NAME2',	'NAME3',	'NUM_TRAN_PHAB',	'PARTIAL_RCHLEN',	'RCHWIDTH',	'RCHWIDTH_WET',	'REPEAT_VISIT',	'SITE_ID',	'SLIDE_YN',	'TRCHLEN',	'INCREMENT',	'NUM_THALWEG',	'VISIT_NO',	'Z_DISTANCEFROMX',	'Z_FINALQA_AUTHORIZED',	'Z_FINALQA_COMMENT',	'Z_FINALQACHECK',	'Z_INDICATORS',	'UID',	'VALXSITE',	'XSTATUS',	'DATE_COL',	'PROJECT',	'PROTOCOL',	'NOT_COLLECTED','LAT_DD_F', 'LON_DD_F'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
siteinfo=cast(siteinfo,'UID~PARAMETER',value='RESULT')
write.csv(siteinfo,'siteinfo.csv', na='')

#photos
#comments

