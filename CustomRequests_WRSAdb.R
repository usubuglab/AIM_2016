ak_bankwidth=tblRetrieve(Parameters='BANKWID',Protocols='AK14',Projects='AKEFO',UIDS=())


#Get all sites sampled 

Allsites=addKEYS(tblRetrieve(Comments='Y',Parameters=c('SITE_ID'),Projects=c('AKEFO','NORCAL','WRSA','GSENM','COPLT','NV'),Protocols=c('AK14','NRSA13','WRSA14','BOAT14')),c('DATE_COL','LOC_NAME','LAT_DD','LON_DD','VALXSITE'))
Allsites2=removeDUP(Allsites,QA='N')


listsites<-tblRetrieve(Parameters=c('LAT_DD_BR','LON_DD_BR','LAT_DD_TR','LON_DD_TR'),Projects=projects,Protocols=protocols)
listsites1<-tblRetrieve(Parameters=c('TRCHLEN','SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','VALXSITE'),Projects=projects,Protocols=protocols,UIDS=c(1344381913,
                                                                                                                                                          2008266646,
                                                                                                                                                          2027464674,
                                                                                                                                                          2053561144,
                                                                                                                                                          2327564080,
                                                                                                                                                          2633963614,
                                                                                                                                                          2818247146,
                                                                                                                                                          3321286437,
                                                                                                                                                          3606952496,
                                                                                                                                                          3686446179,
                                                                                                                                                          4471000038,
                                                                                                                                                          4587443859,
                                                                                                                                                          4635599427,
                                                                                                                                                          4852415254,
                                                                                                                                                          5417874919,
                                                                                                                                                          5762005347,
                                                                                                                                                          6525800459,
                                                                                                                                                          7065148925,
                                                                                                                                                          7605781429,
                                                                                                                                                          7737403744,
                                                                                                                                                          7765571483,
                                                                                                                                                          7842614202,
                                                                                                                                                          8119667904,
                                                                                                                                                          8518774431,
                                                                                                                                                          8889087946,
                                                                                                                                                          9154292041,
                                                                                                                                                          9309721985,
                                                                                                                                                        9962772452))
listsites2<-tblRetrieve(Parameters=c('ELEVATION','ELEVATION_UNITS'),Projects=projects,Protocols=protocols)

pvtlistsites<-cast(listsites1,'UID~PARAMETER',value='RESULT')
pvtlistsites2<-cast(listsites2,'UID+SAMPLE_TYPE~PARAMETER',value='RESULT')
pvtlistsites1<-cast(listsites1,'UID~PARAMETER',value='RESULT')

#Jennifer - WQ WRSA checks
WQ2=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','CAL_INST_ID'), Comments='Y',Projects='WRSA',Years=c('2013','2014'))
WQ1=addKEYS(cast(WQ2,'UID~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','CREW_LEADER'))

WQ2=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED','TEMPERATURE'), Comments='Y',Projects='WRSA',Years=c('2013','2014'))
WQ1=cast(WQ2,'UID~PARAMETER',value='RESULT')
WQind=cast(WQ2,'UID~PARAMETER',value='IND')
WQ3=addKEYS(merge(WQ1,WQind,by=c('UID'),all=T) ,c('SITE_ID','DATE_COL','CREW_LEADER'))
WQ3.sub=subset(WQ3,CORRECTED.x!='Y')
write.csv(WQ3.sub,'not_temp_corrected_conduct.csv')

#box plot conduct
#run DataConsumption with Year Project and Parameter filters
#run top few lines of DataQA_WRSA to read in the data
#run all outlier code from DataQA_WRSA
UnionTBL2$EcoReg.f<-factor(UnionTBL2$EcoReg)
UnionTBL2$RESULT.n<-as.numeric(UnionTBL2$RESULT)
boxplot(RESULT.n~EcoReg.f,data=UnionTBL2,xlab="EcoRegion",ylab="Conductivity uS/cm")
UnionTBL2.subset<-subset(UnionTBL2, EcoReg==c('MN','MP','MS','XE','XN','XS'))
write.csv(UnionTBL2,'Conductivity-boxplot.csv')
UnionTBL2.subset<-read.csv('Conductivity-boxplot_woOT.csv')
UnionTBL2.subset<-subset(UnionTBL2, EcoReg=='MN')
boxplot(RESULT.n~EcoReg.f,data=UnionTBL2.subset,xlab="EcoRegion",ylab="Conductivity uS/cm")

#determine quantiles for EPA WQ data to set typical values for WRSA QC checks
EPAdata=read.csv("Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\1.Comb_21Oct2014_Rinput_DoNotAlter.csv",header=T)
EPAdataWQ=subset(EPAdata,select=c(SITE_ID,ECO10,COND,PHLAB,PHSTVL))
T1=setNames(aggregate(EPAdataWQ$COND, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","COND_0.05"))
T2=setNames(aggregate(EPAdataWQ$COND, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","COND_0.25"))
T3=setNames(aggregate(EPAdataWQ$COND, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","COND_0.75"))
T4=setNames(aggregate(EPAdataWQ$COND, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","COND_0.95"))
T6=join_all(list(T1,T2,T3,T4), by="ECO10")

EPAdataWQ=subset(EPAdata,select=c(SITE_ID,ECO10,COND,PHLAB,PHSTVL))
T1=setNames(aggregate(EPAdataWQ$PHSTVL, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.05,na.rm=TRUE), c("ECO10","PHSTVL_0.05"))
T2=setNames(aggregate(EPAdataWQ$PHSTVL, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.25,na.rm=TRUE), c("ECO10","PHSTVL_0.25"))
T3=setNames(aggregate(EPAdataWQ$PHSTVL, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.75,na.rm=TRUE), c("ECO10","PHSTVL_0.75"))
T4=setNames(aggregate(EPAdataWQ$PHSTVL, by = list(EPAdataWQ$ECO10), FUN = quantile,probs=0.95,na.rm=TRUE), c("ECO10","PHSTVL_0.95"))
T6=join_all(list(T1,T2,T3,T4), by="ECO10")


###2015 Riparian checks
#overstory >100% check
riparian1=tblRetrieve(Parameters=c('CANBTRE','CANSTRE'),Years=c('2013','2014','2015'), Projects=c('NV','GSENM','COPLT','WRSA','NORCAL','2015ProtocolOverlap','AKEFO'))
riparian1PVT=cast(riparian1,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian1PVT_IND=cast(riparian1,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian1pvt=merge(riparian1PVT,riparian1PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian1PVTsub=subset(riparian1pvt,(CANBTRE.x==4 & CANSTRE.x>2) | (CANSTRE.x==4 & CANBTRE.x>2))

#middlestory >100% check
riparian2=tblRetrieve(Parameters=c('UNDNWDY','UNDWDY'),Years=c('2013','2014','2015'), Projects=c('NV','GSENM','COPLT','WRSA','NORCAL','2015ProtocolOverlap','AKEFO'))
riparian2PVT=cast(riparian2,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian2PVT_IND=cast(riparian2,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian2pvt=merge(riparian2PVT,riparian2PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian2PVTsub=subset(riparian2pvt,(UNDNWDY.x==4 & UNDWDY.x>2) | (UNDWDY.x==4 & UNDNWDY.x>2))

#understory >100% check
riparian3=tblRetrieve(Parameters=c('GCNWDY','GCWDY','BARE'),Years=c('2013','2014','2015'), Projects=c('NV','GSENM','COPLT','WRSA','NORCAL','2015ProtocolOverlap','AKEFO'))
riparian3PVT=cast(riparian3,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian3PVT_IND=cast(riparian3,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian3pvt=merge(riparian3PVT,riparian3PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian3PVTsub=subset(riparian3pvt,(GCNWDY.x==4 & GCWDY.x>2) | (GCWDY.x==4 & GCNWDY.x>2) | (GCNWDY.x==4 & BARE.x>2) | (BARE.x==4 & GCNWDY.x>2) | (BARE.x==4 & GCWDY.x>2) | (GCWDY.x==4 & BARE.x>2))

#veg type check
#canopy
riparian4=tblRetrieve(Parameters=c('CANVEG','CANBTRE','CANSTRE'),Years=c('2013','2014','2015'), Projects=c('NV','GSENM','COPLT','WRSA','NORCAL','2015ProtocolOverlap','AKEFO'))
riparian4PVT=cast(riparian4,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian4pvtsub=subset(riparian4PVT,CANVEG=='N' & CANBTRE>0 & CANSTRE>0)

riparian4PVT_IND=cast(riparian4,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian4pvt=merge(riparian4PVT,riparian4PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian4pvtsub2=subset(riparian4pvt,CANVEG.x!='N' & CANBTRE.x==0 & CANSTRE.x==0)


#middle
riparian5=tblRetrieve(Parameters=c('UNDERVEG','UNDNWDY','UNDWDY'),Years=c('2013','2014','2015'), Projects=c('NV','GSENM','COPLT','WRSA','NORCAL','2015ProtocolOverlap','AKEFO'))
riparian5PVT=cast(riparian5,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian5pvtsub=subset(riparian5PVT,UNDERVEG=='N' & UNDNWDY>0 & UNDWDY>0)

##fish cover check #flags sites with undercuts present but no undercut listed for fish cover
under=tblRetrieve(Parameters=c('UNDCUT','UNDERCUT','BKUNDCT'),Years=c('2013','2014','2015'))
underpvt=cast(under,'UID+TRANSECT~PARAMETER',value='RESULT',fun=max)
underpvt.sub=subset(underpvt,UNDCUT==0 & (UNDERCUT>0 | BKUNDCT=='>=5'|BKUNDCT=='<5'))
       
######2015 Width-height checks
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'),  Projects=c('WRSA','NV','GSENM','COPLT'),Years=c('2015'),SiteCodes=c('CO-LS-9448','GS-LS-9009','GS-LS-9010','GS-SS-9006','MN-SS-1137','MP-LS-2005','NB-LS-9119','NB-SS-9108','NY-LS-9230','OT-SS-7109','OT-SS-7122','XE-LS-5026','XS-LS-6007'),Comments='Y')
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects=c('WRSA','NV','GSENM','COPLT'),Years=c('2015'),SiteCodes=c('CO-LS-9448','GS-LS-9009','GS-LS-9010','GS-SS-9006','MN-SS-1137','MP-LS-2005','NB-LS-9119','NB-SS-9108','NY-LS-9230','OT-SS-7109','OT-SS-7122','XE-LS-5026','XS-LS-6007'))
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
wnPVTIND=cast(widhgt,'UID+TRANSECT~PARAMETER',value='IND')     
rawwhPVT=addKEYS(merge(whPVT,wnPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
colnames(rawwhPVT)<-c('UID','TRANSECT','BANKHT','BANKWID','BARWID','INCISED','WETWID','BANKHT_IND','BANKWID_IND','BARWID_ID','INCISED_IND','WETWID_ID','DATE_COL','SITE_ID')
write.csv(rawwhPVT,'problem_sites_cross_valid_bank.csv')

widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'),  Projects=c('NORCAL'),Years=c('2013'),UIDS=2772740176,Comments='Y')
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects=c('NORCAL'),Years=c('2013'),UIDS=2772740176)

widhgt=addKEYS(tblRetrieve(Parameters=c('INCISED'), Projects='NV',Years=c('2015'),SiteCodes=c('NB-SS-9108'),Comments='Y'),c('SITE_ID','DATE_COL'))

#sediment data
Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC','EMBED'),Projects='WRSA',SiteCodes=c('OT-SS-7150'))
pvtSed<-cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
PVTIND=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER',value='IND')     
rawwhPVT=addKEYS(merge(pvtSed,PVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))

widhgt=tblRetrieve(Parameters=c('WETWID','BARWID'),  Projects=c('WRSA','NV','GSENM','COPLT'),Years=c('2015'),SiteCodes=sitecodes,Comments='Y')
widhgt2=tblRetrieve(Parameters=c('WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects=c('WRSA','NV','GSENM','COPLT'),Years=c('2015'),SiteCodes=sitecodes,Comments='Y')

#thalweg checks####couldn't get this to work so did cell referencing in excel to interpolate between values with 1 missing value inbetween
thalweg<-tblRetrieve(Parameters='DEPTH',Projects=c('AKEFO','NORCAL','WRSA','GSENM','COPLT','NV'),Protocols=c('AK14','NRSA13','WRSA14','BOAT14'))
thalweg_depth<-subset(thalweg,SAMPLE_TYPE!='CROSSSECW')
thalweg_depth_pvt<-cast(thalweg_depth,'UID+TRANSECT~POINT', value='RESULT')
thalweg_depth_pvt_order<-thalweg_depth_pvt[with(thalweg_depth_pvt, order(1,29))]
thaleg_depth_NA<-thalweg_depth_pvt [is.na(thalweg_depth_pvt$'1')==TRUE,c(1:2,4)]

#thalweg depth
#wading sites
depthcheck=tblRetrieve(Parameters=c('DEPTH'), Years=c('2013','2014','2015'),Protocols=c('WRSA14','NRSA13','AK14'))
depthcheck.sub=subset(depthcheck,SAMPLE_TYPE!='CROSSSECW')
pvtdepthcheck=cast(depthcheck.sub,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
pvtdepthcheck.sub=subset(pvtdepthcheck,POINT=='0')
width=tblRetrieve(Parameters=c('WETWID'),Years=c('2013','2014','2015'),Protocols=c('WRSA14','NRSA13','AK14'))
check=merge(pvtdepthcheck.sub,width,by=c('UID','TRANSECT'),all=T)
odd_ratio=subset(check,RESULT/(DEPTH/100)>50|RESULT/(DEPTH/100)<1)
#boating sites
depthcheck=tblRetrieve(Parameters=c('DEPTH'), Years=c('2013','2014','2015'),Protocols=c('BOAT14'))
depthcheck.sub=subset(depthcheck,SAMPLE_TYPE!='CROSSSECW')
pvtdepthcheck=cast(depthcheck.sub,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
pvtdepthcheck.sub=subset(pvtdepthcheck,POINT=='0')
width=tblRetrieve(Parameters=c('WETWID'),Years=c('2013','2014','2015'),Protocols=c('BOAT14'))
check=merge(pvtdepthcheck.sub,width,by=c('UID','TRANSECT'),all=T)
odd_ratio=subset(check,RESULT/(DEPTH)>50|RESULT/(DEPTH)<1)

#getting all pool data for a specfic set of sites
pools<-addKEYS(tblRetrieve(Parameters=c('HABTYPE','FORMATION','PTAILDEP','MAXDEPTH','LENGTH'),Projects=projects,Comments='Y'),c('SITE_ID'))
pvtpools=cast(pools,'UID+POINT~PARAMETER',value='RESULT',fun=sum)  
pool_length<-tblRetrieve(Parameters=c('LENGTH'),Projects=projects,Comments='Y')
pvtpools1=cast(pool_length,'UID~PARAMETER',value='RESULT',fun=sum) 
reach_length=tblRetrieve(Parameters=c('TRCHLEN'),Projects=projects)
pvtpools2=cast(reach_length,'UID~PARAMETER',value='RESULT') 
poolsmerge<-merge(pvtpools1,pvtpools2,by=c('UID'),all=T)
pool_great_100<-subset(poolsmerge,LENGTH>TRCHLEN)#no issues found

#####get photo data
Photo=tblRetrieve(Parameters=c('PHOTO_ID','PHOTO_DESCRIP','PHOTO_TYPE','ROD','DIRECTION','COMMENT'),Projects='WRSA',Years=c('2014'),SiteCodes=c('MN-SS-1138'))

#Jennifer - Slope checks
SLOPEp=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'SLOPE%'"))$PARAMETER
crewKC=tblRetrieve(Parameters=c('CREW_LEADER','NAME1'),Filter="RESULT in ('172','184')")
crewKC=addKEYS(cast(crewKC,'UID~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL'))
crewKC$SumKC=crewKC$CREW_LEADER + crewKC$NAME1;crewKC=subset(crewKC,SumKC==356)
SlopeKC=tblRetrieve(Parameters=SLOPEp,UIDS=crewKC$UID);SlopeKC$SlopeFLAG='KathleenClayton'
nonTransit=sqlQuery(wrsa1314,"select * from tblpoint where SAMPLE_TYPE like '%SLOPE%' and PARAMETER='METHOD' and RESULT <> 'TR'")
SlopeTR=tblRetrieve(Parameters=SLOPEp,UIDS=nonTransit$UID);SlopeTR$SlopeFLAG='OddMethod'
SlopeBoat=tblRetrieve(Parameters=SLOPEp,Protocols='BOAT14');SlopeBoat$SlopeFLAG='Boatable'
SlopeAK=tblRetrieve(Parameters=SLOPEp,Projects='AKEFO',Years=c('2015'));SlopeAK$SlopeFLAG='AK'
if(exists('SlopeConnectFail')){SlopeUNC=tblRetrieve(Parameters=SLOPEp,UIDS=c(SlopeConnectFail$UID,SlopeConnectPassEndFail$UID,87601876754740660818804248));SlopeUNC$SlopeFLAG='PartialReach'} else{SlopeUNC=SlopeBoat[0,]}
SlopeIssues=rbind(SlopeKC,SlopeBoat,SlopeTR,SlopeAK,SlopeUNC);
SlopeIssues=addKEYS(cast(SlopeWRSA,'UID+TRANSECT+POINT+SlopeFLAG~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','PROJECT'))

SlopeAK=addKEYS(cast(SlopeAK,'UID+TRANSECT+POINT~PARAMETER',min,value='RESULT')  ,c('SITE_ID','DATE_COL','PROJECT'))
ReachLength=addKEYS(tblRetrieve(Parameters='TRCHLEN',Projects='AKEFO'),c('SITE_ID'))
AllSlope=tblRetrieve(Parameters=SLOPEp,Projects=projects,Protocols=protocols,Years=c(years))
ReachLength=addKEYS(tblRetrieve(Parameters=c('TRCHLEN','SLPRCHLEN'),Projects=projects),c('SITE_ID'))
pvtReachLength=addKEYS(cast(AllSlope,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','PROJECT'))

AllSlope=tblRetrieve(Parameters=c('STARTHEIGHT','ENDHEIGHT','SLOPE'),Projects=projects,Protocols=protocols,Years=c(years),SiteCodes=sitecodes)


############################################################################################################################
#######Project specific requests########
#########################################WRSA###############################################################################
#####Additional QC Checks 2014
#bank cross-validation WRSA checks
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects='WRSA',Years=c('2013','2014'))
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects='WRSA',Years=c('2013','2014'))
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
wnPVTIND=cast(widhgt,'UID+TRANSECT~PARAMETER',value='IND')     
tranPVT=addKEYS(merge(whPVT,bnkPVT,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
rawwhPVT=addKEYS(merge(whPVT,wnPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
colnames(rawwhPVT)<-c('UID','TRANSECT','BANKHT','BANKWID','BARWID','INCISED','WETWID','BANKHT_IND','BANKWID_IND','BARWID_ID','INCISED_IND','WETWID_ID','DATE_COL','SITE_ID')

bankhtcheck=subset(rawwhPVT,INCISED>1.5)#!possible crossvalidation rule to scan for#no bank heights showed up in the legal value or outlier check so wanted to check units
#bankhtcheck=subset(rawwhPVT,BANKHT>INCISED|BANKHT>1.5)#!possible crossvalidation rule to scan for#no bank heights showed up in the legal value or outlier check so wanted to check units
wetwidthchecks=subset(rawwhPVT,WETWID>BANKWID)
write.csv(bankhtcheck,'bankhtincisedcheck_23Oct2014.csv')
write.csv(wetwidthchecks,'wetwidthchecks_23Oct2014.csv')
write.csv(rbind(widhgt2,banks),'WidthHeightRaw_23Oct2014.csv')#need raw output to get IND values

#getting raw bank data for a few problem sites
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects='WRSA',Years=c('2013','2014'),SiteCodes=c('MN-SS-1133','MP-SS-2091','MS-SS-3126','XE-RO-5081','XE-SS-5105','XS-SS-6135', 'OT-LS-7001',	'OT-LS-7012',	'MP-SS-2080',	'XE-SS-5150',	'MS-LS-3026',	'OT-LS-7019',	'OT-SS-7133'))
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects='WRSA',Years=c('2013','2014'),SiteCodes=c('MN-SS-1133','MP-SS-2091','MS-SS-3126','XE-RO-5081','XE-SS-5105','XS-SS-6135', 'OT-LS-7001',  'OT-LS-7012',	'MP-SS-2080',	'XE-SS-5150',	'MS-LS-3026',	'OT-LS-7019',	'OT-SS-7133'))
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
wnPVTIND=cast(widhgt,'UID+TRANSECT~PARAMETER',value='IND')     
rawwhPVT=addKEYS(merge(whPVT,wnPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
colnames(rawwhPVT)<-c('UID','TRANSECT','BANKHT','BANKWID','BARWID','INCISED','WETWID','BANKHT_IND','BANKWID_IND','BARWID_ID','INCISED_IND','WETWID_ID','DATE_COL','SITE_ID')
write.csv(rawwhPVT,'problem_sites_cross_valid_bank.csv')

#third bank parameter check on select UIDs based off of summary
widhgt=addKEYS(tblRetrieve(Parameters=c('BANKHT'), Projects='WRSA',Years=c('2013','2014'),UIDS=c(10376,10381,13517,11833,12717,11847,12648,11836)),c('SITE_ID','DATE_COL'))
widhgt.sub=addKEYS(TBLout,c('SITE_ID','DATE_COL'))

tblRetrieve(Parameters='ANGLE180', SiteCodes='XN-RO-4085')

#Increment cross-validation WRSA checks
incrementcheck=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT','RCHWIDTH'),Projects='WRSA',Years=c('2013','2014'),Protocols=c('NRSA13','WRSA14'))
incrsub=subset(incrementcheck,UID!=10383)#UID:10383  IND 4849393 needs to be deactivated for this to work
incrementPVT=cast(incrsub,'UID~PARAMETER',value='RESULT')
incrementsub=subset(incrementPVT,TRCHLEN/0.01!=INCREMENT)#couldn't get this to work so checked this manually in excel and also checked to make sure that RCHWIDTH*40=TRCHLEN and for RCHWIDTH<2.5 INCREMENT=1 and for RCHWIDTH>2.5<4 INCREMENT=1.5
write.csv(incrementPVT,'incrementPVT.csv')
weridinc=tblRetrieve(Parameters=c('INCREMENT'),UIDS='11852')

#check 0 substrate flagged in legal values
substratecheck=addKEYS(tblRetrieve(Parameters=c('SIZE_NUM'),Projects='WRSA',Years=c('2013','2014'),Protocols=c('NRSA13','WRSA14'), Comments='Y'), c('SITE_ID','DATE_COL'))
zerosubstrate=subset(substratecheck, RESULT==0)
write.csv(zerosubstrate,'zerosubstrate.csv')

#second round cross validation checks
#checked bht and bankwidth 1st round checks again and did not find any values that still needed to be changed
incisedhtcheck=subset(rawwhPVT,INCISED>1.5)#many came up but no unit errors obvious
barwidthchecks=subset(rawwhPVT,BARWID>WETWID)#none found

banks=tblRetrieve(Parameters=c('ANGLE','UNDERCUT'), Projects='WRSA',Years=c('2013','2014'))
banksnum=subset(banks,is.na(as.numeric(RESULT))==F);banksnum$RESULT=as.numeric(banksnum$RESULT)
bnkPVT=cast(banks,'UID+TRANSECT~PARAMETER+POINT',value='RESULT')   
bnkPVTIND=cast(banks,'UID+TRANSECT~PARAMETER+POINT',value='IND')  
rawwhPVT=addKEYS(merge(bnkPVT,bnkPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
undercut_checks=subset(rawwhPVT,UNDERCUT_LF.x>1|UNDERCUT_RT.x>1)
write.csv(undercut_checks,'undercut_checks.csv')#many units issues

#checking UID with poor scan
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), UIDS='11785')
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), UIDS='11785')
banks=tblRetrieve(Parameters=c('ANGLE','UNDERCUT'), UIDS='11785')
banksnum=subset(banks,is.na(as.numeric(RESULT))==F);banksnum$RESULT=as.numeric(banksnum$RESULT)
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
bnkPVT=cast(banks,'UID+TRANSECT~PARAMETER+POINT',value='RESULT')      
tranPVT=addKEYS(merge(whPVT,bnkPVT,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))# all values match field sheet # no transcription errors for bank measurements so did not check substrate measurements

#summary stats site issues
issue<-addKEYS(tblRetrieve(Parameters='INCISED',SiteCodes=c('XE-SS-5105'),Comments='Y'),c('SITE_ID'))
issue<-addKEYS(tblRetrieve(Parameters='BANKWID',SiteCodes=c('OT-LS-7001','XS-SS-6135','MS-SS-3126'),Comments='Y'),c('SITE_ID'))
issue<-addKEYS(tblRetrieve(Parameters='DEPTH',SiteCodes=c('XE-LS-5021','MP-LS-2010'),Comments='Y'),c('SITE_ID'))
issue<-addKEYS(tblRetrieve(Parameters='INCISED',SiteCodes=c('MP-LS-2003','XN-RO-4086','XN-SS-4135','MN-LS-1009','XE-SS-5105','XE-SS-5105','MN-SS-1134','MP-LS-2017','XE-SS-5114','MP-LS-2003','XE-SS-5114','MP-LS-2003'),Comments='Y'),c('SITE_ID'))

#checking all sites have correct project
listsites=tblRetrieve(Parameters='SITE_ID',Projects='WRSA',Protocols=c('WRSA14'))
 
#missing data additional checks
tbl=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BANKWIDTH'),Projects='WRSA', Years='2014')
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))

tbl=tblRetrieve(Parameters=c('EMBED'),Projects='WRSA', Years='2014')
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))

tbl=tblRetrieve(Parameters=c('PTAILDEP','MAXDEPTH','LENGTH'),Project='WRSA',Year='2014')
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))

tbl=tblRetrieve(Parameters=c('DEPTH'),Project='WRSA', Years='2014')
tbl.2=tblRetrieve(Parameters=c('SUB_5_7'),Project='WRSA', Years='2014')
tbl3=cast(tbl.2,'UID~PARAMETER',value='RESULT',mean)
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))
thalweg.missing=merge(tbl.PVT,tbl3, by='UID')

######################################################################AK############################################################

##Alaska Site Inventory
AKdesignation=addKEYS(tblRetrieve(Comments='Y',Parameters=c('SITE_ID'),Projects='AKEFO'),c('DATE_COL','LOC_NAME','LAT_DD','LON_DD','VALXSITE'))
#!need "add comments" ability via flag in tblRetrieve; AKdesignation needs whatever comment contains the site information, may need to change the parameter (manually need to add flags)

AKminesASSESS=addKEYS(tblRetrieve(Comments='Y',Parameters=c('IND_MINES','MINE'),Projects='AKEFO'),c('SITE_ID','DATE_COL','LOC_NAME'))
#? pipes, construction, liming, 'MAN_DREDGING','MAN_TREATMENT','HYDR'

write.csv(AKminesASSESS,'AKmine.csv')

##AK 2015 data request
AK_Thalweg=addKEYS(tblRetrieve(Parameters=c('DEPTH'),Projects='AKEFO',Comments='Y'),c('SITE_ID'))
AK_Increment=addKEYS(tblRetrieve(Parameters=c('INCREMENT','TRCHLEN'),Projects='AKEFO',Comments='Y'),c('SITE_ID'))
AK_Substrate=addKEYS(tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects='AKEFO',Comments='Y'),c('SITE_ID'))
pvtAKSubstrate=addKEYS(cast(AK_Substrate,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
pvtAKIncrement=addKEYS(cast(AK_Increment,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
AK_fish=addKEYS(tblRetrieve(Parameters=c('BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'),Projects='AKEFO',Comments='Y'),c('SITE_ID'))
pvtAKfish=addKEYS(cast(AK_fish,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))

Surveyp=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'SURVEY%'"))$PARAMETER
Surveydata=tblRetrieve(Parameters=Surveyp, Projects='AKEFO',Years='2015')
pvtSurvey=addKEYS(cast(Surveydata,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','PROJECT'))

##Alaska data for Franklin Creek
AKstability=addKEYS(tblRetrieve(Parameters=c('STABLE','EROSION','COVER'),SiteCodes=c('AA-STR-0001','AA-STR-0005'),Comments='Y'),c('SITE_ID'))
pvtAKstability=addKEYS(cast(AKstability,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))

AKstability=addKEYS(tblRetrieve(Parameters=c('STABLE','EROSION','COVER'),Projects='AKEFO',Comments='Y'),c('SITE_ID'))
pvtAKstability=addKEYS(cast(AKstability,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))

AKbank=addKEYS(tblRetrieve(Parameters=c('INCISED','BANKHT'),SiteCodes=c('AA-STR-0001','AA-STR-0005'),Comments='Y'),c('SITE_ID'))
pvtAKbank=addKEYS(cast(AKbank,'UID+TRANSECT~PARAMETER',value='RESULT'), c('SITE_ID'))

##############################################################NorCal#################################################################

##NorCal QC followup
#PilotDB - verify areas
BERW=setdiff(unclass(sqlQuery(wrsa1314,"select PARAMETER from tblMetadata where Sample_TYPE='BERWx'"))$PARAMETER,c("SHIPPING_METHOD","CHANNEL","NOT_COLLECTED","SAMPLE_ID","ACTUAL_DATE","SUBSTRATE"))
ncSAMP=tblRetrieve(Parameters=BERW,Projects='NorCal',Years=c('2013','2014'))
ncSAMPpvt=addKEYS(cast(ncSAMP,'UID~SAMPLE_TYPE+PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL'))
#Nicole - All conductivity values to troubleshoot a low (6) value. 
ncond=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED'), Comments='Y',Projects='NorCal',Years=c('2013','2014'))
ncondPVT=addKEYS(cast(ncond,'UID+FLAG+COMMENT~PARAMETER',value='RESULT') #!comment/flag makes a separate pivot row...consider fixing if this functionaility is frequently desired (moving Comments addition to addKEYS?)
                 ,c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))#Include lat long for site proximity/mapping
write.csv(ncond,'NorCalConductivity_17Sept2014.csv')
write.csv(ncondPVT,'NorCalConductivityCorrected_17Sept2014.csv')#pivoted does not contain IND


###############################################################Other################################################################

######Sarah's originial width height checks that I have modified
#Scott - width and height checks: suspected fieldsheet flip flop and units issues
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects='NorCal',Years=c('2013','2014'))
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects='NorCal',Years=c('2013','2014'))
banks=tblRetrieve(Parameters=c('ANGLE','UNDERCUT'), Projects='NorCal',Years=c('2013','2014'))
banksnum=subset(banks,is.na(as.numeric(RESULT))==F);banksnum$RESULT=as.numeric(banksnum$RESULT)
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
bnkPVT=cast(banks,'UID+TRANSECT~PARAMETER+POINT',value='RESULT')      
tranPVT=addKEYS(merge(whPVT,bnkPVT,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
wh2PVTavg=addKEYS(cast(rbind(banksnum,widhgt2),'UID~PARAMETER',value='RESULT',fun='mean')
                  ,c('SITE_ID','DATE_COL'))
View(subset(wh2PVTavg,BANKHT>INCISED))#!possible crossvalidation rule to scan for

write.csv(rbind(widhgt2,banks),'WidthHeightRaw_17Sept2014.csv')
write.csv(tranPVT,'WidthHeightPivot_17Sept2014.csv')
write.csv(wh2PVTavg,'WidthHeightAvg_17Sept2014.csv')


###############################################################################################################################
#!TN and TP updates, BMI sampleID updates

#!updating "N" for SIDCHN, etc #aquamet made apparent that thalweg Yes/no variables should be populated with "No" too (not just if "yes") despite EPA not providing it (ironically, used in reachlen calc)
YNparamTHALw=c('SEDIMENT','BAR_PRES','SIDCHN','BACKWATER');YNparamTHALb=c('SNAG','OFF_CHAN');thalwb=c('w','b')  
for (i in 1:length(thalwb)){
YNparamTHAL=eval(parse(text=sprintf('YNparamTHAL%s',thalwb[i])))
THAL=tblRetrieve(Parameters=c('CHANUNCD',YNparamTHAL),Years='2013')#if 2014, would need to use depth or something else that is always populated
THAL=subset(THAL,SAMPLE_TYPE==toupper(sprintf('THAL%s',thalwb[i])))
THAL=cast(THAL,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
THAL=ColCheck(THAL,c("UID","TRANSECT","POINT",YNparamTHAL))
for(n in 1:length(YNparamTHAL)){
  THALvar=YNparamTHAL[n]
  THAL[,names(THAL) %in% c(THALvar)]=ifelse(is.na(THAL[,names(THAL) %in% c(THALvar)]),"N",THAL[,names(THAL) %in% c(THALvar)])
}
THAL=THAL[,!names(THAL) %in% c('CHANUNCD')]
THAL=melt(data.frame(THAL),id=c("UID","TRANSECT","POINT"));THAL$RESULT=THAL$value;THAL$PARAMETER=THAL$variable
THAL=ColCheck(subset(THAL,RESULT=='N'),c(VAR,'TRANSECT','POINT','PARAMETER','RESULT'))
THAL$SAMPLE_TYPE=toupper(sprintf('THAL%s',thalwb[i]))
if(i==1){THALout=THAL} else{THALout=rbind(THALout,THAL)}
}
#sqlSave(wrsa1314,dat=THALout,tablename='tblPOINT',rownames=F, append=TRUE)#imported 10/7/2014

#checking for IND duplicates
# select * from(
#   select IND, COUNT(*)as cnt from(
#     select IND, 'v' as tbl, active, insertion,reason, parameter from tblVERIFICATION
#     union select IND, 'r' as tbl , active, insertion,reason, parameter from tblREACH
#     union select IND, 't' as tbl, active, insertion,reason, parameter from tblTRANSECT
#     union select IND, 'c' as tbl , active, insertion,reason, 'comment' parameter from tblcomments
#     union select IND, 'p' as tbl, active, insertion,reason, parameter  from tblpoint) as un
#   group by IND
# ) gr
# join (
#   select IND, 'v' as tbl, active, insertion,reason, parameter from tblVERIFICATION
#   union select IND, 'r' as tbl, active, insertion,reason, parameter from tblREACH
#   union select IND, 't' as tbl, active, insertion,reason, parameter from tblTRANSECT
#   union select IND, 'c' as tbl, active, insertion,reason, 'comment' as parameter from tblcomments
#   union select IND, 'p' as tbl, active, insertion,reason, parameter from tblpoint) as un2
# on un2.ind=gr.ind
# where cnt>1-- and ((REASON is not null and REASON <> 'NA') or (YEAR(INSERTION)=2014 and MONTH(INSERTION)>5))
# order by gr.ind, tbl



