##Alaska Site Inventory
AKdesignation=addKEYS(tblRetrieve(Comments='Y',Parameters=c('SITE_ID'),Projects='AKEFO'),c('DATE_COL','LOC_NAME','LAT_DD','LON_DD','VALXSITE'))
#!need "add comments" ability via flag in tblRetrieve; AKdesignation needs whatever comment contains the site information, may need to change the parameter (manually need to add flags)

AKminesASSESS=addKEYS(tblRetrieve(Comments='Y',Parameters=c('IND_MINES','MINE'),Projects='AKEFO'),c('SITE_ID','DATE_COL','LOC_NAME'))
#? pipes, construction, liming, 'MAN_DREDGING','MAN_TREATMENT','HYDR'

write.csv(AKminesASSESS,'AKmine.csv')

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






CORRECTED='N'


#Jennifer - missing data checks

#Jennifer - Slope checks
SLOPEp=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'SLOPE%'"))$PARAMETER
crewKC=tblRetrieve(Parameters=c('CREW_LEADER','NAME1'),Filter="RESULT in ('172','184')")
crewKC=addKEYS(cast(crewKC,'UID~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL'))
crewKC$SumKC=crewKC$CREW_LEADER + crewKC$NAME1;crewKC=subset(crewKC,SumKC==356)
SlopeKC=tblRetrieve(Parameters=SLOPEp,UIDS=crewKC$UID);SlopeKC$SlopeFLAG='KathleenClayton'
nonTransit=sqlQuery(wrsa1314,"select * from tblpoint where SAMPLE_TYPE like '%SLOPE%' and PARAMETER='METHOD' and RESULT <> 'TR'")
SlopeTR=tblRetrieve(Parameters=SLOPEp,UIDS=nonTransit$UID);SlopeTR$SlopeFLAG='OddMethod'
SlopeBoat=tblRetrieve(Parameters=SLOPEp,Protocols='BOAT14');SlopeBoat$SlopeFLAG='Boatable'
SlopeAK=tblRetrieve(Parameters=SLOPEp,Projects='AKEFO');SlopeAK$SlopeFLAG='AK'
if(exists('SlopeConnectFail')){SlopeUNC=tblRetrieve(Parameters=SLOPEp,UIDS=c(SlopeConnectFail$UID,SlopeConnectPassEndFail$UID,87601876754740660818804248));SlopeUNC$SlopeFLAG='PartialReach'} else{SlopeUNC=SlopeBoat[0,]}
SlopeIssues=rbind(SlopeKC,SlopeBoat,SlopeTR,SlopeAK,SlopeUNC);
SlopeIssues=addKEYS(cast(SlopeIssues,'UID+TRANSECT+POINT+SlopeFLAG~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','PROJECT'))

#Jennifer - WQ WRSA checks
WQ2=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','CAL_INST_ID'), Comments='Y',Projects='WRSA',Years=c('2013','2014'))
WQ1=addKEYS(cast(WQ2,'UID~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','CREW_LEADER'))

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

#bank cross-validation WRSA checks
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects='WRSA',Years=c('2013','2014'))
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects='WRSA',Years=c('2013','2014'))
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
wnPVTIND=cast(widhgt,'UID+TRANSECT~PARAMETER',value='IND')     
tranPVT=addKEYS(merge(whPVT,bnkPVT,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
rawwhPVT=addKEYS(merge(whPVT,wnPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
colnames(rawwhPVT)<-c('UID','TRANSECT','BANKHT','BANKWID','BARWID','INCISED','WETWID','BANKHT_IND','BANKWID_IND','BARWID_ID','INCISED_IND','WETWID_ID','DATE_COL','SITE_ID')

bankhtcheck=subset(rawwhPVT,BANKHT>INCISED|BANKHT>1.5)#!possible crossvalidation rule to scan for#no bank heights showed up in the legal value or outlier check so wanted to check units
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

#Increment cross-validation WRSA checks
incrementcheck=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT','RCHWIDTH'),Projects='WRSA',Years=c('2013','2014'),Protocols=c('NRSA13','WRSA14'))
incrsub=subset(incrementcheck,UID!=10383)#UID:10383  IND 4849393 needs to be deactivated for this to work
incrementPVT=cast(incrsub,'UID~PARAMETER',value='RESULT')
incrementsub=subset(incrementPVT,TRCHLEN/0.01!=INCREMENT)#couldn't get this to work so checked this manually in excel and also checked to make sure that RCHWIDTH*40=TRCHLEN and for RCHWIDTH<2.5 INCREMENT=1 and for RCHWIDTH>2.5<4 INCREMENT=1.5
write.csv(incrementPVT,'incrementPVT.csv')
weridinc=tblRetrieve(Parameters=c('INCREMENT'),UIDS='11852')

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


