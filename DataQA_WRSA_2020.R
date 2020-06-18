#most run DBpassword_doNotgit.R and DataConsumption_WRSAdb.R setup section first
#must run Project, Year, and Protocol filters in DataConsumption_WRSAdb.R first

#all issues that are found but not dealt with immediately should be recorded in OfficeComments -either on google drive or access database
#all edits should be added to Z:\buglab\Research Projects\AIM\Analysis\QC\OfficeUpdates   for data collected from 2016 and beyond #or Z:\buglab\Research Projects\BLM_WRSA_Stream_Surveys\Results and Reports\QA\final QC of all sites collected up to Nov 2015\Office_updates.xsls for WRSA data
#follow instructions in "Z:\buglab\Research Projects\AIM\Database_Development\AIM Data Management.docx" page 21 for how to format the data and put it into OfficeUpdates table 

# #To get data in the correct format for edits run the following query on the transect, point, sample_type, parameter or result of interest. If using multiple of these put "and" in between them and if querying more than one parameter etc. the SQL "IN()" function is very helpful  
# #Do not run the code below without anything in the "where" clause. Otherwise this queries ALL data in the database and will crash R
# edits=sqlQuery(wrsa1314,"select * from View_AllData where parameter='SITE_ID'") # Prefer to run this query directly in SQL though so that I can see error messages from SQL server and proper SQL centax prompts
# write.csv(edits,'edits.csv',na="")
#Jennifer will then copy and paste the edits into the ProbSurvey Access database run the stored export to export a csv. Then run the UpdateDatabase_WRSA.R script and follow its subsequent instructions
 


######################################################################################
#########                          Site Check                                #########
######################################################################################
##First step-do you have all the sites you should?
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
listsites=cast(listsites,'UID~PARAMETER',value='RESULT')
listsites=listsites[c(10,11,12,6,1,2,13,3,7,4,8,5,9)]
listsites=listsites[order(listsites$PROJECT,listsites$PROTOCOL,listsites$SITE_ID),]
write.csv(listsites,'listsites.csv')
#read in sites from final designations and do a left join to determine if any sampled sites are missing or if sample statuses need to be changed
#if sites are missing check error logs on server or emails and check last modfied date

#do all sites have correct final designation?
#check all partial sites by running missing data checks below

######################################################################################
########            Comment Check and Office Comments check                 ##########
######################################################################################
#export comments table from SQL ----prefer to do this prior to missing data check because might fill in missing data
#comments=addKEYS(sqlQuery(wrsa1314,sprintf("select * from tblcomments where year(insertion) in (%s) and datepart(wk,insertion) in (%s)",inLOOP(years),inLOOP(insertion))),c('SITE_ID','PROJECT'))#other option, but below is more elegant
comments=addKEYS(tblRetrieve(Table='tblcomments', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','PROJECT','VALXSITE'))
#comments=subset(comments,PROJECT!='TRAINING'& PROJECT!='TESTORFAKE DATA')
comments=setNames(comments[,c(13,14,15,1,6,4,2:3,5,7:12)],c('PROJECT','SITE_ID','VALXSITE','UID','COMMENT_TYPE','COMMENT','SAMPLE_TYPE','TRANSECT','POINT','IND','ACTIVE','OPERATION','INSERTION','DEPRECATION','REASON'))
write.xlsx(comments,'comments.xlsx')   
    #check for data not entered in fields
    #check for data that needs to be deleted or moved
    #check for data quality issues such as flooding prior to taking water quality
    #check for protocol clarity issues especially dry sites
    #check for dry transects or partial data notes
    #especially review QA comments
    #inactivate resolved or superfluous comments
#QAcomments=tblRetrieve(Parameter=c())

#review office comments and prioritize anything that might affect below checks----need above site information to link UID to comments and make change
#https://docs.google.com/spreadsheets/d/1n58CHXDivjPLKXLX-bPwX1nX6XXtFTbpHlDF528LT-k/edit?usp=sharing

#######################################################################################
########          Preliminary missing data check                            ###########
#######################################################################################
# A preliminary check to make sure all paper field forms were entered, partial sites are flagged and no egregious protocol errors with more than a few indicators with >50% data missing

##missing data parameters
UnionTBL=tblRetrieve(Table='', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
#ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years='years',Protocols='protocols')#Protocols='' brings in failed sites as well

CheckAll='N'#options: 'Y' = Check All Parameters for the protocol; 'N' = Check only Parameters in UnionTBL (i.e. if subsetting UnionTBL to single Table and don't want clutter from parameters not interested in)....this is not done automatically because missing data checks are meant to look for parameters that have ZERO readings for a particular dataset, only use in testing and known scenarios (usually where AllParams='Y')
CommentsCount='N'#'Y' = a comment (as represented by a flag) allows the missing data warning to be ignored; 'N' = missing data is reported regardless and contributes to subsequent percentages. 
MissingXwalk='MissingBackend'; MetricType='Y'#if MetricType=='Y' then groupings will be done by the bin (Type_Xwalk) and the metric type (Indicator, Covariate, QC)

##missing data check
if('POINT' %in% colnames(UnionTBL) ==FALSE){UnionTBL$POINT=NA};if('TRANSECT' %in% colnames(UnionTBL) ==FALSE){UnionTBL$TRANSECT=NA}
Protocols=subset(tblRetrieve(Table='tblVERIFICATION',Parameters='PROTOCOL',UIDS=unique(UnionTBL$UID)),select=c(UID,RESULT))#unique(subset(importmaster,select=RESULT,subset=toupper(PARAMETER)=='PROTOCOL'))
ProtocolsP=unique(Protocols$RESULT)
tblMetadataProtocoltmp=sqlQuery(wrsa1314,sprintf("select * from tblMetadataProtocol"))
emptyFulldataset=tblMetadataProtocoltmp[1,]
emptyFulldataset$TRANSECT=NA;    emptyFulldataset$UID=NA;emptyFulldataset=emptyFulldataset[0,]
for (p in 1:length(ProtocolsP)){
  emptyFulldatasetTmp=emptyFulldataset[0,]
  if(CheckAll=='Y'){PARAMstr=''} else{PARAMstr=sprintf('and PARAMETER in (%s) and SAMPLE_TYPE in (%s)', inLOOP(unique(UnionTBL$PARAMETER)), inLOOP(unique(UnionTBL$SAMPLE_TYPE)))}
  tblMetadataProtocolR=sqlQuery(wrsa1314,sprintf("select * from tblMetadataProtocol where Protocol='%s' and Active='Y' %s",unlist(ProtocolsP[p]),PARAMstr))
  reps=unique(tblMetadataProtocolR$Reps);reps=setdiff(reps,NA)
  tran=c('A','B','C','D','E','F','G','H','I','J','K')
  midtran=c('AB','BC','CD','DE','EF','FG','GH','HI','IJ','JK')
  for (r in 1:length(reps)){
    repcnt=reps[r]
    reptmp=subset(tblMetadataProtocolR,subset=Reps==repcnt);reptmp$TRANSECT=NA;reptmp$UID=NA
    reptmp3=reptmp[0,]
    if(repcnt>1){#parameters that occur at multiple transects
      for(t in 1:length(tran[1:ifelse(repcnt==10,10,11)])){
        reptmp2=subset(reptmp,(PARAMETER %in% c('WETWID','BARWID') & SAMPLE_TYPE=='THALW')==FALSE)
        reptmp2$TRANSECT=tran[t]
        reptmp3=rbind(reptmp3,reptmp2)
      }
      reptmp4=subset(reptmp,repcnt>11)#(PARAMETER %in% c('STABLE','COVER','EROSION','LOC','SIZE_CLS'))| (PARAMETER %in% c('WETWID','BARWID') & SAMPLE_TYPE=='THALW'))#parameter subsetting was too unique to 2014 protocol
      if(nrow(reptmp4)>0){#middle station additional
        for(m in 1:length(midtran)){
          reptmp2=reptmp4
          reptmp2$TRANSECT=midtran[m]
          reptmp3=rbind(reptmp3,reptmp2)
        }}
    } else {reptmp3=reptmp}
    emptyFulldatasetTmp=rbind(emptyFulldatasetTmp,reptmp3)
  }
  #testingUID=c('1.6444949915002e+19','34978445900951396','5.22341843044643e+20','6.05725564046426e+21','6.57547054447886e+26','745880431965292672','9.33867144639421e+23','96984648251121344','1.28761064109694e+21','333543271296521','4.72713814328789e+22','1708915464110931968','2.54415932043491e+20','3546443591531088896','8.6938972431891e+19','6.88074369254182e+25','535384124962793152','573854465591678','3795875273449409024','8.0204662147438e+20','26528638400395079680')
  UIDsADD=unique(subset(UnionTBL,select=UID,subset=UID %in% subset(Protocols,RESULT==ProtocolsP[p])$UID )) #& UID %in% testingUID))
  UIDtmp=emptyFulldatasetTmp
  for (u in 1:nrow(UIDsADD)){  
    UIDtmp$UID=UIDsADD$UID[u]
    emptyFulldataset=rbind(emptyFulldataset,UIDtmp)
  }                         
}

emptytmp=emptyFulldataset#;empty14Jul14=emptyFulldataset

#!method/checking oddities (not sure if these should be handled in emptyFulldataset or MissingCounts)
UnionTBL$TRANSECT=ifelse(UnionTBL$SAMPLE_TYPE %in% c('SLOPEW', 'HABITAT','PHOTO') 
                         & addKEYS(UnionTBL,c('PROTOCOL'))$PROTOCOL %in% c('WRSA14','AK14','WADE2016','BOAT2016'),
                         NA,UnionTBL$TRANSECT)#instances where only care if have at least one, but it's recorded as multiple so TRANSECT unable to join in MissingCounts if different
UnionTBL$TRANSECT=ifelse(UnionTBL$PARAMETER %in% c('FLOOD_WID', 'FLOOD_BFWIDTH'),
                         NA,UnionTBL$TRANSECT)
#!posible issues with INCREMENT, but likely warrants database cleanup

#only reporting values with missing counts
MissingCounts=sqldf("select *, case when MissingCount is null then Points else Points-MissingCount end MissingCNT from 
                    (select *, case when Transect is null then 'O' when Transect='' then 'O' else Transect end as TRE from emptyFulldataset where Points>0) as ExpectedCounts
                    outer left join 
                    (select Sample_Type STO, Parameter PO, [UID] as UIDo, case when Transect is null then 'O' when Transect='' then 'O' else Transect end as TR, COUNT(Result) MissingCount, FLAG  from UnionTBL
                    group by Sample_Type, Parameter, [UID], Transect) as ObservedCounts
                    on ExpectedCounts.Sample_Type=ObservedCounts.STO and ExpectedCounts.Parameter=ObservedCounts.PO and ExpectedCounts.TRE=ObservedCounts.TR and ExpectedCounts.UID=ObservedCounts.UIDo
                    where (MissingCount < Points or MissingCount is null) ")
if(CommentsCount=='Y'){
  COMt=unique(ColCheck(tblRetrieve(Table='tblCOMMENTS',UIDS=unique(UnionTBL$UID)),c('SAMPLE_TYPE','TRANSECT','UID','FLAG')))##6692-2898
  FLAGS=unique(subset(UnionTBL,select=c(UID,SAMPLE_TYPE,TRANSECT,FLAG),is.na(FLAG)==FALSE));FLAGS$PRESENT=1#trying to count only comments that don't have a pair = that's why data is missing
  COMt=merge(COMt,FLAGS,all.x=T);COMt=subset(COMt,is.na(PRESENT))
  COMt=unique(ColCheck(COMt,c('SAMPLE_TYPE','TRANSECT','UID')))  
  COMt$TRANSECT=ifelse(COMt$TRANSECT=="ALL","O",COMt$TRANSECT);MissingCounts$TRANSECT=ifelse(is.na(MissingCounts$TRANSECT),"O",MissingCounts$TRANSECT)
  COMt$COMMENTcnt=1
  MissingCounts=merge(MissingCounts,COMt,by=c('SAMPLE_TYPE','TRANSECT','UID'),all.x=T)
} else{MissingCounts$COMMENTcnt=NA}
  
#count number of parameters with missing data
MissingTotals=sqldf('select UID,count(*) ParamCNT from MissingCounts group by UID')


#Xwalk Sample_types before grouping
#MissingCounts$Sample_Type_ORG=MissingCounts$SAMPLE_TYPE; MissingCounts$Parameter_ORG=MissingCounts$PARAMETER#keeping this for peace of mind, may be able to omit
#emptyFulldataset$Sample_Type_ORG=emptyFulldataset$SAMPLE_TYPE; emptyFulldataset$Parameter_ORG=emptyFulldataset$PARAMETER
if (MissingXwalk==''){
  MissingCounts$TABLE=''#;MissingGrouping='SAMPLE_TYPE'
} else{
  colnames(MissingCounts)[which(colnames(MissingCounts) %in% "Insertion")] = "INSERTION"
   MissingCounts=Xwalk(Source='R',XwalkName=MissingXwalk,XwalkDirection='',Table='MissingCounts',COL=colnames(MissingCounts))#,Filter='',UIDS='BLANK',SiteCodes='',Dates='',Years='',Projects='',Protocols='',Parameters='',ALLp='N'){
   colnames(emptyFulldataset)[which(colnames(emptyFulldataset) %in% "Insertion")] = "INSERTION"
   emptyFulldataset=Xwalk(Source='R',XwalkName=MissingXwalk,XwalkDirection='',Table='emptyFulldataset',COL=colnames(emptyFulldataset))
  if(MetricType=='Y' & MissingXwalk=='MissingBackend'){
    MissingCounts$SAMPLE_TYPE=sprintf('%s:%s',MissingCounts$SAMPLE_TYPE,MissingCounts$TABLE)
    emptyFulldataset$SAMPLE_TYPE=sprintf('%s:%s',emptyFulldataset$SAMPLE_TYPE,emptyFulldataset$TABLE)
  } #else{MissingGrouping='SAMPLE_TYPE'}#can set custom grouping combinations here as desired for specific xwalks
  
}

#!combine Sample_type and table instead of groupings?
#DONE: need to update MetadataProtocol to account for things that need to contribute to missing data checks: ACTRANSP, etc. ; change SIZE_CLS to SIZE_NUM
#!don't group by parameter(metric name), but join at end
#!fix MissingTotals4 (or did this only matter for paramCNT and commentCNT?); just tell folks this has stopped development unless they would like it



#group by Sample_type + Transect #! likely want a more custom solution with something more akin to QAbins in FM
MissingTotals2=sqldf(sprintf("select UID,SAMPLE_TYPE,TRE as TRANSECT,MC MissCNT, PT ExpectedCNT, cast(ifnull(round(MC/PT,2) ,0) as float) MissingPCT, CC COMMENTcnt from
                     (select UID, SAMPLE_TYPE,count(*) PTC, cast(sum(Points) as float) PT, case when Transect is null then 'O' else Transect end as TRE
                     from emptyFulldataset group by UID, SAMPLE_TYPE, Transect) as ExpectedCounts
                     outer left join 
                     (select UID as UIDo, SAMPLE_TYPE STO,case when Transect is null then 'O' else Transect end as TR, count(*) MCC, cast(sum(MissingCNT) as float) MC , sum(COMMENTcnt) as CC
                     from MissingCounts group by UID, SAMPLE_TYPE, Transect) as ObservedCounts
                     on ExpectedCounts.UID=ObservedCounts.UIDo and ExpectedCounts.Sample_Type=ObservedCounts.STO  and ExpectedCounts.TRE=ObservedCounts.TR 
                     %s",ifelse(CommentsCount=='Y','where COMMENTcnt is null','')))


#group by Sample_type only to get reach totals
MissingTotals3=sqldf("select UID,SAMPLE_TYPE, 'ReachTotal' TRANSECT,sum(MissCNT) MissCNT, sum(ExpectedCNT) ExpectedCNT ,  cast(ifnull(round((cast(sum(MissCNT) as float)/cast(sum(ExpectedCNT) as float) ),2),0) as float) MissingPCT, sum(COMMENTcnt) as COMMENTcnt from MissingTotals2 group by UID, SAMPLE_TYPE")

#main ouput (until param and comments fleshed out more)
if (MissingXwalk==''){
  MissingTotals5=sqldf("select UID, 'REACH' SAMPLE_TYPE,  'O' TRANSECT, ParamCNT,MissCNT,  ExpectedCNT, MissingPCT ,COMMENTcnt from MissingTotals 
                     join 
                     (select UID as UIDm,  sum(MissCNT) MissCNT, sum(ExpectedCNT) ExpectedCNT,sum(MissCNT) /sum(ExpectedCNT)  MissingPCT , sum(COMMENTcnt) as COMMENTcnt from MissingTotals2 group by UID) as Totals
                     on MissingTotals.UID=Totals.UIDm
                     ")
  MissingTotals6=MissingTotals5[,!(names(MissingTotals5) %in% c('ParamCNT'))];MissingTotals7=MissingTotals5;MissingTotals7$MissingPCT=MissingTotals7$ParamCNT;MissingTotals7$SAMPLE_TYPE='Param';MissingTotals7=MissingTotals7[,!(names(MissingTotals7) %in% c('ParamCNT'))]
  MissingTotals4=unique(rbind(MissingTotals2,MissingTotals3,MissingTotals6,MissingTotals7)  );MissingTotals4$RESULT=MissingTotals4$MissingPCT;MissingTotals4$PARAMETER=paste("PCT_",MissingTotals4$SAMPLE_TYPE,"_QA",sep='');MissingTotals4$TRANSECT=ifelse(MissingTotals4$TRANSECT=='O',NA,MissingTotals4$TRANSECT);MissingTotals4$POINT=MissingTotals4$TRANSECT;MissingTotals4=ColCheck(MissingTotals4,c(VAR,'TRANSECT','POINT','PARAMETER','RESULT'))
  print ('ready for use in Access import')
} else{MissingTotals4=unique(rbind(MissingTotals2,MissingTotals3)  )
#! need to fix the revisions to MissingTotals4 before this will work
MissingTotals4=MissingTotals4[,!(names(MissingTotals4) %in% c('POINT'))]
MissingTotalsOUT= addKEYS(cast(subset(MissingTotals4,is.na(UID)==FALSE ), 'UID + TRANSECT  ~ SAMPLE_TYPE',value='MissingPCT' ),Columns=c('SITE_ID','DATE_COL','Protocol','Project','VALXSITE','CREW_LEADER','LOC_NAME')) 
MissingTotalsREACH=subset(MissingTotalsOUT,TRANSECT=='ReachTotal')
names=gsub("^INDICATOR:","",colnames(MissingTotalsREACH))
MissingTotalsREACH=setNames(MissingTotalsREACH,names)
columns=c('PROJECT',	'PROTOCOL',	'SITE_ID',	'LOC_NAME','VALXSITE',	'UID',	'DATE_COL',	'CREW_LEADER','REACH INFO',	'MACROINVERT',	'CONDUCTIVITY',	'PH',	'TEMPERATURE','VEG',	'CANOPY COVER',	'BANKFULL HEIGHT',	'BANKFULL WIDTH',	'WETTED WIDTH',	'BENCH HEIGHT',	'FLOODPRONE WIDTH',	'BANK STABILITY',	'STREAMBED PARTICLES',	'HUMAN INFLUENCE',	'LARGE WOOD',	'SLOPE',	'POOL',	'PHOTO',	'TNTP',	'TURBIDITY','THALWEG DEPTH PROFILE',	'BANK ANGLE','POOL TAIL FINES','VEGCOMPLEX','FISH COVER'	) %>%
    map_dfr( ~tibble(!!.x :=logical()))
MissingTotalsREACH=bind_rows(columns,MissingTotalsREACH)
#Indicators=c('PROJECT',	'PROTOCOL',	'SITE_ID',	'VALXSITE',	'UID',	'DATE_COL',	'REACH',	'MACROINVERT',	'CONDUCTIVITY',	'PH',	'TEMPERATURE',	'BLM RIPARIAN',	'RIPARIAN COVER',	'CANOPY',	'BANK HEIGHT',	'BANK WIDTH',	'WET WIDTH',	'INCISION HEIGHT',	'FLOODPRONE WIDTH',	'STABILITY',	'SUBSTRATE',	'HUMAN INFLUENCE',	'LWD IN',	'SLOPE',	'POOL',	'PHOTO',	'CHEMISTRY',	'DEPTH',	'FISH COVER',	'ANGLE',	'TURBIDITY')
#MissingTotalsREACH=MissingTotalsREACH[,c(Indicators)]
#grep("^COVARIATE",colnames(MissingTotalsREACH),grep("^QC",colnames(MissingTotalsREACH)
MissingTotalsREACH=MissingTotalsREACH[order(MissingTotalsREACH$PROJECT,MissingTotalsREACH$PROTOCOL,MissingTotalsREACH$SITE_ID),]
MissingTotalsREACH=MissingTotalsREACH[,c('PROJECT',	'PROTOCOL',	'SITE_ID',	'LOC_NAME','VALXSITE',	'UID',	'DATE_COL',	'CREW_LEADER','REACH INFO',	'MACROINVERT',	'CONDUCTIVITY',	'PH',	'TEMPERATURE','VEG',	'CANOPY COVER',	'BANKFULL HEIGHT',	'BANKFULL WIDTH',	'WETTED WIDTH',	'BENCH HEIGHT',	'FLOODPRONE WIDTH',	'BANK STABILITY',	'STREAMBED PARTICLES',	'HUMAN INFLUENCE',	'LARGE WOOD',	'SLOPE',	'POOL',	'PHOTO',	'TNTP',	'TURBIDITY','THALWEG DEPTH PROFILE',	'BANK ANGLE','POOL TAIL FINES','VEGCOMPLEX','FISH COVER'	)]
MissingTotalsREACH$AvgMissingData=rowMeans(MissingTotalsREACH[,c(13:18,20:23)],na.rm='TRUE')
MissingTotalsREACHdf=as.data.frame(MissingTotalsREACH)
MissingTotalsREACH2=reshape2::melt(MissingTotalsREACHdf,id.vars=c('PROJECT',	'PROTOCOL',	'SITE_ID',	'LOC_NAME','VALXSITE',	'UID',	'DATE_COL',	'CREW_LEADER'))
MissingTotalsREACH2sub=subset(MissingTotalsREACH2,(value>0.45&variable!='AvgMissingData')|(variable=='AvgMissingData' & (VALXSITE!='PARBYWADE'&VALXSITE!='INTPARBYWADE') &value>0.05))
MissingTotalsREACH2sub$value=MissingTotalsREACH2sub$value*100
MissingTotalsREACH2sub$ERROR=ifelse(MissingTotalsREACH2sub$variable=='AvgMissingData'|(MissingTotalsREACH2sub$variable=='FLOODPRONE WIDTH'& MissingTotalsREACH2sub$VALXSITE!='PARBYWADE'),"More than 5% of data missing on average across transect data or two floodprone widths were not collected. Sample status may need to be changed to partially sampled. Please confirm what the correct sample status is and ensure crew understands special situation protocols","Indicator supposed to have been collected and more than 45% of data missing. Please verify that all data was properly entered into the app. If additional data is found, consult the NOC on next steps. If additional data is not found, the indicator associated with this data will likely not be computed.")
MissingTotalsREACH2sub$DataType=ifelse(MissingTotalsREACH2sub$variable=='AvgMissingData',"Sample Status- Partial Reach","Missing data")
contingentindicators=read.csv("Z:/buglab/Research Projects/AIM/Analysis/QC/2020/contingent_indicators.csv")
MissingTotalsREACH2sub=join(MissingTotalsREACH2sub,contingentindicators, by=c('PROJECT','variable'))
MissingTotalsREACH2sub=subset(MissingTotalsREACH2sub,Collect!='No'|is.na(Collect)==TRUE)
MissingTotalsREACH2sub=setNames(MissingTotalsREACH2sub[,c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','LOC_NAME','VALXSITE','DataType','variable','value','ERROR','PROTOCOL','UID')],c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','LOC_NAME','VALXSITE','DataType','IndicatorMethodOrField','ValueOrPercentDataMissing','ERROR','PROTOCOL','UID'))

#write.xlsx(MissingTotalsREACH,"MissingDataGeneral.xlsx")
#write.xlsx(MissingTotalsREACH2sub,"MissingDataErrors.xlsx")
#MissingTotalsTRAN=subset(MissingTotalsOUT,TRANSECT!='ReachTotal');write.csv(MissingTotalsTRAN,"MissingDataQCttran_nocom.csv")
#write.csv(MissingCounts,"MissingCounts.csv")
}

#combine reach totals
#!revision to MissingTotals4 (once complete, comment out MissingTotals4 and 6 above)
# MissingTotals4=unique(rbind(melt(MissingTotals2,id=c('SAMPLE_TYPE','TRANSECT','UID')),melt(MissingTotals3,id=c('SAMPLE_TYPE','TRANSECT','UID')),melt(MissingTotals5,id=c('SAMPLE_TYPE','TRANSECT','UID'))))
# MissingTotals4$TRANSECT=ifelse(MissingTotals4$variable=='ParamCNT','ReachTotal',MissingTotals4$TRANSECT)
# MissingTotals4=subset(MissingTotals4,variable=='MissingPCT' );MissingTotals4$RESULT=MissingTotals4$value;MissingTotals4$PARAMETER=ifelse(MissingTotals4$variable=='MissingPCT',paste("PCT_",MissingTotals4$SAMPLE_TYPE,"_QA",sep=''),MissingTotals4$variable);MissingTotals4$TRANSECT=ifelse(MissingTotals4$TRANSECT=='O',NA,MissingTotals4$TRANSECT);MissingTotals4$POINT=MissingTotals4$TRANSECT;MissingTotals4=ColCheck(MissingTotals4,c(VAR,'TRANSECT','POINT','PARAMETER','RESULT'))
#! or statement removed from above subset | (variable %in% c('CommentCNT','ParamCNT') & TRANSECT=='ReachTotal')
#!trying to add commentcnt at all levels, but was causing issues in the MissingTotalsOUT pivot and counts seem high and not sure if folks will appreciate
#!paramcnt not working with new melt method

  #print("Warning! The following sites failed missing data checks for the specified number of parameters.")
  #print(MissingTotals5)    
  #checking individual sites#View(subset(MissingCounts,subset=UID==''))#View(subset(importmaster,subset=UID==''))# View(subset(tblCOMMENTSin,subset=UID==''))



#####################################################################################################
#########                             Parameter Specific Checks                             #########
#####################################################################################################

######## GPS ##########
#get all coordinates and site metadata
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','CREW_LEADER','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR','Z_DISTANCEFROMX','TRCHLEN','REPEAT_VISIT','SLIDE_YN','MERGE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
listsites=cast(listsites,'UID~PARAMETER',value='RESULT')

# #Check all Z_DISTANCEFROMX to verify within 250 or 500m or allowable distance to be slid
# # this field is not always accurate..so the calculated "CAL_DISTFROMX" field should be used below...THis field is writen out in the coordinate_design_QC file
# SlideIssues=subset(listsites,as.numeric(Z_DISTANCEFROMX)>250)# not working because of "?"
# write.csv(SlideIssues,'SlideIssues.csv')#CHECK IN coordinate_design_QC instead

# #Check for merged sites
# Merge=subset(listsites,MERGE!='N')
# #write.csv(Merge,'Merge.csv')#CHECK IN coordinate_design_QC instead
# #look at FieldTracking or ScoutTracking spreadsheets on the google drive and fill in as needed
# 
# #Pull QC sites
# RepeatVisits=subset(listsites,REPEAT_VISIT!='N')
# #write.csv(RepeatVisits,'QC_sites.csv') #CHECK IN coordinate_design_QC instead

#Check that the straight-line distance between BR and TR does not exceed the total reach length (i.e. sinuosity <1 should never happen)
listsites$straightline=acos(sin(as.numeric(listsites$LAT_DD_BR)*3.141593/180)*sin(as.numeric(listsites$LAT_DD_TR)*3.141593/180) + cos(as.numeric(listsites$LAT_DD_BR)*3.141593/180)*cos(as.numeric(listsites$LAT_DD_TR)*3.141593/180)*cos(as.numeric(listsites$LON_DD_TR)*3.141593/180-as.numeric(listsites$LON_DD_BR)*3.141593/180)) * 6371000
listsites$SINUOSITY=as.numeric(listsites$TRCHLEN)/as.numeric(listsites$straightline)
SinuosityCheck=subset(listsites,SINUOSITY<1)
#write.csv(SinuosityCheck,'SinuosityCheck.csv') #CHECK IN coordinate_design_QC instead

#get design coordinates and attributes
#eventually read in design table from SQL but for now the table should be read in from the path below to compare original coordinates with those that were collected
#designs=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\AIM_DataManagement\\ProjectMngtSystem\\design_table2.csv')
#designs=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\design_coordinates_QC_Rinput.csv')
#designs=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\GIS_table_for_Design_Database.csv')
designs=read.xlsx('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Technology\\2019SARAHDevelopment\\Table updates\\lkp_GRTS_SiteInfo.xlsx')
postseasonmetadata=join(listsites,designs, by="SITE_ID", type="left")
#get ecoregional and stream size info for context for values
#designmetadata=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\GRTS_CodeGuidance\\MasterSample\\MasterSampleDraws\\Aquatic\\LoticMasterSample\\Attributed\\LoticMasterSampleAttributedPtsWithHybridEcoregions.csv')
#postseasonmetadata=join(postseason,designmetadata, by="MS_ID", type="left")
#eventually need to edit the read in csv above to reflect the sampled coordinates for future sample draws at the end of the season
#Z_DISTANCEFROMXthis is not always accurate or filled in..so we calculate this ourselves in "CAL_DISTFROMX"
postseasonmetadata$DistanceFromDesignCoordinatesMeters=acos(sin(as.numeric(postseasonmetadata$LAT_DD)*3.141593/180)*sin(as.numeric(postseasonmetadata$LAT)*3.141593/180) + cos(as.numeric(postseasonmetadata$LAT_DD)*3.141593/180)*cos(as.numeric(postseasonmetadata$LAT)*3.141593/180)*cos(as.numeric(postseasonmetadata$LONG)*3.141593/180-as.numeric(postseasonmetadata$LON_DD)*3.141593/180)) * 6371000
#coordinate_design_QC=postseasonmetadata[,c('PROJECT','PROTOCOL','VALXSITE','SITE_ID','LOC_NAME','DATE_COL','UID','LAT_DD','LON_DD','LAT_DD_TR','LON_DD_TR','LAT_DD_BR','LON_DD_BR','DESIGN_LAT','DESIGN_LON','Z_DISTANCEFROMX','CALC_DISTFROMX','SLIDE_YN','SINUOSITY','TRCHLEN','straightline','MERGE','REPEAT_VISIT','StreamOr_1','District','FieldOffice','ADMU_ADMIN','COUNTY','Ecoregion_spelledout','Climate_spelledout')]
postseasonmetadata$Coordinates=paste("MidReach:",postseasonmetadata$LAT_DD,",",postseasonmetadata$LON_DD,"BottomReach:",postseasonmetadata$LAT_DD_BR,",",postseasonmetadata$LON_DD_BR,"TopReach",postseasonmetadata$LAT_DD_TR,",",postseasonmetadata$LON_DD_TR)
coordinate_design_QC=postseasonmetadata[,c('PROJECT','CREW_LEADER','PROTOCOL','VALXSITE','SITE_ID','LOC_NAME','DATE_COL','UID','SINUOSITY','Coordinates','DistanceFromDesignCoordinatesMeters','TRCHLEN','LAT_DD','LON_DD','LAT_DD_TR','LON_DD_TR','LAT_DD_BR','LON_DD_BR','LAT','LONG','Z_DISTANCEFROMX','SLIDE_YN','straightline')]

#check the below file below for the following
    #1 make sure the f-transect (xsite) was placed within a the allowable sliding distance of the originial coordinates (250m,500m, or within reach)
          #Columns:Z_DISTANCEFROMX(app calc)	CALC_DISTFROMX(our calc- sometimes helpful if coordinates have been edited or Z_distancefromx="NA")	SLIDE_YN,TRCHLEN
    #2 make sure the BR and TR coordinates are correct and not duplicated.
          #This is checked using the sinuosity column. Sinuosities <1 or > ~1.6 are generally suspect.
    #1-2 plot all suspect coordinates to verify
    #3 Check any sites that have MERGE=Y or REPEAT_VISIT=Y to see if they could be merged sites or QC sites. 
          #Sites that are simply revisited for other reasons among years are not tracked using these fields at the moment
          #Merge sites should be verified using comments and design management spreadsheet
          #QC sites should also be verified with the QC cew design management spreadsheet
    #4 Check that the design database GIS table has been updated with all the needed attributes for these sites
          #columns Stream_OR - Climate_spelledout
coordinate_design_QC=coordinate_design_QC[order(coordinate_design_QC$SINUOSITY),]
coordinate_design_QCsub=subset(coordinate_design_QC,SINUOSITY<0.9|(SINUOSITY>1.6& (VALXSITE!='PARBYWADE'&VALXSITE!='INTPARBYWADE')))
coordinate_design_QCsub$ERROR=paste(coordinate_design_QCsub$Coordinates,';Plot coordinates and provide corrected coordinates if coordinates can be accurately obtained from google earth (i.e. good quality imagery exists and stream features can easily be seen). Sinuosity of 0.8 or 0.9 may indicate slight GPS errors; values less than 0.8 mean at least one coordinate is completely wrong; values >1.6 should be visually inspected on a map to ensure a fairly sinuous channel and that one or more coordinates are not wrong; anything over 3 indicates significant issues')
coordinate_design_QCsub$IndicatorMethodOrField='Sinuosity'
coordinate_design_QCsub$ValueOrPercentDataMissing=coordinate_design_QCsub$SINUOSITY
coordinate_design_QCsub2=subset(coordinate_design_QC,DistanceFromDesignCoordinatesMeters>250& DistanceFromDesignCoordinatesMeters>TRCHLEN)
coordinate_design_QCsub2$ERROR=paste(coordinate_design_QCsub2$Coordinates,'; PointID incorrect, targeted point coordinates changed, point moved more than allowable distance, or coordinate incorrect. Record the correct PointID or coordinate or confirm targeted point coordinates were changed. If point moved more than allowable distance, consult the NOC.')
coordinate_design_QCsub2$IndicatorMethodOrField='Distance from design coordinates (m)'
coordinate_design_QCsub2$ValueOrPercentDataMissing=coordinate_design_QCsub2$DistanceFromDesignCoordinatesMeters
coordinateQCErrors=rbind.fill(coordinate_design_QCsub,coordinate_design_QCsub2)
coordinateQCErrors$DataType='Coordinates'
coordinateQCErrors=coordinateQCErrors[,c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','LOC_NAME','VALXSITE','DataType','IndicatorMethodOrField','ValueOrPercentDataMissing','ERROR','UID')]
#write.csv(coordinate_design_QC,'GeneralcoordinateQC.csv',row.names = FALSE)
#write.csv(coordinateQCErrors,'coordinateQCErrors.csv',row.names = FALSE)


# #you can use the code below to plot the coordinates in R
# coordinate_design_QC$LAT_DD=as.numeric(coordinate_design_QC$LAT_DD)
# coordinate_design_QC$LON_DD=as.numeric(coordinate_design_QC$LON_DD)
# coordinate_design_QC$LON_DD_TR=as.numeric(coordinate_design_QC$LON_DD_TR)
# coordinate_design_QC$LON_DD_BR=as.numeric(coordinate_design_QC$LON_DD_BR)
# coordinate_design_QC$LAT_DD_TR=as.numeric(coordinate_design_QC$LAT_DD_TR)
# coordinate_design_QC$LAT_DD_BR=as.numeric(coordinate_design_QC$LAT_DD_BR)
# 
# # issues=c('RS-SS-11625', 'RS-SS-11561','KS-SS-14609','RA-TR-1060','WD-TR-022','KN-SS-12577')
# 
# #coordinate_design_QC=subset(coordinate_design_QC, SITE_ID %in% issues)
# 
# 
# library(sf)
# library(mapview)
# midcord= st_as_sf(coordinate_design_QC, coords = c("LON_DD", "LAT_DD"), crs = 4269,agr = "constant")#NAD 83 
# mid=mapview(midcord,map.types="Esri.WorldImagery", color="orange",label=midcord$SITE_ID)
# TRcord= st_as_sf(coordinate_design_QC, coords = c("LON_DD_TR", "LAT_DD_TR"), crs = 4269,agr = "constant")
# TR=mapview(TRcord,color="red",map.types="Esri.WorldImagery",label=TRcord$SITE_ID)
# BRcord= st_as_sf(coordinate_design_QC, coords = c("LON_DD_BR", "LAT_DD_BR"), crs = 4269,agr = "constant")
# BR=mapview(BRcord,color="yellow",map.types="Esri.WorldImagery",label=BRcord$SITE_ID)
# designcord=st_as_sf(coordinate_design_QC, coords = c("DESIGN_LON", "DESIGN_LAT"), crs = 4269,agr = "constant")
# design=mapview(designcord,map.types="Esri.WorldImagery",label=designcord$SITE_ID)
# design+mid+TR+BR
# mid+TR+BR



#After all GPS coordinates check out export coordinates for Ryan Lokteff or GIS tech to compute WestWide bug OE model and EC,TN,TP models
#write.csv(listsites,'postseason_site_coordinates.csv')


# #elevation (could start checking to see if elevation is recorded in m vs. ft but app doesn't record units. we would have to use gis or squint to determine what units were)
# #indicator not used so not high priority
# elev=tblRetrieve(Parameters=c('ELEVATION','ELEVATION_UNITS'),Years=years, Projects=projects,Protocol=protocols,SiteCodes=sitecodes,Insertion=insertion)
# pvtelev=cast(elev,'UID~SAMPLE_TYPE+PARAMETER', value='RESULT')


####### Bugs #########
#get all bug data
# Bugs=tblRetrieve(Parameters=c('JAR_NO','ACTUAL_DATE','AREA_SAMP','TRAN_NUM','SAMPLER','AREA','BUG_METHOD','VALXSITE','PROTOCOL','PROJECT'),Years=years, Projects=projects,Protocol=protocols,SiteCodes=sitecodes,Insertion=insertion)
# Bugspvt=addKEYS(cast(Bugs,'UID~SAMPLE_TYPE+PARAMETER',value='RESULT'),c('SITE_ID','DATE_COL','CREW_LEADER','PROJECT'))
# 
# 
# #check surber net and mini surber net areas because they were wrong in the app at the begining of 2016 field season
# #these were all correct in 2017 so should be able to ignore this check in 2018
# AreaCheck1=subset(Bugspvt,(as.numeric(BERW_AREA_SAMP)!=0.093 & BERW_SAMPLER=='SU'))
# AreaCheck2=subset(Bugspvt,(as.numeric(BERW_AREA_SAMP)!=0.0413 & BERW_SAMPLER=='MI'))
# AreaCheck3=subset(Bugspvt,(as.numeric(BERW_AREA_SAMP)!=0.093 & BERW_SAMPLER=='KC'))
# if(nrow(AreaCheck1)>0){write.csv(AreaCheck1,'AreaCheck1.csv')}
# if(nrow(AreaCheck2)>0){write.csv(AreaCheck2,'AreaCheck2.csv')}
# if(nrow(AreaCheck3)>0){write.csv(AreaCheck3,'AreaCheck3.csv')}
# Bugspvt=Bugspvt[c(10,16,11,12,14,1,13,2,9,5,8,3,4,7,6)]
# Bugspvt=Bugspvt[order(Bugspvt$VERIF_PROJECT,Bugspvt$VERIF_PROTOCOL,Bugspvt$SITE_ID),]
# write.csv(Bugspvt,'Bugspvt.csv')
# 
# 
# #check to make sure 8 or 11 TRAN_NUM at all sites
# #the check placed in the app has made this error vitually non-existent, if csvs blank there are no issues
# SamplingCheck1=subset(Bugspvt,(as.numeric(BERW_TRAN_NUM)<8 & BERW_BUG_METHOD=='TARGETED RIFFLE'))
# SamplingCheck2=subset(Bugspvt,(as.numeric(BERW_TRAN_NUM)<11 & BERW_BUG_METHOD=='REACH WIDE'))
# if(nrow(SamplingCheck1)>0){write.csv(SamplingCheck1,'TargetedRiffleTranNumCheck.csv')}
# if(nrow(SamplingCheck2)>0){write.csv(SamplingCheck2,'ReachWideTranNumCheck.csv')}

# #get data ready to submit via http://www.usu.edu/buglab/SampleProcessing/SampleSubmission/
# postseasonmetadata=subset(postseasonmetadata,PROTOCOL!='FAILED')
# SubMetadata=c('UID','SITE_ID','LOC_NAME','LAT_DD','LON_DD','PROJECT')###'STRATUM')# change ADMU_ADMIN back to STATE once all master sample points attributed with state
# Metadata=postseasonmetadata[SubMetadata]
# Bugspvtsub=c('UID','BERW_JAR_NO','BERW_ACTUAL_DATE','BERW_AREA','BERW_SAMPLER','BERW_BUG_METHOD')
# Bugspvt2=Bugspvt[Bugspvtsub]
# BugsSubmit=join(Bugspvt2,Metadata, by="UID")
# BugsSubmit$BERW_SAMPLER=ifelse(BugsSubmit$BERW_SAMPLER=='SU'|BugsSubmit$BERW_SAMPLER=='MI',"Surber net",ifelse(BugsSubmit$BERW_SAMPLER=='KC',"Kick net",BugsSubmit$BERW_SAMPLER))
# BugsSubmit$BERW_BUG_METHOD=ifelse(BugsSubmit$BERW_BUG_METHOD=='TARGETED RIFFLE',"Targeted Riffle",ifelse(BugsSubmit$BERW_BUG_METHOD=='REACH WIDE',"Reachwide",BugsSubmit$BERW_BUG_METHOD))
# BugsSubmit=BugsSubmit[,c(13,7,1,2,10,8,9,11,12,3,5,6,4)]#fill in desired columns
# write.csv(BugsSubmit,'BugsSubmit.csv')
# 
# #Export list of sites for watershed delineation for bug models and WQ models..all sites should have bug data so just export all sites and delineate and calc WQ stats for all sites is simplest
# SubMetadata=c('UID','MS_ID','SITE_ID','LOC_NAME','DATE_COL','STATE','LAT_DD','LON_DD')
# Delineation=postseasonmetadata[SubMetadata]
# Delineation=setNames(Delineation,c('UID','MS_ID','reachid','location','SampleDate','state','Lat','Long'))
# Delineation$SiteID2=1:nrow(Delineation)
# Delineation$rolled=0
# #write.csv(Delineation,'SitesForDelineation.csv')
                      
######## WQ checks ########
#corrected for temperature check
WQ2=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED','TEMPERATURE'), Comments='Y', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
WQ1=cast(WQ2,'UID~PARAMETER',value='RESULT')
WQind=cast(WQ2,'UID~PARAMETER',value='IND')
WQ3=addKEYS(merge(WQ1,WQind,by=c('UID'),all=T) ,c('SITE_ID','DATE_COL','CREW_LEADER','PROJECT','LOC_NAME'))
WQ3.sub=subset(WQ3,CORRECTED.x!='Y')
#if(nrow(WQ3.sub)>0){write.csv(WQ3.sub,'not_temp_corrected_conduct.csv')}
Corrected=addKEYS(tblRetrieve(Parameters=c('CORRECTED'), Comments='Y', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROJECT','SITE_ID','CREW_LEADER','LOC_NAME','DATE_COL','VALXSITE'))
WQCTemp=subset(Corrected, RESULT!='Y')
WQCTemp$ERROR='Crew recorded that conductivity was not temperature corrected; Please verify if conductivity vs. specific conductance (temperature corrected conductivity) was collected. AIM protocol states that temperature corrected conductivity should be recorded. Check why specific conductance was recorded and make sure comments are made to justify this.'

# #Chem check the hours prior to freezing
# WQtbl=tblRetrieve(Parameters=c('NTL','PTL','TN_PRED','TP_PRED','TIME_UNFROZEN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# WQpvt=addKEYS(cast(WQtbl,'UID~PARAMETER',value='RESULT'),c('SITE_ID','CREW_LEADER'))
# WQpvt$OE_TN=WQpvt$NTL-WQpvt$TN_PRED
# WQpvt$OE_TP=WQpvt$PTL-WQpvt$TP_PRED
# #graph time_unfrozen with raw values and OE values
# plot(WQpvt$TIME_UNFROZEN,WQpvt$OE_TN)
# plot(WQpvt$TIME_UNFROZEN,WQpvt$OE_TP) 
#                       
# #make condition determination and average time_unfrozen in each condition
# WQpvt$OE_TNrtg=ifelse(WQpvt$OE_TN <=52.1,'Good',ifelse(WQpvt$OE_TN >114.7, 'Poor','Fair'))
# WQpvt$OE_TPrtg=ifelse(WQpvt$OE_TP <=9.9,'Good',ifelse(WQpvt$OE_TP >21.3, 'Poor','Fair'))
# ConditionCheck1=aggregate(TIME_UNFROZEN~OE_TNrtg, data=WQpvt, FUN=mean)
# ConditionCheck2=aggregate(TIME_UNFROZEN~OE_TPrtg, data=WQpvt, FUN=mean) 
#                       
#view any typical values violations for Conductivity and PH
Conduct=addKEYS(tblRetrieve(Comments='Y',Parameters=c('CONDUCTIVITY'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROJECT','SITE_ID','CREW_LEADER','LOC_NAME','DATE_COL','VALXSITE'))
ConductQuestions=subset(Conduct,RESULT<30 | RESULT>1000)# review comments for any sites flagged here 
#postseasonmetadata_ecoregion=postseasonmetadata[,c('UID','ECO_10')]
#ConductQuestions=join(ConductQuestions,postseasonmetadata_ecoregion, by="UID",type="left")
ConductQuestions=ConductQuestions[order(ConductQuestions$RESULT),]
ConductQuestions$ERROR='Value is outside the typical value range of 30-1000 uS/cm. Did crew recalibrate and verify value? If not project lead should determine if value should be trusted or omitted. Values should not be removed unless it is justifiable, please explain your reasoning.'
#if(nrow(ConductQuestions)>0){write.csv(ConductQuestions,'ConductQuestions.csv')}

PH=addKEYS(tblRetrieve(Comments='Y',Parameters=c('PH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROJECT','SITE_ID','CREW_LEADER','LOC_NAME','DATE_COL','VALXSITE'))
PHQuestions=subset(PH,RESULT<6 | RESULT>9)# review comments for any sites flagged here
#PHQuestions=join(PHQuestions,postseasonmetadata_ecoregion, by="UID",type="left")
PHQuestions=PHQuestions[order(PHQuestions$RESULT),]
PHQuestions$ERROR='Value is outside typical range of 6-9. Did crew recalibrate and verify value? If not project lead should determine if value should be trusted or omitted.'
#if(nrow(PHQuestions)>0){write.csv(PHQuestions,'PHQuestions.csv')}


Temperature=addKEYS(tblRetrieve(Comments='Y',Parameters=c('TEMPERATURE'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROJECT','SITE_ID','CREW_LEADER','LOC_NAME','DATE_COL','VALXSITE'))
TemperatureQuestions=subset(Temperature,RESULT<2|RESULT>23)
TemperatureQuestions=TemperatureQuestions[order(TemperatureQuestions$RESULT),]
TemperatureQuestions$ERROR='Value is outside typical range of 2-23. Please confirm if value should be trusted or omitted.'


Turb=addKEYS(tblRetrieve(Parameters=c('TURBIDITY'), Comments='Y', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROJECT','SITE_ID','CREW_LEADER','LOC_NAME','DATE_COL','VALXSITE'))
TurbQuestions=subset(Turb,RESULT>60)
TurbQuestions$ERROR='Value is atypical. Please confirm if value should be trusted or omitted. Check that the crew calibrated the meter AND cleaned the vial properly.'


WQ=c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','VALXSITE','DataType','IndicatorMethodOrField','ValueOrPercentDataMissing','ERROR','UID','IND','SAMPLE_TYPE'	) %>%
  map_dfr( ~tibble(!!.x :=logical()))
WQList=c("WQCTemp","ConductQuestions","PHQuestions","TemperatureQuestions","TurbQuestions")
for (s in 1:length(WQList)) {
  if(exists(WQList[s])==TRUE){WQ=rbind.fill(list(WQ,as.data.frame(get(WQList[s]))),by="UID")}
  else {WQ=WQ}
}
#WQ=rbind.fill(WQCTemp,ConductQuestions,PHQuestions,TemperatureQuestions,TurbQuestions)
WQ$DataType='Water Quality'

WQ=setNames(WQ[,c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','LOC_NAME','VALXSITE','DataType','PARAMETER','RESULT','ERROR','COMMENT','FLAG','UID','IND','SAMPLE_TYPE')],c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','LOC_NAME','VALXSITE','DataType','IndicatorMethodOrField','ValueOrPercentDataMissing','ERROR','COMMENT','FLAG','UID','IND','SAMPLE_TYPE'))




# #compare any questionable values to ecoregional EPA data
# #should automate this process so that only values that fall outside this range get flagged ---need to join the ecoregion data to the design table; can't remember where all that site metadata from the master sample ended up
#   #could pull code from condition determinations to do that but this code will be pretty complex....for now just leave as a manual task
# read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\EPA_WQ_typical_values.csv')
# 
# #instrument check
# #pull all data for instrument if values still not resolved after looking at ecoregional values # not as pertinent this year and in the future because the same instrument will be used for any given proejct and confounded with ecoregional differences
# WQ2=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','CAL_INST_ID'), Comments='Y', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# WQ1=addKEYS(cast(WQ2,'UID~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','CREW_LEADER'))
# WQ1=subset(WQ1,CAL_INST_ID=='')# fill in data of interest here

                      
####### incision and bank height ############
#cross checks implemented in app so just check extreme values, outliers (above), and for unit issues
heights=addKEYS(tblRetrieve(Parameters=c('INCISED','BANKHT','BANKWID','WETWID'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','CREW_LEADER','PROJECT','LOC_NAME','PROTOCOL','VALXSITE','DATE_COL'))
heights$decimals=nchar(strsplit(as.character(heights$RESULT), "\\.")[[1]][2])

decimalplaces <- function(x) {
  ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
         nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
         0)
}

heights$decimals=decimalplaces(heights$RESULT)
heightssub=subset(heights,decimals>2)
heightssub$ERROR='Heights should be collected in cm and widths should be collected in meters. For data storage purposes, heights and widths displayed here are both in meters and should only have 2 decimal places. Let the NOC know if the value displayed here was recorded in the wrong units or needs rounded to the nearest 0.01 meters.'

thalweg=addKEYS(tblRetrieve(Parameters=c('DEPTH'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','CREW_LEADER','PROJECT','LOC_NAME','PROTOCOL','VALXSITE','DATE_COL'))
thalweg$decimals=decimalplaces(thalweg$RESULT)
thalwegsub=subset(thalweg,decimals>0)
thalwegsub$ERROR='Thalweg should be measured in cm without decimals. Ensure this is not a units issue and ensure the crew knows to round to the nearest cm when measuring heights and depths. Let the NOC know if data needs converted to different units or simply rounded.'



slopedecimals=addKEYS(tblRetrieve(Parameters=c('STARTHEIGHT','ENDHEIGHT'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','CREW_LEADER','PROJECT','LOC_NAME','PROTOCOL','VALXSITE','DATE_COL'))
slopedecimals$decimals=decimalplaces(slopedecimals$RESULT)
slopedecimalssub=subset(slopedecimals,decimals>0)
slopedecimalssub$ERROR='Slope should be recorded in cm without decimals. Values were either recorded to the wrong precision or wrong units. NOC will correct values but please verify crew the crew has proper equipment or the crew knows how to use the equipment properly and follows the protocol recording slope in cm.'


decimals2=rbind.fill(heightssub,thalwegsub,slopedecimalssub)
decimals2$DataType='Units, Precision, or Typo'

decimals2=setNames(decimals2[,c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','LOC_NAME','VALXSITE','DataType','PARAMETER','RESULT','ERROR','UID','IND','SAMPLE_TYPE')],c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','LOC_NAME','VALXSITE','DataType','IndicatorMethodOrField','ValueOrPercentDataMissing','ERROR','UID','IND','SAMPLE_TYPE'))




LwdCat=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%'"))$PARAMETER
Lwd=tblRetrieve(Parameters=LwdCat,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
Lwdpvt=setNames(addKEYS(cast(Lwd,'UID~ACTIVE',value='RESULT',fun=sum),c('SITE_ID','DATE_COL','LOC_NAME','CREW_LEADER','PROJECT','PROTOCOL','VALXSITE')),c('UID','ValueOrPercentDataMissing','SITE_ID','DATE_COL','LOC_NAME','CREW_LEADER','PROJECT','PROTOCOL','VALXSITE'))
Lwdpvtsub=subset(Lwdpvt,ValueOrPercentDataMissing>20)
Lwdpvtsub$ERROR='Value is abnormally high. Please verify this was not a typo.'
Lwdpvtsub$DataType='Units, Precision, or Typo'
Lwdpvtsub$IndicatorMethodOrField='Total number of pieces of wood'


angle=addKEYS(tblRetrieve(Parameters=c('ANGLE180'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','CREW_LEADER','PROJECT','LOC_NAME','PROTOCOL','VALXSITE','DATE_COL'))
anglesub=subset(angle,RESULT=='0')
anglesub$DataType='Units, Precision, or Typo'
anglesub$ERROR='Angle of 0 is not possible. This would indicate a tunnel/bridge/or culvert. Please confirm that this was a typo and should be omitted.'
anglesub=setNames(anglesub[,c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','VALXSITE','DataType','PARAMETER','RESULT','ERROR','UID','IND','SAMPLE_TYPE')],c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','VALXSITE','DataType','IndicatorMethodOrField','ValueOrPercentDataMissing','ERROR','UID','IND','SAMPLE_TYPE'))

elevation=addKEYS(tblRetrieve(Parameters=c('ELEVATION'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','CREW_LEADER','PROJECT','LOC_NAME','PROTOCOL','VALXSITE','DATE_COL'))
elevation$RESULT=as.numeric(elevation$RESULT)
elevationsub=subset(elevation, RESULT>4000)
elevationsub$DataType='Units, Precision, or Typo'
elevationsub$ERROR='Value may be in ft when it should be in meters. Please confirm the units.'

ErrorLog=c('PROJECT','CREW_LEADER','SITE_ID','DATE_COL','VALXSITE','DataType','IndicatorMethodOrField','ValueOrPercentDataMissing','ERROR','UID','IND','SAMPLE_TYPE'	) %>%
  map_dfr( ~tibble(!!.x :=logical()))
ErrorList=c("coordinateQCErrors","MissingTotalsREACH2sub","WQ","decimals2","Lwdpvtsub","anglesub","elevationsub")
for (s in 1:length(ErrorList)) {
  if(exists(ErrorList[s])==TRUE){ErrorLog=rbind.fill(list(ErrorLog,as.data.frame(get(ErrorList[s]))),by="UID")}
  else {ErrorLog=ErrorLog}
}


ErrorLog$DateAppended=Sys.Date()
ErrorLog$Resolved=NA  


write.csv(ErrorLog,'ErrorLog.csv',row.names=FALSE)
                      
#######   Interrupted Flow Site Check  #########
#Exporting UIDs that have some line of evidence that they are dry
#look at this file first and then look at width and depth issues in the next section
Interrupt=tblRetrieve(Parameters=c('VALXSITE'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
Interrupt=subset(Interrupt,RESULT=='INTWADE')
InterruptSites=data.frame("InterruptSites"=unique(Interrupt$UID))

DryTran=tblRetrieve(Parameters=c('TRANDRY'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
DryTran=subset(DryTran,RESULT=='Y' & !(TRANSECT %in% c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')))
DryTranSites=data.frame("DryTranSites"=unique(DryTran$UID))

WetWid=tblRetrieve(Parameters=c('WETWID'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
WetWid=subset(WetWid,RESULT==0 & !(TRANSECT %in% c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')))
WetWid0Sites=data.frame("WetWidSites"=unique(WetWid$UID))

DryThalweg=tblRetrieve(Parameters=c('DEPTH'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
DryThalweg=subset(DryThalweg,RESULT==0)
DryThalwegSites=data.frame("DryThalweg"=unique(DryThalweg$UID))
#write.xlsx(InterruptSites,'InterruptedFlowChecks.xlsx')
#write.xlsx(DryTranSites,'InterruptedFlowChecks.xlsx',append=TRUE)

wb = createWorkbook()
sheet = addWorksheet(wb, "All")
writeData(wb=wb,sheet=sheet,InterruptSites, startCol=1, rowNames=FALSE)
writeData(wb=wb,sheet=sheet,DryTranSites, startCol=2, rowNames=FALSE)
writeData(wb=wb,sheet=sheet,WetWid0Sites, startCol=3, rowNames=FALSE)
writeData(wb=wb,sheet=sheet,DryThalwegSites,  startCol=4, rowNames=FALSE)
saveWorkbook(wb, "InterruptedFlowChecks.xlsx",overwrite=TRUE) #check this file with those exported above and comments to see if they should be classified as interrupted or not




#######   width  and depth dry checks #########
#cross check for protocol issues related to dry sites                       
Depths=tblRetrieve(Parameters=c('DEPTH'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
Depths=subset(Depths,RESULT==0 & POINT==1)
query=NA
for (row in 1:nrow(Depths))
{ UID=Depths[row,"UID"]
TRANSECT=Depths[row,"TRANSECT"]
if(row<nrow(Depths))
{query[row]=print(paste("(UID='",UID,"' and TRANSECT='",TRANSECT,"') OR",sep=""),quote=FALSE,collapse="")}
else query[row]={print(paste("(UID='",UID,"' and TRANSECT='",TRANSECT,"')",sep=""),quote=FALSE,collapse="")}
}
query2=print(paste(query,collapse=""))  
width0=sqlQuery(wrsa1314,sprintf(paste("select * from tbltransect where parameter in('wetwid','trandry') and (%s)"), query2)) 
width0pvt=cast(width0,"UID+TRANSECT~PARAMETER",value='RESULT')
widthpvt0=subset(width0pvt,(WETWID!=0) |(TRANDRY=='N')|is.na(TRANDRY)=='TRUE')
depthwidthpvt0=merge(widthpvt0,Depths, by=c('UID','TRANSECT'))
if(nrow(depthwidthpvt0)>0){write.csv(depthwidthpvt0,'widths_should_be_0_based_on_depths.csv')} #any exported results should be checked with comments and other lines of evidence to see if the transect was dry or not  


#do the opposite query
Widths=tblRetrieve(Parameters=c('WETWID','BANKWID','TRANDRY'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)                      
pvtWidths=cast(Widths,'UID+TRANSECT~PARAMETER',value='RESULT')
Widths=subset(Widths,RESULT==0 & !(TRANSECT %in% c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')))                     
query=NA
for (row in 1:nrow(Widths))
{ UID=Widths[row,"UID"]
TRANSECT=Widths[row,"TRANSECT"]
if(row<nrow(Widths))
{query[row]=print(paste("(UID='",UID,"' and TRANSECT='",TRANSECT,"') OR",sep=""),quote=FALSE,collapse="")}
else query[row]={print(paste("(UID='",UID,"' and TRANSECT='",TRANSECT,"')",sep=""),quote=FALSE,collapse="")}
}
query2=print(paste(query,collapse=""))  
depth0=sqlQuery(wrsa1314,sprintf(paste("select * from tblpoint where parameter='Depth' and point='1' and (%s)"), query2)) 
depth0=subset(depth0,RESULT!=0)
widthdepth0=merge(depth0,pvtWidths, by=c('UID','TRANSECT'))
if(nrow(widthdepth0)>0){write.csv(widthdepth0,'depths_should_be_0_based_on_widths.csv')} #any exported results should be checked with comments and other lines of evidence to see if the transect was dry or not  

#dry transects
DryCheck=subset(pvtWidths,(WETWID!=0 & TRANDRY=='Y')|(WETWID==0 & TRANDRY=='N')|(WETWID==0 & is.na(TRANDRY)=='TRUE'))#likely needs tweaking                      
DryCheck=subset(DryCheck,!(TRANSECT %in% c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')))
if(nrow(DryCheck)>0){write.csv(DryCheck,'DryCheck.csv')}# any exported results should be checked with comments and other lines of evidence to see if the transect was dry or not  



#####  floodprone width #######
FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID','FLOOD_BFWIDTH','FLOOD_HEIGHT','FLOOD_BFHEIGHT'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
FloodWidthpvt=addKEYS(cast(FloodWidth,'UID+TRANSECT~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','CREW_LEADER','LOC_NAME','VALXSITE','DATE_COL'))
FloodWidthpvtsub=subset(FloodWidthpvt,FLOOD_WID<FLOOD_BFWIDTH)
if(nrow(FloodWidthpvtsub)>0) {write.csv(FloodWidthpvtsub,'FloodWidthLessBankfull.csv')}
                      
# ######   bank stability and cover ######
# #gut check values coming out of the app quickly, for real indicator values go to indicator script
# Bank=tblRetrieve(Parameters=c('Z_PCTEROSIONALBANKS_COVERED', 'Z_PCTEROSIONALBANKS_STABLE','Z_PCTEROSIONALBANKS_TOTAL'),Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# Bankpvt=addKEYS(cast(Bank,'UID~PARAMETER',value='RESULT'),c('PROJECT','SITE_ID','DATE_COL','VALXSITE','CREW_LEADER'))
# write.csv(Bankpvt,'Bankpvt.csv')

# ####### substrate #######
# #get sediment data
# Sediment=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# A_Sed2014=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')
# 
# # ##check % of paricles that were "other" categories (No longer need to check because don't collect "other" categories any more)
# # #2013 data
# # Sediment_OT=subset(Sediment,RESULT=="OT"| RESULT=="WD")
# # Sediment_SED=subset(Sediment,RESULT!="OT"& RESULT!="WD")
# # Sed_OT=setNames(count(Sediment_OT,"UID"),c('UID','countOT'))
# # Sed_SED=setNames(count(Sediment_SED,"UID"),c('UID','countSED'))
# # Sed_count=merge(Sed_OT,Sed_SED,by='UID',all=T)
# # Sed_count$PCT=Sed_count$countOT/(Sed_count$countOT+Sed_count$countSED)*100#if greater than 10% look to see what the OT is
# # write.csv(Sed_count,'OtherCount2013.csv')
# # #2014+ data
# # Sed2014_OT=subset(Sed2014,RESULT==0 & PARAMETER=='SIZE_NUM')
# # Sed2014_SED=subset(Sed2014, RESULT!=0 & PARAMETER=='SIZE_NUM')
# # A_Sed2014_OT=setNames(cast(Sed2014_OT,'UID~PARAMETER', value='RESULT',fun=length),c('UID','countOT'))
# # A_Sed2014_SED=setNames(cast(Sed2014_SED,'UID~PARAMETER', value='RESULT',fun=length),c('UID','countSED'))
# # A_Sed2014_count=merge(A_Sed2014_OT,A_Sed2014_SED,by='UID',all=T)
# # A_Sed2014_count$PCT=A_Sed2014_count$countOT/(A_Sed2014_count$countOT+A_Sed2014_count$countSED)*100#if greater than 10% look to see what the OT is
# # write.csv(A_Sed2014_count,'OtherCount2014.csv')
# 
# #check sample sizes and that bed and bank protocols were followed                       
# C_Sed2014=A_Sed2014[!A_Sed2014$LOC== "BANK", ]
# Nbed_Sed2014pvt=aggregate(.~UID, data=C_Sed2014, length)#number of bed pebbles
# Nbed_Sed2014pvt=setNames(Nbed_Sed2014pvt[,c(1,5)],c("UID","nbed"))
# Nall_Sed2014pvt=setNames(cast(Sed2014,'UID~PARAMETER',value='RESULT',fun=length),c("UID","nLOC","nall"))#number of all collected pebbles
# Nall_Sed2014pvt=Nall_Sed2014pvt[,c(1,3)]
# sample_size=addKEYS(join(Nbed_Sed2014pvt,Nall_Sed2014pvt, by="UID"),c('SITE_ID','PROJECT','CREW_LEADER'))                      
# sample_size=sample_size[order(sample_size$nall),]
# write.csv(sample_size,'sed_sample_size.csv')

# ###### Angle  ########                      
# #outlier checks (above) and check for missing SLANT to check if angle was being calculated in app properly
# #only issues are exported, no issues were found in 2017 with this so can likely ignore for the most part this year
# Angle=tblRetrieve(Parameters=c('ANGLE180','SLANT'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
# pvtAngle=addKEYS(cast(Angle,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'), c('SITE_ID','PROJECT','CREW_LEADER'))                     
# AngleCheck1=subset(pvtAngle, is.na(pvtAngle$SLANT)=='TRUE')                      
# AngleCheck2=subset(pvtAngle, SLANT=='OB' & as.numeric(ANGLE180)<90)  
# if(nrow(AngleCheck1)>0) {write.csv(AngleCheck1,'AngleCheckSlant.csv')}# slant being filled out properly for all angles
# if(nrow(AngleCheck2)>0) {write.csv(AngleCheck2,'AngleCheckSubtraction.csv')}# subtraction occuring in app properly for all obtuse angles
#                  
# #######  Riparian Veg  ########
# #overstory >100% check
# riparian1=tblRetrieve(Parameters=c('CANBTRE','CANSTRE'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
# riparian1PVT=cast(riparian1,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# riparian1PVT_IND=cast(riparian1,'UID+TRANSECT+POINT~PARAMETER',value='IND')
# riparian1pvt=merge(riparian1PVT,riparian1PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
# riparian1PVTsub=subset(riparian1pvt,(CANBTRE.x==4 & CANSTRE.x>2) | (CANSTRE.x==4 & CANBTRE.x>2))
# if(nrow(riparian1PVTsub)>0){write.csv(riparian1PVTsub,'rip1.csv')}
# 
# #middlestory >100% check
# riparian2=tblRetrieve(Parameters=c('UNDNWDY','UNDWDY'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
# riparian2PVT=cast(riparian2,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# riparian2PVT_IND=cast(riparian2,'UID+TRANSECT+POINT~PARAMETER',value='IND')
# riparian2pvt=merge(riparian2PVT,riparian2PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
# riparian2PVTsub=subset(riparian2pvt,(UNDNWDY.x==4 & UNDWDY.x>2) | (UNDWDY.x==4 & UNDNWDY.x>2))
# if(nrow(riparian2PVTsub)>0){write.csv(riparian2PVTsub,'rip2.csv')}
# 
# #understory >100% check
# riparian3=tblRetrieve(Parameters=c('GCNWDY','GCWDY','BARE'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
# riparian3PVT=cast(riparian3,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# riparian3PVT_IND=cast(riparian3,'UID+TRANSECT+POINT~PARAMETER',value='IND')
# riparian3pvt=merge(riparian3PVT,riparian3PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
# riparian3PVTsub=subset(riparian3pvt,(GCNWDY.x==4 & GCWDY.x>2) | (GCWDY.x==4 & GCNWDY.x>2) | (GCNWDY.x==4 & BARE.x>2) | (BARE.x==4 & GCNWDY.x>2) | (BARE.x==4 & GCWDY.x>2) | (GCWDY.x==4 & BARE.x>2))
# if(nrow(riparian3PVTsub)>0) {write.csv(riparian3PVTsub,'rip3.csv')}
# 
# #veg type missing check
# #canopy
# riparian4=tblRetrieve(Parameters=c('CANVEG','CANBTRE','CANSTRE'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
# riparian4PVT=cast(riparian4,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# riparian4pvtsub=subset(riparian4PVT,CANVEG=='N' & CANBTRE>0 & CANSTRE>0)
# if(nrow(riparian4pvtsub)>0) {write.csv(riparian4pvtsub,'rip4.csv')}
# 
# riparian4PVT_IND=cast(riparian4,'UID+TRANSECT+POINT~PARAMETER',value='IND')
# riparian4pvt=merge(riparian4PVT,riparian4PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
# riparian4pvtsub2=subset(riparian4pvt,CANVEG.x!='N' & CANBTRE.x==0 & CANSTRE.x==0)
# if(nrow(riparian4pvtsub2)>0){write.csv(riparian4pvtsub2,'rip42.csv')}
# 
# #middle
# riparian5=tblRetrieve(Parameters=c('UNDERVEG','UNDNWDY','UNDWDY'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
# riparian5PVT=cast(riparian5,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# riparian5pvtsub=subset(riparian5PVT,UNDERVEG=='N' & UNDNWDY>0 & UNDWDY>0)
# if(nrow(riparian5pvtsub)>0){write.csv(riparian5pvtsub,'rip5.csv') }                     
# 
# riparian5=tblRetrieve(Parameters=c('UNDERVEG','UNDNWDY','UNDWDY'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion)
# riparian5PVT=cast(riparian5,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# riparian5pvtsub2=subset(riparian5PVT,UNDERVEG!='N' & UNDNWDY==0 & UNDWDY==0)
# if(nrow(riparian5pvtsub2)>0){write.csv(riparian5pvtsub2,'rip52.csv')}


##### Non-Natives ######



# # if species was in the state list, met minium data requirements, and are currently blank should have value of 0 if not make cells NA
# statelists=read.csv("Z:\\buglab\\Research Projects\\AIM\\Protocols\\NonNativeVeg\\Comprehensive Aquatic AIM Nonnative Riparian Plant Species List_JC.csv")
# state=unique(statelists$STATE)
# for (i in 1:length(state)){
#  statelists2=statelists[statelists$STATE==state[i],]
#  assign(paste0(state[i]),unique(statelists2$ScientificName))
# }
# 
# sub=pvtnonnative
# 
# sub2=sub[,CO]
# 
# for (s in 1:length(state)){
#   sub=pvtnonnative[which(pvtnonnative$STATE==paste0(state[1])),]
#   cols=colnames(sub)
#       for (i in 1:ncol(cols)){
#           for r in 1:nrow(sub)){
#               if(cols[64] %in% CO & is.na(sub[3,64])==TRUE) {sub[3,64]==0}
#               else {sub[3,64]==sub[3,64] }
#               if(cols[i] %in% CO & sub[2,1]=>0) {sub[2,1]==sub[2,1]}
#               else {sub[2,1]==NA } 
#           }
#       }
# }
# 
# for (s in 1:length(state)){
#   sub=pvtnonnative[which(pvtnonnative$STATE==paste0(state[1])),]
#   cols=colnames(sub)
#   for (i in 1:ncol(cols)){
#     for r in 1:nrow(sub)){
#       if(cols[s] %in% cols[i] & sub[r,i]==NA) {sub[r,i]==0}
#       else {sub[r,i]==sub[r,i] }
#       if(cols[i] %in% state[s] & sub[r,i]=>0) {sub[r,i]==sub[r,i]}
#       else {sub[r,i]==NA } 
#     }
#   }
# }
# 
                      
###### Thalweg   #########

#Custom thalweg missing data because number of station is dynamic
tbl=tblRetrieve(Parameters=c('DEPTH'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
tbl.2=tblRetrieve(Parameters=c('NUM_THALWEG'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
tbl3=cast(tbl.2,'UID~PARAMETER',value='RESULT',mean)
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT',length),c('SITE_ID','CREW_LEADER','PROJECT'))# count is default
thalweg.missing=merge(tbl.PVT,tbl3, by='UID')
thalweg.missing$pctcomplete=thalweg.missing$DEPTH/(thalweg.missing$NUM_THALWEG*10)*100
write.csv(thalweg.missing,'thalweg.missing.csv')
# 
# #2014-2015
# thalweg.missing2014=sqlQuery(wrsa1314,sprintf("select Station.UID, depth.transect,StationCNT,DepthCNT from 
#                                               (select distinct UID,
#                                                CASE 
#                                                WHEN parameter='sub_5_7' and RESULT='5' THEN cast(RESULT*2 as numeric)
#                                                WHEN parameter='sub_5_7' and RESULT='7' THEN cast(RESULT*2+1 as numeric)
#                                                WHEN parameter='sub_5_7' and RESULT='14' THEN cast (RESULT*2+2 as numeric)
#                                                ELSE 'ISSUE'
#                                                END as StationCNT from tbltransect where parameter='SUB_5_7' and ACTIVE='true') as station
#                                               join
#                                               (select UID, transect, count(point) as DepthCNT from tblpoint where parameter='DEPTH' and POINT not in('CT','LC','LF','RC','RT') and ACTIVE='true' group by UID, transect) as depth
#                                               on station.uid=depth.uid
#                                               --where StationCNT > DepthCNT 
#                                               order by Station.UID, depth.transect"))
# thalweg.missing2014$pctcomplete=thalweg.missing2014$DepthCNT/thalweg.missing2014$StationCNT*100
# thalweg.missing2014_2=aggregate(pctcomplete~UID, data=thalweg.missing2014,mean)
# 
# #2013
# thalweg.missing2013=sqlQuery(wrsa1314,sprintf("select Station.UID, StationDUPLICATES,StationCNT,DepthCNT from 
#                                               (select distinct UID,
#                                                CASE 
#                                                WHEN RESULT='5' THEN cast(RESULT*20 as numeric)
#                                                WHEN RESULT='7' THEN cast(RESULT*20+10 as numeric)
#                                                WHEN RESULT='14' THEN cast (RESULT*20+20 as numeric)
#                                                ELSE 'ISSUE'
#                                                END as StationCNT from tblpoint where parameter='SUB_5_7' and ACTIVE='true') as station
#                                               join
#                                               (select UID,count(result) as StationDUPLICATES from (select distinct UID, result from tblpoint where parameter='SUB_5_7' and ACTIVE='true') as stcnt group by UID) as stationcount
#                                               on station.uid=stationcount.uid
#                                               join 
#                                               (select UID, count(point) as DepthCNT from tblpoint where parameter='DEPTH' and POINT not in('CT','LC','LF','RC','RT') and ACTIVE='true' group by UID) as depth
#                                               on station.uid=depth.uid
#                                               --where StationCNT > DepthCNT or stationDUPLICATES>1
#                                               order by Station.UID"))
# thalweg.missing2013$pctcomplete=thalweg.missing2013$DepthCNT/thalweg.missing2013$StationCNT*100
# thalweg.missing2013_2=aggregate(pctcomplete~UID, data=thalweg.missing2013,mean)                             

# #Increment cross-validation checks
# incrementcheck=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT','RCHWIDTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# incrsub=subset(incrementcheck,UID!='1500BC4F-C9B3-4FFC-9639-B5054B0FCD62')#UID:10383  IND 4849393 needs to be deactivated for this to work
# incrementPVT=cast(incrsub,'UID~PARAMETER',value='RESULT')
# incrementsub=subset(incrementPVT,TRCHLEN/0.01!=INCREMENT)#also check manually in excel and also checked to make sure that RCHWIDTH*40=TRCHLEN and for RCHWIDTH<2.5 INCREMENT=1 and for RCHWIDTH>2.5<4 INCREMENT=1.5
# #write.csv(incrementPVT,'incrementPVT.csv')
# #weridinc=tblRetrieve(Parameters=c('INCREMENT'),UIDS='11852')
                      
# #thalweg checks####couldn't get this to work so did cell referencing in excel to interpolate between values with 1 missing value inbetween
# #this interpolation was not done in 2017 because it is too much work for no reason.....
# thalweg<-tblRetrieve(Parameters='DEPTH',Projects=projects,Years=years, Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# thalweg_depth<-subset(thalweg,SAMPLE_TYPE!='CROSSSECW')
# thalweg_depth_pvt<-cast(thalweg_depth,'UID+TRANSECT~POINT', value='RESULT')
# thalweg_depth_pvt_order<-thalweg_depth_pvt[with(thalweg_depth_pvt, order(1,29))]
# thaleg_depth_NA<-thalweg_depth_pvt [is.na(thalweg_depth_pvt$'1')==TRUE,c(1:2,4)]
# 
# #check too deep variable for any depths that need trig
# #only one such instance in 2017....not sure why so few and if this is a crew or app issue on not an issue at all
# #true depth=sin(angle recorded in comments)*recorded height, note that sin function in R requires input to be in radians rather than degrees so you ether need to convert or use a different calculator 
# toodeep=tblRetrieve(Comments='Y',Parameters=c('TOODEEP','DEPTH_ANGLE','DEPTH'),Projects=projects,Years=years, Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# pvttoodeep=cast(toodeep,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# TooDeepCheck=subset(pvttoodeep,TOODEEP=='Y')
# TooDeepCheck=join(TooDeepCheck,toodeep, by="UID",type="left")# join to get comments back in but this creates a mess. probably better to reach out previous line and then look for comments individually
# if(nrow(TooDeepCheck)>0){write.csv(TooDeepCheck,'TooDeepCheck.csv')}

# #thalweg depth/ width EPA check
# #wading sites #skipped this in 2017 but should do in 2018
# depthcheck=tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# depthcheck.sub=subset(depthcheck,SAMPLE_TYPE!='CROSSSECW')
# pvtdepthcheck=cast(depthcheck.sub,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# pvtdepthcheck.sub=subset(pvtdepthcheck,POINT=='1')#1 for 2016 and 0 for pre-2016
# width=tblRetrieve(Parameters=c('WETWID'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# check=join_all(list(pvtdepthcheck.sub,width),by=c('UID','TRANSECT'))
# check$RESULT=as.numeric(check$RESULT)
# check$DEPTH=as.numeric(check$DEPTH)
# odd_ratio=addKEYS(subset(check,RESULT/(DEPTH/100)>50|RESULT/(DEPTH/100)<1),c('SITE_ID','CREW_LEADER','PROJECT'))
# if(nrow(odd_ratio)>0){write.csv(odd_ratio,'odd_ratio.csv')}
# # #boating sites
# # odd_ratio=subset(check,RESULT/(DEPTH)>50|RESULT/(DEPTH)<1)
# # write.csv(odd_ratioBoat,'odd_ratio.csv')
#                  
#                  
#########  slope   ################                    
Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','TRCHLEN','PARTIAL_RCHLEN','POOLRCHLEN','SLOPE_COLLECT','PCT_GRADE','VALXSITE','Z_SLOPEPASSQA'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)                 
pvtSlope=addKEYS(cast(Slope,'UID~PARAMETER',value='RESULT'), c('SITE_ID','CREW_LEADER','PROJECT','DATE_COL','VALXSITE'))                
# sites with pct_grade==0 likely had some app quirk and slope reach length or avg slope didn't get calculated for some reason
SlopeCheck_typicalvalues=subset(pvtSlope,as.numeric(PCT_GRADE)>14|as.numeric(PCT_GRADE)<1)
SlopeCheck_partialno=subset(pvtSlope,SLOPE_COLLECT=='PARTIAL'|SLOPE_COLLECT=='NO SLOPE')
Pass=tblRetrieve(Parameters=c('ENDTRAN'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)                 
SlopeCheck_more2passes=subset(Pass,TRANSECT>2)# if more than 2 passes need to manually check which ones to average
#NOT10PER=subset(pvtSlope,Z_SLOPEPASSQA=='N') #not particularly helpful because gets flagged as "N" if they do more than 2 passes and doesn't get flagged back as "Y" if 2 of those are within 10%
# for any sites that failed one of the above checks, see individual passes below
IndividualSlope=tblRetrieve(Parameters=c('SLOPE','STARTHEIGHT','ENDHEIGHT'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
pvtIndividualSlopeSum=addKEYS(cast(IndividualSlope,'UID+TRANSECT~PARAMETER',value='RESULT', fun=sum),c('SITE_ID','CREW_LEADER','PROJECT','DATE_COL','VALXSITE'))#note Transect=Pass
pvtIndividualSlopeSum=pvtIndividualSlopeSum[,c(1:3,5,4,6,7)]
pvtIndividualSlopeRaw=addKEYS(cast(IndividualSlope,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID','CREW_LEADER','PROJECT','DATE_COL','VALXSITE'))#note Transect=Pass
pvtIndividualSlopeRaw=pvtIndividualSlopeRaw[,c(1:4,6,5,7:10)]
#always double check sites with 3 passes get averaged properly in app
write.csv(pvtSlope,'pctGrade.csv')
#write.csv(SlopeCheck_more2passes,'SlopeCheck_more2passes.csv')#I prefer just to check this in the raw slope file rather than getting this seperate export
#write.csv(SlopeCheck_typicalvalues,'SlopeCheck_typicalvalues.csv')#I prefer just to check this in the pctGrade file rather than getting this seperate export
#write.csv(SlopeCheck_partialno,'SlopeCheck_partialno.csv')#I prefer just to check this in the pctGrade file rather than getting this seperate export
#write.csv(NOT10PER,'NOT10PER.csv')# not particularly usefull and I prefer just to check this in the sum slope file rather than getting this seperate export
write.csv(pvtIndividualSlopeSum,'pvtIndividualSlopeSum.csv')
write.csv(pvtIndividualSlopeRaw,'pvtIndividualSlopeRaw.csv')

#########  pools   ################                                           
#getting all pool data for a specfic set of sites---not collecting one of the parameters below anymore
pools<-tblRetrieve(Parameters=c('HABTYPE','PTAILDEP','MAXDEPTH','LENGTH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion, Comments='Y')
pvtpools=addKEYS(cast(pools,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID','PROJECT','CREW_LEADER'))
write.csv(pvtpools,'pvtpools.csv')#sort and look for min and max and 0 data or unit issues

# flow and collected checks
PoolCollect<-tblRetrieve(Parameters=c('POOL_COLLECT','VALXSITE','POOLRCHLEN','TRCHLEN','SLOPE_COLLECT','SLPRCHLEN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion, Comments='Y')
pvtPoolCollect=addKEYS(cast(PoolCollect,'UID~PARAMETER',value='RESULT'),c('SITE_ID','CREW_LEADER','PROJECT','DATE_COL'))
pvtPoolCollect=pvtPoolCollect[order(pvtPoolCollect$VALXSITE,pvtPoolCollect$POOL_COLLECT),]
#write.csv(pvtPoolCollect,paste0('poolcollect',Sys.Date(),'.csv'))
write.csv(pvtPoolCollect,'poolcollect.csv') 
#pvtPoolCheck=subset(pvtPoolCollect,VALXSITE=='INTWADE'|POOL_COLLECT=='NF'|POOL_COLLECT=='NC')
#write.csv(pvtPoolCheck,'poolNCNFCheck.csv')#check all exported sites to make sure interrupted and partial site protocols properly followed

#check sum of lengths not > than total reach length  
pool_length<-tblRetrieve(Parameters=c('LENGTH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion, Comments='Y')
pool_length$RESULT=as.numeric(pool_length$RESULT)
pvtpools1=cast(pool_length,'UID~PARAMETER',value='RESULT',fun=sum) 
reach_length=tblRetrieve(Parameters=c('POOLRCHLEN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
pvtpools2=cast(reach_length,'UID~PARAMETER',value='RESULT') 
poolsmerge<-merge(pvtpools1,pvtpools2,by=c('UID'),all=T)
pool_great_100<-subset(poolsmerge,LENGTH>POOLRCHLEN)
if(nrow(pool_great_100)>0){write.csv(pool_great_100,'pool_great_100.csv')}#exports any sites that have over 100% pools

#pool tail fines
PoolFines=tblRetrieve(Parameters=c('POOLFINES2','POOLFINES6','POOLNOMEAS','POOLFINES6_512'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
if(nrow(PoolFines)>0){
pvtPoolFines=addKEYS(cast(PoolFines,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID','PROJECT','CREW_LEADER','DATE_COL','VALXSITE'))#need to pivot to create the pctPoolFInes variable
pvtPoolFines$PctPoolFines2_CHECK=pvtPoolFines$POOLFINES2/(50-pvtPoolFines$POOLNOMEAS)*100
pvtPoolFines$PctPoolFines6_CHECK=pvtPoolFines$POOLFINES6/(50-pvtPoolFines$POOLNOMEAS)*100
write.csv(pvtPoolFines,'pvtPoolFines.csv')
aggpvt1PoolFines=aggregate(PctPoolFines2_CHECK~UID+TRANSECT,data=pvtPoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt2PoolFines=aggregate(PctPoolFines6_CHECK~UID+TRANSECT,data=pvtPoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt3PoolFines=aggregate(PctPoolFines2_CHECK~UID,data=aggpvt1PoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt4PoolFines=aggregate(PctPoolFines6_CHECK~UID,data=aggpvt2PoolFines, FUN='mean')#average pool fines at a pool first # note these exclude NAs
aggpvt5PoolFines=setNames(aggregate(PctPoolFines2_CHECK~UID,data=aggpvt1PoolFines, FUN='sd'),c("UID","PctPoolFines2SD"))#average pool fines at a pool first # note these exclude NAs
aggpvt6PoolFines=setNames(aggregate(PctPoolFines6_CHECK~UID,data=aggpvt2PoolFines, FUN='sd'),c("UID","PctPoolFines6SD"))#average pool fines at a pool first # note these exclude NAs
FinalpvtPoolFines=addKEYS(join_all(list(aggpvt3PoolFines,aggpvt4PoolFines,aggpvt5PoolFines,aggpvt6PoolFines),by=c('UID')),c('SITE_ID','PROJECT','CREW_LEADER','DATE_COL','VALXSITE'))
#FinalpvtPoolFines=join_all(list(aggpvt3PoolFines,aggpvt5PoolFines),by=c('UID'))
FinalpvtPoolFines$PctPoolFines2_CHECK=round(FinalpvtPoolFines$PctPoolFines2_CHECK,digits=0)
FinalpvtPoolFines$PctPoolFines6_CHECK=round(FinalpvtPoolFines$PctPoolFines6_CHECK,digits=0)
write.csv(FinalpvtPoolFines,'FinalpvtPoolFines.csv')
#calc CV for PIBO QC check
FinalpvtPoolFines$PctPoolFines2CV=FinalpvtPoolFines$PctPoolFines2SD/FinalpvtPoolFines$PctPoolFines2_CHECK#sites with CV >1.414 should be QCed 
FinalpvtPoolFines$PctPoolFines6CV=FinalpvtPoolFines$PctPoolFines6SD/FinalpvtPoolFines$PctPoolFines6_CHECK#sites with CV >1.414 should be QCed 
subQC=subset(FinalpvtPoolFines,FinalpvtPoolFines$PctPoolFines6CV>=1.41|FinalpvtPoolFines$PctPoolFines2CV>=1.41)#sites with CV >1.414 should be QCed 
#write.csv(subQC,'pooltailfines_highvariance.csv')
}


# ########  side channels  ############
# #check how many side channels crews are at sites to keep an eye on workload of sampling side channels
# side=tblRetrieve(Parameters=c('SIDCHN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# side=subset(side,RESULT=='Y')
# sidepvt=addKEYS(cast(side,'UID~PARAMETER',value='RESULT',length),c('SITE_ID','CREW_LEADER','PROJECT'))
# write.csv(sidepvt,'SideChannels.csv')



#########  photos  ###################
#use to check any questionable values such as bankfull widths and heights
#this hasn't been used to date. photo comments should be reviewed at some point
Photo=tblRetrieve(Parameters=c('PHOTO_ID','PHOTO_DESCRIP','PHOTO_TYPE','ROD','DIRECTION','COMMENT'),Projects=projects,Years=years,SiteCodes=sitecodes,Insertion=insertion)
pvtPhotos=addKEYS(cast(Photo,'UID+POINT~PARAMETER',value='RESULT'),c('PROJECT','CREW_LEADER','DATE_COL','VALXSITE','SITE_ID'))
write.csv(pvtPhotos,'photos.csv')


#######################################################################
########               summary stats                       ############
#######################################################################

#This outputs all numeric fields in our database. As such, it is repetitive with many of the above checks. Most fields are better check above. 
#However, there are a few indicators that don't get checked above or additional checks can be done in this file
#ANGLE180--check for typos or angles that are 0 or 180, 180 is less concerning
#BANKHT--check for unit issues
#BANKWID--check for unit issues
#BARWID--check for typos or unit issues
#DENSIOM--check for legal values
#DEPTH--check for unit issues or typos
#LWD--dry and wet pieces, check for typos
#ELEVATION--check for unit issues ft to m
#FLOODProne parameters-- unit issues or more than <25 cm between 2 heights
#INCISED--unit issues
#SIZENUM-- legal values
#TEMPERATURE--legal values
#WETWID--units

#Users can skip other parameters or make edits and then run this summary as a way to double check that edits have been made properly or caught all issues.

#retrieve all possible tables by protocol groups and pivot
#for exploratory purposes to review data and determine expected values, not intended to replace modular SQL solutions for multiple tools
#UnionTBLnum_pvtQUANTmean_Site is pretty hard to look at. I prefer to look at UnionTBLnum_pvtSUMMARYn_Site sort by parameter and then sort values to look at min and maxes for each parameter across all sites
#uses tblMETADATA to pull what parameters are in these files so tblMETADATA must be up to date for this to pull newer parameters

# UnionTBL=tblRetrieve(Table='', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# 
# tblCOL=c('UID', 'SAMPLE_TYPE','PARAMETER','RESULT','TRANSECT','POINT')
# pvtCOL='UID %s ~ SAMPLE_TYPE + PARAMETER';pvtCOLdefault=sprintf(pvtCOL,'');pvtCOL2=sprintf(pvtCOL,'+ TRANSECT + POINT')
# AggLevel='Site'#options = Site, All
# dbPARAM=sqlQuery(wrsa1314,"Select SAMPLE_TYPE, PARAMETER, LABEL,VAR_TYPE from tblMETADATA where ACTIVE='TRUE'")#parameter names (SWJ to do: iterate over Sample_Type groups to generate pivots)
# params_N=subset(dbPARAM, subset=VAR_TYPE=='NUMERIC')
# tblNAME='UnionTBLnum'
# if(min(c('SAMPLE_TYPE',tblCOL) %in% colnames(UnionTBL))==1){#if minimum needed columns are present, proceed, otherwise assume it is a pivoted or otherwise human readable table
#   #summarized quantitative data (average values per pivot cell which is per site per parameter)
#   tblNUM=subset(UnionTBL,subset=PARAMETER %in% params_N$PARAMETER )
#   if(nrow(tblNUM)>1){#only assign pivot to variable if not empty and only dive into subsequent if not empty
#     if(AggLevel=='Site'){pvtCOL4='UID + SAMPLE_TYPE + PARAMETER ~.'; pvtCOL5='RESULT~UID + SAMPLE_TYPE + PARAMETER'; colUID='tblPVTnSUM2$UID';nameUID=c('UID','SAMPLE_TYPE','PARAMETER','Quant1','Quant2')} else if (AggLevel=='All') {pvtCOL4='SAMPLE_TYPE + PARAMETER ~.';pvtCOL5='RESULT~SAMPLE_TYPE + PARAMETER';colUID='';nameUID=c('SAMPLE_TYPE','PARAMETER','Quant1','Quant2')}
#     tblNUM$RESULT=as.numeric(tblNUM$RESULT)
#     tblNUM=subset(tblNUM,subset= is.na(RESULT)==FALSE)#apparently not removing NAs during pivot aggregation, so done manually because causing errors - have to do after conversion to number
#     tblPVTn=cast(tblNUM, eval(parse(text=pvtCOLdefault)),value='RESULT',fun.aggregate='mean')#pivot reach average by site
#     tblPVTnSUM1=cast(tblNUM, eval(parse(text=pvtCOL4)),value='RESULT',fun.aggregate=c(length,mean,median,min,max,sd),fill='NA') # pivot summary stats by all sites combined or individual sites
#     tblPVTnSUM2=aggregate(eval(parse(text=pvtCOL5)),data=tblNUM,FUN='quantile',probs=c(0.25,0.75),names=FALSE)
#     tblPVTnSUM2=data.frame(cbind(eval(parse(text=colUID)),tblPVTnSUM2$SAMPLE_TYPE,tblPVTnSUM2$PARAMETER,tblPVTnSUM2$RESULT[,1],tblPVTnSUM2$RESULT[,2]));colnames(tblPVTnSUM2)=nameUID
#     tblPVTnSUM=merge(tblPVTnSUM1,tblPVTnSUM2,by=setdiff(nameUID,c('Quant1','Quant2')))
#     #need to do this by UID for WRSA13 QA duplicate comparison
#     assign(sprintf('%s_pvtQUANTmean_%s',tblNAME,AggLevel),tblPVTn)
#     assign(sprintf('%s_pvtSUMMARYn_%s',tblNAME,AggLevel),tblPVTnSUM)
#   }
# }
# 
# # #export results
# # QUANTtbls=c(grep('pvtQUANTmean',ls(),value=T),setdiff(grep('pvtSUMMARYn',ls(),value=T),"pvtSUMMARYn"))
# # for (t in 1:length(QUANTtbls)){
# #   write.csv(eval(parse(text=QUANTtbls[t])),sprintf('%s.csv',QUANTtbls[t]))#could merge-pvtQUANTmean_, but I like them grouped by categories
# # }
# 
# UnionTBLnum_pvtSUMMARYn_Site=subset(UnionTBLnum_pvtSUMMARYn_Site,PARAMETER %in% c('ANGLE180', 'BANKHT','BANKWID','BARWID','DENSIOM','DEPTH','LWD','ELEVATION','INCISED','SIZENUM','TEMPERATURE','WETWID','FLOOD_MAX_DEPTH','FLOOD_WID','FLOOD_BF_HEIGHT','FLOOD_BFWIDTH','FLOOD_HEIGHT'))
# UnionTBLnum_pvtSUMMARYn_Site=addKEYS(UnionTBLnum_pvtSUMMARYn_Site,c('PROJECT','SITE_ID','CREW_LEADER'))
# 
# write.csv(UnionTBLnum_pvtSUMMARYn_Site,'UnionTBLnum_pvtSUMMARYn_Site.csv')
# 



############################################################################################################################
# #Final pivot checks on all indicators to make sure no duplicate data exists from data edits
# #run summary data and sort again to make sure no wacky values
# #pay close attention to sample sizes when running indicators....should probably eventually run missing data checks this way rather than the knarly script above
# 
# #siteinfo
# listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR','DEWATER','BEAVER_FLOW_MOD','BEAVER_SIGN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# listsites=setNames(cast(listsites,'UID~PARAMETER',value='RESULT'),c("UID","BEAVER_FLOW_MOD_CHECK","BEAVER_SIGN_CHECK","DATE_COL_CHECK","WATER_WITHDRAWAL_CHECK","LAT_DD_CHECK","LAT_DD_BR_CHECK","LAT_DD_TR_CHECK","LOC_NAME_CHECK","LON_DD_CHECK","LON_DD_BR_CHECK","LON_DD_TR_CHECK","PROJECT_CHECK","PROTOCOL_CHECK","SITE_ID_CHECK",'VALXSITE_CHECK'))
# listsites=listsites[,c(15,9,1,4,6,10,13,14,16,7,11,8,12,2,3,5)]
# TRCHLEN=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)#not using TRCHLEN
# TRCHLEN1=cast(TRCHLEN,'UID~PARAMETER',value='RESULT')
# 
# #canopy cover
# densiom=tblRetrieve(Parameters='DENSIOM',Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# unique(densiom$RESULT)
# densiompvt=cast(densiom,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
# unique(densiom$POINT)
# 
# #riparian
# RipALL=tblRetrieve(Parameters=c("BARE","CANBTRE","CANSTRE","CANVEG","GCNWDY","GCWDY","UNDERVEG","UNDNWDY","UNDWDY"),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# RipAllpvt=cast(RipALL,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
# unique(RipAllpvt$TRANSECT)
# unique(RipALL$RESULT)
# 
# #BLM riparian cover and frequency
# RipBLM=tblRetrieve(Parameters=c('CANRIPW','UNRIPW','GCRIP','INVASW', 'NATIVW','INVASH','NATIVH','SEGRUSH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# pvtRipBLM=cast(RipBLM,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# 
# #WQ
# WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','NTL','PTL','TURBIDITY','TEMPERATURE','EC_PRED','TN_PRED','TP_PRED'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')
# 
# #pools
# pools<-addKEYS(tblRetrieve(Parameters=c('HABTYPE','PTAILDEP','MAXDEPTH','LENGTH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion, Comments='Y'),c('SITE_ID'))
# pvtpools=cast(pools,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# PoolCollect<-tblRetrieve(Parameters=c('POOL_COLLECT','VALXSITE','POOLRCHLEN','TRCHLEN','SLOPE_COLLECT','SLPRCHLEN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion, Comments='Y')
# pvtPoolCollect=addKEYS(cast(PoolCollect,'UID~PARAMETER',value='RESULT'),c('SITE_ID','CREW_LEADER'))
# 
# #LWD
# LwdCatWet=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'W%'"))$PARAMETER
# LwdCatDry=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'D%'"))$PARAMETER
# LwdWet=addKEYS(tblRetrieve(Parameters=LwdCatWet,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID','DATE_COL'))
# LwdDry=tblRetrieve(Parameters=LwdCatDry,Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# pvtLwdWet=cast(LwdWet, 'UID~TRANSECT+PARAMETER',value='RESULT')
# pvtLwdDry=cast(LwdDry,'UID~TRANSECT+PARAMETER',value='RESULT')
# 
# #sediment
# Sediment=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# unique(Sediment$RESULT)
# Sedimentpvt=cast(Sediment,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
# unique(Sedimentpvt$TRANSECT)# check data structure
# Sed2014pvt=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
# unique(Sed2014pvt$POINT)# check data structure
# unique(Sed2014pvt$LOC)
# min(Sed2014pvt$SIZE_NUM);max(Sed2014pvt$SIZE_NUM)
# 
# #pool tail fines
# PoolFines=tblRetrieve(Parameters=c('POOLFINES2','POOLFINES6','POOLNOMEAS','POOLFINES6_512'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
# pvtPoolFines=addKEYS(cast(PoolFines,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))#need to pivot to create the pctPoolFInes variable
# 
# #bank stability
# BankStab=tblRetrieve(Parameters=c('STABLE','EROSION','COVER','BNK_VEG','BNK_COBBLE','BNK_LWD','BNK_BEDROCK'), Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# SideBank=tblRetrieve(Parameters=c('SIDCHN_BNK'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# unique(BankStab$RESULT)
# BankStabpvt=addKEYS(cast(BankStab,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
# unique(BankStab$POINT)
# unique(BankStab$TRANSECT)
# BankStabCoverClass=tblRetrieve(Parameters=c('BNK_VEG','BNK_COBBLE','BNK_LWD','BNK_BEDROCK'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# pvtBankStabCoverClass=addKEYS(cast(BankStabCoverClass,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID'))
# 
# #LINCIS_H - floodplain connectivity
# Incision=tblRetrieve(Parameters=c('INCISED','BANKHT'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# min(Incision$RESULT);max(Incision$RESULT)
# incisionpvt=cast(Incision,'UID+TRANSECT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
# 
# #XFC_NAT- fish cover
# fish=tblRetrieve(Parameters=c('BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# unique(fish$RESULT)# check data structure
# fishpvt=cast(fish,'UID+TRANSECT~PARAMETER',value='RESULT')# check data structure to make sure no duplicates
# 
# #Angle-PIBO method only
# Angle=tblRetrieve(Parameters=c('ANGLE180'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# Anglepvt=cast(Angle,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# unique(Anglepvt$TRANSECT)
# min(Angle$RESULT);max(Angle$RESULT)
# 
# #Thalweg mean , CV, and pct dry
# thalweg=addKEYS(tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion),c('PROTOCOL'))
# thalweg=subset(thalweg,SAMPLE_TYPE!='CROSSSECW')
# thalwegpvt=cast(thalweg,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# unique(thalwegpvt$POINT)
# max(thalwegpvt$DEPTH);min(thalwegpvt$DEPTH)
# 
# #Channel Dimensions
# WetWid=tblRetrieve(Parameters=c('WETWIDTH'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)#Wetted widths from thalweg
# WetWid2=tblRetrieve(Parameters=c('WETWID'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)#Wetted widths from main transects
# BankWid=tblRetrieve(Parameters=c('BANKWID'),Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
# WetWidpvt=cast(WetWid,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
# WetWid2pvt=cast(WetWid2,'UID+TRANSECT~PARAMETER',value='RESULT')
# BankWidpvt=cast(BankWid,'UID+TRANSECT~PARAMETER',value='RESULT')
# 
# #Floodprone width
# FloodWidth=tblRetrieve(Parameters=c('FLOOD_WID','FLOOD_BFWIDTH'), Projects=projects, Years=years,Protocols=protocols,SiteCode=sitecodes,Insertion=insertion)
# FloodWidthpvt=cast(FloodWidth,'UID+TRANSECT~PARAMETER',value='RESULT')
# 
# #Slope
# Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','PCT_GRADE','TRCHLEN','PARTIAL_RCHLEN','POOLRCHLEN','SLOPE_COLLECT','VALXSITE','Z_SLOPEPASSQA'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
# pvtSlope2=cast(Slope,'UID~PARAMETER',value='RESULT')
# 

