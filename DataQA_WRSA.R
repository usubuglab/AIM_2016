##missing data parameters
UnionTBL=tblRetrieve(Table='',Parameters='',ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
CheckAll='Y'#options: 'Y' = Check All Parameters for the protocol; 'N' = Check only Parameters in UnionTBL (i.e. if subsetting UnionTBL to single Table and don't want clutter from parameters not interested in)....this is not done automatically because missing data checks are meant to look for parameters that have ZERO readings for a particular dataset, only use in testing and known scenarios (usually where AllParams='Y')
CommentsCount='N'#'Y' = a comment (as represented by a flag) allows the missing data warning to be ignored; 'N' = missing data is reported regardless and contributes to subsequent percentages. 

##testing
# UnionTBL=tblRetrieve(Table='tblREACH',ALLp=AllParam,ALL=AllData,SiteCodes=sitecodes,Years=years,Projects=projects)
# CheckAll='N'
##NorCal1314
# UnionTBL=tblRetrieve(ALLp='Y',Years=c('2013','2014'),Projects='NorCal')
# CheckAll='Y'
# CommentsCount='Y'
##post hitch
# HitchImport=Sys.Date() # Sys.Date()  for today or text string like '2014-08-18'
# UIDhitch=UIDselect(Filter=sprintf("INSERTION='%s'",HitchImport))
# UnionTBL=tblRetrieve(ALLp='Y',UIDS=UIDhitch$UID)
# CheckAll='Y'
# CommentsCount='N'


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

#emptytest=emptyFulldataset;empty14Jul14=emptyFulldataset


#only reporting values with missing counts
MissingCounts=sqldf("select *, case when MissingCount is null then Points else Points-MissingCount end MissingCNT from 
                    (select *, case when Transect is null then 'O' else Transect end as TRE from emptyFulldataset where Points>0) as ExpectedCounts
                    outer left join 
                    (select Sample_Type STO, Parameter PO, [UID] as UIDo, case when Transect is null then 'O' else Transect end as TR, COUNT(Result) MissingCount, FLAG  from UnionTBL
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
MissingTotals3=sqldf("select UID,SAMPLE_TYPE, 'ReachTotal' TRANSECT,sum(MissCNT) MissCNT, sum(ExpectedCNT) ExpectedCNT ,  cast(ifnull(round((cast(sum(MissCNT) as float)/cast(sum(ExpectedCNT) as float) ),2),0) as float) MissingPCT, sum(COMMENTcnt) as COMMENTcnt from MissingTotals2 group by UID, Sample_Type")
#combine reach totals
MissingTotals5=sqldf("select UID, 'REACH' SAMPLE_TYPE,  'O' TRANSECT, ParamCNT,MissCNT,  ExpectedCNT, MissingPCT ,COMMENTcnt from MissingTotals 
                     join 
                     (select UID as UIDm,  sum(MissCNT) MissCNT, sum(ExpectedCNT) ExpectedCNT,sum(MissCNT) /sum(ExpectedCNT)  MissingPCT , sum(COMMENTcnt) as COMMENTcnt from MissingTotals2 group by UID) as Totals
                     on MissingTotals.UID=Totals.UIDm
                     ")
MissingTotals6=MissingTotals5[,!(names(MissingTotals5) %in% c('ParamCNT'))];MissingTotals7=MissingTotals5;MissingTotals7$MissingPCT=MissingTotals7$ParamCNT;MissingTotals7$SAMPLE_TYPE='Param';MissingTotals7=MissingTotals7[,!(names(MissingTotals7) %in% c('ParamCNT'))]
MissingTotals4=unique(rbind(MissingTotals2,MissingTotals3,MissingTotals6,MissingTotals7)  );MissingTotals4$RESULT=MissingTotals4$MissingPCT;MissingTotals4$PARAMETER=paste("PCT_",MissingTotals4$SAMPLE_TYPE,"_QA",sep='');MissingTotals4$TRANSECT=ifelse(MissingTotals4$TRANSECT=='O',NA,MissingTotals4$TRANSECT);MissingTotals4$POINT=MissingTotals4$TRANSECT;MissingTotals4=ColCheck(MissingTotals4,c(VAR,'TRANSECT','POINT','PARAMETER','RESULT'))
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


MissingTotals4=MissingTotals4[,!(names(MissingTotals4) %in% c('POINT'))]
MissingTotalsOUT= addKEYS(cast(subset(MissingTotals4,is.na(UID)==FALSE ), 'UID + TRANSECT  ~ PARAMETER',value='RESULT' ),Columns=c('SITE_ID','DATE_COL','Protocol')) 
MissingTotalsREACH=subset(MissingTotalsOUT,TRANSECT=='ReachTotal');write.csv(MissingTotalsREACH,"MissingDataQCr_nocom.csv")
MissingTotalsTRAN=subset(MissingTotalsOUT,TRANSECT!='ReachTotal');write.csv(MissingTotalsTRAN,"MissingDataQCt_nocom.csv")
write.csv(MissingCounts,"MissingCounts.csv")


  
  #custom missing data check for thalweg since flexible
  ThalwegCheck=sqldf("select Station.UID, StationDUPLICATES,StationCNT,DepthCNT from 
                     (select distinct UID, cast((result*2)-1 as numeric) as StationCNT from importmaster where parameter='SUB_5_7') as station
                     join
                     (select UID,count(result) as StationDUPLICATES from (select distinct UID, result from importmaster where parameter='SUB_5_7') as stcnt group by UID) as stationcount
                     on station.uid=stationcount.uid
                     join 
                     (select UID, max(cast(point as numeric)) as DepthCNT from importmaster where parameter='DEPTH' group by UID) as depth
                     on station.uid=depth.uid
                     where StationCNT > DepthCNT or stationDUPLICATES>1
                     order by Station.UID")
  
  print("Warning! Number of Thalweg depths does not match the number expected from the widths/stations!")
  #conflicts happen (i.e. multiple sub_5_7 values per site) when crews forget their reach widths on the first few transects, missing data check added in FM to warn them
  print(ThalwegCheck)  
  
  ##outlier check

#set strata for later iteration
typestrata=c('EcoReg','Size')#must match column names that are created
numstrata=length(typestrata)
UnionTBL$EcoReg=substr(UnionTBL$SITE_ID,1,2)#-- Switch to climatic rather than ecoreg?  ; ; may need to explicitly join an ecoregion column if sitecodes change over different projects, this works for NRSA only; also needs to be more expandable for additional strata
UnionTBL$Size=substr(UnionTBL$SITE_ID,4,5)
#this section is highly dependent on WRSA siteID naming structure and GRTS strata

#QA boxplots
give.n <- function(x){return(data.frame(y = max(x)+1, label = paste("n =",length(x))))}#SWJ to do: improve to handle the multiple classes for Categorical 
subcol=c('UID','SITE_ID','PARAMETER','STRATATYPE','STRATA','PARAMRES','PARAMCAT','TRANSECT','POINT')
#compile parameter list
allparams=unique(paste(UnionTBL$SAMPLE_TYPE,UnionTBL$PARAMETER,sep=" "))#numeric: allparams=c("BANKW INCISED", "BANKW WETWID" )#categorical: allparams=c("CROSSSECW SIZE_CLS","HUMINFLUW WALL")
excludeparams=c("FIELDMEAS DO",'THALW BAR_PRES',"THALW SIDCHN" ,"THALW BACKWATER",grep("VERIF",allparams,value=T),grep("CALIB",allparams,value=T),grep("BERW",allparams,value=T),"SLOPEW METHOD","FIELDMEAS LOCATION","FIELDMEAS TIME" ,"FIELDMEAS CORRECTED",'CROSSSECW DIST_LB','SLOPEW SLOPE_UNITS','CROSSSECW SUB_5_7','THALW INCREMENT')
combineparams=c("CANCOVERW DENSIOM",'CROSSSECW XSIZE_CLS',grep("LWD",allparams,value=T),grep("HUMINFLU",allparams,value=T),grep("VISRIP",allparams,value=T),grep("FISHCOV",allparams,value=T),grep("ASSESS",allparams,value=T),grep("TORR",allparams,value=T))#need to exclude originals from allparams list and add new names back; some of these may be useable, just want to ponder them a bit more (run a few examples through the existing framework)
allparams1=setdiff(allparams,c(excludeparams,combineparams))
UnionTBL2=UnionTBL#subset data here if do not want all db results (UnionTBL is not filtered by UIDs, UnionTBL1 is); most likely scenario is intensifications (NorCal, CoPlateau, NV), other designs with diff strata; see second round of filtering at the allsites variable
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='XSIZE_CLS','SIZE_CLS',UnionTBL2$PARAMETER)#for all our analysis purposes, these are the same
UnionTBL2$PARAMETER=ifelse(UnionTBL2$SAMPLE_TYPE=='LWDW','LWDtally',UnionTBL2$PARAMETER)#for preliminary analysis purposes, these are the same
UnionTBL2$PARAMETER=ifelse(UnionTBL2$SAMPLE_TYPE=='HUMINFLUW','HumanPresence',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='AGR','AGRicultural',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='IND','INDustrial',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='MAN','MANagement',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='REC','RECreation',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='RES','RESidential',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$SAMPLE_TYPE=='TORR' & UnionTBL2$PARAMETER!='TSD011','Torrent',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='BARE','BARE',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='CANBTRE'|UnionTBL2$PARAMETER=='CANSTRE','CAN_TREE',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='CANVEG'|UnionTBL2$PARAMETER=='UNDERVEG','VEG_TYPE',UnionTBL2$PARAMETER)##
UnionTBL2$SAMPLE_TYPE=ifelse(UnionTBL2$PARAMETER=='VEG_TYPE','VISRIP2W',UnionTBL2$SAMPLE_TYPE)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='GCNWDY'|UnionTBL2$PARAMETER=='UNDNWDY','NONWOOD',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='GCWDY'|UnionTBL2$PARAMETER=='UNDWDY','WOODY',UnionTBL2$PARAMETER)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='DENSIOM' & UnionTBL2$POINT %in% c('LF','RT'),'DENSIOMbank',ifelse(UnionTBL2$PARAMETER=='DENSIOM' & (UnionTBL2$POINT  %in% c('LF','RT')==FALSE),'DENSIOMcenter',UnionTBL2$PARAMETER))#for preliminary analysis purposes, these need to be divided (and are believed to be separated in aquamet)
combineparamNEW=c('CANCOVERW DENSIOMbank','CANCOVERW DENSIOMcenter','CROSSSECW SIZE_CLS','LWDW LWDtally','TORR Torrent','ASSESS AGRicultural','ASSESS INDustrial','ASSESS MANagement','ASSESS RECreation','ASSESS RESidential','HUMINFLUW HumanPresence','VISRIPW BARE','VISRIPW CAN_TREE','VISRIP2W VEG_TYPE','VISRIPW NONWOOD','VISRIPW WOODY')##still need to ponder HUMINFLU, VISRIP, FISHCOV, and ASSESS and add back in here
allparams1=union(allparams1,combineparamNEW)
#binned parameters
bin='Y'#'Y' if would like to apply specified binning to parameters in binparams
binparams=c("CROSSSECW SIZE_CLS",grep("LWD",allparams1,value=T),grep("CANCOVER",allparams1,value=T),'VISRIP2W VEG_TYPE','TORR Torrent','HUMINFLUW HumanPresence',grep("ASSESS",allparams1,value=T),grep("VISRIP",allparams1,value=T))#also list any parameters that should be treated as categorical that are otherwise in params_N
binMETA=read.csv('binMETADATAtemp.csv')##feed in from SQL once solified in FM, R, SQL; also used to order categoricals
for (p in 1:length(allparams1)){#this is a standard loop for iterating, could put it in a function that allows you to plug in a string for the most nested middle
  typeparam=strsplit(allparams1[p]," ")
  type=typeparam[[1]][[1]]; param=typeparam[[1]][[2]]
  paramTBL=subset(UnionTBL2,subset=PARAMETER==param & SAMPLE_TYPE==type)
  paramTBL$CHAR=as.character(paramTBL$RESULT)
  paramTBL$NUM=as.numeric(paramTBL$CHAR)
  if(nrow(paramTBL)>0){
    if(allparams1[p] %in% paste(substr(params_C$SAMPLE_TYPE,1,nchar(params_C$SAMPLE_TYPE)-1),params_C$PARAMETER,sep=" ") |allparams1[p] %in% binparams){
      paramTBL$PARAMRES=paramTBL$CHAR#previous if statement: is.na(min(paramTBL$NUM)) & is.na(max(paramTBL$NUM))
      paramSTATUS='CHAR'
      paramMATCH=param %in% binMETA$PARAMETER
      typeMATCH=type %in% as.character(unlist(subset(binMETA,select=SAMPLE_TYPE,subset=is.na(PARAMETER)|PARAMETER=='')))
      if(bin=='Y' & (paramMATCH=='TRUE'|typeMATCH=='TRUE')){#if match in binMETA
        if(paramMATCH=='TRUE'){temp=merge(paramTBL,binMETA,by=c('SAMPLE_TYPE','PARAMETER','RESULT'))
        }else if (typeMATCH=='TRUE'){temp=merge(paramTBL,binMETA,by=c('SAMPLE_TYPE','RESULT')); temp$PARAMETER=param}
        if(nrow(paramTBL) != nrow(temp)) {print(sprintf('WARNING: categories are missing from binMETA for %s and are excluded.',typeparam))}##need to determine standard way of reporting what they are once we run into one
        paramTBL=temp
        paramTBL$PARAMRES=factor(paramTBL$Bin,levels=unique(paramTBL$Bin[order(paramTBL$Order,paramTBL$Bin)]),ordered=TRUE)##bin may be blank if only feeding in order (not yet doing this), determine how to handle depending on final binMETA structure #this method did not work: paramTBL=paramTBL[with(paramTBL,order(Order)),]##will order be retained? ##resetting paramTBL$PARAMRES to paramTBL$Bin automatically made it a factor...this is turned off in options and was problematic for quantiative summaries, not sure implications for boxplots##alternative way may be to order the factor list and apply to the factor levels (but this may be more complicate for ones with no binning, only ordering, unless specifying bin=result (instead of blank))
        rm(temp)
        # order and/or bin 
      } else{print(sprintf('Sorting Order and Binning unknown for %s',typeparam))}
    } else{paramTBL$PARAMRES=paramTBL$NUM
           paramSTATUS='NUM'}
    if(paramSTATUS=='CHAR') {numstrata3=numstrata+1;typestrata3=c('SITE_ID',typestrata)} else{numstrata3=numstrata;typestrata3=typestrata}
    for (n in 1:numstrata3) {
      paramTBL3=paramTBL
      paramTBL3$STRATATYPE=typestrata3[n]
      paramTBL3$STRATA=unlist(paramTBL3[typestrata3[n]])#paramTBL3$STRATA='UNK'
      if (n==1) { paramTBL2=paramTBL3
      } else { 
        paramTBL2=rbind(paramTBL2,paramTBL3)
      } }
    strata=unique(paste(paramTBL2$STRATATYPE,paramTBL2$STRATA,sep="_" ))
    if(paramSTATUS=='NUM'){
      paramTBL3=aggregate(PARAMRES~UID+SITE_ID+PARAMETER+STRATATYPE+STRATA,data=paramTBL2,FUN=mean)
      paramTBL3$PARAMCAT='None'
    } else if(paramSTATUS=='CHAR'){
      paramTBL3a=aggregate(IND~PARAMRES+UID+SITE_ID+PARAMETER+STRATATYPE+STRATA,data=paramTBL2,FUN=length)
      paramTBL3b=aggregate(IND~UID+SITE_ID+PARAMETER+STRATATYPE+STRATA,data=paramTBL2,FUN=length)
      paramTBL3=merge(paramTBL3a,paramTBL3b,by=c('UID','SITE_ID','STRATATYPE','STRATA','PARAMETER'))
      paramTBL3$PARAMCAT=paramTBL3$PARAMRES;paramTBL3$PARAMRES=paramTBL3$IND.x/paramTBL3$IND.y
    }
    
    for (n in 1:numstrata) {#re-enter for loop now that all STRATA are complete and aggregated
      paramTBL4=subset(paramTBL3,subset=STRATATYPE==typestrata[n])
      stratabox=ggplot(paramTBL4,aes(y=PARAMRES, x=STRATA,fill=PARAMCAT)) 
      stratabox=stratabox+ geom_boxplot(outlier.colour = "red", outlier.size = 10)+#for reviewing all data by strata
        stat_summary(fun.data =give.n, geom = "text") +
        labs (title=sprintf('STRATA- %s ~ PARAM- %s',typestrata[n],param))
      ggsave(filename=sprintf('%s.jpg',stratabox$labels$title),plot=stratabox)#assign(stratabox$labels$title,stratabox)#save jpeg or # assign(sprintf('box_STRATA_%s_%s',typestrata[n],param),stratabox)
    }
    allsites=intersect(unique(paramTBL$UID),unique(UnionTBL1$UID))##would be nice if this  linked to a second set of UIDs to narrow down sites, but still feed all sites through the aggregations; interim solution is to match UnionTBL (all data) to UnionTBL1 (UID restricted) which will still run all data if alldata='Y'
    for (s in 1:length(allsites)){#2){
      paramTBL3$TRANSECT='ALL';paramTBL3$POINT='ALL'
      paramTBL3$POINT=ifelse(paramSTATUS=='CHAR' & paramTBL3$STRATATYPE=='SITE_ID',paramTBL3$IND.y,paramTBL3$POINT)#set up for later N (sample size) use
      paramTBL5=subset(paramTBL3,select=subcol)
      if(paramSTATUS=='NUM'){
        paramTBL$STRATATYPE='UID';paramTBL$STRATA=paramTBL$SITE;paramTBL$PARAMCAT='None'; paramTBL$STRATA=factor(paramTBL$STRATA,levels=unique(paramTBL$STRATA))
        paramTBL6=subset(paramTBL,select=subcol)
        paramTBL6=rbind(paramTBL6,paramTBL5)
      } else if(paramSTATUS=='CHAR'){paramTBL6=paramTBL5}
      stratas=unique(subset(paramTBL6,select=STRATA,subset=UID==allsites[s]))
      paramTBL6=subset(paramTBL6,subset=STRATA %in% stratas$STRATA)
      paramTBL6$STRATATYPE=factor(paramTBL6$STRATATYPE,levels=c("UID",typestrata))
      paramTBL6$PARAMCAT=factor(paramTBL6$PARAMCAT)
      #label prep#
      #label site average
      siteavg=unique(subset(paramTBL6,subset=STRATATYPE !="UID" & UID==allsites[s] , select=c('PARAMRES', 'PARAMCAT')))
      #label sample size
      paramN=aggregate(PARAMRES~STRATATYPE,data=paramTBL6,FUN='length')#paramN$STRATATYPE=factor(paramN$STRATATYPE,levels=levels(paramTBL6$STRATATYPE))#might be needed to keep in the same order, unsure
      if(paramSTATUS=='CHAR'){paramN$PARAMRES=ifelse(paramN$STRATATYPE=='UID',min(paramTBL6$POINT),round(paramN$PARAMRES/length(unique(paramTBL6$PARAMCAT)),0))}
      #label quantiles with SiteCode
      paramquant=aggregate(PARAMRES~STRATATYPE+PARAMCAT,data=paramTBL6,FUN='quantile',probs=c(0.05,0.95),names=FALSE);colnames(paramquant)=c('STRATATYPE','PARAMCAT','Quant')
      paramTBL6=merge(paramTBL6,paramquant,by=c('STRATATYPE','PARAMCAT'))
      paramTBL6$SiteLabelOUT=ifelse(paramTBL6$PARAMRES<paramTBL6$Quant[,1],paramTBL6$SITE_ID,ifelse(paramTBL6$PARAMRES>paramTBL6$Quant[,2],paramTBL6$SITE_ID,NA))#create a site label if an outlier
      paramTBL6$SiteLabelOUT=ifelse(paramTBL6$STRATATYPE=="UID"& paramSTATUS=='CHAR',NA, ifelse(paramTBL6$STRATATYPE=="UID" & is.na(paramTBL6$SiteLabelOUT)==FALSE,paste(paramTBL6$TRANSECT,paramTBL6$POINT,sep=":"),paramTBL6$SiteLabelOUT))#change site label to transect if raw data
      paramQ=subset(paramTBL6,is.na(SiteLabelOUT)==FALSE)
      #label outliers with SiteCode
      paramoutlrM=aggregate(PARAMRES~STRATATYPE+PARAMCAT,data=paramTBL6,FUN='mean');colnames(paramoutlrM)=c('STRATATYPE','PARAMCAT','Mean')
      paramoutlrS=aggregate(PARAMRES~STRATATYPE+PARAMCAT,data=paramTBL6,FUN='sd');colnames(paramoutlrS)=c('STRATATYPE','PARAMCAT','SD')
      paramTBL6=merge(paramTBL6,paramoutlrM,by=c('STRATATYPE','PARAMCAT'));paramTBL6=merge(paramTBL6,paramoutlrS,by=c('STRATATYPE','PARAMCAT'))
      paramTBL6$SiteLabelOUT2=ifelse(paramTBL6$PARAMRES>(paramTBL6$Mean + (2*paramTBL6$SD)),paramTBL6$SITE_ID,ifelse(paramTBL6$PARAMRES<(paramTBL6$Mean - (2*paramTBL6$SD)),paramTBL6$SITE_ID,NA))#create a site label if an outlier
      paramTBL6$SiteLabelOUT2=ifelse(paramTBL6$STRATATYPE=="UID"& paramSTATUS=='CHAR',NA, ifelse(paramTBL6$STRATATYPE=="UID" & is.na(paramTBL6$SiteLabelOUT2)==FALSE,paste(paramTBL6$TRANSECT,paramTBL6$POINT,sep=":"),paramTBL6$SiteLabelOUT2))#change site label to transect if raw data
      paramMSD=subset(paramTBL6,is.na(SiteLabelOUT2)==FALSE);paramMSDpres=nrow(paramMSD); if(paramMSDpres==0){paramMSD=paramTBL6[1,]}#geom_text will fail if no rows are present
      #generate box plot in ggplot2
      sitebox=ggplot(paramTBL6,aes(y=PARAMRES, x=PARAMCAT,fill=PARAMCAT,colour=PARAMCAT)) + geom_boxplot(outlier.colour='darkred',outlier.size=10,colour='black') + facet_grid(.~STRATATYPE)+
        geom_hline(aes(yintercept=PARAMRES, colour=PARAMCAT),siteavg,size=1)  + #mark the average for the site
        scale_colour_brewer(drop=FALSE,palette='Set1') + scale_fill_brewer(palette='Set1')+#sync colors between lines and boxplots (especially important for categorical)
        geom_text(data=paramN,aes(label=sprintf('n=%s',PARAMRES),x=(length(unique(paramTBL6$PARAMCAT))/2)+0.5,y=max(paramTBL6$PARAMRES)+0.25),inherit.aes=FALSE, parse=FALSE)+#annotate("text",x=2,y=max(paramTBL6$PARAMRES)+0.5,label=sprintf('n=%s',paramN$PARAMRES)) +#annotate: n(sites) for strata plots and n(points) for site  (function defined above) #messy for categorical#previous (not working as anticipated, particularly for categorical): #stat_summary(fun.data =give.n, geom = "text") + #function for adding sample size to boxplots #
        labs(title=sprintf('SITE- %s (%s) ~ PARAM- %s',unique(subset(paramTBL6, subset=STRATATYPE=='UID', select=SITE_ID)),allsites[s],param)) +
        #geom_text(data=paramQ,aes(label=SiteLabelOUT),show_guide=F,size=3,position= position_jitter(width = 0.5, height=0))+#jitter a little strange, but makes it readable
        geom_text(data=paramMSD,aes(label=SiteLabelOUT2),show_guide=F,size=3,position= position_jitter(width = 0.25, height=0))+
        theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank())  #remove x axis 
      ggsave(filename=sprintf('%s.jpg',sitebox$labels$title),plot=sitebox)#assign(sitebox$labels$title,sitebox)#save jpeg or assign var (need to refine naming)
      if(paramMSDpres>0){if(p==1 & s==1){outlierTBL=paramMSD} else {outlierTBL=rbind(outlierTBL,paramMSD)}}
    } } }
rm(paramTBL3,paramTBL4,paramTBL6,paramTBL5,paramTBL3a,paramTBL3b)

outlierTBL=unique(subset(outlierTBL,select=c('STRATATYPE','STRATA','SITE_ID','UID','PARAMETER','PARAMCAT','TRANSECT','POINT','PARAMRES','Mean','SD')))
stratat=unique(outlierTBL$STRATATYPE)
for (s in 1:length(stratat)){
  outlierTBLst=subset(outlierTBL,subset=STRATATYPE==stratat[s])
  write.csv(outlierTBLst,file=sprintf('Outliers_2SDmean_%s.csv',stratat[s]))#could export as a single table, but a bit overwhelming
}

#SWJ to do (2/11/14):
#print cv or other metric as a warning for the spread? compare cv of site to avg cv of all/strata sites? (Scott--> cv only for repeatable data, didn't give alternate spread)
#mimic labelling of site boxplots in strata
#add "all" boxplot in strata




##crossvalidation/business rules
WetWidthDIFF=sqlQuery(wrsa1314,"select WetTRAN.UID, WetTRAN.TRANSECT, RESULT_PNTthal, RESULT_TRAN
 from (select UID, TRANSECT, RESULT as RESULT_PNTthal from tblpoint
where parameter like 'wetwid%'
and POINT='0') as WetPNTthal
join (select  UID, TRANSECT, RESULT as RESULT_TRAN from tbltransect
where parameter like 'wetwid%') as WetTRAN
on (WetTRAN.UID=WetPNTthal.UID and WetTRAN.TRANSECT=WetPNTthal.TRANSECT)
--where ROUND(convert(float,result_pntthal),1) <> ROUND(convert(float,result_tran),1)
--and WetTRAN.UID=11625 --query struggles when running the convert function with multiple UID, makes no sense, running where statement externally in excel via Exact()
")#should only occur on paper forms where value is recorded twice and therefore appears in the db twice

##! store and dynamically compose validation rules 
