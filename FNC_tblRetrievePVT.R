#COLUMN CHECKING FUNCTION# 
#! setup - move to function
VAR=c("UID" , "SAMPLE_TYPE",  "FLAG",  "IND" , "ACTIVE", "OPERATION" ,"INSERTION", "DEPRECATION" ,"REASON")
TableNAMES=c('tblVERIFICATION','tblCOMMENTS','tblREACH','tblTRANSECT','tblPOINT')
IndMax=1
for (t in 1:length(TableNAMES)){
  IndMaxTMP=sqlQuery(wrsa1314, sprintf('select max(IND) from %s',TableNAMES[t]));
  IndMax=ifelse(IndMaxTMP>IndMax,IndMaxTMP+1,IndMax)
}
##SWJ to do: index cleanup (only change index for NAMC data; create new record and deprecate old for EPA data)
# select IND, COUNT(result) as CR 
# from (select 
#       UID  ,SAMPLE_TYPE  ,PARAMETER,  RESULT	,FLAG,	IND	,ACTIVE	,OPERATION	,INSERTION	,DEPRECATION	,REASON
#       from tblPOINT 
#       union 
#       select UID	,SAMPLE_TYPE	,PARAMETER,	RESULT	,FLAG,	IND	,ACTIVE	,OPERATION	,INSERTION	,DEPRECATION	,REASON
#       from tblTRANSECT 
#       union select 
#       UID	,SAMPLE_TYPE	,PARAMETER,	RESULT	,FLAG,	IND	,ACTIVE	,OPERATION	,INSERTION	,DEPRECATION	,REASON
#       from tblREACH) as s
# group by IND
# having COUNT(result)>1

ColCheck = function(TBL,VAR){
  MissingCheck=setdiff(VAR,colnames(TBL))
  if(length(MissingCheck)>0){
    for (c in 1: length(MissingCheck)){
      if(MissingCheck[c]=='INSERTION'){TBL$NEW=as.character(Sys.Date())}
      else if(MissingCheck[c]=='DEPRECATION'){TBL$NEW='9999-12-31'}#may need to format as date
      else if(MissingCheck[c]=='ACTIVE'){TBL$NEW=TRUE}
      else if(MissingCheck[c]=='OPERATION'){TBL$NEW='O'}
      else if(MissingCheck[c]=='TRANSECT'){
        if(max(colnames(TBL) %in% 'LINE')==1){TBL$NEW=TBL$LINE}
        else {TBL$NEW=NA}
      }
      else if(MissingCheck[c]=='POINT'){
        if(max(colnames(TBL) %in% 'BANK')==1){TBL$NEW=TBL$BANK}
        else if(max(colnames(TBL) %in% 'TRANSDIR')==1){TBL$NEW=TBL$TRANSDIR}
        else if(max(colnames(TBL) %in% 'REP')==1){TBL$NEW=TBL$REP}
        else if(max(colnames(TBL) %in% 'STATION')==1){TBL$NEW=TBL$STATION}
        else if(max(colnames(TBL) %in% 'LINE')==1){TBL$NEW=TBL$LINE}
        else {TBL$NEW=NA}
      }
      else if(MissingCheck[c]=='IND'){TBL$NEW=seq(from=unlist(IndMax),to=unlist(IndMax)+nrow(TBL)-1);IndMax=max(TBL$NEW)+1;assign('IndMax',IndMax, envir = .GlobalEnv)}#this needs to retrieve the max index number
      else{
        TBL$NEW=NA
      }
      #SWJ to do: if one of the date fields, populate with default (Insertion=TODAY, Deprecation=9999,) --> same with Active and Operation? Reason?
      colnames(TBL)[length(colnames(TBL))]=MissingCheck[c]}} #add any missing columns
  ExtraCheck=setdiff(colnames(TBL),VAR)
  if(length(ExtraCheck)>0){TBL=subset(TBL,select=colnames(TBL) %in% VAR)}
  if(max(colnames(TBL) %in% 'TRANSECT')==1){TBL$TRANSECT=sub('-','',TBL$TRANSECT)}#keep transect with AB not A-B for intertransect measurements
  return(TBL)
}
#end COLUMN CHECKING FUNCTION#


#inLOOP: concatenate list objects into an "IN" string for insertion into queries
inLOOP=function(inSTR) {
  inSTR=unlist(inSTR)
  if (inSTR[1]==''){loopSTR="''"} else{
  for (i in 1:length(inSTR)){
    comma=ifelse(i==length(inSTR),'',',')
    STRl=sprintf("'%s'%s",inSTR[i],comma)
    if(i==1){loopSTR=STRl} else{loopSTR=paste(loopSTR,STRl)}
  } }
  return(loopSTR) 
}




#PVTconstruct: dyanamic generation of SQL pivot queries
PVTconstruct=function(parameters=c('SITE_ID'), tblTYPE=tblVERIFICATION, filter=''){
  tblPVTstr="select UID %s, %s
FROM (SELECT UID %s,Parameter, Result
FROM %s) p PIVOT (min(Result) FOR Parameter IN (%s)) AS pvt
WHERE ACTIVE='TRUE' and %s
"
  if(parameters==''){parameters=sqlQuery(wrsa1314,sprintf("select distinct parameter from %s", table))}#repeat from tblRetrieve() push to subfunction at some point
  if(tblTYPE %in% c('tblVERIFICATION', 'tblREACH')){ParamResolution=''
  } else if(tblTYPE=='tblTRANSECT'){ParamResolution=',Transect'
  } else if(tblTYPE=='tblPOINT'){ParamResolution=',Transect, Point'
  } else{ParamResolution=''}
  for (p in 1:length(parameters)){
    comma=ifelse(p==length(parameters),'',',')
    Pas=sprintf("[%s] AS %s%s",parameters[p],parameters[p],comma)
    Psq=sprintf("[%s]%s",parameters[p],comma)
    if(p==1){loopPas=Pas; loopPsq=Psq} else{loopPas=paste(loopPas,Pas); loopPsq=paste(loopPsq,Psq)}
  }
  sqlTEXT=sprintf(tblPVTstr, ParamResolution, loopPas, ParamResolution,tblTYPE,loopPsq,filter)
  return(sqlTEXT)
}


#UID select
UIDselect=function(ALL='N',Filter='',UIDS='BLANK',SiteCodes='',Dates='',Years='',Projects='',Protocols=''){#UIDS default is "Blank" so it isn't subject to the replacement (gsub) and can still have the UNION...want the user to be able to use UID and FILTER to ADD samples to the main request (i.e. because those assume they have working knowledge of the database/SQL), not INTERSECT like others
  if(UIDS[1]!='BLANK'){UIDsubstr=sprintf(" left(cast(UID as nvarchar),10) in (%s) ",inLOOP(substr(UIDS,1,10)))} else{UIDsubstr="(cast(UID as nvarchar) in ('BLANK'))" }
  if(Filter==''){FilterSTR=''} else{FilterSTR=sprintf("UNION
    select UID, Active  from tblVERIFICATION where 
    %s
    UNION
    select UID, Active  from tblREACH where 
    %s
    UNION
    select UID, Active  from tblPOINT where 
    %s
    UNION
    select UID, Active  from tblTRANSECT where 
    %s",Filter,Filter,Filter,Filter)}
  UIDstr=sprintf("select distinct UID from  (
  select UID , Active from tblVERIFICATION  where
		Parameter='PROJECT' and Result in (%s)
INTERSECT
	select UID, Active  from tblVERIFICATION where
		Parameter='Protocol' and Result in (%s)
INTERSECT
	select UID, Active  from tblVERIFICATION where 
		Parameter='SITE_ID' and Result in (%s)
INTERSECT
	select UID, Active  from tblVERIFICATION  where 
		Parameter='DATE_COL' and Result in (%s)
INTERSECT
	select UID, Active  from tblVERIFICATION where 
		PARAMETER='DATE_COL' and RIGHT(result,4) in (%s)
UNION
	select UID, Active  from tblVERIFICATION where 
		%s
%s
 ) UnionTBL1
where (active='TRUE') "
                 ,inLOOP(Projects),inLOOP(Protocols),
                 inLOOP(SiteCodes),
                 inLOOP(Dates),inLOOP(Years),
                 UIDsubstr,FilterSTR)
  UIDstr=gsub("in \\(''\\)","like '%'",UIDstr)#if(ALL=='Y' | paste(Filter,UIDS,SiteCodes,Dates,Years,Projects,Protocols,sep='')==''){UIDstr=gsub("in \\(''\\)","like '%'",UIDstr)}
  if((UIDS[1]=='BLANK' &  Filter=='') ==FALSE|paste(SiteCodes,Dates,Years,Projects,Protocols,sep='')==''){UIDstr=gsub("like '%'","like ''",UIDstr)}#not including UID and Filter, similar to setting UID to "BLANK", this also helps the INTERSECTS and UNION to be implement properly
  qryRSLT=sqlQuery(wrsa1314, UIDstr)
  if(class(qryRSLT)=="character"){  print('Unable to interpret the provided parameters. No table retrieved.')}#not running query because could cause overload
  return(qryRSLT)
}

#addKEYS
#add desired parameters as columns for easier interpretation
addKEYS=function(Table,Columns){
  KEYS=tblRetrieve(Table='tblVerification',UIDS=unique(Table$UID),Parameters=Columns)
  KEYS=cast(KEYS,'UID~PARAMETER',value='RESULT')
  Table=merge(Table,KEYS, by="UID",all.x=T)
  return(Table)
}

#UNIONTBL/tblRetrieve
tblRetrieve=function(Table='',ALL='N',Comments='N',Filter='',UIDS='BLANK',SiteCodes='',Dates='',Years='',Projects='',Protocols='',Parameters='',ALLp='N'){
  UIDselected=UIDselect(ALL=ALL,Filter=Filter,UIDS=UIDS,SiteCodes=SiteCodes,Dates=Dates,Years=Years,Projects=Projects,Protocols=Protocols)
  UIDstr=sprintf(" left(cast(UID as nvarchar),10) in (%s) ",inLOOP(substr(UIDselected$UID,1,10)))#! if want to add tblMetadata, etc, need to make UIDstr blank
  if(Table==''){#previously empty tran and point set to cast(Null as nvarchar(5)) but '' used to facilitate tblComments join, not sure if it will work with comments when Table specified, but in most cases it's better to specify a list of parameters and let the union figure out where they are
    TableSTR=sprintf("(select  UID, SAMPLE_TYPE, TRANSECT, POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
      from tblPOINT where %s 
      union
      select   UID, SAMPLE_TYPE, TRANSECT, '' POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON  
      from tbltransect where %s
      union
      select   UID, SAMPLE_TYPE, '' TRANSECT, '' POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
      from tblreach where %s
      union
      select UID, SAMPLE_TYPE, '' TRANSECT, '' POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
      from tblverification where %s
    ) UnionTBL",UIDstr,UIDstr,UIDstr,UIDstr)
  } else{TableSTR=Table}
  if(toupper(Table)=='TBLCOMMENTS'){PARAMstr=''} else{PARAMstr=sprintf(" and Parameter in (%s)",inLOOP(Parameters))}
  if(Comments=='Y'){tblJoin=ifelse(Table=='','UnionTBL',Table);
                    CommentSTR=sprintf("left join (select COMMENT, UID U, FLAG F, SAMPLE_TYPE S, case when TRANSECT='ALL' then '' when TRANSECT is NULL then '' else TRANSECT end T, case when TRANSECT='ALL' then '' when TRANSECT is NULL then '' else PAGE end  P from  tblCOMMENTS where ACTIVE='TRUE') as c on %s.UID=c.U and %s.SAMPLE_TYPE=c.S and %s.FLAG=c.F %s %s",tblJoin,tblJoin,tblJoin,ifelse(tblJoin %in% c('tblVERIFICATION','tblREACH'),'',sprintf("and %s.TRANSECT=c.T",tblJoin)),ifelse(tblJoin %in% c('tblVERIFICATION','tblREACH','tblTRANSECT'),'',sprintf("and %s.POINT=c.P",tblJoin)))
  } else{CommentSTR=''}
  UnionSTR=sprintf("select * from %s %s where ACTIVE='TRUE' and %s %s"
    ,TableSTR, CommentSTR, UIDstr, PARAMstr)
  if(ALLp=='Y' | Parameters==''){UnionSTR=gsub("Parameter in \\(''\\)","Parameter like '%'",UnionSTR)}
  if(ALL=='Y' | paste(Filter,UIDS,SiteCodes,Dates,Years,Projects,Protocols,sep='')==''){UnionSTR=gsub("in \\(''\\)","like '%'",UnionSTR)}
  qryRSLT=sqlQuery(wrsa1314,UnionSTR)
  qryRSLT=qryRSLT[,!(names(qryRSLT) %in% c('U','F','S','T','P'))]
  if(class(qryRSLT)=="character"){  print('Unable to interpret the provided parameters. No table retrieved.')}#not running query because could cause overload#previous query: sqlQuery(wrsa1314, sprintf('select * from %s  %s',table, ifelse(filter=='',"where ACTIVE='TRUE'",paste("where ACTIVE='TRUE' and ", filter))))
  return(qryRSLT)#could auto return the pivoted view, but currently assuming that is for on the fly viewing and is not the easiest way to perform metrics
}


#XWALK
Xwalk=function(Source='SQL',XwalkName='WRSA',XwalkDirection='',COL='',Table='',ALL='N',Filter='',UIDS='BLANK',SiteCodes='',Dates='',Years='',Projects='',Protocols='',Parameters='',ALLp='N'){
if(Source=='SQL' | Table==''){
TBLtmp=tblRetrieve(Table=Table,ALL=ALL,Filter=Filter,UIDS=UIDS,SiteCodes=SiteCodes,Dates=Dates,Years=Years,Projects=Projects,Protocols=Protocols,Parameters=Parameters,ALLp=ALLp)
#XwalkDirection=''#user given control in the function, rather than assuming based on source
} else if (Source=='R') {
TBLtmp=eval(parse(text=Table))
#XwalkDirection='_Xwalk' #user given control in the function, rather than assuming based on source
}
COL=setdiff(COL,c(VAR,'PARAMETER','TRANSECT','POINT','RESULT'))
TBLtmp=ColCheck(TBLtmp,c(VAR,'PARAMETER','TRANSECT','POINT','RESULT',COL))
XwalkParam=sqlQuery(wrsa1314,sprintf("select *, case when right(SAMPLE_TYPE,1)='X' then left(SAMPLE_TYPE,len(SAMPLE_TYPE)-1) else SAMPLE_TYPE end as 'TABLE' from tblXWALK where Name_XWALK='%s' ",XwalkName))
XwalkTBL=sqldf(sprintf(" select  UID, TRANSECT, POINT,upper(XwalkParam.Table_Xwalk) as 'TABLE', 
upper(case when '%s'='_Xwalk' or  XwalkParam.Type_Xwalk='' or  XwalkParam.Type_Xwalk is null then XwalkParam.SAMPLE_TYPE else XwalkParam.Type_Xwalk end) as SAMPLE_TYPE, 
upper(case when '%s'='_Xwalk' or  XwalkParam.PARAMETER_Xwalk='' or  XwalkParam.PARAMETER_Xwalk is null then XwalkParam.PARAMETER else XwalkParam.PARAMETER_Xwalk end) as PARAMETER, 
RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON %s
from TBLtmp   JOIN  XwalkParam on upper(TBLtmp.PARAMETER)= upper(XwalkParam.PARAMETER%s) and upper(TBLtmp.SAMPLE_TYPE)= upper(XwalkParam.[TABLE%s])
 ",XwalkDirection,XwalkDirection,gsub("'","]",ifelse(COL=='','',gsub(", '",",[",paste(", " , inLOOP(COL),sep='')))),XwalkDirection,ifelse(XwalkName=='FMstr','',XwalkDirection)))
XwalkTBL$SAMPLE_TYPE=ifelse(toupper(substr(XwalkTBL$SAMPLE_TYPE,nchar(XwalkTBL$SAMPLE_TYPE),nchar(XwalkTBL$SAMPLE_TYPE)))=='X',substr(XwalkTBL$SAMPLE_TYPE,1,nchar(XwalkTBL$SAMPLE_TYPE)-1), XwalkTBL$SAMPLE_TYPE)
names(XwalkParam)=toupper(names(XwalkParam))
if(nrow(TBLtmp)!=nrow(XwalkTBL)){print(sprintf('WARNING! Different number of rows in original (%s rows) and xwalk (%s rows) tables.',nrow(TBLtmp),nrow(XwalkTBL)))
  XwalkParamCheck=subset(XwalkParam,select=c(sprintf('TABLE%s',ifelse(XwalkName=='FMstr','',toupper(XwalkDirection))),sprintf('PARAMETER%s',toupper(XwalkDirection))));names(XwalkParamCheck)=c('SAMPLE_TYPE','PARAMETER')
  SPmissing=unique(setdiff(toupper(paste(TBLtmp$SAMPLE_TYPE,TBLtmp$PARAMETER,sep=':')),toupper(paste(XwalkParamCheck$SAMPLE_TYPE,XwalkParamCheck$PARAMETER,sep=':'))))
  if(length(SPmissing)>0){print('The following parameters do not have a xwalk match:')
  print(SPmissing)
  }}
return(XwalkTBL)
}

#converting variable or other names from a dictionary
varConvert=function(x){ #for use within figures to convert to variable names
  NAMESlist=sub('name','',ls(envir=as.environment(1))[grep('name',ls(envir=as.environment(1)))])
  temp=paste(setdiff(gsub('rtg','',x),NAMESlist),collapse=',')
  if(nchar(temp)>0){print(sprintf('Variables missing presentation names are: %s', temp))}
  presentationNAMES=character()
  for (v in 1:length(variables)){
    if(variables[[v]] %in% NAMESlist) {varNAME=eval(parse(text=sprintf('%sname',variables[[v]])))} else{varNAME='TBD1'}
    presentationNAMES=append(presentationNAMES,varNAME)}
  abc=list(variables, sprintf('%srtg',variables), presentationNAMES)# consider using hash package http://cran.r-project.org/web/packages/hash/
  y=character()
  for (j in 1:length(x)){
    y2=NA
    for (i in 1:length(variables)){
      if(x[[j]]==abc[[1]][[i]] | x[[j]]==abc[[2]][[i]]) {y2=abc[[3]][[i]]} 
      else if(i==length(variables) & is.na(y2)) {y2='TBD2'}
    }
    y=append(y,y2)} 
  return(list(names=y, color=rainbow(length(y))))
}


bintranslate=function(Table,ST,PM=''){#ST=c("HUMINFLUW" ,     "VISRIPW" ,  "CROSSSECW"); PM=c( "SIZE_NUM")
  TBLtmp=eval(parse(text=Table))#TBLtmp=tblRetrieve(Table='tblPOINT' , Parameters=c('SIZE_NUM','SIZE_CLS','DENSIOM','PARK','ROAD','GCWDY','UNDNWDY'),UIDS=c(10368 ,31667445819269712))
  TBLtmp$RESULTorg=TBLtmp$RESULT
  TBLrow=nrow(TBLtmp)
  NullSTR="and (PARAMETER='' or PARAMETER is null)"
  BINtbl=sqlQuery(wrsa1314,sprintf("select * from tblMETADATAbin where SAMPLE_TYPE in (%s) and PARAMETER in (%s) and ACTIVE='TRUE'
                                   union select * from tblMETADATAbin where SAMPLE_TYPE in (%s) %s and ACTIVE='TRUE'",
                                   inLOOP(ST),inLOOP(PM),inLOOP(ST),NullSTR))
  if(nrow(subset(BINtbl,LOWHIGH!=0))>0) {BINtblN=cast(subset(BINtbl,LOWHIGH!=0),'SAMPLE_TYPE+PARAMETER+BIN+SORTORDER~LOWHIGH',value='RESULT');BINtblN$H=ifelse(is.na(BINtblN$H),BINtblN$L,BINtblN$H);BINtblN$LOWHIGH=1} else {BINtblN=data.frame(cbind('','','','','','',''))}
  if(nrow(subset(BINtbl,LOWHIGH==0))>0) {BINtblC=cbind(subset(BINtbl,LOWHIGH==0,select=c(SAMPLE_TYPE,PARAMETER,BIN,SORTORDER,RESULT)),NA,0);colnames(BINtblC)=c('SAMPLE_TYPE','PARAMETER','BIN','SORTORDER','H','L','LOWHIGH');BINtblC$L=BINtblC$H} else {BINtblC=data.frame(cbind('','','','','','',''))}
  BINtbl=sqldf('select * from BINtblN union select * from BINtblC');colnames(BINtbl)=c('SAMPLE_TYPE','PARAMETER','BIN','SORTORDER','H','L','LOWHIGH')
  UnionSTR="select * from TBLtmp join (select SAMPLE_TYPE as ST, PARAMETER as PM, BIN,SORTORDER,LOWHIGH,L,H from BINtbl %s) bin%s on bin%s.st=TBLtmp.sample_type %s %s"
  JOINtbl=sqldf(sprintf('%s union %s union %s union %s',
                        sprintf(UnionSTR,"where L=H",1,1,"and bin1.pm=TBLtmp.parameter",'and TBLtmp.result=bin1.L'),
                        sprintf(UnionSTR,sprintf("where L=H %s",NullSTR),2,2,"",'and TBLtmp.result=bin2.L'),
                        sprintf(UnionSTR,"where L<>H",3,3,"and bin3.pm=TBLtmp.parameter",'and cast(TBLtmp.result as float)>=cast(bin3.L as float) and cast(TBLtmp.result as float)<=cast(bin3.H as float)'),
                        sprintf(UnionSTR,sprintf("where L<>H %s",NullSTR),4,4,"",'and cast(TBLtmp.result as float)>=cast(bin4.L as float) and cast(TBLtmp.result as float)<=cast(bin4.H as float)')
                ))
  JOINtbl$RESULT=JOINtbl$BIN; JOINtbl=JOINtbl[,!(names(JOINtbl) %in% setdiff(names(JOINtbl),c('SORTORDER',names(TBLtmp))))]
  TBLtmp$SORTORDER=0;TBLtmpU=subset(TBLtmp,(IND %in% JOINtbl$IND)==FALSE)
  TBLtmp=rbind(JOINtbl,TBLtmpU)
  TBLrow2=nrow(TBLtmp)
  if(TBLrow!=TBLrow2){print('WARNING: OUtput table different length than incoming table.')}
  JOINsp=unique(sprintf('%s-%s',JOINtbl$SAMPLE_TYPE,JOINtbl$PARAMETER))
  TBLtmp2=TBLtmp;TBLtmp2$SP=sprintf('%s-%s',TBLtmp2$SAMPLE_TYPE,TBLtmp2$PARAMETER);TBLtmp2=subset(TBLtmp2,SP %in% JOINsp)#TBLspr=TBLspr[grep(list(JOINspr),TBLspr)]
  TBLspr=unique(sprintf('%s-%s-%s',TBLtmp2$SAMPLE_TYPE,TBLtmp2$PARAMETER,TBLtmp2$RESULT))
  JOINspr=unique(sprintf('%s-%s-%s',JOINtbl$SAMPLE_TYPE,JOINtbl$PARAMETER,JOINtbl$RESULT))
  sprDIFF=setdiff(TBLspr,JOINspr);sprROW=length(sprDIFF)
  if(sprROW>0){sprintf("WARNING: %s unmatched results that were not binned.",sprROW);print(sprDIFF)}
  print('RESULT is now translated and binned. Columns SORTORDER and RESULTorg added.')
  return(TBLtmp)
#   #DONE planned usage in NRSAmetrics
#   #example usage in outlier checks
#   typeMATCH=type %in% as.character(unlist(subset(binMETA,select=SAMPLE_TYPE,subset=is.na(PARAMETER)|PARAMETER=='')))
#   if(bin=='Y' & (paramMATCH=='TRUE'|typeMATCH=='TRUE')){#if match in binMETA
#     if(paramMATCH=='TRUE'){temp=merge(paramTBL,binMETA,by=c('SAMPLE_TYPE','PARAMETER','RESULT'))
#     }else if (typeMATCH=='TRUE'){temp=merge(paramTBL,binMETA,by=c('SAMPLE_TYPE','RESULT')); temp$PARAMETER=param}
#     if(nrow(paramTBL) != nrow(temp)) {print(sprintf('WARNING: categories are missing from binMETA for %s and are excluded.',typeparam))}##need to determine standard way of reporting what they are once we run into one
#     paramTBL=temp
#     paramTBL$PARAMRES=factor(paramTBL$Bin,levels=unique(paramTBL$Bin[order(paramTBL$Order,paramTBL$Bin)]),ordered=TRUE)##bin may be blank if only feeding in order (not yet doing this), determine how to handle depending on final binMETA structure #this method did not work: paramTBL=paramTBL[with(paramTBL,order(Order)),]##will order be retained? ##resetting paramTBL$PARAMRES to paramTBL$Bin automatically made it a factor...this is turned off in options and was problematic for quantiative summaries, not sure implications for boxplots##alternative way may be to order the factor list and apply to the factor levels (but this may be more complicate for ones with no binning, only ordering, unless specifying bin=result (instead of blank))
#     rm(temp)
#     # order and/or bin 
#   } else{print(sprintf('Sorting Order and Binning unknown for %s',typeparam))}
#   #DONE example usage in SpSurvey

}
