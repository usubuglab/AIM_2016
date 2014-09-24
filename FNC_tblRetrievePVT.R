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
		%s %s
 ) UnionTBL1
where (active='TRUE') "
                 ,inLOOP(Projects),inLOOP(Protocols),
                 inLOOP(SiteCodes),
                 inLOOP(Dates),inLOOP(Years),
                 UIDsubstr,ifelse(Filter=='','',sprintf('OR %s',Filter)))
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

