#inLOOP: concatenate list objects into an "IN" string for insertion into queries
inLOOP=function(inSTR) {
  inSTR=unlist(inSTR)
  if (inSTR==''){loopSTR="''"} else{
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
		(cast(UID as nvarchar) in (%s)) %s
 ) UnionTBL1
where (active='TRUE') "
                 ,inLOOP(Projects),inLOOP(Protocols),
                 inLOOP(SiteCodes),
                 inLOOP(Dates),inLOOP(Years),
                 inLOOP(UIDS),ifelse(Filter=='','',sprintf('OR %s',Filter)))
  UIDstr=gsub("in \\(''\\)","like '%'",UIDstr)#if(ALL=='Y' | paste(Filter,UIDS,SiteCodes,Dates,Years,Projects,Protocols,sep='')==''){UIDstr=gsub("in \\(''\\)","like '%'",UIDstr)}
  if((UIDS=='BLANK' &  Filter=='') ==FALSE|paste(SiteCodes,Dates,Years,Projects,Protocols,sep='')==''){UIDstr=gsub("like '%'","like ''",UIDstr)}#not including UID and Filter, similar to setting UID to "BLANK", this also helps the INTERSECTS and UNION to be implement properly
  qryRSLT=sqlQuery(wrsa1314, UIDstr)
  if(class(qryRSLT)=="character"){  print('Unable to interpret the provided parameters. No table retrieved.')}#not running query because could cause overload
  return(qryRSLT)
}


#UNIONTBL/tblRetrieve
tblRetrieve=function(Table='',ALL='N',Filter='',UIDS='BLANK',SiteCodes='',Dates='',Years='',Projects='',Protocols='',Parameters='',ALLp='N'){
  UIDselected=UIDselect(ALL=ALL,Filter=Filter,UIDS=UIDS,SiteCodes=SiteCodes,Dates=Dates,Years=Years,Projects=Projects,Protocols=Protocols)
  UIDstr=sprintf(" left(cast(UID as nvarchar),10) in (%s) ",inLOOP(substr(UIDselected$UID,1,10)))
  if(Table==''){
    TableSTR=sprintf("(select  UID, SAMPLE_TYPE, TRANSECT, POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
      from tblPOINT where %s 
      union
      select   UID, SAMPLE_TYPE, TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON  
      from tbltransect where %s
      union
      select   UID, SAMPLE_TYPE, cast(Null as nvarchar(5)) TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
      from tblreach where %s
      union
      select UID, SAMPLE_TYPE, cast(Null as nvarchar(5)) TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
      from tblverification where %s
    ) UnionTBL",UIDstr,UIDstr,UIDstr,UIDstr)
  } else{TableSTR=Table}
  UnionSTR=sprintf("select * from %s where ACTIVE='TRUE' and %s and Parameter in (%s)"
    ,TableSTR, UIDstr, inLOOP(Parameters))
  if(ALLp=='Y' | Parameters==''){UnionSTR=gsub("Parameter in \\(''\\)","Parameter like '%'",UnionSTR)}
  if(ALL=='Y' | paste(Filter,UIDS,SiteCodes,Dates,Years,Projects,Protocols,sep='')==''){UnionSTR=gsub("in \\(''\\)","like '%'",UnionSTR)}
  qryRSLT=sqlQuery(wrsa1314,UnionSTR)
  if(class(qryRSLT)=="character"){  print('Unable to interpret the provided parameters. No table retrieved.')}#not running query because could cause overload#previous query: sqlQuery(wrsa1314, sprintf('select * from %s  %s',table, ifelse(filter=='',"where ACTIVE='TRUE'",paste("where ACTIVE='TRUE' and ", filter))))
  return(qryRSLT)#could auto return the pivoted view, but currently assuming that is for on the fly viewing and is not the easiest way to perform metrics
}
#XWALK
#!convert to function with shared string
#!XwalkUnion=function(source='SQL',XwalkName,filter='',UIDs='',SiteCodes='',Dates=''){
#if(source='SQL){
#run UNIONtbl
#join to Xwalk
#} else if {source='R'
# merge to Xwalk as in FMimport #used for FM metadata updates
#}
#}

XwalkUnion=sqlQuery(wrsa1314,
                    "
select  XwalkTBL.Table_Xwalk, UID, SAMPLE_TYPE=case when XwalkTBL.Type_Xwalk='' then XwalkTBL.Type2 else XwalkTBL.Type_Xwalk end, TRANSECT, POINT,XwalkTBL.Parameter_Xwalk as PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
from (
  select  UID, SAMPLE_TYPE, TRANSECT, POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
  from tblPOINT
  union
  select   UID, SAMPLE_TYPE, TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON  
  from tbltransect
  union
  select   UID, SAMPLE_TYPE, cast(Null as nvarchar(5)) TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
  from tblreach
  union
  select UID, SAMPLE_TYPE, cast(Null as nvarchar(5)) TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
  from tblverification
) UnionTBL
JOIN (select *, left(SAMPLE_TYPE,len(SAMPLE_TYPE)-1) as Type2 from tblXWALK where Name_XWALK='Aquamet1') XwalkTBL on UnionTBL.PARAMETER= XwalkTBL.PARAMETER and UnionTBL.SAMPLE_TYPE= XwalkTBL.Type2
where ACTIVE='TRUE'
")