#inLOOP: concatenate list objects into an "IN" string for insertion into queries
inLOOP=function(inSTR) {
  inSTR=unlist(inSTR)
  for (i in 1:length(inSTR)){
    comma=ifelse(i==length(inSTR),'',',')
    STRl=sprintf("'%s'%s",inSTR[i],comma)
    if(i==1){loopSTR=STRl} else{loopSTR=paste(loopSTR,STRl)}
  }   
  return(loopSTR) 
}


#tblRetrieve: standard retrieval query
tblRetrieve=function(table, parameters='', filter=''){
  if(parameters==''){parameters=sqlQuery(wrsa1314,sprintf("select distinct parameter from %s", table))}
  sqlTABLE=sqlQuery(wrsa1314, sprintf('select * from %s where UID in (%s) and parameter in (%s) %s',table, inLOOP(UIDs),inLOOP(parameters), ifelse(filter=='','',paste('and', filter))))
  if(class(sqlTABLE)=="character"){#if returns a SQL error, assume that UID or parameter is not present and just retrieve the whole table
    sqlTABLE=sqlQuery(wrsa1314, sprintf('select * from %s  %s',table, ifelse(filter=='','',paste('where', filter))))
  }
  return(sqlTABLE)#could auto return the pivoted view, but currently assuming that is for on the fly viewing and is not the easiest way to perform metrics
}

#PVTconstruct: dyanamic generation of SQL pivot queries
PVTconstruct=function(parameters=c('SITE_ID'), tblTYPE=tblVERIFICATION, filter=''){
  tblPVTstr="select UID %s, %s
FROM (SELECT UID %s,Parameter, Result
FROM %s) p PIVOT (min(Result) FOR Parameter IN (%s)) AS pvt
WHERE %s
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