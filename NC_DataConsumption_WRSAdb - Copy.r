#------------------------------------------------------DOCUMENTATION------------------------------------------#
#for detailed explanation of R and SQL structure, see buglab\Research Projects\BLM_WRSA_Stream_Surveys\Technology\WRSA data management.docx


#-------------------------------------------------------INPUTS--------------------------------------------------------#
#In the ideal world, users should only need to put inputs here and be able to get results out of the 'black box' below using existing functions.
DBpassword=''#Always leave blank when saving for security and because changes annually. Contact Sarah Judson for current password.
DBuser=''#ditto as with DBpassword
DBserver=''#ditto as with DBpassword
#this is a change

#--------------------------------------------------------SETUP--------------------------------------------------------#
#LOAD required packages#
requiredPACKAGES=c('reshape', 'RODBC','ggplot2','grid','gridExtra','xlsx','sqldf','jpeg','spsurvey')
for (r in 1:length(requiredPACKAGES)){
  if ((requiredPACKAGES[r] %in% installed.packages()[,1])==FALSE){install.packages(requiredPACKAGES[r])}#auto-install if not present
  library(requiredPACKAGES[r],character.only = TRUE)
}

#default working directory is the location of the Rproject which is custom to each collaborator and should automatically be set when project is loaded
#setwd('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\WRSA')#SWJ to do: map more dynamically but securely
#setwd('C:\\Users\\Sarah\\Desktop\\NAMCdevelopmentLocal\\WRSA')##Sarah desktop

##Establish an ODBC connection##
#the db was created in SQL Server Manager on 11/19/2013 by Sarah Judson#
wrsaConnectSTR=sprintf("Driver={SQL Server Native Client 10.0};Server=%s;Database=WRSAdb;Uid=%s; Pwd=%s;",DBserver,DBuser, DBpassword)
wrsa1314=odbcDriverConnect(connection = wrsaConnectSTR)
#SWJ to do: throw this into a function that also prompts for server and password if missing (='')
#SWJ to do: throw the function into a separate referenced script because multiple files are using this

options(stringsAsFactors=F,"scipen"=50)#general option, otherwise default read is as factors which assigns arbitrary number behind the scenes to most columns

#SQL assistance functions
#loaded from a separate R script
source('FNC_tblRetrievePVT.R')
#common sQL strings that need to be incorporated:
##(select * from tblVERIFICATION where PARAMETER='site_id') v on v.UID=tblPOINT.uid


#------------------------------------------------------New Data Requests-----------------------------------------------#
#To get the metadata table you must use the SQL code. 
tblMETADATA= sqlQuery(wrsa1314, "select * from tblMETADATA")

#To get WQ data for 3 parameters for all NorCal sites
WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','NTL','PTL'),Projects='NorCal')
WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')
WQfinal=addKEYS(WQpvt,c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))







#------------------------------------------------------SARAH'S EXAMPLES------------------------------------------------------------------#

#Most data requests use the following basic workflow and structure. Save any custom requests created to CustomRequest_WRSAdb.R for documentation.
#CALL data in using tblRetrieve() #at least ONE filter required, Parameters NOT required, Comments optional (default is no). For possible filters, see "WRSA data managment.docx" OR use getAnywhere(tblRetrieve) and examine available varaiables in the function() inputs section.
EXAMPLEcond=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED'), Comments='N',Projects='NorCal',Years=c('2013','2014'))
#PIVOT data using cast() function for easier viewing. IND will be lost if need for tracking. Alternative: aggregate() function OR PVTconstruct() assists in building SQL string for custom PIVOTS in SQL Server.
EXAMPLEcondPVT=cast(EXAMPLEcond,'UID~PARAMETER',value='RESULT') 
#KEYS added for data interpretability. Any parameters stored in tblVERIFICATION are available to add. Suggested minimum additions are Site_ID + Date_COL. In this example, coordinates for mapping.
EXAMPLEcondPVT=addKEYS(EXAMPLEcondPVT ,c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))
#EXPORT results via csv
write.csv(EXAMPLEcondPVT,'ExampleConductivityCorrected_TodaysDate.csv')#pivoted does not contain IND

#Example of retrieving all raw data for an entire project
NorCal1314=tblRetrieve(ALLp='Y',Years=c('2013','2014'),Projects='NorCal')
NorCal1314subCOND=subset(NorCal1314,PARAMETER %in% c('CONDUCTIVITY','CORRECTED'))#and again subsetting it just for a few parameters like EXAMPLEcond

#METADATA for reference
#use RODBC package sqlQuery() function, not tblRetrieve
METADATA=sqlQuery (wrsa1314,"select * from tblMETADATA where ACTIVE='TRUE'")#see "Label" for interpretable names #be careful with SQL strings, enclose in double quote and use single quotes for text
METADATAprotocol=sqlQuery (wrsa1314,"select * from tblMETADATAprotocol where ACTIVE='Y' and Protocol='WRSA14'")#expected counts
METADATArange=sqlQuery (wrsa1314,"select * from tblMETADATArange where ACTIVE='TRUE' and Protocol='WRSA14'")#legal values
METADATAindicators=sqlQuery (wrsa1314,"select * from tblXwalk where NAME_xwalk='MissingBackend' and type_xwalk='Indicator'")
indicators=NULL
for (i in 1:nrow(METADATAindicators)){indicators=paste(indicators,METADATAindicators$Parameter_Xwalk[i],sep="|")}
indicators=unique(unlist(strsplit(indicators,"\\|")));indicators=indicators[2:length(indicators)]
for (p in 1:length(indicators)){
  METADATAparameters=sqlQuery (wrsa1314,sprintf("select * from tblXwalk where NAME_xwalk='MissingBackend' and PARAMETER_Xwalk like '%%%s%%'",indicators[p]))
  METADATAparameters$INDICATOR=indicators[p]
  if(p==1){parameters=METADATAparameters} else{parameters=rbind(parameters,METADATAparameters)}
}  
View(indicators);View(parameters)























#-------------------------------------------------------OLD FILTERS---------------------------------------------------#
#These are old filters but may come in handy so do not delete
#FILTERS
##from most to least specific
sitecodes=c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')
dates=c('05/05/2005')
hitchs=c('')#NOT WORKING YET
crews=c('R1')#NOT WORKING YET
projects=c('NRSA')#NOT WORKING YET

#PARAMETERS
#specify if desired (will make queries less intensive):
testP=c('ANGLE','APPEALING','ALGAE')#test, one from each level of table
bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')


#--------------------------------------------------------SETUP--------------------------------------------------------#
#LOAD required packages#
requiredPACKAGES=c('reshape', 'RODBC')
for (r in 1:length(requiredPACKAGES)){
  if ((requiredPACKAGES[r] %in% installed.packages()[,1])==FALSE){install.packages(requiredPACKAGES[r])}#auto-install if not present
  library(requiredPACKAGES[r],character.only = TRUE)
}

#setwd dynamically ##SWJ to do!

##Establish an ODBC connection##
#the db was created in SQL Server Manager on 11/19/2013 by Sarah Judson#
wrsaConnectSTR=sprintf("Driver={SQL Server Native Client 10.0};Server=129.123.16.13,1433;Database=WRSAdb;Uid=feng; Pwd=%s;",DBpassword)
wrsa1314=odbcDriverConnect(connection = wrsaConnectSTR)


#SQL assistance functions
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
tblRetrieve=function(table, parameters=''){
  if(parameters==''){parameters=sqlQuery(wrsa1314,sprintf("select distinct parameter from %s", table))}
  sqlTABLE=sqlQuery(wrsa1314, sprintf('select * from %s where UID in (%s) and parameter in (%s)',table, inLOOP(UIDs),inLOOP(parameters)))
  return(sqlTABLE)#could auto return the pivoted view, but currently assuming that is for on the fly viewing and is not the easiest way to perform metrics
}
#PVTconstruct: dyanamic generation of SQL pivot queries
PVTconstruct=function(parameters=c('SITE_ID'), tblTYPE=tblVERIFICATION, filter=''){
  tblPVTstr="select UID %s, %s
FROM (SELECT UID %s,Parameter, Result
FROM %s) p PIVOT (min(Result) FOR Parameter IN (%s)) AS pvt
WHERE %s
"
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


#--------------------------------------------------------SQL RETRIEVE--------------------------------------------------------#
#SQL tables are created and imports managed via an independent R scripts (createNRSA1314db_SWJ.r)
#all possible tables
sqlTables(wrsa1314)
sqlColumns(wrsa1314,"tblPOINT")

#select samples
UIDs=sqlQuery(wrsa1314, sprintf("select distinct UID from tblVERIFICATION 
                                where (active='TRUE') 
                                AND ((Parameter='SITE_ID' and Result in (%s)) OR (Parameter='DATE_COL' and Result in (%s)))"
                                ,inLOOP(sitecodes),inLOOP(dates)))
#SWJ to do: add additional filters
#SWJ to do: prompt for data entry (mini-GUI)


#retrieve desired tables
#EXAMPLES of tblRetrieve function# (note: parameter lists were specified in the "Inputs" section at the beginning of this script)
tblREACH=tblRetrieve('tblREACH')#not specifying parameters will retrieve the entire table
tblPOINT=tblRetrieve('tblPOINT')
tblTRANSECT=tblRetrieve('tblTRANSECT')
tblVERIFICATION=tblRetrieve('tblVERIFICATION')
tblMETADATA= sqlQuery(wrsa1314, "select * from tblMETADATA")
tblREACHtest=tblRetrieve('tblREACH',testP)
tblPOINTbank=tblRetrieve('tblPOINT',bankP)




#Close ODBC connection when done talking to SQL Server
odbcClose(wrsa1314)

#--------------------------------------------------------PIVOT VIEWS--------------------------------------------------------#
##RESHAPE to PIVOT## 
#EXAMPLES of both methods#
#SQL option ('View' creation to copy/paste)
bankPVTstr=PVTconstruct(parameters=bankP,tblTYPE='tblPOINT', filter="POINT in ('LF','RT')");print(bankPVTstr)#- need permission from Sarah Judson and to reopen ODBC before saving Views for permanent use in SQL Server
  #retrieve said query from SQL
    wrsa1314_2=odbcDriverConnect(connection = wrsaConnectSTR)
    tblPOINTbankPVTs=sqlQuery(wrsa1314_2,bankPVTstr)
    odbcClose(wrsa1314_2)
#R option (cast)
tblPOINTbankPVTr=cast(subset(tblPOINTbank,select=c(UID, TRANSECT,POINT,PARAMETER,RESULT)), UID + TRANSECT + POINT ~ PARAMETER)#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function


#--------------------------------------------------------ANALYSIS--------------------------------------------------------#
##AGGREGATION##
#EXAMPLES#
#count number of records per parameter to check for missing data
qastatsBANK_CNTcast=cast(tblPOINTbank, UID ~ PARAMETER, value='RESULT', fun.aggregate=length)#should this filter out NULLs or flags? does EPA write a line for each record even if no value recorded?
qastatsBANK_CNTagg=aggregate(tblPOINTbank,FUN='length', by=list(tblPOINTbank$UID,tblPOINTbank$TRANSECT,tblPOINTbank$POINT))
#cast seems like the more elegant solution
#convert numerics before performing stats
tblPOINTbankNUM=subset(tblPOINTbank,subset= is.na(as.numeric(as.character(tblPOINTbank$RESULT)))==FALSE);tblPOINTbankNUM$RESULT=as.numeric(as.character(tblPOINTbankNUM$RESULT))
qastatsBANK_MEANcast=cast(tblPOINTbankNUM, UID ~ PARAMETER, value='RESULT', fun.aggregate=mean)
##########
#--------------------------------------------------------ANALYSIS--------------------------------------------------------#
##AGGREGATION##
#EXAMPLES#
#count number of records per parameter to check for missing data
qastatsBANK_CNTcast=cast(tblPOINTbank, UID ~ PARAMETER, value='RESULT', fun.aggregate=length)#should this filter out NULLs or flags? does EPA write a line for each record even if no value recorded?
qastatsBANK_CNTagg=aggregate(tblPOINTbank,FUN='length', by=list(tblPOINTbank$UID,tblPOINTbank$TRANSECT,tblPOINTbank$POINT))
#cast seems like the more elegant solution
#convert numerics before performing stats
tblPOINTbankNUM=subset(tblPOINTbank,subset= is.na(as.numeric(as.character(tblPOINTbank$RESULT)))==FALSE);tblPOINTbankNUM$RESULT=as.numeric(as.character(tblPOINTbankNUM$RESULT))
qastatsBANK_MEANcast=cast(tblPOINTbankNUM, UID ~ PARAMETER, value='RESULT', fun.aggregate=mean)

##QA checks##
for (p in 1:length(unique(paste(UnionTBL$SAMPLE_TYPE,UnionTBL$PARAMETER)))){#this is a standard loop for iterating, could put it in a function that allows you to plug in a string for the most nested middle
  typeparam=strsplit(unique(paste(UnionTBL$SAMPLE_TYPE,UnionTBL$PARAMETER))[p]," ")
  type=typeparam[[1]][[1]]; param=typeparam[[1]][[2]]
  paramTBL=subset(UnionTBL,subset=PARAMETER==param & SAMPLE_TYPE==type)
  paramTBL$CHAR=as.character(paramTBL$RESULT)
  paramTBL$NUM=as.numeric(paramTBL$CHAR)
  #iterate over strata: sites, all values combined, ecoregion/climatic, size
  #example: extract size from SITE_ID - UnionTBL$SIZE=substr(UnionTBL$SITE_ID,4,5)
  if(is.na(min(paramTBL$NUM)) & is.na(max(paramTBL$NUM))){paramTBL$PARAMRES=paramTBL$CHAR
                                                          print (sprintf("%s is CHARACTER format",param))
                                                          hist(paramres)#histogram - inclu "pseudo categorical" (densiom, visrip)                                                      
  } else{paramTBL$PARAMRES=paramTBL$NUM#write.csv(paramTBL,'PARAMRES_NUM_WETWIDTH.csv')
         print (sprintf("%s is NUMBER format",param))
        
         #Make boxplots for each strata using reach averages of paramres and paramres values
         #Examples:
         #1. Boxplot for each ecoregion using reach averages
         #2. Boxplot for each stream size using reach averages
         #3. Boxplot for each site using paramres values. 
         boxplot(PARAMRES, xlab=unique(PARAMETER))   
         
         
         #boxplot 
         
         #outlier detection - percentile flags
  }
}


#iteration example
list=c(1,2,4,5,6,7)
for (i in 1:length(list)){
  if(list[i]<5){
    print(list[i] + 2)
  } else {print(list[i] *5 )}
}

#Nicole's Iteration ex
n.list=c(0,1,2,3,5,7,9)
for(i in 1:length(n.list)){ 
  if(n.list[i]<5){
    print(n.list[i]+2) 
    } else{print(n.list[i]+7)}
}
