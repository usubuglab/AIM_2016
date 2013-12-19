#-------------------------------------------------------INPUTS--------------------------------------------------------#
#In the ideal world, users should only need to put inputs here and be able to get results out of the 'black box' below using existing functions.
DBpassword=''#Always leave blank when saving for security and because changes annually. Contact Sarah Judson for current password.
DBuser=''#ditto as with DBpassword
DBserver=''#ditto as with DBpassword
#this is a change

#FILTERS
##from most to least specific
AllData='Y'#set to 'Y' (meaning 'yes') if you want to query all sites (note this is quite time consuming and large, use provided filters wherever possible)
sitecodes=c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')
dates=c('05/05/2005')
hitchs=c('')#NOT WORKING YET
crews=c('R1')#NOT WORKING YET
projects=c('NRSA')#NOT WORKING YET
years=c(2013)#NOT WORKING YET


#PARAMETERS
#specify if desired (will make queries less intensive):
AllParam='Y'#set to 'Y' (meaning 'yes') if you want to query all parameters
testP=c('ANGLE','APPEALING','ALGAE')#test, one from each level of table
bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')



#--------------------------------------------------------SETUP--------------------------------------------------------#
#LOAD required packages#
requiredPACKAGES=c('reshape', 'RODBC')
for (r in 1:length(requiredPACKAGES)){
  if ((requiredPACKAGES[r] %in% installed.packages()[,1])==FALSE){install.packages(requiredPACKAGES[r])}#auto-install if not present
  library(requiredPACKAGES[r],character.only = TRUE)
}

#setwd('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\WRSA')#SWJ to do: map more dynamically but securely

##Establish an ODBC connection##
#the db was created in SQL Server Manager on 11/19/2013 by Sarah Judson#
wrsaConnectSTR=sprintf("Driver={SQL Server Native Client 10.0};Server=%s;Database=WRSAdb;Uid=%s; Pwd=%s;",DBserver,DBuser, DBpassword)
wrsa1314=odbcDriverConnect(connection = wrsaConnectSTR)
#SWJ to do: throw this into a function that also prompts for server and password if missing (='')
#SWJ to do: throw the function into a separate referenced script because multiple files are using this


#SQL assistance functions
#loaded from a separate R script
#common sQL strings that need to be incorporated:
##(select * from tblVERIFICATION where PARAMETER='site_id') v on v.UID=tblPOINT.uid


source('FNC_tblRetrievePVT.R')



#--------------------------------------------------------SQL RETRIEVE--------------------------------------------------------#
#SQL tables are created and imports managed via an independent R scripts (createNRSA1314db_SWJ.r)
dbTBL=sqlTables(wrsa1314, tableType="TABLE")#all possible tables
dbCOL=sqlColumns(wrsa1314,"tblPOINT")#column names (similar structure for each tbl since flat)
dbPARAM=sqlQuery(wrsa1314,'Select SAMPLE_TYPE, PARAMETER, LABEL from tblMETADATA')#parameter names (SWJ to do: iterate over Sample_Type groups to generate pivots)
tmpTYPE=as.character(unique(dbPARAM$SAMPLE_TYPE))
dbTYPE=substr(tmpTYPE,1,nchar(tmpTYPE) - 1)#substr to get rid of the random "x" at the end of each name
#UID and parameter prep (SWJ to do: could roll into a selectUID function and into the final sql call function (i.e. the winner btwn tblRetrieve and PvTconstruct))
UIDall=ifelse(AllData=='Y','%','')#swj to do: add | (or) for if all possible filters ==''
parameters=ifelse(AllParam='Y','',NULL)#functions interpret '' as all parameters, otherwise it is a specified list in the function input

#select samples
UIDs=sqlQuery(wrsa1314, sprintf("select distinct UID from tblVERIFICATION 
                                where (active='TRUE') 
                                AND ((Parameter='SITE_ID' and Result in (%s)) OR (Parameter='DATE_COL' and Result in (%s)) OR (UID like '%s'))"
                                ,inLOOP(sitecodes),inLOOP(dates),UIDall))
#SWJ to do: add additional filters
#SWJ to do: prompt for data entry (mini-GUI)


#retrieve desired tables
#EXAMPLES of tblRetrieve function# (note: parameter lists were specified in the "Inputs" section at the beginning of this script)
tblREACH=tblRetrieve('tblREACH')#not specifying parameters will retrieve the entire table
tblREACHtest=tblRetrieve('tblREACH',testP)
tblPOINTbank=tblRetrieve('tblPOINT',bankP)

#retrieve all possible tables by protocol groups and pivot
tblCOL=c('UID', 'PARAMETER','RESULT')
pvtCOL='UID %s ~ PARAMETER'
for (t in 1:nrow(dbTBL)){
  tblNAME=dbTBL$TABLE_NAME[t]
  tbl=tblRetrieve(tblNAME)
  if(min(c('SAMPLE_TYPE',tblCOL) %in% colnames(tbl))==1){#if minimum needed columns are present, proceed, otherwise assume it is a pivoted or otherwise human readable table
      if(tblNAME=='tblPOINT'){tblCOL2=append(tblCOL,c('TRANSECT','POINT'), after=1); pvtCOL2=sprintf(pvtCOL,'+ TRANSECT + POINT')
  } else if (tblNAME=='tblTRANSECT'){tblCOL2=append(tblCOL,'TRANSECT', after=1); pvtCOL2=sprintf(pvtCOL,'+ TRANSECT')
  } else {tblCOL2=tblCOL; pvtCOL2=sprintf(pvtCOL,'')}
  for(s in 1:length(dbTYPE)){
    tblPVT=cast(subset(tbl,select=tblCOL2, subset=SAMPLE_TYPE %in% dbTYPE[s]), eval(parse(text=pvtCOL2)))#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function
    if(nrow(tblPVT)>1 & is.na(tblPVT$UID)==FALSE){#only assign pivot to variable if not empty
      assign(sprintf('%s_pvt_%s',tblNAME,dbTYPE[s]),tblPVT)   
    }
  }
}
}

#Close ODBC connection when done talking to SQL Server
odbcClose(wrsa1314); rm(DBpassword); rm(DBserver); rm(DBuser)

#--------------------------------------------------------CUSTOM PIVOT VIEWS--------------------------------------------------------#
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

##QA checks##
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

##GRTS adjusted weights##
#TBD# Pull from UTBLM

##EPA aquamet##
#TBD# Pull from aquamet 1.0 provided by Tom Kincaid and Curt Seegler via Marlys Cappaert
#go to NRSAmetrics_SWJ.R

##OE computation##
#TBD# Pull from VanSickle

#Predicted WQ##
#TBD#  Pull from UTBLM, John Olson/Ryan Hill

##NMDS##
#TBD#

##GIS connections##
#TBD#


#--------------------------------------------------------REPORTING--------------------------------------------------------#
##Figures and Tables##
#TBD# Pull from UTBLM

##SWEAVE##
#TBD#

##BibTex##
#TBD#


#--------------------------------------------------------sarah's Gibberish-------------------------------------------------------#
# #pseudocode - consume data from flat db
# #ODBC connection to SQL server WRSAdb
# #import via SQL string call - include filters on data (i.e. hitch, project, crew)
# #mash (merge) tables (?) OR pvt for viewing (?) -- SQL: view, Access: Query
# ##ex (old): merge(EVENT3,subset(original,select=selectCOLparse),by="SampleID")
# ##demonstrate complexity of calling by column name vs. parameter text in both SQL and R
# ###SQL: filter query --> possibly PIVOT to view --> aggregate query
# ###R: filter strings, apply across multiple --> PVT to view --> aggregate OR run predefined(EPA,R)/custom functions
# ###common: convert numerics
# ###differences: null handling, reproducability and documentation
# ###leaning (SWJ): R for dynamic queries/code, reproducability and 'instant' documentation; in either mode, PIVOTS should be treated as temporary views for scanning data, not basis for subsequent queries because they will then be tied to column names 
# ##ex: library('reshape'); cast(data, x~y)
# #separate numbers and characters (will R autodetect?) -- SQL: Cast/Convert
# #filter by parameter and run metric  -- SQL: sub-queries
# ##ex: subset(tblPOINT, subset=Parameter=='Angle')
# ##could set it up so that user doesn't even need to which table
# ##set up to easily call the parameters table and other metadata (crew, hitch) tables --> will we store crew and hitch info in sampletracking access or SQL server?
# #aggregate by site and crew  -- SQL: group by (aggregate) query
# ##ex (old): aggregate(x=as.numeric(sampDATAin$SampleID),FUN=agg3,by=list(sampDATAin$SamplingEvent,sampDATAin$Station,sampDATAin$WaterYear))
# #report -- R SWEAVE vs. Access report vs. Crystal Reports
# 
# #check for existing packages
# #install.packages('reshape')
# library('reshape')
# 
# #establish an ODBC connection#
# #the db was created in SQL Server Manager on 11/19/2013
# #manually set up the database (WRSAdb) and the odcb connection (WRSAconnect)
# library("RODBC")
# user='feng'
# #ENTER DB PASSWORD
# print ("Please enter Password")
# password='Something~Clever!@'#("Enter Password")#raw_input() in python, not sure of R equivalent #http://rosettacode.org/wiki/Dynamic_variable_names#R
# nrsa1314<-odbcConnect("WRSAconnect",uid=user,pwd=password)
# #SQL assistance functions
# #inLOOP: concatenate list objects into an "IN" string for insertion into queries
# inLOOP=function(inSTR) {
#   inSTR=unlist(inSTR)
#   for (i in 1:length(inSTR)){
#     comma=ifelse(i==length(inSTR),'',',')
#     STRl=sprintf("'%s'%s",inSTR[i],comma)
#     if(i==1){loopSTR=STRl} else{loopSTR=paste(loopSTR,STRl)}
#   }   
#   return(loopSTR) 
# }
# #tblRetrieve: standard retrieval query
# tblRetrieve=function(table, parameters=''){
#   if(parameters==''){parameters=sqlQuery(nrsa1314,sprintf("select distinct parameter from %s", table))}
#   sqlTABLE=sqlQuery(nrsa1314, sprintf('select * from %s where UID in (%s) and parameter in (%s)',table, inLOOP(UIDs),inLOOP(parameters)))
#   return(sqlTABLE)#could auto return the pivoted view, but currently assuming that is for on the fly viewing and is not the easiest way to perform metrics
# }
# 
# 
# #FILTERS
# ##from most to least specific
# sitecodes=c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')
# dates=c('05/05/2005')
# hitchs=c('')
# crews=c('R1')
# projects=c('NRSA')
# 
# 
# 
# #select samples
# UIDs=sqlQuery(nrsa1314, sprintf("select distinct UID from tblVERIFICATION 
#                                 where (active='TRUE') 
#                                 AND ((Parameter='SITE_ID' and Result in (%s)) OR (Parameter='DATE_COL' and Result in (%s)))"
#                                 ,inLOOP(sitecodes),inLOOP(dates)))
# #SWJ to do: add additional filters
# #SWJ to do: prompt for data entry (mini-GUI)
# 
# #PARAMETERS
# #specify if desired (will make queries less intensive):
# testP=c('ANGLE','APPEALING','ALGAE')#test, one from each level of table
# bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')
# 
# #retrieve desired tables
# tblREACH=tblRetrieve('tblREACH')#not specifying parameters will retrieve the entire table
# tblREACHtest=tblRetrieve('tblREACH',testP)
# tblPOINTbank=tblRetrieve('tblPOINT',bankP)
# 
# #pivot tables for viewing
# tblPOINTbankPVT=cast(subset(tblPOINTbank,select=c(UID, TRANSECT,POINT,PARAMETER,RESULT)), UID + TRANSECT + POINT ~ PARAMETER)#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function
# 
# #further subset data in custom ways
# 
# #compute aggregate statistics
# #count number of records per parameter to check for missing data
# qastatsBANK_CNTcast=cast(tblPOINTbank, UID ~ PARAMETER, value='RESULT', fun.aggregate=length)#should this filter out NULLs or flags? does EPA write a line for each record even if no value recorded?
# qastatsBANK_CNTagg=aggregate(tblPOINTbank,FUN='length', by=list(tblPOINTbank$UID,tblPOINTbank$TRANSECT,tblPOINTbank$POINT))
# #cast seems like the more elegant solution
# #convert numerics before performing stats
# tblPOINTbankNUM=subset(tblPOINTbank,subset= is.na(as.numeric(as.character(tblPOINTbank$RESULT)))==FALSE);tblPOINTbankNUM$RESULT=as.numeric(as.character(tblPOINTbankNUM$RESULT))
# qastatsBANK_MEANcast=cast(tblPOINTbankNUM, UID ~ PARAMETER, value='RESULT', fun.aggregate=mean)
# 
# #plugging into aquamet
# 
# #end ODBC connection#
# odbcClose(nrsa1314)
