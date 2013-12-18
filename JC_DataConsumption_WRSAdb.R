
##test push
#-------------------------------------------------------INPUTS--------------------------------------------------------#
#In the ideal world, users should only need to put inputs here and be able to get results out of the 'black box' below using existing functions.
DBpassword=''#Always leave blank when saving for security and because changes annually. Contact Sarah Judson for current password.

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
sqlTables(wrsa1314)####are any of the QA, pivot or aggregation queries dependent on these 3 lines of code; shouldn't this be in the setup
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
tblREACHtest=tblRetrieve('tblREACH',testP)
tblPOINTbank=tblRetrieve('tblPOINT',bankP)

####retrieve all tables
tblPOINT=tblRetrieve('tblPOINT')
tblTRANSECT=tblRetrieve('tblTRANSECT')
tblVERIFICATION=tblRetrieve('tblVERIFICATION')
tblREACH=tblRetrieve('tblREACH')

trial<-sqlQuery(wrsa1314,"select * from tblPOINT where PARAMETER='slope'") #basic select query must use "

getmeta<-sqlQuery(wrsa1314,"select * from tblMETADATA")#### getting the metadata table



#Close ODBC connection when done talking to SQL Server
odbcClose(wrsa1314)

#--------------------------------------------------------PIVOT VIEWS--------------------------------------------------------#
##RESHAPE to PIVOT## 
#EXAMPLES of both methods#
#SQL option ('View' creation to copy/paste)
bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')
tblPOINTbank=tblRetrieve('tblPOINT',bankP)
bankPVTstr=PVTconstruct(parameters=bankP,tblTYPE='tblPOINT', filter="POINT in ('LF','RT')");print(bankPVTstr)#- need permission from Sarah Judson and to reopen ODBC before saving Views for permanent use in SQL Server
  #retrieve said query from SQL
    wrsa1314_2=odbcDriverConnect(connection = wrsaConnectSTR)
    tblPOINTbankPVTs=sqlQuery(wrsa1314_2,bankPVTstr)
    odbcClose(wrsa1314_2)
#R option (cast)
tblPOINTbankPVTr=cast(subset(tblPOINTbank,select=c(UID, TRANSECT,POINT,PARAMETER,RESULT)), UID + TRANSECT + POINT ~ PARAMETER)#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function

########is there a delay in the sequel version showing up in R
########the R option creates a table with the site filters in it because tblPOINTbank is liknked to the Retrieve function which is linked to UID which is linked to site filters
#####however the sequel version has all sites because it is not linked to the site filter####actually it should because the function concatenates sitecodes!!
#####below is an example trying to include sitecodes as a filter but it appears to give same results as without that filter

bankPVTstr7=PVTconstruct(parameters=bankP,tblTYPE='tblPOINT', filter="POINT in ('LF','RT')" and uid in ());print(bankPVTstr)#- need permission from Sarah Judson and to reopen ODBC before saving Views for permanent use in SQL Server
#retrieve said query from SQL
wrsa1314_2=odbcDriverConnect(connection = wrsaConnectSTR)
tblPOINTbankPVTs7=sqlQuery(wrsa1314_2,bankPVTstr7)
odbcClose(wrsa1314_2)
#R option (cast)

#####on a related note how do you run the above R query with all sites and no site filters?
#####do you have to include all site codes in your filter?

#-------------------------------------------------------QAQC---------------------------------------------------------------#
#slope QA#
#---------------check all slopes are recorded in cm---------------------------
sqlQuery(wrsa1314,"select * from tblPOINT where PARAMETER='slope_units' and RESULT not like 'cm'")####returns no values (i.e. all slopes were measured in cm)####do I need to store this into an object?

#--------------check for missing slope values--------------------------------
sqlQuery(wrsa1314,"select * from tblPOINT where PARAMETER='slope' and RESULT is null")######returns no missing values

sqlQuery(wrsa1314,"select * from tblPOINT where PARAMETER='slope' and RESULT='0'")######returns 22 0s were these supposed to be null? Why do only 2 have flags?

sqlQuery(wrsa1314, "select * from tblPOINT where PARAMETER='slope'")##### display all slope values; not to hard to look at because only 1 result per transect, why is slope in point instead of transect?

####select min and max values as well as 25% and 75% values to give range of expected values for next year

#bank QA#
####bank angle
#--------------check all bank angles for missing values but will need to exclude NA at inbetween transects
#--------------check all bank angles to make sure between 0-360
##--------------------see if min(bank angle result) < 0 or if max(bank angle) > 360 if so run a query on that value to see what site and transect it goes with
####bank stability
#-----Richard checked for transcription errors
#-----don't need to check bank stability values because input into access and values were locked down
#-----check to make sure crews only called banks eroding if the they were uncovered and within 10 degrees of vertical
#-----we can only check this on transects because angle was not taken inbetween transects so we need to remove in between transects # where angle is not (na) 
#-----display bank angle and cover for stable=E ###need to create pivot

sitecodes=c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')######how do I run any functions without this filter and get all sites?
erosionalP<-c('angle','cover','stable')
tblerosional<-tblRetrieve('tblPOINT',erosionalP)####gives a warning message "only first element will be used"###do you have to concatenate the values first or could you skip that step and input them in this statment? ###why does the table need quotes around it?
#FIRST ATTEMPT
erosionalpvt<-PVTconstruct(parameters=erosionalP,tblTYPE='tblPOINT',filter="POINT in ('LF','RT')");print(erosionalpvt)##what does the print function do?# added the point in (lf and rt) to filter out what?????###why is there no parameter called stability-----lf and rt are not unique to stability 
wrsa1314_2=odbcDriverConnect(connection = wrsaConnectSTR)
tblPOINTerosional=sqlQuery(wrsa1314_2,erosionalpvt)
odbcClose(wrsa1314_2)
#SECOND ATTEMPT####why does this not create a dataframe?????
erosionalpvt2<-PVTconstruct(parameters=erosionalP,tblTYPE='tblPOINT',filter="POINT in ('LF','RT') and Stable='E'");print(erosionalpvt2)# added E to limit results to only banks that were erosional
wrsa1314_2=odbcDriverConnect(connection = wrsaConnectSTR)
tblPOINTerosional2=sqlQuery(wrsa1314_2,erosionalpvt2)
odbcClose(wrsa1314_2)

#----------or a better option would be "count if bank angle is not between 80-100 and cover=U and stable=E"

####undercut
#------------check all undercuts for missing values but will need to exclude NA at inbetween transects



#--------------------------------------------------------ANALYSIS--------------------------------------------------------#
##AGGREGATION##
#EXAMPLES#
#count number of records per parameter to check for missing data
qastatsBANK_CNTcast=cast(tblPOINTbank, UID ~ PARAMETER, value='RESULT', fun.aggregate=length)#should this filter out NULLs or flags? does EPA write a line for each record even if no value recorded?
qastatsBANK_CNTagg=aggregate(tblPOINTbank,FUN='length', by=list(tblPOINTbank$UID,tblPOINTbank$TRANSECT,tblPOINTbank$POINT))
#cast seems like the more elegant solution

qastatsBANK_CNTerosional=cast(tblerosional, UID ~ PARAMETER, value='RESULT', fun.aggregate=length)

#convert numerics before performing stats
tblPOINTbankNUM=subset(tblPOINTbank,subset= is.na(as.numeric(as.character(tblPOINTbank$RESULT)))==FALSE);tblPOINTbankNUM$RESULT=as.numeric(as.character(tblPOINTbankNUM$RESULT))
qastatsBANK_MEANcast=cast(tblPOINTbankNUM, UID ~ PARAMETER, value='RESULT', fun.aggregate=mean)

##GRTS adjusted weights##
#TBD# Pull from UTBLM

##EPA aquamet##
#TBD# Pull from aquamet 1.0 provided by Tom Kincaid and Curt Seegler via Marlys Cappaert

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
