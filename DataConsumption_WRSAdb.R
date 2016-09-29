#------------------------------------------------------DOCUMENTATION------------------------------------------#
#for detailed explanation of R and SQL structure, see buglab\Research Projects\BLM_WRSA_Stream_Surveys\Technology\WRSA data management.docx
#test

#-------------------------------------------------------INPUTS--------------------------------------------------------#
#In the ideal world, users should only need to put inputs here and be able to get results out of the 'black box' below using existing functions.
DBpassword=''#Always leave blank when saving for security and because changes annually. Contact Sarah Judson for current password.
DBuser=''#ditto as with DBpassword
DBserver=''#ditto as with DBpassword
DBname=''#ditto as with DBpassword
#this is a change

#--------------------------------------------------------SETUP--------------------------------------------------------#
#LOAD required packages#
requiredPACKAGES=c('reshape', 'RODBC','ggplot2','grid','gridExtra','xlsx','sqldf','jpeg','spsurvey','tcltk')
for (r in 1:length(requiredPACKAGES)){
  if ((requiredPACKAGES[r] %in% installed.packages()[,1])==FALSE){install.packages(requiredPACKAGES[r])}#auto-install if not present
  library(requiredPACKAGES[r],character.only = TRUE)
}

#default working directory is the location of the Rproject which is custom to each collaborator and should automatically be set when project is loaded
#setwd('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\WRSA')#SWJ to do: map more dynamically but securely
#setwd('C:\\Users\\Sarah\\Desktop\\NAMCdevelopmentLocal\\WRSA')##Sarah desktop



##Establish an ODBC connection##
#the db was created in SQL Server Manager on 11/19/2013 by Sarah Judson#
wrsaConnectSTR=sprintf("Driver={SQL Server Native Client 10.0};Server=%s;Database=%s;Uid=%s; Pwd=%s;",DBserver,DBname,DBuser, DBpassword)#specify backupdatabase or orignial database
#wrsaConnectSTR=sprintf("Driver={SQL Server Native Client 10.0};Server=%s;Database=WRSAdb;Uid=%s; Pwd=%s;",DBserver,DBuser, DBpassword)
wrsa1314=odbcDriverConnect(connection = wrsaConnectSTR)
#test that connection is open # sqlQuery(wrsa1314,"select top 10 * from tblVerification")
#SWJ to do: throw this into a function that also prompts for server and password if missing (='')
#SWJ to do: throw the function into a separate referenced script because multiple files are using this

options(stringsAsFactors=F,"scipen"=50)#general option, otherwise default read is as factors which assigns arbitrary number behind the scenes to most columns

#SQL assistance functions
#loaded from a separate R script
source('FNC_tblRetrievePVT.R')
#common sQL strings that need to be incorporated:
##(select * from tblVERIFICATION where PARAMETER='site_id') v on v.UID=tblPOINT.uid



#------------------------------------------------------DEFAULTS-----------------------------------------------
#FILTERS
##from most to least specific
AllData='N'#set to 'Y' (meaning 'yes') if you want to query all sites (note this is quite time consuming and large, use provided filters wherever possible)
sitecodes=c('OT-SS-7112')#c('EL-LS-8134','EL-SS-8127','MN-LS-1004','MN-SS-1104','MS-SS-3103','XE-RO-5086','XN-LS-4016','XN-SS-4128','XS-LS-6029' )#QAduplicateSites#c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')#sites for NorCalTesting
years=c('2013','2014','2015')#as character, not number
years=c('2016')
#years=c('2015')#as character, not number
#protocols=c('BOAT14')#for separating differences in overall protocol, may not be relevant for some parameters
protocols=c('NRSA13','WRSA14','BOAT14','AK14')#for separating differences in overall protocol, may not be relevant for some parameters
protocols=c('WADE2016')
projects=c('CO_FR_STANDARD_2016','CO_NW_STANDARD_2016','ID_SA_STANDARD_2016','ID_STATE_STANDARD_2016','NM_FMD_STANDARD_2016','OR_PR_PERENNIAL_2016','UT_GR_STANDARD_2016','UT_WD_STANDARD_2016','WY_RA_STANDARD_2016')
protocols=c('WRSA14')#for separating differences in overall protocol, may not be relevant for some parameters
projects=c('WRSA','NV','GSENM','COPLT','2015ProtocolOverlap','AKEFO','NORCAL')# most useful for separating NorCal and WRSA, note that abbreviations differ between Access and SQL/FM
projects=c('OR_PR_PERENNIAL_2016')# most useful for separating NorCal and WRSA, note that abbreviations differ between Access and SQL/FM
dates=''##example:c('05/05/2005')
hitchs=c('')#NOT WORKING YET, hitch and crew level generally maintained by Access not SQL
crews=c('R1')#NOT WORKING YET, hitch and crew level generally maintained by Access not SQL#see crewKC in customrequests for possible method
filter=''#custom filter (need working knowledge of Parameter:Result pairs and SQL structure; example: "(Parameter='ANGLE' and Result>50) OR (Parameter='WETWID' and Result<=0.75))"
UIDs='BLANK'#custom filter (need working knowledge of primary keys)
QAdup='N'#set QAdup='N' to eliminate site QA duplicates
#NorCal settings: #years=c('2013','2014');projects='NorCal';protocols=c('WRSA14','NRSA13')
#WRSA QC settings: #years=c('2014'); projects='WRSA';protocols=c('WRSA14')

#PARAMETERS
#specify if desired (will make queries less intensive):
AllParam='Y'#set to 'Y' (meaning 'yes') if you want to query all parameters
testP=c('ANGLE','APPEALING','ALGAE')#test, one from each level of table
bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')


#------------------------------------------------------EXAMPLES------------------------------------------------------------------#

#Most data requests use the following basic workflow and structure. Save any custom requests created to CustomRequest_WRSAdb.R for documentation.
#CALL data in using tblRetrieve() #at least ONE filter required, Parameters NOT required, Comments optional (default is no). For possible filters, see "WRSA data managment.docx" OR use getAnywhere(tblRetrieve) and examine available varaiables in the function() inputs section.
EXAMPLEcond=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED'), Comments='N',Projects='NorCal',Years=c('2013','2014'))
#remove QA duplicates
EXAMPLEcond=removeDUP(EXAMPLEcond,QA='N')
#PIVOT data using cast() function for easier viewing. IND will be lost if need for tracking. Alternative: aggregate() function OR PVTconstruct() assists in building SQL string for custom PIVOTS in SQL Server.
EXAMPLEcondPVT=cast(EXAMPLEcond,'UID~PARAMETER',value='RESULT') 
#KEYS added for data interpretability. Any parameters stored in tblVERIFICATION are available to add. Suggested minimum additions are Site_ID + Date_COL. In this example, coordinates for mapping.
EXAMPLEcondPVT=addKEYS(EXAMPLEcondPVT ,c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))
#EXPORT results via csv
write.csv(EXAMPLEcondPVT,'ExampleConductivityCorrected_TodaysDate.csv')#pivoted does not contain IND

#Example of retrieving all raw data for an entire project
NorCal1314=tblRetrieve(ALLp='Y',Years=c('2013','2014'),Projects='NorCal')
NorCal1314subCOND=subset(NorCal1314,PARAMETER %in% c('CONDUCTIVITY','CORRECTED'))#and again subsetting it just for a few parameters like EXAMPLEcond

##---------METADATA for reference---------##
#use RODBC package sqlQuery() function, not tblRetrieve
#parameter descriptions
METADATA=sqlQuery (wrsa1314,"select * from tblMETADATA where ACTIVE='TRUE'")#see "Label" for interpretable names #be careful with SQL strings, enclose in double quote and use single quotes for text
#query a particular protocol
METADATAprotocol=sqlQuery (wrsa1314,"select * from tblMETADATAprotocol where ACTIVE='Y' and Protocol='WRSA14'")#expected counts
#compare protocols
METADATAprotocolS=sqlQuery(wrsa1314, "select distinct Result from tblverification where parameter='Protocol' union select distinct Protocol from tblmetadataprotocol")
    #METADATAprotocolS=protocols
    METADATAprotocolS=data.frame(METADATAprotocolS)
    protocolSTR1="select m.SAMPLE_TYPE,m.PARAMETER,m.UNITS,m.LABEL,m.VAR_TYPE,m.ACTIVE,m.INSERTION,m.REASON"
    protocolSTR2a="left join (select PROTOCOL,SAMPLE_TYPE as ST, PARAMETER as PM, POINTS,REPS,Insertion,NOTE  from tblMetadataProtocol where Protocol='%s' and ACTIVE='Y') %s on %s.ST=substring(m.SAMPLE_TYPE,1,len(m.SAMPLE_TYPE)-1) and %s.PM=m.parameter"
    METADATAprotocolSpairs=subset(expand.grid(P1=unclass(METADATAprotocolS)[[1]],P2=unclass(METADATAprotocolS)[[1]]),P1!=P2)
    protocolSTR3a="(isnull(%s.Points*%s.Reps,0) <> isnull(%s.Points*%s.Reps,0))"
    for (p in 1:nrow(METADATAprotocolS)){
      currP=METADATAprotocolS[p,1]
      protocolSTR1=sprintf("%s, %s.*",protocolSTR1,currP)
      protocolSTR2=sprintf("%s %s",ifelse(p==1,'',protocolSTR2),sprintf(protocolSTR2a,currP,currP,currP,currP))
      pair=subset(METADATAprotocolSpairs,P1==currP)
      for (r in 1:nrow(pair)){
        p1=pair$P1[r];p2=pair$P2[r]
        protocolSTR3=sprintf("%s %s",ifelse(p==1,'',sprintf("%s or", protocolSTR3)),sprintf(protocolSTR3a,p1,p1,p2,p2))
      }
    }
    protocolSTRdiff=sprintf("%s from tblmetadata m %s where %s order by m.SAMPLE_TYPE, m.parameter",protocolSTR1,protocolSTR2,protocolSTR3)
    protocolSTRcomp=sprintf("%s from tblmetadata m %s order by m.SAMPLE_TYPE, m.parameter",protocolSTR1,protocolSTR2)
    METADATAprotocolCOMPARE=sqlQuery(wrsa1314,protocolSTRcomp)  
    METADATAprotocolDIFF=sqlQuery(wrsa1314,protocolSTRdiff)  
    View(METADATAprotocolDIFF)
#legal values for parameters
METADATArange=sqlQuery (wrsa1314,"select * from tblMETADATArange where ACTIVE='TRUE' and Protocol='WRSA14'")#legal values
#matchup of parameters to indicators
METADATAindicators=sqlQuery (wrsa1314,"select * from tblXwalk where NAME_xwalk='MissingBackend' and type_xwalk='Indicator'")
    indicators=NULL
    for (i in 1:nrow(METADATAindicators)){indicators=paste(indicators,METADATAindicators$Parameter_Xwalk[i],sep="|")}
    indicators=unique(unlist(strsplit(indicators,"\\|")));indicators=indicators[2:length(indicators)];indicators=gsub(" ","",indicators)
    for (p in 1:length(indicators)){
      METADATAparameters=sqlQuery (wrsa1314,sprintf("select * from tblXwalk where NAME_xwalk='MissingBackend' and PARAMETER_Xwalk like '%%%s%%'",indicators[p]))
      METADATAparameters$INDICATOR=indicators[p]
      if(p==1){parameters=METADATAparameters} else{parameters=rbind(parameters,METADATAparameters)}
    }  
    View(indicators);View(parameters)


#--------------------------------------------------------SQL RETRIEVE (old examples)--------------------------------------------------------#

#select samples
UIDs=UIDselect(ALL=AllData,Filter=filter,UIDS='',SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
#SWJ to do: add additional filters
#SWJ to do: prompt for data entry (mini-GUI)


#retrieve all data as a single list table
UnionTBL=tblRetrieve(Table='',Parameters='',ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)


Sites=subset(UnionTBL,select=c(UID,RESULT),subset=PARAMETER=='SITE_ID'); colnames(Sites)=c('UID','SITE_ID')#!append sitecode instead of UID to make the table more readable --> migrate this into tblRetrieve or some kind of "convert" function
UnionTBL=merge(UnionTBL,Sites)
UnionTBL$SITE_ID=as.character(UnionTBL$SITE_ID)
UnionTBL1=merge(UnionTBL,UIDs)#limit by UIDs ("select samples)

#retrieve desired tables
#EXAMPLES of tblRetrieve function# (note: parameter lists were specified in the "Inputs" section at the beginning of this script)
tblREACH=tblRetrieve('tblREACH')#not specifying parameters will retrieve the entire table
tblREACHtest=tblRetrieve('tblREACH',testP)
tblPOINTbank=tblRetrieve('tblPOINT',bankP)
#SWJ to do - could add GIS tables (pull from PilotDB if possible)
#SWJ to do - could add logistics tables (pull from UTBLM.accdb)



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

#iteration example
list=c(1,2,4,6,7)
for (i in 1:length(list)){
  if(list[i]<5){
    print(list[i] + 2)
  } else {print(list[i] *5 )}
}


##QA checks##
##!QA checks moved to DataQA_WRSA


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
