#-------------------------------------------------------INPUTS--------------------------------------------------------#
#In the ideal world, users should only need to put inputs here and be able to get results out of the 'black box' below using existing functions.
DBpassword=''#Always leave blank when saving for security and because changes annually. Contact Sarah Judson for current password.
DBuser=''#ditto as with DBpassword
DBserver=''#ditto as with DBpassword

#createWRSAdb.db

#code to create the tables for the 2013/14 NRSA database.


#RUN PARAMETERS#
setwd('M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\WRSA_2013\\EPAexport')
MODE=c('FMGOin')#options: 'CREATE','EPAin', 'REVISE', APPin','FMGOin' #rule: EPAin and APPin are mutually exclusive
#SWJ to do: add a test mode to run test select queries and truncate the imports for testing
#SWJ to do: add a "NAMCin" mode for bank stability and photo import
##SWJ to do: add an if statement that either creates the tables or just imports them depending on whether "new" or "existing" db (call it "MODES" - CREATE, EPAimport, APPimport)
#DATA IN: app data table = one giant flat table with full spatial resolution (reach, transect, point) and don't worry about normalization and null reduction until porting to large database, shuttle into these tables based on whether finer spatial levels are NULL or not
#DATA OUT: to combine tables with additional spatial information (transect, bank), perform a union and then xwalk the fully flat table to other databases or uses
#SWJ to do: write example data in and out tables + scripts
#SWJ to do: list of incoming tables for import, fuzzy match names (EPA added date suffixes) and set table strings to variables throughout; error message if poor match to cue buildling of a manual translation array
#SWJ to do: imports need to verify that records don't already exist for that site by checking a few key records

#establish an ODBC connection#
#the db was created in SQL Server Manager on 11/19/2013
#manually set up the database (WRSAdb) and the odcb connection (WRSAconnect)
library("RODBC")
#ENTER DB PASSWORD
print ("Please enter Password")
##Establish an ODBC connection##
#the db was created in SQL Server Manager on 11/19/2013 by Sarah Judson#
wrsaConnectSTR=sprintf("Driver={SQL Server Native Client 10.0};Server=%s;Database=WRSAdb;Uid=%s; Pwd=%s;",DBserver,DBuser, DBpassword)
wrsa1314=odbcDriverConnect(connection = wrsaConnectSTR)
options(stringsAsFactors=F)
#end ODBC connection#


#SQL assistance functions
#loaded from a separate R script
source('FNC_tblRetrievePVT.R')



if('FMGOin' %in% MODE){
  source('FMimport.R')
}#end FMGO MODE



if('REVISE' %in% MODE){
  #DEPRECATION/ACTIVE checks
  #if importing revised data, export these so can add back in if needed (REVISE mode)
  #otherwise, simply use this to look at deprecated values and their corresponding revisions 
  #see also, Office_Updates for tracking in Access
  DEPstr="select * from 
(select * from %s where ACTIVE='FALSE') Dep
join (select * from %s where ACTIVE='TRUE' and 
      exists (select * from %s AS Dep 
              where ACTIVE='FALSE' 
              and DEP.UID=%s.UID 
              and DEP.Sample_Type=%s.SAMPLE_TYPE
              and DEP.Parameter=%s.Parameter
              %s--add transect and point
              --could also assume required match between insertion and deprecation dates, but that's if trying to trace multiple revisions which we haven't yet encountered (currently would see a line for each deprecation with the current active comment...should be sufficient with sorting)
      )
) as Cor
on DEP.UID=cor.UID 
and DEP.Sample_Type=cor.SAMPLE_TYPE
and DEP.Parameter=cor.Parameter
%s--add transect and point
--add deprec/insertion date match if desired
order by Cor.UID, Cor.SAMPLE_TYPE, Cor.PARAMETER -- add transect and point"
  #replace %s in the following order: (table,table,table,table,table,table,transect/point DEP-> table join (or blank),transect/point DEP->COR join (or blank))"
  
  tbls=c('tblVERIFICATION', 'tblREACH', 'tblCOMMENTS', 'tblTRANSECT', 'tblPOINT')
  for (t in 1:length(tbls)){
    if (tbls[t] =='tblTRANSECT' ){DEPjoin='and DEP.TRANSECT=tblTRANSECT.TRANSECT'; CORjoin='and DEP.TRANSECT=COR.TRANSECT' 
    } else if (tbls[t] =='tblPOINT') {DEPjoin='and DEP.TRANSECT=tblPOINT.TRANSECT and DEP.POINT=tblPOINT.POINT'; CORjoin='and DEP.TRANSECT=COR.TRANSECT and DEP.POINT=COR.POINT'
    } else {DEPjoin=''; CORjoin=''}
  tblDEP=sqlQuery(wrsa1314, sprintf(DEPstr,tbls[t],tbls[t],tbls[t],tbls[t],tbls[t],tbls[t], DEPjoin, CORjoin))
  outTBL=sprintf('DEP_%s',tbls[t])
  assign(outTBL,tblDEP)
  write.csv(tblDEP,sprintf('%s.csv',outTBL))
}
  
}


#SWJ: combine Site level (1 measurement per site visit) tables
#EPA tables combined in REACH: tblASSESSMENT, tblCHANNELCONST,  tblTORRENT
if('CREATE' %in% MODE){
  #GENERAL TABLE STRINGS#
  #SQL TABLE CREATION#
  CREATEstr="create table %s
    		(UID                 numeric(30,0)             NOT NULL,
				SAMPLE_TYPE         nvarchar(50)    NOT NULL
        %s
				,IND                 int  NOT NULL
        ,ACTIVE              nvarchar(50)    NOT NULL
        ,OPERATION           nvarchar(50)    NULL
        ,INSERTION           datetime        NULL
        ,DEPRECATION         datetime        NULL
        ,REASON              nvarchar(500)   NULL
        ,CONSTRAINT %s
        PRIMARY KEY CLUSTERED (
        UID ASC
        ,SAMPLE_TYPE ASC
        ,IND ASC, ACTIVE ASC
        %s
				) )
				"
  #replace %s in the following order: TableName, addition of [Transect] or [Point] for finer resolution tables, ConstraintName, addition to constraints
  #,IND   int IDENTITY(1,1) NOT NULL ##SWJ changed to nvarchar(50) throughout this script since EPA provides this value and likely wants it if they ever need to cross walk the data back; reimplement on the AIM database once independent from EPA
  #end TABLE STRINGS#
  
sqlQuery(wrsa1314, sprintf(CREATEstr,'tblREACH',
                           ',PARAMETER           nvarchar(50)    NOT NULL
                            ,RESULT              nvarchar(50)    NULL
                           ,FLAG                nvarchar (50)   NULL',
                           '[PK_tblREACH]',''))
sqlQuery(wrsa1314, sprintf(CREATEstr,'tblTRANSECT',
                           ',TRANSECT         nvarchar(5)    NOT NULL
                            ,PARAMETER           nvarchar(50)    NOT NULL
                            ,RESULT              nvarchar(50)    NULL
                           ,FLAG                nvarchar (50)   NULL',
                           '[PK_tblTRANSECT]',
                           ',TRANSECT ASC,PARAMETER ASC'))
sqlQuery(wrsa1314, sprintf(CREATEstr,'tblPOINT',
                           ',TRANSECT         nvarchar(5)    NOT NULL,
                           POINT        nvarchar(5)    NOT NULL
                            ,PARAMETER           nvarchar(50)    NOT NULL
                            ,RESULT              nvarchar(500)    NULL
                           ,FLAG                nvarchar (50)   NULL',
                           '[PK_tblPOINT]',
                           ',TRANSECT ASC,PARAMETER ASC, POINT ASC'))#results increased to 500 for photo descriptions, could consider porting to comments

  #!SWJ to do: needs cleanup
  sqlQuery(wrsa1314, sprintf(sub('UID                 numeric(30,0)             NOT NULL,','',CREATEstr)
                             ,'tblMetadataRange',
                             ',PARAMETER           nvarchar(50)    NOT NULL
                              ,STAT             nvarchar(50)    NULL
                            ,RESULT              nvarchar(50)    NULL
                           ,EXPLANATION                nvarchar (2000)   NULL',
                             '[PK_tblRange]',''))#!in theory, sub() should removed UID, but wasn't working, so did it manually in SQL server
  
  
  
sqlQuery(wrsa1314, sprintf(CREATEstr,'tblVERIFICATION','','[PK_tblVERIFICATION]',',PARAMETER ASC'))
VER=read.csv('tblVERIFICATIONDec22013.csv')
VER=ColCheck(VER)
sqlSave(wrsa1314,dat=VER,tablename='tblVERIFICATION',rownames=F, append=TRUE)#,fast=FALSE)

META=read.csv("M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Database\\DRAFTtblPARAMETERDESCRIPTIONS.csv")
sqlSave(wrsa1314,dat=META,tablename='tblMETADATA',rownames=F, append=TRUE)#,fast=FALSE)

XWALK=read.csv("XWALK_18Dec13add.csv")
sqlSave(wrsa1314, dat=XWALK,tablename='tblXWALK',rownames=F, append=TRUE)

#SAMPLE_TYPE= (CHEM, PERI, SEDE, BERW, BELG) 
#SWJ: Sample Table kept separate because tracks primary keys, doesn't store data

# sqlQuery(wrsa1314, sprintf(CREATEstr,'tblSAMPLES','','[PK_tblSAMPLES]',',PARAMETER ASC'))
# SAM=read.csv('tblSAMPLESOct312013.csv'); SAM=SAM[-1]
# sqlSave(wrsa1314,dat=SAM,tablename='tblSAMPLES',rownames=F, append=TRUE)#,fast=FALSE) ---> send to tblREACH instead


sqlQuery(wrsa1314, sprintf(CREATEstr,'tblCOMMENTS',#table name
                           ',TRANSECT       nvarchar (10)  NOT NULL,
                COMMENT             nvarchar(2000)  NULL,
                PAGE                nvarchar(10)           NOT NULL
                ,FLAG                nvarchar (50)   NOT NULL',#added fields
                           '[PK_tblCOMMENTS]', #constraint name
                           ',TRANSECT ASC ,FLAG ASC ,PAGE ASC'))#added constraints
#SWJ to do: what is "PAGE" in comments? why is it in the constraint?
COM=read.csv('tblCOMMENTSDec22013.csv'); COM=COM[-1]
sqlSave(wrsa1314,dat=COM,tablename='tblCOMMENTS',rownames=F, append=TRUE)#,fast=FALSE)
#had to manually paste COM into table, not sure why sqlSAVE would not work
#SWJ to do: combine SAMPLES and VERIFICATION into below structure
}#end create MODE if


#SWJ to do: in imports below, need to add to "VAR" for transect and point columns
if('EPAin' %in% MODE){
  #Apr2014 data includes revisions to NorCAl - wipe DB and reimport from scratch, keeping Deprecation records and bankstability
  VerificationFiles=c('tblVERIFICATIONApr2014')#tblverification
  ReachFiles=c('tblASSESSMENTApr2014', 'tblTORRENTApr2014','tblCHANNELCONSTApr2014','tblFIELDApr2014','tblSAMPLESApr2014')#NorCal2013 # c('tblASSESSMENTDec22013', 'tblTORRENTDec22013','tblCHANNELCONSTDec22013','tblFIELDDec22013','tblSAMPLESDec22013')
  TransectFiles=c('tblBENTSAMPApr2014','tblCHANNELApr2014','tblINVASIVEApr2014')#NorCal2013 # c('tblBENTSAMPDec22013','tblCHANNELDec22013')
  PointFiles= c('tblCHANRIPApr2014','tblCHANCROSSSECApr2014','tblSLOPEApr2014','tblTHALWEGApr2014','tblLITTORALApr2014','NAMC_BankStability')#NorCal2013 #  c('tblCHANRIPDec22013','tblCHANCROSSSECDec22013','tblSLOPEDec22013','tblTHALWEGDec22013')
  CommentFiles=c('tblCOMMENTSApr2014')
  #still need to handle comments (imported by copy paste for NorCal) and crew/tracking (currently handled in more detail in UTBLM.accdb)
  #could make this fancier using file lookups using grep from the xwalk between nrsa and wrsa, but only planning on doing this manually twice (while documenting the xwalk) and possible minor touchups if paper forms used in 2014
}
if('APPin' %in% MODE){ #this double if assumes APPin and EPAin are mutually exclusive!
  ReachFiles=('M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Database\\DB_appLITE.csv')
  PointFiles=('BankStab_ExportToSQLserverXN')#SWJ to do: photo will be a "point" with a neighboring transect and a photo number for point
  #see FM import below (in progress)
  #during import, update Transect in BankSTB as follows: TBL$TRANSECT=sub("-","",TBL$TRANSECT)
  #SWJ to do: 'TEMP_NAMC' comment added to help flag that UID needs updated  #TBL$FLAG=ifelse((TBL$FLAG)=='','TEMP_NAMC',substr(as.character(TBL$FLAG),0,49))
  #TransectFiles=TBD#if not one file just juggling nulls, also need to handle below loops and VARS a little differently depending on structure (or could structure the null elimination here)
  }

FileSETS=c('VerificationFiles','CommentFiles','ReachFiles', 'TransectFiles','PointFiles')# less --> more spatial resolution
TableSETS=c('tblVERIFICATION','tblCOMMENTS','tblREACH','tblTRANSECT','tblPOINT')#matching database table for each file set
PR=c("PARAMETER" ,  "RESULT" )
VarSETS=list(PR,c('TRANSECT','COMMENT','PAGE'),PR,c(PR,'TRANSECT'),c(PR,'TRANSECT','POINT'))#gradual additions to Var as tables expand from less to more spatial resolution
if(length(FileSETS)==length(TableSETS) & length(FileSETS)==length(VarSETS)){
  for (s in 1:length(FileSETS)){
    Files=eval(parse(text=FileSETS[s]))
    dbTable=TableSETS[s]
    if(is.na(VarSETS[s])==FALSE){VAR2=c(VAR,unlist(VarSETS[s]))}
    for (f in 1:length(Files)) {  
      TBL=read.csv(sprintf('%s%s', Files[f],ifelse(length(grep('.csv',Files[f]))==0,'.csv',''))); 
      #if having date issues (need to add to ColCheck fx) -# TBL$INSERTION=as.Date(TBL$INSERTION,"%m/%d/%Y")#TBL$DEPRECATION=replace(substr(TBL$DEPRECATION,1, nchar(TBL$DEPRECATION)-5), " ","")[-6166]
      TBL=ColCheck(TBL,VAR2)# #EPA export files in 2013 have a dummy row number column; this assumes APPin and EPAin are mutually exclusive!##old solution: if('EPAin' %in% MODE){TBL=TBL[-1]}
      #SWJ to do: need to update point and other names (done manually in SQL for first import) --> LEFT=LF, LF=LF, L=LF, RIGHT=RT, RT=RT, R=RT
      sqlSave(wrsa1314,dat=TBL,tablename=dbTable,rownames=F, append=TRUE)
  }}
} else {
  print('Import failed. Different numbers of files, tables, or columns. Verify lists in FileSETS and TableSETS and column names in VAR.')
}
#s=1 and 2 failed

#SWJ to do: could loop further over each db table

odbcClose(wrsa1314)

##see DataConsumption_WRSAdb.R for more up to date versions
#generate SQL views (possibly pass via ODBC too)
#bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')
#veriP=c('SITE_ID','SITESAMP','DATE_COL','LAT_DD','LON_DD')


#parameters=bankP
#tblTYPE='tblPOINT'
#filter="POINT in ('LF','RT')"
# parameters=veriP
# tblTYPE='tblVERIFICATION'
# filter="SITESAMP='Y'"
#SWJ to do: looks like this should be converted into a "PIVOT" funciton


# 
# PVTloop=function(parameters=c('SITE_ID'), tblTYPE=tblVERIFICATION, filter=''){
#   tblPVTstr="select UID %s %s,
# FROM (SELECT UID %s,Parameter, Result
# FROM %s) p PIVOT (min(Result) FOR Parameter IN (%s)) AS pvt
# WHERE %s
# "
#   if(tblTYPE %in% c('tblVERIFICATION', 'tblREACH')){ParamResolution=''
#                                                     } else if(tblTYPE=='tblTRANSECT'){ParamResolution=',Transect'
#                                                     } else if(tblTYPE=='tblPOINT'){ParamResolution=',Transect, Point'
#                                                     } else{ParamResolution=''}
#   for (p in 1:length(parameters)){
#     comma=ifelse(p==length(parameters),'',',')
#     Pas=sprintf("[%s] AS %s%s",parameters[p],parameters[p],comma)
#     Psq=sprintf("[%s]%s",parameters[p],comma)
#     if(p==1){loopPas=Pas; loopPsq=Psq} else{loopPas=paste(loopPas,Pas); loopPsq=paste(loopPsq,Psq)}
# }
#   sqlTEXT=sprintf(tblPVTstr, ParamResolution, loopPas, ParamResolution,tblTYPE,loopPsq,filter)
#   return(sqlTEXT)
# }

  



#SWJ- low priority


#SWJ - logistics (should be covered in the tracking db, but verify)
#tblCREW: SAMPLE_TYPE = VERIF
# 
# sqlQuery(nrsa1314, "create table tblCREW (
#          UID                 int             NOT NULL
#          ,SAMPLE_TYPE         nvarchar(50)    NOT NULL
#          ,PARAMETER           nvarchar(50)    NOT NULL
#          ,RESULT              nvarchar(50)    NULL
#          ,IND                 int IDENTITY(1,1) NOT NULL
#          ,ACTIVE              nvarchar(50)    NULL
#          ,OPERATION           nvarchar(50)    NULL
#          ,INSERTION           datetime        NULL
#          ,DEPRECATION         datetime        NULL
#          ,REASON              nvarchar(500)   NULL
#          ,CONSTRAINT [PK_tblCREW] PRIMARY KEY CLUSTERED (
#          UID ASC
#          ,SAMPLE_TYPE ASC
#          ,PARAMETER ASC
#          ,IND ASC
#          ) ON [PRIMARY]
# ) ON [PRIMARY]
#          ");
# 
# 
# #the first tables to create are tblSAMPLETRACKING and tblSITETRACKING.
# 
#      sqlQuery(nrsa1314, "create table tblTRACKING (
#                    	UID                 int             NOT NULL
# 				,SAMPLE_TYPE         nvarchar(50)    NOT NULL
# 				,SHIP_ID			 nvarchar(50)    NOT NULL
# 				,PARAMETER           nvarchar(50)    NOT NULL
# 				,RESULT              nvarchar(500)    NULL
# 				,IND                 int IDENTITY(1,1) NOT NULL
# 				,ACTIVE              nvarchar(50)    NULL
# 				,OPERATION           nvarchar(50)    NULL
# 				,INSERTION           datetime        NULL
# 				,DEPRECATION         datetime        NULL
# 				,REASON              nvarchar(500)   NULL
# 				,CONSTRAINT [PK_tblTRACKING] PRIMARY KEY CLUSTERED (
# 				UID ASC
# 				,SAMPLE_TYPE ASC
# 				,PARAMETER ASC
# 				,SHIP_ID ASC
# 				,IND ASC
# 				) ON [PRIMARY]
# 				) ON [PRIMARY]
# 				");         
# 
#       
# 
# 
# # Parameter descriptions, used in validation and to link images (FORM_TYPE to FRMIMAG)
# 
# sqlQuery(nrsa1314, "create table tblPARAMETERDESCRIPTIONS (
# 				SAMPLE_TYPE 		 nvarchar(50)    NOT NULL
#                 ,FORM_TYPE           nvarchar (50)   NOT NULL
# 				,PARAMETER           nvarchar(50)    NOT NULL
#                 ,UNITS               nvarchar (50)   NULL
#                 ,LABEL               nvarchar (1500) NULL
#                 ,VAR_TYPE            nvarchar (50)   NULL
#                 ,RANGE_HIGH          float           NULL
#                 ,RANGE_LOW           float           NULL
#                 ,LEGAL_VALUES        nvarchar (500)  NULL
#                 ,REFERENCE           nvarchar (500)  NULL
# 				,IND                 int IDENTITY(1,1) NOT NULL
# 				,ACTIVE              nvarchar(50)    NULL
# 				,OPERATION           nvarchar(50)    NULL
# 				,INSERTION           datetime        NULL
# 				,DEPRECATION         datetime        NULL
# 				,REASON              nvarchar(500)   NULL
# 				,CONSTRAINT [PK_tblPARAMETERDESCRIPTIONS] PRIMARY KEY CLUSTERED (
# 				SAMPLE_TYPE ASC
# 				,PARAMETER ASC
#                 ,FORM_TYPE ASC
# 				,IND ASC
# 				) ON [PRIMARY]
# 				) ON [PRIMARY]
# 				"); 
# 
# 
# 
# 
# 
# 
# 
