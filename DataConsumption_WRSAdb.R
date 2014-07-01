#-------------------------------------------------------INPUTS--------------------------------------------------------#
#In the ideal world, users should only need to put inputs here and be able to get results out of the 'black box' below using existing functions.
DBpassword=''#Always leave blank when saving for security and because changes annually. Contact Sarah Judson for current password.
DBuser=''#ditto as with DBpassword
DBserver=''#ditto as with DBpassword
#this is a change

#FILTERS
##from most to least specific
AllData='N'#set to 'Y' (meaning 'yes') if you want to query all sites (note this is quite time consuming and large, use provided filters wherever possible)
sitecodes=c('EL-LS-8134','EL-SS-8127','MN-LS-1004','MN-SS-1104','MS-SS-3103','XE-RO-5086','XN-LS-4016','XN-SS-4128','XS-LS-6029' )#QAduplicateSites#c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')#sites for NorCalTesting
dates=c('05/05/2005')
hitchs=c('')#NOT WORKING YET
crews=c('R1')#NOT WORKING YET
projects=c('NRSA')#NOT WORKING YET, most useful for separating NorCal and WRSA
years=c(2013)#NOT WORKING YET


#PARAMETERS
#specify if desired (will make queries less intensive):
AllParam='Y'#set to 'Y' (meaning 'yes') if you want to query all parameters
testP=c('ANGLE','APPEALING','ALGAE')#test, one from each level of table
bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')



#--------------------------------------------------------SETUP--------------------------------------------------------#
#LOAD required packages#
requiredPACKAGES=c('reshape', 'RODBC','ggplot2','grid','gridExtra','xlsx','sqldf')
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

options(stringsAsFactors=F)#general option, otherwise default read is as factors which assigns arbitrary number behind the scenes to most columns

#SQL assistance functions
#loaded from a separate R script
source('FNC_tblRetrievePVT.R')
#common sQL strings that need to be incorporated:
##(select * from tblVERIFICATION where PARAMETER='site_id') v on v.UID=tblPOINT.uid


#--------------------------------------------------------SQL RETRIEVE--------------------------------------------------------#
#SQL tables are created and imports managed via an independent R scripts (createNRSA1314db_SWJ.r)
dbTBL=sqlTables(wrsa1314, tableType="TABLE")#all possible tables
dbCOL=sqlColumns(wrsa1314,"tblPOINT")#column names (similar structure for each tbl since flat)
dbPARAM=sqlQuery(wrsa1314,"Select SAMPLE_TYPE, PARAMETER, LABEL,VAR_TYPE from tblMETADATA where ACTIVE='TRUE'")#parameter names (SWJ to do: iterate over Sample_Type groups to generate pivots)
tmpTYPE=as.character(unique(dbPARAM$SAMPLE_TYPE))
dbTYPE=substr(tmpTYPE,1,nchar(tmpTYPE) - 1)#substr to get rid of the random "x" at the end of each name
#UID and parameter prep (SWJ to do: could roll into a selectUID function and into the final sql call function (i.e. the winner btwn tblRetrieve and PvTconstruct))
UIDall=ifelse(AllData=='Y','%','')#swj to do: add | (or) for if all possible filters ==''
parameters=ifelse(AllParam=='Y','',NULL)#functions interpret '' as all parameters, otherwise it is a specified list in the function input

#select samples
UIDs=sqlQuery(wrsa1314, sprintf("select distinct UID from tblVERIFICATION 
                                where (active='TRUE') 
                                AND ((Parameter='SITE_ID' and Result in (%s)) OR (Parameter='DATE_COL' and Result in (%s)) OR (UID like '%s'))"
                                ,inLOOP(sitecodes),inLOOP(dates),UIDall))
#SWJ to do: add additional filters
#SWJ to do: prompt for data entry (mini-GUI)


#retrieve all data as a single list table
UnionTBL=sqlQuery(wrsa1314,#SWJ to do remove redundancy with XwalkUnion in NRSAmetrics_SWJ, likely move into tblRetrieve (if not table is specified)
"select *
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
where ACTIVE='TRUE'
")

Sites=subset(UnionTBL,select=c(UID,RESULT),subset=PARAMETER=='SITE_ID'); colnames(Sites)=c('UID','SITE_ID')#append sitecode instead of UID to make the table more readable --> migrate this into tblRetrieve or some kind of "convert" function
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


#retrieve all possible tables by protocol groups and pivot
#for exploratory purposes to review data and determine expected values, not intended to replace modular SQL solutions for multiple tools
tblCOL=c('UID', 'PARAMETER','RESULT')
pvtCOL='UID %s ~ PARAMETER';pvtCOLdefault=sprintf(pvtCOL,'')
AggLevel='Site'#options = Site, All
params_N=subset(dbPARAM, subset=VAR_TYPE=='NUMERIC')
params_C=subset(dbPARAM, subset=VAR_TYPE=='CHARACTER')#also used in boxplot QA (with some modifications)
for (t in 1:nrow(dbTBL)){
  tblNAME=dbTBL$TABLE_NAME[t]
  tbl=tblRetrieve(tblNAME)#could simplify and use UnionTBL
  if(min(c('SAMPLE_TYPE',tblCOL) %in% colnames(tbl))==1){#if minimum needed columns are present, proceed, otherwise assume it is a pivoted or otherwise human readable table
      if(tblNAME=='tblPOINT'){tblCOL2=append(tblCOL,c('TRANSECT','POINT'), after=1); pvtCOL2=sprintf(pvtCOL,'+ TRANSECT + POINT')
  } else if (tblNAME=='tblTRANSECT'){tblCOL2=append(tblCOL,'TRANSECT', after=1); pvtCOL2=sprintf(pvtCOL,'+ TRANSECT')
  } else {tblCOL2=tblCOL; pvtCOL2=pvtCOLdefault}
  for(s in 1:length(dbTYPE)){#this hits it with a hammer, could narrow down via xwalk to only the relevant sample_types
    #raw data (one value per pivot cell which is per transect/point per parameter)
    tblTYPE=subset(tbl,select=tblCOL2, subset=SAMPLE_TYPE %in% dbTYPE[s])
    tblPVT=cast(tblTYPE, eval(parse(text=pvtCOL2)))#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function
    if(nrow(tblPVT)>1 & is.na(tblPVT$UID)==FALSE){#only assign pivot to variable if not empty and only dive into subsequent if not empty
      assign(sprintf('%s_pvt_%s',tblNAME,dbTYPE[s]),tblPVT) 
    #missing data checks (counted values per pivot cell which is per site per parameter)
    tblPVTm=cast(tblTYPE, eval(parse(text=pvtCOLdefault)),fun.aggregate='length')
      assign(sprintf('%s_pvtMISSINGcnt_%s',tblNAME,dbTYPE[s]),tblPVTm)
      #missing context of expected number of values...need to compare to metadata
    #summarized categorical data (counted values per pivot cell which is per site per parameter+result)
    tblCAT=subset(tblTYPE,subset=PARAMETER %in% params_C$PARAMETER)
      if(nrow(tblCAT)>1){#only assign pivot to variable if not empty and only dive into subsequent if not empty
        pvtCOL3=paste(pvtCOLdefault,"+ RESULT")
        tblCAT$CNT=1
        tblPVTc=cast(tblCAT, eval(parse(text=pvtCOL3)),fun.aggregate='length', value='CNT')
        assign(sprintf('%s_pvtCATdistb_%s',tblNAME,dbTYPE[s]),tblPVTc)
      }
    #summarzied quantitative data (average values per pivot cell which is per site per parameter)
    tblNUM=subset(tblTYPE,subset=PARAMETER %in% params_N$PARAMETER )
      if(nrow(tblNUM)>1){#only assign pivot to variable if not empty and only dive into subsequent if not empty
        if(AggLevel=='Site'){pvtCOL4='UID + PARAMETER ~.'; pvtCOL5='RESULT~UID + PARAMETER'; colUID='tblPVTnSUM2$UID';nameUID=c('UID','PARAMETER','Quant1','Quant2')} else if (AggLevel=='All') {pvtCOL4='PARAMETER ~.';pvtCOL5='RESULT~PARAMETER';colUID='';nameUID=c('PARAMETER','Quant1','Quant2')}
        tblNUM$RESULT=as.numeric(tblNUM$RESULT)
        tblNUM=subset(tblNUM,subset= is.na(RESULT)==FALSE)#apparently not removing NAs during pivot aggregation, so done manually because causing errors - have to do after conversion to number
        tblPVTn=cast(tblNUM, eval(parse(text=pvtCOLdefault)),fun.aggregate='mean')#pivot reach average by site
        tblPVTnSUM1=cast(tblNUM, eval(parse(text=pvtCOL4)),fun.aggregate=c(length,mean,median,min,max,sd),fill='NA') # pivot summary stats by all sites combined or individual sites
        tblPVTnSUM2=aggregate(eval(parse(text=pvtCOL5)),data=tblNUM,FUN='quantile',probs=c(0.25,0.75),names=FALSE)
        tblPVTnSUM2=data.frame(cbind(eval(parse(text=colUID)),tblPVTnSUM2$PARAMETER,tblPVTnSUM2$RESULT[,1],tblPVTnSUM2$RESULT[,2]));colnames(tblPVTnSUM2)=nameUID
        tblPVTnSUM=merge(tblPVTnSUM1,tblPVTnSUM2,by=setdiff(nameUID,c('Quant1','Quant2')))
        #need to do this by UID for WRSA13 QA duplicate comparison
        assign(sprintf('%s_pvtQUANTmean_%s',tblNAME,dbTYPE[s]),tblPVTn)
        assign(sprintf('%s_pvtSUMMARYn_%s_%s',tblNAME,dbTYPE[s],AggLevel),tblPVTnSUM)
        
      }
    }
  }
}
}

#export results
QUANTtbls=grep('pvtQUANTmean',ls(),value=T)
for (t in 1:length(QUANTtbls)){
  write.csv(eval(parse(text=QUANTtbls[t])),sprintf('%s.csv',QUANTtbls[t]))#could merge-pvtQUANTmean_, but I like them grouped by categories
}
#could export _pvtCATdistrb_, but I find the Categorical variables not to be readily interpretable (also didn't make a summary table for them yet because of this)
rm(pvtSUMMARYn)
nSUMtbls=grep('pvtSUMMARYn',ls(),value=T)
nSUMtbls=grep(AggLevel,nSUMtbls,value=T)
for (t in 1:length(nSUMtbls)){
  tblPVTnSUM=eval(parse(text=nSUMtbls[t]))
  if( ncol(tblPVTnSUM)==1) {} else{
    if (t==1) {pvtSUMMARYn=tblPVTnSUM} else {pvtSUMMARYn=rbind(pvtSUMMARYn,tblPVTnSUM)}
  }
  write.csv(pvtSUMMARYn,sprintf('pvtSUMMARYn_%s.csv',AggLevel))
}



#why is tblPOINt_pvt_BANKW coming thru with just ones?

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
#DONE#fix sample size annotation to work for categorical
#DONE#bin - allow toggle on or off
#DONE#list of "numeric" categorical variables
#DONE#troubleshoot outlier labelling (especially for categorical) #DONE for numeric# print figures by print(`SITE: AR-LS-8007 ~ PARAM: EROSION`)
#DONE#hiccuping on BANKW:STABLE (full loop run on 2/18/14)
#DONE#fix all sites to match UIDs (see comment on allsites)
#DONE:#consider outlier significance testing (Scott-->simple Mean + SD (1 or 2)) or  #EXAMPLE: see values that are ACTIVE='FALSE' to see common transctiprion errors and if outlier scans would catch them
#print cv or other metric as a warning for the spread? compare cv of site to avg cv of all/strata sites? (Scott--> cv only for repeatable data, didn't give alternate spread)
#DONE:determine combos for HumanInfl, VisRip, Assesment, and Torrent
#DONE:excel export of "problem" sites and parameters (so can narrow down search)
#mimic labelling of site boxplots in strata
#add "all" boxplot in strata


#iteration example
list=c(1,2,4,6,7)
for (i in 1:length(list)){
  if(list[i]<5){
  print(list[i] + 2)
} else {print(list[i] *5 )}
}



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
