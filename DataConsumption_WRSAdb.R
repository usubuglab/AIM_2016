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
requiredPACKAGES=c('reshape', 'RODBC','ggplot2','grid','gridExtra')
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


#SQL assistance functions
#loaded from a separate R script
source('FNC_tblRetrievePVT.R')
#common sQL strings that need to be incorporated:
##(select * from tblVERIFICATION where PARAMETER='site_id') v on v.UID=tblPOINT.uid


#--------------------------------------------------------SQL RETRIEVE--------------------------------------------------------#
#SQL tables are created and imports managed via an independent R scripts (createNRSA1314db_SWJ.r)
dbTBL=sqlTables(wrsa1314, tableType="TABLE")#all possible tables
dbCOL=sqlColumns(wrsa1314,"tblPOINT")#column names (similar structure for each tbl since flat)
dbPARAM=sqlQuery(wrsa1314,'Select SAMPLE_TYPE, PARAMETER, LABEL,VAR_TYPE from tblMETADATA')#parameter names (SWJ to do: iterate over Sample_Type groups to generate pivots)
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
UnionTBL=merge(UnionTBL,UIDs)#limit by UIDs ("select samples)
#append sitecode instead of UID to make the table more readable --> migrate this into tblRetrieve or some kind of "convert" function
Sites=subset(UnionTBL,select=c(UID,RESULT),subset=PARAMETER=='SITE_ID'); colnames(Sites)=c('UID','SITE_ID')
UnionTBL=merge(UnionTBL,Sites)
UnionTBL$SITE_ID=as.character(UnionTBL$SITE_ID)

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
params_N=subset(dbPARAM, subset=VAR_TYPE=='NUMERIC')
params_C=subset(dbPARAM, subset=VAR_TYPE=='CHARACTER')
for (t in 1:nrow(dbTBL)){
  tblNAME=dbTBL$TABLE_NAME[t]
  tbl=tblRetrieve(tblNAME)
  if(min(c('SAMPLE_TYPE',tblCOL) %in% colnames(tbl))==1){#if minimum needed columns are present, proceed, otherwise assume it is a pivoted or otherwise human readable table
      if(tblNAME=='tblPOINT'){tblCOL2=append(tblCOL,c('TRANSECT','POINT'), after=1); pvtCOL2=sprintf(pvtCOL,'+ TRANSECT + POINT')
  } else if (tblNAME=='tblTRANSECT'){tblCOL2=append(tblCOL,'TRANSECT', after=1); pvtCOL2=sprintf(pvtCOL,'+ TRANSECT')
  } else {tblCOL2=tblCOL; pvtCOL2=pvtCOLdefault}
  for(s in 1:length(dbTYPE)){
    #raw data (one value per pivot cell which is per transect/point per parameter)
    tblTYPE=subset(tbl,select=tblCOL2, subset=SAMPLE_TYPE %in% dbTYPE[s])
    tblPVT=cast(tblTYPE, eval(parse(text=pvtCOL2)))#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function
    if(nrow(tblPVT)>1 & is.na(tblPVT$UID)==FALSE){#only assign pivot to variable if not empty and only dive into subsequent if not empty
      assign(sprintf('%s_pvt_%s',tblNAME,dbTYPE[s]),tblPVT) 
    #missing data checks (counted values per pivot cell which is per site per parameter)
    tblPVTm=cast(tblTYPE, eval(parse(text=pvtCOLdefault)),fun.aggregate='length')
      assign(sprintf('%s_pvtMISSINGcnt_%s',tblNAME,dbTYPE[s]),tblPVTm)
    #summarized categorical data (counted values per pivot cell which is per site per parameter+result)
    tblCAT=subset(tblTYPE,subset=PARAMETER %in% params_C$PARAMETER)
    pvtCOL3=paste(pvtCOLdefault,"+ RESULT")
    tblPVTc=cast(tblCAT, eval(parse(text=pvtCOL3)),fun.aggregate='length', value='POINT')
      assign(sprintf('%s_pvtCATdistb_%s',tblNAME,dbTYPE[s]),tblPVTc)
    #summarzied quantitative data (average values per pivot cell which is per site per parameter)
    tblNUM=subset(tblTYPE,subset=PARAMETER %in% params_N$PARAMETER)
    tblNUM$RESULT=as.numeric(tblNUM$RESULT)
    tblPVTn=cast(tblNUM, eval(parse(text=pvtCOLdefault)),fun.aggregate='mean')
      assign(sprintf('%s_pvtQUANTmean_%s',tblNAME,dbTYPE[s]),tblPVTn)
    }
  }
}
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
give.n <- function(x){#function for adding sample size to boxplots
  return(data.frame(y = max(x)+1, label = paste("n =",length(x))))
}

allparams=unique(paste(UnionTBL$SAMPLE_TYPE,UnionTBL$PARAMETER,sep=" "))#numeric: allparams=c("BANKW INCISED", "BANKW WETWID" )#categorical: allparams=c("CROSSSECW SIZE_CLS","HUMINFLUW WALL")
for (p in 1:length(allparams)){#this is a standard loop for iterating, could put it in a function that allows you to plug in a string for the most nested middle
  typeparam=strsplit(allparams[p]," ")
  type=typeparam[[1]][[1]]; param=typeparam[[1]][[2]]
  paramTBL=subset(UnionTBL,subset=PARAMETER==param & SAMPLE_TYPE==type)
  paramTBL$CHAR=as.character(paramTBL$RESULT)
  paramTBL$NUM=as.numeric(paramTBL$CHAR)
  #iterate over strata: sites, all values combined, ecoregion/climatic, size
  #example: extract size from SITE_ID - UnionTBL$SIZE=substr(UnionTBL$SITE_ID,4,5)
  if(is.na(min(paramTBL$NUM)) & is.na(max(paramTBL$NUM))){paramTBL$PARAMRES=paramTBL$CHAR
                                                          print (sprintf("%s is CHARACTER format",param))
      # hist(PARAMRES) #histogram - inclu "pseudo categorical" (densiom, visrip)           --> use params_C  + all LWD ; ; also need a way to assign levels (especially for pebbles) and to combine a few categories (namely LWD, and (x)SIZE_CLS)
                                                          #commented snippets of code below for categorical that needs to find it's most organized happy home and most consilence with existing results for continuous data
  } else{paramTBL$PARAMRES=paramTBL$NUM#write.csv(paramTBL,'PARAMRES_NUM_WETWIDTH.csv')
         print (sprintf("%s is NUMBER format",param))}
     #boxplot
      #outlier detection - percentile flags
         #EXAMPLE: see values that are ACTIVE='FALSE' to see common transctiprion errors and if outlier scans would catch them
         
         #NC
         #Make boxplots for each strata using reach averages of paramres and paramres values
         #Examples:
         #1. Boxplot for each ecoregion using reach averages
         #2. Boxplot for each stream size using reach averages
         #3. Boxplot for each site using paramres values. 
         #boxplot(PARAMRES, xlab=unique(PARAMETER))  

        for (n in 1:numstrata) {
           paramTBL3=paramTBL
           paramTBL3$STRATATYPE=typestrata[n]
           paramTBL3$STRATA=unlist(paramTBL3[typestrata[n]])#paramTBL3$STRATA='UNK'
           if (n==1) { paramTBL2=paramTBL3
           } else { 
             paramTBL2=rbind(paramTBL2,paramTBL3)
           } }
         strata=unique(paste(paramTBL2$STRATATYPE,paramTBL2$STRATA,sep="_" ))
         paramTBL3=aggregate(PARAMRES~SITE_ID+PARAMETER+STRATATYPE+STRATA,data=paramTBL2,FUN=mean)#if(is.numeric(paramTBL3$PARAMRES)){}
#          paramTBL3a=aggregate(IND~PARAMRES+SITE_ID+PARAMETER+STRATATYPE+STRATA,data=paramTBL2,FUN=length)#if(is.char)
#          paramTBL3b=aggregate(IND~SITE_ID+PARAMETER+STRATATYPE+STRATA,data=paramTBL2,FUN=length)#if(is.char)
#          paramTBL3=merge(paramTBL3a,paramTBL3b,by=c('SITE_ID','STRATATYPE','STRATA','PARAMETER'))#if(is.char)
#          paramTBL3$PARAMCAT=paramTBL3$PARAMRES;paramTBL3$PARAMRES=paramTBL3$IND.x/paramTBL3$IND.y#if(is.char)
         
         for (n in 1:numstrata) {#re-enter for loop now that all STRATA are complete and aggregated
           paramTBL4=subset(paramTBL3,subset=STRATATYPE==typestrata[n])
           stratabox=ggplot(paramTBL4,aes(y=PARAMRES, x=STRATA)) 
#            stratabox=ggplot(paramTBL4,aes(y=PARAMRES, x=STRATA,fill=PARAMCAT))#if(is.char)
           stratabox=stratabox+ geom_boxplot(outlier.colour = "red", outlier.size = 10)+#for reviewing all data by strata
           stat_summary(fun.data =give.n, geom = "text") +
            labs (title=sprintf('STRATA: %s ~ PARAM: %s',typestrata[n],param))
          #save jpeg or # assign(sprintf('box_STRATA_%s_%s',typestrata[n],param),stratabox)
         }
allsites=unique(paramTBL$SITE_ID)
 for (s in 1:length(allsites)){#may need to explicitly join an ecoregion column if sitecodes change over different projects, this works for NRSA only
   #if numeric, boxplot of reach avg, if character, histogram
   paramTBL$STRATATYPE='Site';paramTBL$STRATA=paramTBL$SITE; paramTBL$STRATA=factor(paramTBL$STRATA,levels=unique(paramTBL$STRATA))
   paramTBL6=subset(paramTBL,select=c('SITE_ID','PARAMETER','STRATATYPE','STRATA','PARAMRES'))
   paramTBL6=rbind(paramTBL6,paramTBL3)
   stratas=unique(subset(paramTBL6,select=STRATA,subset=SITE_ID==allsites[s]))
   paramTBL6=subset(paramTBL6,subset=STRATA %in% stratas$STRATA)
   paramTBL6$STRATATYPE=factor(paramTBL6$STRATATYPE,levels=c("Site",typestrata))
   siteavg=unique(subset(paramTBL6,subset=STRATATYPE !="Site" & SITE_ID==allsites[s] , select=PARAMRES))
    #for reviewing all data by a particular site at the site and across all strata
#     #if categorical
#    paramTBL6=subset(paramTBL6,subset=STRATATYPE=='Site')
#    sitehist=ggplot(paramTBL6,aes(x=PARAMRES)) +geom_histogram() #--> or just apply above box plots (results will be single lines for sites)
#     ##grid arrange with boxplots
   #if numeric
   sitebox=ggplot(paramTBL6,aes(y=PARAMRES, x=STRATATYPE)) +geom_boxplot(outlier.colour='red',outlier.size=10) +
     geom_hline(aes(yintercept=PARAMRES),siteavg,color='blue')  + #mark the average for the site
     stat_summary(fun.data =give.n, geom = "text") + #annotate: n(sites) for strata plots and n(points) for site  (function defined above)
     labs(title=sprintf('SITE: %s ~ PARAM: %s',allsites[s],param))
   #save jpeg or assign var
 } } 
rm(paramTBL3,paramTBL4,paramTBL5,paramTBL6,paramTBL3a,paramTBL3b)

#better resolved by adding "stratatype" to the site level data rather than dividing and recombining
# for (s in 1:length(allsites)){
#   for (p in 1:length(allparams)){  
#     typeparam=strsplit(allparams[p]," ");param=typeparam[[1]][[2]]#type=typeparam[[1]][[1]]
#     sitebox=eval(parse(text=sprintf('box_SITE_%s_%s',gsub("-","",allsites[s]),param)))
#     strata1=textGrob('NONE'); strata2=textGrob('NONE'); strata3=textGrob('NONE'); strata4=textGrob('NONE')
#     strata1I=textGrob('NONE'); strata2I=textGrob('NONE'); strata3I=textGrob('NONE'); strata4I=textGrob('NONE')
#     if(numstrata>4){print('only the first 4 strata will be displayed')}
#   for (n in 1:numstrata) { 
#     strataTXT=as.character(unique(subset(UnionTBL[c('SITE_ID',typestrata[n])],subset=SITE_ID==allsites[s]))[2])
#     stratabox=eval(parse(text=sprintf('box_STRATA_%s_%s',typestrata[n],param)))
#     strataboxI=eval(parse(text=sprintf('box_STRATA_%s_%s_%s',typestrata[n],strataTXT,param)))
#     assign(sprintf('strata%s',n),stratabox);assign(sprintf('strata%sI',n),strataboxI)
#     if(n==numstrata) {grid.arrange(strata1,strata2,strata3,strata4,nrow=numstrata,ncol=1)}#this level produces duplicates more than needed
#    }
# grid.arrange(sitebox,strata1I,strata2I,strata3I,strata4I, nrow=1,ncol=numstrata+1)
# }}



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
