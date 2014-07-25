
#Filemaker import consumption

  library(reshape)#clean up packages for auto-install if not present
  library(xlsx)
  datetest <- function(x) c(ifelse(is.numeric(x),FALSE,ifelse(is.character(x),FALSE,TRUE)))
  setwd('M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Field Work\\Post Sample\\iPad backup')#setwd('C:\\Users\\Sarah\\Documents')#default export location for desktop version of FM
  tables=list.files(getwd(),pattern='*.xlsx')#tables=read.csv('RelationalTest.csv')#tables=c('FMout_tbl_yr2014doy156Sarah-PC_Hitch.xlsx')#test table
  importcols=c(VAR,'TRANSECT','POINT','PARAMETER','RESULT');#importcols=setdiff(importcols,c('REASON'))
  importmaster=data.frame(t(rep(NA,length(importcols))))
  names(importmaster)=importcols;importmaster=subset(importmaster,subset=is.na(UID)==FALSE)
  for (t in 1:length(tables)){
    table=read.xlsx(tables[t],1)
    #if(dim(table)[2]>100){#large single table export vs. files named for each app table subprotocol; 
    tblgroups=unique(substr(names(table),1,regexpr("[.]{2}",names(table))-1))#maybe do this regardless of single or multiple table import, can't hurt = if brackets commented out
    #}
    for (g in 1:length(tblgroups)){
      if(tblgroups[g]==""){##alternatively, could find these and set them to be TRACK_REACH, but that table name doesn't matter in the long run
        matchtest='TRUE'
        grepSTR="[.]{2}" 
        tblgroups[g]='REACH'
        #!need to combine GRTS_SITEinfo into this...run the colnames(tableSUB) regexp splitting early in an elseif (may need a list of reach level tables this applies to)
      } else {matchtest='FALSE'
              grepSTR=sprintf("%s[.]{2}",tblgroups[g])        
      }
      groupcol=grep(grepSTR,colnames(table),value=T,invert=matchtest)
      tableSUB=subset(table,select= groupcol);orgCNT=nrow(tableSUB)
      tableSUB=tableSUB[rowSums(is.na(tableSUB)) != ncol(tableSUB),];blankCNT=nrow(tableSUB)#remove Nulls,
      tableSUB=unique(tableSUB);uniqueCNT=nrow(tableSUB)
      if(nrow(tableSUB)>0){
      if(orgCNT!=blankCNT){print(sprintf('%s blank rows omitted',orgCNT-blankCNT))
      } else if (blankCNT!=uniqueCNT){print(sprintf('WARNING! %s DUPLICATES omitted',blankCNT-uniqueCNT))} #do we want these sent to a table for review?
      colnames(tableSUB)=toupper(substr(names(tableSUB),regexpr("[.]{2}",names(tableSUB))+2,nchar(colnames(tableSUB))))
      if(tblgroups[g]=='TRACK_Transect'){tableSUB$TRANSECT=tableSUB$CURTRANSECT;tableSUB=tableSUB[,!(names(tableSUB) %in% c('CURTRANSECT'))]}# caveat for curtransect (too intertwined in fm to change there)
      if(tblgroups[g]=='WaterQuality_CALIB'){tableSUB$UID=sprintf('%s%s',tableSUB$CAL_INST_ID , gsub("-","",as.character(tableSUB$CAL_INST_DATE)))}# caveats for water quality
      cols=subset(colnames(tableSUB),subset=colnames(tableSUB) %in% importcols==TRUE) 
      dt=sapply(tableSUB, datetest);  tableSUB[dt] <- lapply( tableSUB[dt], as.character)#convert dates to character (custom function above)
      tableFLAT=melt(tableSUB,id.vars=cols,variable_name='PARAMETER')# for (c in 1:length(cols)) #using melt instead
      tableFLAT$PARAMETER=toupper(tableFLAT$PARAMETER)
      tableFLAT$SAMPLE_TYPE=tblgroups[g]#!actually assign later, but may need a dummy here which varies depending on multi or single table import, leaning towards only allowing single once verified to many;tableFLAT$SAMPLE_TYPE=sub('.csv','',tables[t])#!or other way to indicate the protocol (could look up to tblMetadata via tblCrosswalk, most matches are 1:1, but would need to be careful about flagging boatable)
      tableFLAT$RESULT=tableFLAT$value;
      tableFLAT=ColCheck(tableFLAT,importcols);flatCNT=nrow(tableFLAT)
      importmaster=ColCheck(importmaster,importcols)
      tableFLAT=subset(tableFLAT,subset=is.na(RESULT)==FALSE);nullCNT=nrow(tableFLAT)
      if(flatCNT!=nullCNT){print(sprintf('%s NULLS omitted',flatCNT-nullCNT)) } #do we want these sent to a table for review?
      importmaster=rbind(importmaster,tableFLAT)
      }
    }
  }
  
  
  importmasterTEMP=importmaster#temporary copy saved after import
  #importmaster2=importmaster #save copy of first import test that successfully went through the  loop
  #importmaster14Jul14=importmasterTEMP;importloopr14Jul14=list(t,tables,g,tblgroups); names(importloopr14Jul14)=c('t','tables','g','tblgroups');# running into problems on photo  table with more recent exports
  #!screen null and duplicate values that are warned about
  #!save all warning messages to a table for export/reference (right now, all are printed to the console; how is error handling supposed to be done in R packages, a lot of times, they say, "type WARN to see all warnings")
  #! pending testing - water quality calibration handling changed and comment/flags updated in FM
  #! export failure and QA (pivoted) and samplelist to AccessDB
  
  
  

  UIDSremove=unique(subset(importmaster,select=UID,subset= (PARAMETER=='DB' & RESULT =='WRSA_AIM')|(PARAMETER=='DEVICE' & RESULT =='ProAdvanced 13.0v3/C:/Users/Sarah/Documents/')))#exclude UIDs used by Sarah in testing and ones already imported, as well as monitored duplicates
  UIDSexist=sqlQuery(wrsa1314, "select distinct UID from tblVerification where parameter='SITE_ID'")
  UIDSmanualOMIT=c('8.58443864583729e+22','585759337742704256','324224919440318080','1.00590424753275e+20','3.12445349814274e+20','30317561401913393152','85565470919978896','9.79114249176033e+20','2.42573446594801e+21','7.467934950944e+19','7.1001238480827e+19','7.41736074664104e+22','7.42868294554285e+24','10445148556604496','6.22708454798246e+20','2.25618143476838e+23','8.77503374780117e+20','4.49266934743765e+21','5.64896083614649e+21',
                   '15371499864863700', '6289849184966886400','4.86796721145911e+21','36066246794627100','36066246794627104','3281462015442028544','15371499864863704','7.08938994416638e+23','4.57921803104368e+25','18934588289520738304','9.72630743819978e+21','850630406814675200','850630406814675000','18934588289520700000','4.86796721145911E+21','6289849184966880000','324224919440318000','6.34916723436864e+21','7.03114033341499E+21','7.03114033341499e+21','4.57921803104368E+25', '3281462015442020000', '88015264382921100000','88015264382921129984','6.34916723436864E+21', '9.72630743819978E+21','7.08938994416638E+23'#beta testing
                   )#! auto remove sites with less than 10 lines of data (app defaults)
  UIDSremoveLIST=c(UIDSremove$UID,UIDSexist$UID, UIDSmanualOMIT)#could query intentionally removed duplicates from Office_Comments in AccessDB#'1044' = duplicate site that crew entered in both app versions, confirmed with crew and Jennifer Courtwright; 6227 and 2256 = duplicates with only default data populated
  importmaster=subset(importmaster,subset= (UID %in% UIDSremoveLIST)== FALSE)
  #check for duplicate siteIDS
  SITEdup=subset(importmaster, subset=PARAMETER %in% c('SITE_ID') & SAMPLE_TYPE=='SampleEvent');SITEdup$SITE=SITEdup$RESULT;SITEdup=SITEdup[,!(names(SITEdup) %in% c('RESULT','IND','PARAMETER'))]
  DATEdup=subset(importmaster, subset=PARAMETER %in% c('DATE_COL') & SAMPLE_TYPE=='SampleEvent');DATEdup$DATE=DATEdup$RESULT;DATEdup=DATEdup[,!(names(DATEdup) %in% c('RESULT','IND','PARAMETER'))]
  SITEdup=merge(SITEdup,DATEdup)
  SITEcnt=aggregate(SITEdup,by=list(SITEdup$SITE),FUN=length)
  if(max(SITEcnt$UID)>1){
    SITEdup2=subset(SITEcnt,subset=UID>1)
    SITEcntDATE=aggregate(SITEdup,by=list(SITEdup$SITE,SITEdup$DATE),FUN=length)
    DATEdup2=subset(SITEcntDATE,subset=UID>1)
    DUPuid=subset(importmaster,select=UID,subset= PARAMETER=='SITE_ID' & RESULT %in% SITEdup2$Group.1 & SAMPLE_TYPE=='SampleEvent')
    DUPverif=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE=='SampleEvent')#provides verification information for comparison
    DUPwq=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE=='WaterQuality')#provides times for comparison
    DUPtran=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE %in% c('Tran','Bank','CrossSection') & TRANSECT %in% c('A','F','K'))#look at a few transects
    DUPfail=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE=='FailedSite')#look at any failures
    DUPcomment=subset(importmaster, subset=UID %in% DUPuid$UID & substr(PARAMETER,1,nchar('COMMENT'))=='COMMENT')
    print('WARNING! Duplicate site. Review outputs throughly before proceeding. If UIDs should be omitted, added to UIDSremove and re-subset importmaster.');print(SITEdup2);print(DATEdup2);View(DUPcomment);View(DUPverif);View(DUPtran);View(DUPwq);View(DUPfail)
  }
  statusAC=subset(importmaster,toupper(PARAMETER)=='STATUS' & RESULT=='AC')#! check for AC status, but be careful of ones that are duplicates (skip to tblQAstatcnt of Access import to investigate)
  if(nrow(statusAC)>0){print('WARNING! Possible blank sites!'); print (statusAC)}
  
  #! there are a lot of null UIDs...need to see what these are!!
  #importmaster=unique(importmaster)#! need to remove index before doing this, should index just be added later?; without index, could accidentally omit "0" point with identical size class;with index: perpetuates duplicates of Calibration comments (since the same record persists over the whole field season)

  
##convert transects for middle station thalweg (similar to thalweg dup in filemaker)##
middletranlist=list(A="AB",B="BC",C="CD",D="DE",E="EF",F="FG",G="GH",H="HI",I="IJ",J="JK",K="KX")
importmaster$TRANSECT=ifelse(substr(importmaster$PARAMETER,1,1)=="X"|importmaster$PARAMETER=="COMMENT_WIDTH"|importmaster$PARAMETER=="FLAG_WIDTH"|((substring(importmaster$PARAMETER,1,3)=="COM"|substring(importmaster$PARAMETER,1,3)=="FLA") & importmaster$SAMPLE_TYPE=='Thalweg_Inter'),as.character(middletranlist[as.character(importmaster$TRANSECT) ]),importmaster$TRANSECT)
  
##revisions for early app deployments (#!continue to screen for these issues)
#comment corrections
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' &  importmaster$RESULT=='thick algae mat','COMMENT_PEBBLE2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' &  importmaster$RESULT=='see misc. pictures','COMMENT_BANK2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' & importmaster$PARAMETER=='FLAG_PEBBLE','FLAG_PEBBLE2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' & importmaster$PARAMETER=='FLAG_BANK','FLAG_BANK2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='228284433826712128' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' &  substr(importmaster$PARAMETER,1,3)=='FLA','FLAG_PEBBLE2',importmaster$PARAMETER)
#uncopied temporary coordinates
uu=unique(subset(importmaster,select=UID,subset=substr(PARAMETER,1,4)=="LAT_"));#unique(subset(importmaster,select=UID,subset=substr(PARAMETER,1,5)=="WYPT_"))
cc=c('LAT_','LON_','WYPT','ELEV')
for (u in 1:nrow(uu)){
  mainsub=subset(importmaster,subset=UID==uu$UID[u] & substr(PARAMETER,1,4) %in% cc & substr(PARAMETER,nchar(PARAMETER)-3,nchar(PARAMETER))!="TEMP" & substr(PARAMETER,nchar(PARAMETER)-4,nchar(PARAMETER))!="UNITS")
  maincnt=nrow(mainsub)
  tempsub=subset(importmaster,subset=UID==uu$UID[u] & substr(PARAMETER,1,4) %in% cc & substr(PARAMETER,nchar(PARAMETER)-3,nchar(PARAMETER))=="TEMP")
  tempcnt=nrow(tempsub)
  if(maincnt<tempcnt){
    #for (c in 1:length(cc)) {
    #tempsub=subset(importmaster,subset=UID==uu$UID[u] & substr(PARAMETER,1,4) %in% cc & substr(PARAMETER,nchar(PARAMETER)-3,nchar(PARAMETER))=="TEMP")
    tempsub$PARAMETER=gsub('_TEMP','',tempsub$PARAMETER)
    tempsub=subset(tempsub,(PARAMETER %in% mainsub$PARAMETER)==F)
    importmaster=rbind(importmaster,tempsub)
    #}
  }
}
#'0' station pebbles (assumes multiple parameters are roughly in the same order...only matters if trying to match rows)
uu=unique(subset(importmaster,select=UID,subset=substr(PARAMETER,1,5)=="XSIZE" & POINT==0))
cc=c('XSIZE_CLS','XLOC','COMMENT_PEBBLE2','FLAG_PEBBLE2')
for (u in 1:nrow(uu)){
  for (c in 1:length(cc)) {
  tempsub=subset(importmaster,UID==uu$UID[u] & PARAMETER==cc[c] & (POINT==0|is.na(POINT)))
  trans=unique(tempsub$TRANSECT)
  if(nrow(tempsub)>0){
  for (t in 1:length(trans)){
  tempsub1=subset(tempsub,TRANSECT==trans[t])
  minrow=min(as.integer(rownames(tempsub1)))#as.integer(rownames(cars[34:50,]))
  importmaster$POINT=ifelse(importmaster$UID==uu$UID[u] & importmaster$PARAMETER==cc[c] & importmaster$TRANSECT==trans[t] & (importmaster$POINT==0|is.na(importmaster$POINT)),paste("0", (as.integer(rownames(importmaster))-minrow),sep='.'),importmaster$POINT)
}}}}
  

#importmaster14Jul14clean=importmaster
 
  
##START comments##
  importmaster$POINT=ifelse(importmaster$SAMPLE_TYPE %in% c('TRACK_Transect','REACH'),importmaster$TRANSECT,importmaster$POINT)
  importmaster$TRANSECT=ifelse(nchar(importmaster$TRANSECT)>3,NA,importmaster$TRANSECT)#!need to be careful with artificially named transects (WaterQuality, O, etc for FM tracking) - in the app, make O longer!
  importmaster$TRANSECT=ifelse(importmaster$TRANSECT %in% c('NULL','NA'),NA,importmaster$TRANSECT)
  importmaster$POINT=ifelse(importmaster$POINT %in% c('NULL','NA'),NA,importmaster$POINT)
  importmaster$PARAMETER=ifelse(importmaster$PARAMETER =='CAL_REASON','COMMENT_REASON',importmaster$PARAMETER);importmaster$PARAMETER=ifelse(importmaster$PARAMETER =='COMMENTS','COMMENT_FAIL',importmaster$PARAMETER)#!temporary, will be corrected in app by July 2014
  #!need to reconcile assign flags to YSI readings (FIELDMEAS) if calibration failed (like probes, probably do this processing at the end of the season)
  
  #separate and match flags and comments (abbreviate and number flags (Letter:FieldSuffix:Transect - ex: U_Wid_B), port comments to separate table with flag, match flag to rows based on protocol (xwalk))
  tblCOMMENTStmp=subset(importmaster,subset= substr(PARAMETER,1,nchar('COMMENT'))=='COMMENT');tblCOMMENTStmp$COMMENT=tblCOMMENTStmp$RESULT;tblCOMMENTStmp$PARAMETER=substr(tblCOMMENTStmp$PARAMETER,nchar('COMMENT_')+1,nchar(tblCOMMENTStmp$PARAMETER))
  tblFLAGStmp=subset(importmaster,subset= substr(PARAMETER,1,nchar('FLAG'))=='FLAG');tblFLAGStmp$PARAMETER=substr(tblFLAGStmp$PARAMETER,nchar('FLAG_')+1,nchar(tblFLAGStmp$PARAMETER))
  tblFLAGStmp$FLAG=sprintf('%s_%s%s%s',
                           tblFLAGStmp$RESULT,
                           tblFLAGStmp$PARAMETER,
                           ifelse(is.na(tblFLAGStmp$TRANSECT),'',sprintf('_%s',tblFLAGStmp$TRANSECT)),
                           ifelse(is.na(tblFLAGStmp$POINT),'',sprintf('_%s',tblFLAGStmp$POINT)))
  flagCNT=nrow(tblFLAGStmp);flagdupCNT=nrow(unique(cbind(tblFLAGStmp$FLAG,tblFLAGStmp$UID,tblFLAGStmp$TRANSECT)))
  if(flagCNT!=flagdupCNT){sprintf('WARNING! %s identical flags',flagCNT-flagdupCNT)}
  tblCOMMENTStmp=tblCOMMENTStmp[,!(names(tblCOMMENTStmp) %in% c('FLAG','RESULT','IND'))];tblFLAGStmp=tblFLAGStmp[,!(names(tblFLAGStmp) %in% c('RESULT','IND'))];
  tblCOMMENTSin=merge(tblCOMMENTStmp,tblFLAGStmp,all=T)#in theory, shouldn't get any comments without flags and shouldn't get many flags without comments
  tblCOMMENTSin$FLAG=ifelse(is.na(tblCOMMENTSin$FLAG),tblCOMMENTSin$PARAMETER,tblCOMMENTSin$FLAG);tblCOMMENTSin$FLAG=ifelse(tblCOMMENTSin$FLAG=="",tblCOMMENTSin$SAMPLE_TYPE,tblCOMMENTSin$FLAG)
  flagonlyCNT=nrow(subset(tblCOMMENTSin,is.na(COMMENT)));commentonlyCNT=nrow(subset(tblCOMMENTSin,is.na(FLAG)));sprintf('WARNING! %s Comments without Flags and %s Flags without Comments',commentonlyCNT,flagonlyCNT)
  tblCOMMENTSin$PAGE=tblCOMMENTSin$POINT#!should PAGE (EPA format) be formally switched to point here and in WRSAdb....always 1 in old EPA data
  #apply comments to master table
  tblCOMMENTSin=tblCOMMENTSin[,!(names(tblCOMMENTSin) %in% c('IND'))];importmaster=importmaster[,!(names(importmaster) %in% c('FLAG'))]
  CommentMatch=sqlQuery(wrsa1314, "select * from tblxwalk where Name_Xwalk='fmstr'")
  CommentMatch$Parameter_Xwalk=toupper(substr(CommentMatch$Parameter_Xwalk,nchar('COMMENT_')+1,nchar(CommentMatch$Parameter_Xwalk)))
  CommentMatch$PARAMETERMATCH=toupper(CommentMatch$PARAMETER);CommentMatch$PARAMETER=CommentMatch$Parameter_Xwalk
  tblCOMMENTmulti=unique(merge(tblCOMMENTSin,CommentMatch))#multiply the comment via tblMetadata::CommentMatch to multiple applicable parameters
  commentfail=subset(merge(tblCOMMENTSin,CommentMatch,all.x=T),subset=is.na(PARAMETERMATCH))
  commentfailCNT=nrow(commentfail)
  tblCOMMENTmulti=tblCOMMENTmulti[,!(names(tblCOMMENTmulti) %in% c('PARAMETER'))];tblCOMMENTmulti$PARAMETER=tblCOMMENTmulti$PARAMETERMATCH#drop parameter and reassign as parametermatch
  tblCOMMENTmulti$SAMPLE_TYPE=ifelse(tblCOMMENTmulti$SAMPLE_TYPE=='FailedSite','SampleEvent',tblCOMMENTmulti$SAMPLE_TYPE)#seems to overcome the matching for this particular sitation in a way that FMstr and previous matching can't, try to find a better solution.
  importmaster=merge(importmaster,tblCOMMENTmulti,all.x=T)#!does this match null transect/point properly?; by default: intersect(names(importmaster),names(tblCOMMENTmulti))
  commentnullCNT=nrow(subset(merge(importmaster,tblCOMMENTmulti,all.y=T),is.na(RESULT)))
  importmaster=subset(importmaster,subset= substr(PARAMETER,1,nchar('COMMENT'))!='COMMENT'& substr(PARAMETER,1,nchar('FLAG'))!='FLAG')#remove Comments and Flags since these have already been extracted
  tblCOMMENTSin=ColCheck(tblCOMMENTSin,setdiff(c(VAR,'COMMENT','TRANSECT',"PAGE"),c('RESULT',"PARAMETER")))#!should PAGE (EPA format) be formally switched to point here and in WRSAdb....always 1 in old EPA data
  importmaster=ColCheck(importmaster,importcols)
  commentCNT=nrow(tblCOMMENTSin);
  if (commentCNT>commentfailCNT|commentCNT>commentnullCNT){sprintf('%s comments with unknown parameter match and %s comments with no result match (null result i.e. a comment was used to indicate missing data)',commentfailCNT,commentnullCNT)}
  ##END comments##
  #need to clean up duplicates - issue was with Xwalk, make sure not affecting main body of data (shouldn't bc matching sample_type+parameter)
  #!fix thalweg_inter comments...having problems because of transect conversion? noticed 7/15/2014: commentfail11Jul14=commentfail --> 7/16/14 added to middletran conversion
  #! consider adding first point for comments that don't have a match due to a null parameter (i.e. comment explains why parameter was not collected)
  
  #tblCOMMENTSin14Jul14=tblCOMMENTSin
  #importmaster14Jul14nocomm=importmaster
  
  #Xwalk all parameters to non-FM names and assign proper Sample_Type (don't think it needs to be assigned earlier)
  XwalkFM=sqlQuery(wrsa1314, "select * from tblxwalk where Name_Xwalk='fm'")
  XwalkFM$SAMPLE_TYPE=substr(XwalkFM$SAMPLE_TYPE,1,nchar(XwalkFM$SAMPLE_TYPE)-1)
  XwalkFM$Parameter_Xwalk=toupper(XwalkFM$Parameter_Xwalk)
  #match sample_type for comments (not done earlier because original parameter names need to be retained for comment matching)
  tblCOMMENTmulti$Parameter_Xwalk=tblCOMMENTmulti$PARAMETER;tblCOMMENTmulti=tblCOMMENTmulti[,!(names(tblCOMMENTmulti) %in% c('PARAMETER','SAMPLE_TYPE'))]#use tblCOMMENTmulti to bring in the parameter match to avoid duplicates#tblCOMMENTSin$Table_Xwalk=tblCOMMENTSin$SAMPLE_TYPE;tblCOMMENTSin=tblCOMMENTSin[,!(names(tblCOMMENTSin) %in% c('SAMPLE_TYPE'))]
  tblCOMMENTmulti=merge(tblCOMMENTmulti,XwalkFM,by=c('Parameter_Xwalk'),all.x=T)#tblCOMMENTSin=merge(tblCOMMENTSin,unique(subset(XwalkFM,select=c(SAMPLE_TYPE,Table_Xwalk))),by=c('Table_Xwalk'),all.x=T)
  tblCOMMENTst=unique(ColCheck(tblCOMMENTmulti,c('UID','FLAG','SAMPLE_TYPE')))
  #!merge tblCOMMENTSst to tblCOMMENTSin using UID and FLAG and reduce to unique after ColCheck (below)
  tblCOMMENTSin$STold=tblCOMMENTSin$SAMPLE_TYPE;tblCOMMENTSin= tblCOMMENTSin[,!(names( tblCOMMENTSin) %in% c('SAMPLE_TYPE'))]
  tblCOMMENTSin=unique(merge(tblCOMMENTSin, tblCOMMENTst,by=c('UID','FLAG'),all.x=T))
  tblCOMMENTSin=subset(tblCOMMENTSin,SAMPLE_TYPE!='Tracking')
  tblCOMMENTSin=ColCheck(tblCOMMENTSin,setdiff(c(VAR,'COMMENT','TRANSECT',"PAGE"),c('RESULT','POINT',"PARAMETER")))#!should PAGE (EPA format) be formally switched to point here and in WRSAdb....always 1 in old EPA data
  ##match sample_type for main data
  importmaster$SAMPLE_TYPE_Xwalk=importmaster$SAMPLE_TYPE;importmaster=importmaster[,!(names(importmaster) %in% c('SAMPLE_TYPE'))]
  importmaster$Parameter_Xwalk=importmaster$PARAMETER;importmaster=importmaster[,!(names(importmaster) %in% c('PARAMETER'))]
  importmaster=merge(importmaster,XwalkFM,by=c('Parameter_Xwalk'),all.x=T)
  importmaster=subset(importmaster,subset=PARAMETER!='OMIT'|is.na(PARAMETER))#Omit tracking and other unnecessary fields
  omitCNT=nrow(subset(importmaster,subset=PARAMETER=='OMIT'))
  unmatchedPARAM=unique(subset(importmaster,select=c('SAMPLE_TYPE','PARAMETER','SAMPLE_TYPE_Xwalk','Parameter_Xwalk'),subset=is.na(PARAMETER)))
  if (nrow(unmatchedPARAM)>0){print("WARNING! Unmatched parameters. Reconcile before proceeding with import."); print(unmatchedPARAM)}
  importmaster=ColCheck(importmaster,importcols)
  
  #importmaster14Jul14xwalk=importmaster;comments14Jul14xwalk=tblCOMMENTSin
  
  tblPOINTin=subset(importmaster,subset=is.na(POINT)==FALSE  )
  tblTRANSECTin=subset(importmaster,is.na(POINT)==TRUE & is.na(TRANSECT)==FALSE );tblTRANSECTin[,!(names(tblTRANSECTin) %in% c('POINT'))]
  tblFAILUREin=subset(importmaster,SAMPLE_TYPE=='Failure'|PARAMETER=='Proximity');#unique(tblFAILUREin$PARAMETER)
  tblQAin=subset(importmaster,SAMPLE_TYPE=='Tracking' & toupper(PARAMETER)!='OMIT')#unique(tblQAin$PARAMETER)
  tblVERIFICATIONin=subset(importmaster,SAMPLE_TYPE=='VERIF');tblVERIFICATIONin[,!(names(tblVERIFICATIONin) %in% c('POINT','TRANSECT'))]
  tblREACHin=subset(importmaster,is.na(POINT)==TRUE & is.na(TRANSECT)==TRUE & SAMPLE_TYPE!='Failure' & SAMPLE_TYPE!='Tracking' & SAMPLE_TYPE!='VERIF');tblREACHin[,!(names(tblREACHin) %in% c('POINT','TRANSECT'))] #any remaining with not in  tblVERIFICATIONin and parameter <> comment/flag
  masterCNT=nrow(importmaster);pointCNT=nrow(tblPOINTin);transectCNT=nrow(tblTRANSECTin);failCNT=nrow(tblFAILUREin);qaCNT=nrow(tblQAin);reachCNT=nrow(tblREACHin);verifCNT=nrow(tblVERIFICATIONin);
  unacctCNT=masterCNT-#total rows expected
    sum(pointCNT,transectCNT,reachCNT,commentCNT,verifCNT,failCNT,qaCNT)-#total rows accounted for in partitioned tables
    (commentCNT-flagonlyCNT-commentonlyCNT)- #double count the overlap between flags and comments
    omitCNT #tracking parameters that were omitted
  
  
  #View(cast(tblVERIFICATIONin,'UID + TRANSECT + POINT ~ PARAMETER',value='RESULT' ))
  
  sprintf('wARNING! %s rows unaccounted for after table partitioning',unacctCNT)
  
  
  ##! sync metadata between FM and SQL -> odbcConnect() # FM set up to share, but need to setup DSN
  
  
  ##missing data check
  #simpler than the query on the backend which has to union tables
  failedUID = subset(importmaster,select=UID,subset=PARAMETER=='VALXSITE'& RESULT %in% c('DRYVISIT','NOACCESS','DRYNOVISIT','INACCPERM','NOTWADE','OTHER_NST','OTHER_NOACCESS','INACCTEMP'))
  importmaster$RESULT=ifelse(importmaster$PARAMETER=='Protocol'& importmaster$RESULT=='WRSA14'  & importmaster$UID %in% failedUID$UID ,
                             'Failed',importmaster$RESULT)#FM has been corrected for this in version July 1 2014
  missingProtocol=setdiff(failedUID$UID,subset(importmaster,select=UID,subset=PARAMETER=='Protocol')$UID)
  failedAppend=subset(importmaster,subset=PARAMETER=='Protocol'& RESULT %in% c('Failed')); failedAppend$UID=NA; failedAppend$IND=NA; failedAppend=unique( failedAppend)
  failedAppendtmp=failedAppend;failedAppend=failedAppend[0,]
  for (u in 1:length(  missingProtocol)){  #could use this in general for critical missing parameters
    failedAppendtmp$UID= missingProtocol[u]
    failedAppend=rbind(failedAppend,failedAppendtmp)
  }  
  importmaster=rbind(importmaster,failedAppend)
  Protocols=unique(subset(importmaster,select=RESULT,subset=toupper(PARAMETER)=='PROTOCOL'))
  tblMetadataProtocoltmp=sqlQuery(wrsa1314,sprintf("select * from tblMetadataProtocol"))
  emptyFulldataset=tblMetadataProtocoltmp[1,]
  emptyFulldataset$TRANSECT=NA;    emptyFulldataset$UID=NA;emptyFulldataset=emptyFulldataset[0,]
  for (p in 1:nrow(Protocols)){
    emptyFulldatasetTmp=emptyFulldataset[0,]
    tblMetadataProtocolR=sqlQuery(wrsa1314,sprintf("select * from tblMetadataProtocol where Protocol='%s' and Active='Y'",unlist(Protocols$RESULT[p])))
    reps=unique(tblMetadataProtocolR$Reps);reps=setdiff(reps,NA)
    tran=c('A','B','C','D','E','F','G','H','I','J','K')
    midtran=c('AB','BC','CD','DE','EF','FG','GH','HI','IJ','JK')
    for (r in 1:length(reps)){
      repcnt=reps[r]
      reptmp=subset(tblMetadataProtocolR,subset=Reps==repcnt);reptmp$TRANSECT=NA;reptmp$UID=NA
      reptmp3=reptmp[0,]
      if(repcnt>1){#parameters that occur at multiple transects
        for(t in 1:length(tran[1:ifelse(repcnt==10,10,11)])){
          reptmp2=subset(reptmp,(PARAMETER %in% c('WETWID','BARWID') & SAMPLE_TYPE=='THALW')==FALSE)
          reptmp2$TRANSECT=tran[t]
          reptmp3=rbind(reptmp3,reptmp2)
        }
        reptmp4=subset(reptmp,(PARAMETER %in% c('STABLE','COVER','EROSION','LOC','SIZE_CLS'))| (PARAMETER %in% c('WETWID','BARWID') & SAMPLE_TYPE=='THALW'))
        if(nrow(reptmp4)>0){#middle station additional
          for(m in 1:length(midtran)){
            reptmp2=reptmp4
            reptmp2$TRANSECT=midtran[m]
            reptmp3=rbind(reptmp3,reptmp2)
          }}
      } else {reptmp3=reptmp}
      emptyFulldatasetTmp=rbind(emptyFulldatasetTmp,reptmp3)
    }
    #testingUID=c('1.6444949915002e+19','34978445900951396','5.22341843044643e+20','6.05725564046426e+21','6.57547054447886e+26','745880431965292672','9.33867144639421e+23','96984648251121344','1.28761064109694e+21','333543271296521','4.72713814328789e+22','1708915464110931968','2.54415932043491e+20','3546443591531088896','8.6938972431891e+19','6.88074369254182e+25','535384124962793152','573854465591678','3795875273449409024','8.0204662147438e+20','26528638400395079680')
    UIDsADD=unique(subset(importmaster,select=UID,subset=RESULT==unlist(Protocols$RESULT[p]) )) #& UID %in% testingUID))
    UIDtmp=emptyFulldatasetTmp
    for (u in 1:nrow(UIDsADD)){  
      UIDtmp$UID=UIDsADD$UID[u]
      emptyFulldataset=rbind(emptyFulldataset,UIDtmp)
    }                         
  }
 
#emptytest=emptyFulldataset;empty14Jul14=emptyFulldataset
  
MissingCounts=sqldf("select *, case when MissingCount is null then Points else Points-MissingCount end MissingCNT from 
                      (select *, case when Transect is null then 'O' else Transect end as TRE from emptyFulldataset where Points>0) as ExpectedCounts
                      outer left join 
                      (select Sample_Type STO, Parameter PO, [UID] as UIDo, case when Transect is null then 'O' else Transect end as TR, COUNT(Result) MissingCount from importmaster
                      group by Sample_Type, Parameter, [UID], Transect) as ObservedCounts
                      on ExpectedCounts.Sample_Type=ObservedCounts.STO and ExpectedCounts.Parameter=ObservedCounts.PO and ExpectedCounts.TRE=ObservedCounts.TR and ExpectedCounts.UID=ObservedCounts.UIDo
                      where MissingCount < Points or MissingCount is null")
  #!adapt SQL statement to work with backend, but still utilize emptyfulldataset
  
MissingTotals=sqldf('select UID,count(*) ParamCNT from MissingCounts group by UID')
MissingTotals2=sqldf("select UID,Sample_Type,TRE as Transect,MC ObservedCNT, PT ExpectedCNT, cast(ifnull(round(MC/PT,2) ,0) as float) MissingPCT from
                      (select UID, Sample_Type,count(*) PTC, cast(sum(Points) as float) PT, case when Transect is null then 'O' else Transect end as TRE
                        from emptyFulldataset group by UID, SAMPLE_TYPE, Transect) as ExpectedCounts
                       outer left join 
                       (select UID as UIDo, Sample_Type STO,case when Transect is null then 'O' else Transect end as TR, count(*) MCC, cast(sum(MissingCNT) as float) MC 
                        from MissingCounts group by UID, SAMPLE_TYPE, Transect) as ObservedCounts
                       on ExpectedCounts.UID=ObservedCounts.UIDo and ExpectedCounts.Sample_Type=ObservedCounts.STO  and ExpectedCounts.TRE=ObservedCounts.TR 
                       ")
MissingTotals3=sqldf("select UID, Sample_Type, 'O' Transect,sum(ObservedCNT) ObservedCNT, sum(ExpectedCNT) ExpectedCNT , cast(ifnull(round(ObservedCNT/ExpectedCNT,2) ,0) as float) MissingPCT from MissingTotals2 group by UID, Sample_Type")
MissingTotals5=sqldf("select UID, 'REACH' Sample_Type,  'O' Transect, ParamCNT,ObservedCNT,  ExpectedCNT, MissingPCT  from MissingTotals 
                       join 
                       (select UID as UIDm,  sum(ObservedCNT) ObservedCNT, sum(ExpectedCNT) ExpectedCNT,sum(ObservedCNT)/sum(ExpectedCNT) MissingPCT from MissingTotals2 group by UID) as Totals
                        on MissingTotals.UID=Totals.UIDm
                       ");MissingTotals6=MissingTotals5[,!(names(MissingTotals5) %in% c('ParamCNT'))];MissingTotals7=MissingTotals5;MissingTotals7$MissingPCT=MissingTotals7$ParamCNT;MissingTotals7$Sample_Type='Param';MissingTotals7=MissingTotals7[,!(names(MissingTotals7) %in% c('ParamCNT'))]
MissingTotals4=unique(rbind(MissingTotals2,MissingTotals3,MissingTotals6,MissingTotals7)  );MissingTotals4$RESULT=MissingTotals4$MissingPCT;MissingTotals4$PARAMETER=paste("PCT_",MissingTotals4$Sample_Type,"_QA",sep='');MissingTotals4$SAMPLE_TYPE=MissingTotals4$Sample_Type;MissingTotals4$TRANSECT=ifelse(MissingTotals4$Transect=='O',NA,MissingTotals4$Transect);MissingTotals4$POINT=MissingTotals4$TRANSECT;MissingTotals4=ColCheck(MissingTotals4,importcols)
  
  print("Warning! The following sites failed missing data checks for the specified number of parameters.")
  print(MissingTotals5)    
  #checking individual sites#View(subset(MissingCounts,subset=UID==''))#View(subset(importmaster,subset=UID==''))# View(subset(tblCOMMENTSin,subset=UID==''))
  
  
  #custom missing data check for thalweg since flexible
  ThalwegCheck=sqldf("select Station.UID, StationDUPLICATES,StationCNT,DepthCNT from 
                     (select distinct UID, cast((result*2)-1 as numeric) as StationCNT from importmaster where parameter='SUB_5_7') as station
                     join
                     (select UID,count(result) as StationDUPLICATES from (select distinct UID, result from importmaster where parameter='SUB_5_7') as stcnt group by UID) as stationcount
                     on station.uid=stationcount.uid
                     join 
                     (select UID, max(cast(point as numeric)) as DepthCNT from importmaster where parameter='DEPTH' group by UID) as depth
                     on station.uid=depth.uid
                     where StationCNT > DepthCNT or stationDUPLICATES>1
                     order by Station.UID")
  
  print("Warning! Number of Thalweg depths does not match the number expected from the widths/stations!")
  #conflicts happen (i.e. multiple sub_5_7 values per site) when crews forget their reach widths on the first few transects, missing data check added in FM to warn them
  print(ThalwegCheck)  
  
  ##outlier check
  
  
  
  
  ##if pass (missing, accounted, outlier), migrate to WRSAdb and access db
  
  if(unacctCNT==0){print('#!send to WRSAdb (except FAILURE)! send VERIF + Failure + QA to Access')
                   #repivot tables going to Access
                   #since has to be done on remote desktop, easier to run here, export tables, re-consume and import on remote (remote can't talkt so SQL and Sarah local can't talk to Access)
                   tblFAILUREcomments=subset(tblCOMMENTSin,FLAG=='FAIL');tblFAILUREcomments$SAMPLE_TYPE='Failure';tblFAILUREcomments=unique(tblFAILUREcomments);tblFAILUREcomments$PARAMETER='COMMENTS';tblFAILUREcomments$RESULT=tblFAILUREcomments$COMMENT;tblFAILUREcomments=ColCheck(tblFAILUREcomments,colnames(tblFAILUREin))
                   pvtFAIL= cast(rbind(tblFAILUREin,tblFAILUREcomments), 'UID ~ PARAMETER',value='RESULT') #! use options to decode VALXSITE subcategories, etc  
                   pvtFAIL=pvtFAIL[rowSums(is.na(pvtFAIL)) != ncol(pvtFAIL)-2,]#remove Nulls,
                   tblQAin=merge(tblQAin,MissingTotals4,all=T)#rbind(tblQAin,MissingTotals4)#add percent missing
                   tblQAin=unique(tblQAin[,!(names(tblQAin) %in% c('IND'))])
                   tblQAstat=subset(tblQAin,PARAMETER=='Status')
                   tblQAstatcnt=cast(tblQAstat,'UID + TRANSECT + POINT ~ PARAMETER',value='RESULT' ) 
                   tblQAstatcnt=subset(tblQAstatcnt,select=UID,Status==2)
                   tblQAin=subset(tblQAin,(PARAMETER=='Status' & RESULT=='AC' & UID %in% tblQAstatcnt$UID)==FALSE)#eliminate duplicate status - all seem to have an extra "Active" status
                   pvtQA= cast(subset(tblQAin,is.na(UID)==FALSE), 'UID + TRANSECT + POINT ~ PARAMETER',value='RESULT' ) 
                   pvtQAtran=subset(pvtQA,is.na(POINT)==FALSE)
                   pvtQArch=subset(pvtQA,is.na(POINT)) 
                   if(sessionInfo()$R.version$major==2){
                     library('RODBC')
                     probsurv14=odbcConnect("ProbSurveyDB")#have to run on remote desktop (gisUser3) or machine which has 64bit R and 64bit Access
                     #alternate more generic path based method to test:  channel <- odbcConnectAccess("C:/Documents/Name_Of_My_Access_Database")
                     #sample numbering
                     #!this needs to account for two main projects: norcal vs. WRSA+intensifications
                     maxSamp=sqlQuery(probsurv14,'select max(SampleNumber) from SampleTracking where Year(SampleDate)=Year(Now())')
                   } else {print('Run on Remote Desktop and set maxSamp here');maxSamp=NA}
                   maxSamp=as.numeric(ifelse(exists("maxSamp")==F,1,1+maxSamp))
                   sampletmp=subset(importmaster,PARAMETER=='SAMPLE_ID' & SAMPLE_TYPE=='BERW')
                   sampletmp$PARAMETER='SampleNumber'
                   for (s in 1:nrow(sampletmp)){sampletmp$RESULT[s]=maxSamp;maxSamp=maxSamp+1}
                   #prep fields for SampleEvent
                   commenttmp=unique(subset(tblCOMMENTSin,SAMPLE_TYPE=='VERIF'))
                   commentUID=unique(subset(commenttmp, select='UID'));commenttmp4=data.frame()
                   for (u in 1:nrow(commentUID)){commenttmp2=as.character(subset(commenttmp,select=COMMENT,UID==commentUID$UID[u]));commenttmp3=subset(commenttmp,UID==commentUID$UID[u])[1,];commenttmp3$RESULT=commenttmp2;commenttmp4=rbind(commenttmp4,commenttmp3)}
                   commenttmp4$PARAMETER='Comments';commenttmp4=commenttmp4[,!(names(commenttmp4) %in% c('PAGE','COMMENT'))];commenttmp4$POINT=NA
                   XwalkACCst=sqlQuery(wrsa1314, "select * from tblxwalk where Name_Xwalk='ProbSurveyDB' and Table_Xwalk='SampleTracking'")#strongly consider making a function for Xwalks in which name is specified and then it does the column dropping and merging
                   XwalkACCst$PARAMETER=toupper(XwalkACCst$PARAMETER);XwalkACCst$SAMPLE_TYPE=substr(XwalkACCst$SAMPLE_TYPE,1,nchar(XwalkACCst$SAMPLE_TYPE)-1)
                   eventtmp=subset(importmaster,is.na(UID)==FALSE & PARAMETER %in% XwalkACCst$PARAMETER )
                   eventtmp=merge(eventtmp,XwalkACCst,by=c('PARAMETER','SAMPLE_TYPE'),all.x=T);eventtmp=eventtmp[,!(names(eventtmp) %in% c('PARAMETER'))];eventtmp$PARAMETER=eventtmp$Parameter_Xwalk;eventtmp=ColCheck(eventtmp,importcols)
                   eventtmp2=rbind(eventtmp,rbind(sampletmp,commenttmp4))
                   #eventtmp2=unique(eventtmp2[,!(names(eventtmp2) %in% c('IND'))])#!temporary fix for duplicate lat/longs from early FM versions
                   #eventtmp2=subset(eventtmp2,RESULT!='38.298293' & RESULT !='-111.952851' & RESULT !='41.212761' & RESULT !='-119.788766')#!temporary fix for duplicate lat/longs from early FM versions - removing duplicate for specific site: UID=='3822530414932466688' & 535384124962793152
                   pvtEVENT= cast(eventtmp2, 'UID ~ PARAMETER',value='RESULT') 
                   pvtEVENT=pvtEVENT[rowSums(is.na(pvtEVENT)) != ncol(pvtEVENT)-3,]#remove Nulls,
                   pvtEVENT=subset(pvtEVENT,is.na(SiteCode)==F)
                   write.xlsx(pvtEVENT,'AccessImport//pvtEVENT.xlsx')
                   write.xlsx(pvtFAIL,'AccessImport//pvtFAIL.xlsx')
                   write.xlsx(pvtQArch,'AccessImport//pvtQArch.xlsx')#QA tables will likely be revised to have more readable text in memo fields and to included 1st vs. last check of the data
                   write.xlsx(pvtQAtran,'AccessImport//pvtQAtran.xlsx')
                   write.xlsx(tblCOMMENTSin,'AccessImport//tblCOMMENTS.xlsx')
                   print('Tables exported. Imported into ProbSurveyDB (Access) via the saved imports prefixed with Tracking or associated button under admin tasks (both methods pending setup). Export as csv and right insert loop script (like Python recipes) if want a more automated process.')
  }
  
  
  #import to WRSAdb (SQL server)
  TablesOUT=c('tblVERIFICATIONin','tblCOMMENTSin','tblREACHin','tblTRANSECTin','tblPOINTin')
  TableNAMES=c('tblVERIFICATION','tblCOMMENTS','tblREACH','tblTRANSECT','tblPOINT')
  for (t in 1:length(TablesOUT)) {  
    TBL=eval(parse(text=TablesOUT[t]));dbTable=TableNAMES[t]
    sqlSave(wrsa1314,dat=TBL,tablename=dbTable,rownames=F, append=TRUE)
  }
 
  
  
