
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
    table$GRTS_SiteInfo..UID=table$UID##add manually because otherwise UID unattached from Site_Info
    table=table[,!(names(table) %in% c('GRTS_SiteInfo..Project','GRTS_SiteInfo..SITE_ID'))]#drop project and Site_ID from GRTS_SiteInfo which are copied over to SampleEvent   - #!could/should omit in FMexport
    tblgroups=unique(substr(names(table),1,regexpr("[.]{2}",names(table))-1))#maybe do this regardless of single or multiple table import, can't hurt = if brackets commented out
    #}
    for (g in 1:length(tblgroups)){
      if(tblgroups[g]==""){##alternatively, could find these and set them to be TRACK_REACH, but that table name doesn't matter in the long run
        matchtest='TRUE'
        grepSTR="[.]{2}" 
        tblgroups[g]='TRACK_REACH'
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
      if(tblgroups[g]=='WaterQuality_CALIB'){
        #tableSUB=subset(importmasterTEMP, PARAMETER %in% c('CAL_INST_ID','CAL_INST_DATE'));tableSUB$CAL_INST_DATE=tableSUB$RESULT;tableSUB$CAL_INST_ID=tableSUB$RESULT;#testing dataset
        #this date reformatting is repeated for all date fields later on
        tableSUB$CAL_INST_DATE=ifelse(is.na(as.numeric(as.character(tableSUB$CAL_INST_DATE)))==FALSE,format(as.Date(as.numeric(tableSUB$CAL_INST_DATE)-25569, origin="1970-01-01"),format='%m/%d/%Y'),as.character(tableSUB$CAL_INST_DATE))
        tableSUB$CAL_INST_DATE=ifelse(substr(as.character(tableSUB$CAL_INST_DATE),1,3)=="201",format(as.Date(tableSUB$CAL_INST_DATE,format='%Y-%m-%d'),format='%m/%d/%Y'),tableSUB$CAL_INST_DATE)
        tableSUB$CAL_INST_DATE=ifelse(is.na(as.Date(as.character(tableSUB$CAL_INST_DATE),format='%m/%d/%Y')),"0000",tableSUB$CAL_INST_DATE)
        tableSUB$UID=sprintf('%s%s',gsub("NAMC ","",tableSUB$CAL_INST_ID) , gsub("/","",gsub("-","",as.character(tableSUB$CAL_INST_DATE))))
        }# caveats for water quality
      if(tblgroups[g]=='Photos' & (is.na(max(tableSUB$POINT))|is.null(tableSUB$POINT))){#caveats for photo - sequential number set to point....likely will need to be in place until 2015 app, but this accounts for Point being populated as updated in the app 7/31/14
        uup=unique(tableSUB$UID)
        tableSUB$POINT=NA
        for (u in 1:length(uup)){
          minrownum=min(as.numeric(rownames(subset(tableSUB,UID==uup[u]))))-1
          tableSUB$POINT=ifelse(tableSUB$UID==uup[u],as.numeric(rownames(tableSUB))-minrownum,tableSUB$POINT)#assumes sequential rows, which is current format of FM export
          #tableSUB$Photos..Point=ifelse(tableSUB$Photos..UID==uup[u],seq(nrow(subset(tableSUB,Photos..UID==uup[u]))),tableSUB$Photos..Point)#ifelse(is.null(tableSUB$Photos..Point),NA,tableSUB$Photos..Point))#randomly out of order sometimes in unpredictable ways
        }}
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
  
  
  importmasterTEMP=importmaster#temporary copy saved after import for easy reversion without restarting xlsx import
  #importmaster2=importmaster #save copy of first import test that successfully went through the  loop
  #importmasterTEMPboat=importmaster;importmasterTEMPboat2=importmaster
  #importmaster14Jul14=importmasterTEMP;importloopr14Jul14=list(t,tables,g,tblgroups); names(importloopr14Jul14)=c('t','tables','g','tblgroups');# running into problems on photo  table with more recent exports
  #importmaster30Jul14=importmasterTEMP #!rerun indexes once 14Jul14 import to WRSAdb complete
  #!screen null and duplicate values that are warned about
  #!save all warning messages to a table for export/reference (right now, all are printed to the console; how is error handling supposed to be done in R packages, a lot of times, they say, "type WARN to see all warnings")
  
  
  
  PROCEED=1

  UIDSremove=unique(subset(importmaster,select=UID,subset= (PARAMETER=='DB' & RESULT =='WRSA_AIM')|(PARAMETER=='DEVICE' & RESULT =='ProAdvanced 13.0v3/C:/Users/Sarah/Documents/')))#exclude UIDs used by Sarah in testing and ones already imported, as well as monitored duplicates
  UIDSexist=sqlQuery(wrsa1314, "select distinct UID from tblVerification where parameter='SITE_ID'")
  UIDS10=unique(intersect(substr(importmaster$UID,1,10),substr(UIDSexist$UID,1,10)))
  UIDSexist10=unique(subset(UIDSexist, select=UID,subset=substr(UID,1,10) %in% UIDS10))
  UIDSnew10=unique(subset(importmaster, select=UID,subset=substr(UID,1,10) %in% UIDS10))
  UIDSmatch10=setdiff(UIDSnew10$UID,UIDSexist10$UID)
  UIDSmismatch10=subset(importmaster,subset=PARAMETER %in% c('SITE_ID','DATE_COL') & UID %in% UIDSmatch10); UIDSmismatch10= UIDSmismatch10[with(UIDSmismatch10,order(UID)),]
  if(length(UIDSmatch10)>0){sprintf('Verify %s non-exact matches for UID (1st 10 characters match). These will be automatically omitted unless otherwise indicated.',length(UIDSmatch10));View(UIDSmismatch10); sprintf('select * from tblverification where left(cast(UID as nvarchar),10) in (%s)',inLOOP(substr(UIDSmatch10,1,10)))}
  UIDSmanualOMIT=c('8.58443864583729e+22','4948737546099460407266','15631373425099638047420','585759337742704256','324224919440318080','452002440992807387126','56939717898642521064404','1.00590424753275e+20','3.12445349814274e+20','30317561401913393152','85565470919978896','9.79114249176033e+20','2.42573446594801e+21','7.467934950944e+19','7.1001238480827e+19','7.42868294554285e+24','10445148556604496','6.22708454798246e+20','2.25618143476838e+23','8.77503374780117e+20','4.49266934743765e+21','5.64896083614649e+21',
                   '15371499864863700', '6289849184966886400','6778495559','4.86796721145911e+21','40929284494044758016','3.46298187240046e+24','4795923406292041','238904821513005888','1.56313734250996e+22','36066246794627100','36066246794627104','3281462015442028544','15371499864863704','7.08938994416638e+23','4.57921803104368e+25','18934588289520738304','9.72630743819978e+21','850630406814675200','850630406814675000','18934588289520700000','4.86796721145911E+21','6289849184966880000','324224919440318000','6.34916723436864e+21','7.03114033341499E+21','7.03114033341499e+21','4.57921803104368E+25', '3281462015442020000', '88015264382921100000','88015264382921129984','6.34916723436864E+21', '9.72630743819978E+21','7.08938994416638E+23'#beta testing
                   )#! auto remove sites with less than 10 lines of data (app defaults)?; record reason in Access Office_Comments
  UIDSremoveLIST=c(UIDSremove$UID,UIDSexist$UID, UIDSmanualOMIT, UIDSmatch10)#could query intentionally removed duplicates from Office_Comments in AccessDB#'1044' = duplicate site that crew entered in both app versions, confirmed with crew and Jennifer Courtwright; 6227 and 2256 = duplicates with only default data populated
  importmaster=subset(importmaster,subset= (UID %in% UIDSremoveLIST)== FALSE)
  #check for duplicate siteIDS
  SITEdup=subset(importmaster, subset=PARAMETER %in% c('SITE_ID') & SAMPLE_TYPE=='SampleEvent');SITEdup$SITE=SITEdup$RESULT;SITEdup=SITEdup[,!(names(SITEdup) %in% c('RESULT','IND','PARAMETER'))]
  DATEdup=subset(importmaster, subset=PARAMETER %in% c('DATE_COL') & SAMPLE_TYPE=='SampleEvent');DATEdup$DATE=DATEdup$RESULT;DATEdup=DATEdup[,!(names(DATEdup) %in% c('RESULT','IND','PARAMETER'))]
  SITEdup=merge(SITEdup,DATEdup)
  SITEcnt=aggregate(SITEdup,by=list(SITEdup$SITE),FUN=length)
  SITEdup2=subset(SITEcnt,subset=UID>1)
  if(nrow(SITEdup2)>0){
    SITEcntDATE=aggregate(SITEdup,by=list(SITEdup$SITE,SITEdup$DATE),FUN=length)
    DATEdup2=subset(SITEcntDATE,subset=UID>1)
    DUPuid=subset(importmaster,select=UID,subset= PARAMETER=='SITE_ID' & RESULT %in% SITEdup2$Group.1 & SAMPLE_TYPE=='SampleEvent')
    DUPverif=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE=='SampleEvent');DUPverif=DUPverif[with(DUPverif,order(UID)),]#provides verification information for comparison
    DUPwq=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE=='WaterQuality');DUPwq=DUPwq[with(DUPwq,order(UID)),]#provides times for comparison
    DUPtran=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE %in% c('Tran','Bank','CrossSection') & TRANSECT %in% c('A','F','K'));DUPtran=DUPtran[with(DUPtran,order(UID)),]#look at a few transects
    DUPfail=subset(importmaster, subset=UID %in% DUPuid$UID & SAMPLE_TYPE=='FailedSite');DUPfail=DUPfail[with(DUPfail,order(UID)),]#look at any failures
    DUPcomment=subset(importmaster, subset=UID %in% DUPuid$UID & substr(PARAMETER,1,nchar('COMMENT'))=='COMMENT');DUPcomment=DUPcomment[with(DUPcomment,order(UID)),]
    print('WARNING! Duplicate site. Review outputs throughly before proceeding. If UIDs should be omitted, added to UIDSremove and re-subset importmaster.');print(SITEdup2);print(DATEdup2);View(DUPcomment);View(DUPverif);View(DUPtran);View(DUPwq);View(DUPfail)
  }
  statusAC=intersect(unlist(subset(importmaster,select=UID,toupper(PARAMETER)=='STATUS' & RESULT=='AC')),unlist(subset(importmaster,select=UID,toupper(PARAMETER)=='PROTOCOL' & RESULT!='Failed')))#! check for AC status, but be careful of ones that are duplicates (skip to tblQAstatcnt of Access import to investigate)
  if(length(statusAC)>0){print('WARNING! Possible blank sites!'); print (statusAC);ACsites=subset(importmaster, subset=UID %in% statusAC & SAMPLE_TYPE %in% c('SampleEvent','REACH'));ACsites=ACsites[with(ACsites,order(UID)),]; View(ACsites)}
importmaster=subset(importmaster,is.na(UID)==FALSE)#there were a lot of null UIDs...need to see what these are!! --> most appear to be null states/streams, so corrected GRTS_SITEINFO to have UID

#####################################STOP###########CHECK#DUPLICATES###############################################################################################
#! force script stop at key scripts, if none of these work, implement if statements that require the user to change a variable
#browser(); print("STOP. Enter Q to resume.")#Sys.sleep(10)#Pause=-readline(prompt="Pause? ")#break#stop("Check possible duplicates before proceeding.")#none of these are preventing subsequent lines of code from running
#print('did i stop?')  
if(nrow(SITEdup2)==0 & nrow(statusAC)==0) {PROCEED=2}

  
if(PROCEED<2) {print("Check for duplicates and set PROCEED=2 (in console) to continue.")} else{ print(PROCEED)
##convert transects for middle station thalweg (similar to thalweg dup in filemaker)##
middletranlist=list(A="AB",B="BC",C="CD",D="DE",E="EF",F="FG",G="GH",H="HI",I="IJ",J="JK",K="KX")
importmaster$TRANSECT=ifelse(substr(importmaster$PARAMETER,1,1)=="X"|importmaster$PARAMETER=="COMMENT_WIDTH"|importmaster$PARAMETER=="FLAG_WIDTH"|((substring(importmaster$PARAMETER,1,3)=="COM"|substring(importmaster$PARAMETER,1,3)=="FLA") & importmaster$SAMPLE_TYPE=='Thalweg_Inter'),as.character(middletranlist[as.character(importmaster$TRANSECT) ]),importmaster$TRANSECT)
 #clean up point and transect
  importmaster$POINT=ifelse(importmaster$SAMPLE_TYPE %in% c('TRACK_Transect','REACH'),importmaster$TRANSECT,importmaster$POINT)
  importmaster$TRANSECT=ifelse(nchar(importmaster$TRANSECT)>3,NA,importmaster$TRANSECT)#!need to be careful with artificially named transects (WaterQuality, O, etc for FM tracking) - in the app, make O longer!
  importmaster$TRANSECT=ifelse(importmaster$TRANSECT %in% c('NULL','NA'),NA,importmaster$TRANSECT)
  importmaster$POINT=ifelse(importmaster$POINT %in% c('NULL','NA'),NA,importmaster$POINT)  
  
  
##revisions for early app deployments (#!continue to screen for these issues)
#comment corrections (can't migrate to Access Office_Updates because would impede xwalk matching)
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' &  importmaster$RESULT=='thick algae mat','COMMENT_PEBBLE2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' &  importmaster$RESULT=='see misc. pictures','COMMENT_BANK2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' & importmaster$PARAMETER=='FLAG_PEBBLE','FLAG_PEBBLE2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='9.90634514616468e+20' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' & importmaster$PARAMETER=='FLAG_BANK','FLAG_BANK2',importmaster$PARAMETER)
importmaster$PARAMETER=ifelse( importmaster$UID=='228284433826712128' & importmaster$SAMPLE_TYPE=='Thalweg_Inter' &  substr(importmaster$PARAMETER,1,3)=='FLA','FLAG_PEBBLE2',importmaster$PARAMETER)
importmaster$POINT=ifelse(importmaster$UID=='5207424420831349760' & importmaster$PARAMETER %in% c('COVER','EROSION','STABLE') & is.na(importmaster$POINT), 'LF',importmaster$POINT )
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
##revisions that regularly need to be checked for, but are more common in earlier app versions
#autocalculations from Verification that go into lower level (Transect/point) tables 
#thalweg increment # found in UIDs 1325207444760163,22784934936124764160,26206468843598077952,9.27728440149079e+21,9.20869476436781e+22
uu1=subset(importmaster,select=UID,PARAMETER=='INCREMENT' )
uu2=unique(subset(importmaster,select=UID,PARAMETER=='SUB_5_7' ))
uu3=setdiff(uu1$UID,uu2$UID)
importmaster=subset(importmaster,(UID %in% uu3 & PARAMETER=='INCREMENT')==FALSE)
  
#convert dates/times to preferred format, not EXCEL numeric format
DateParams=c('DATE_COL','CAL_INST_DATE','ACTUAL_DATE_BER','ACTUAL_DATE_CHEM')
importmaster$RESULT=ifelse(importmaster$PARAMETER %in% DateParams  & is.na(as.numeric(importmaster$RESULT))==FALSE,format(as.Date(as.numeric(importmaster$RESULT)-25569, origin="1970-01-01"),format='%m/%d/%Y'),importmaster$RESULT)
importmaster$RESULT=ifelse(importmaster$PARAMETER %in% DateParams  & substr(importmaster$RESULT,5,5)=="-",format(as.Date(importmaster$RESULT,format='%Y-%m-%d'),format='%m/%d/%Y'),importmaster$RESULT)
importmaster$FLAG=ifelse(is.na(as.Date(importmaster$RESULT,format='%m/%d/%Y')) & importmaster$PARAMETER %in% DateParams,"U_DATE",importmaster$FLAG)#flag dates in non-standard format
importmaster$RESULT=ifelse(importmaster$PARAMETER %in% c('PROBE_STARTTIME','PROBE_ENDTIME') ,gsub("1899-12-30 ","",importmaster$RESULT),importmaster$RESULT)

#additional calibration cleanup
importmaster=subset(importmaster, (PARAMETER=='CAL_INST_DATE' & RESULT=='12/30/1899')==FALSE)
  
#'0' station pebbles (assumes multiple parameters are roughly in the same order...only matters if trying to match rows--->if so, implement something like Photos during import)
uu=unique(subset(importmaster,select=UID,subset=substr(PARAMETER,1,4) %in% c("SIZE","XSIZ") & (POINT==0|is.na(POINT))))
cc=c('XSIZE_CLS','XLOC','COMMENT_PEBBLE2','FLAG_PEBBLE2','EMBED','LOC','SIZE_CLS','COMMENT_PEBBLE','FLAG_PEBBLE')
for (u in 1:nrow(uu)){
  for (c in 1:length(cc)) {
  tempsub=subset(importmaster,UID==uu$UID[u] & PARAMETER==cc[c] & (POINT==0|is.na(POINT)))
  trans=unique(tempsub$TRANSECT)
  if(nrow(tempsub)>0){
  for (t in 1:length(trans)){
  tempsub1=subset(tempsub,TRANSECT==trans[t])
  minrow=min(tempsub1$IND)#min(as.integer(rownames(tempsub1)))#as.integer(rownames(cars[34:50,]))#rownum=seq(from=minrow,length.out=nrow(tempsub1))#risk or hang hat on IND always being sequential for sets of UID +  parameters + transects#
  importmaster$POINT=ifelse(importmaster$UID==uu$UID[u] & importmaster$PARAMETER==cc[c] & importmaster$TRANSECT==trans[t] & (importmaster$POINT==0|is.na(importmaster$POINT)),paste("0", importmaster$IND-minrow,sep='.'),importmaster$POINT)#(as.integer(rownames(importmaster))-minrow)
}}}}
#duplicate data with blank transect (if app is working correctly, these are only omissions of default values unneeded in rows created above transect level such as for temporary reach width calculations)
  nulltran=rbind(subset(importmaster, (SAMPLE_TYPE %in% c('Thalweg','Tran','Canopy','CrossSection'  ) & substr(PARAMETER,1,4)!='COMM' & PARAMETER!='INCREMENT' & PARAMETER!='RCHW' & is.na(TRANSECT))),subset(importmaster, (SAMPLE_TYPE=='Slope'    & PARAMETER %in% c('METHOD','PROP','SLOPE_UNITS') & is.na(POINT))))
  if(nrow(nulltran)>0){sprintf("WARNING: %s rows will be omitted because they are missing TRANSECT when expected",nrow(nulltran)); View(nulltran)}
  importmaster=subset(importmaster, (SAMPLE_TYPE %in% c('Thalweg','Tran','Canopy','CrossSection' )  & substr(PARAMETER,1,4)!='COMM' & PARAMETER!='INCREMENT' & PARAMETER!='RCHW' & is.na(TRANSECT)) ==FALSE)
  importmaster=subset(importmaster, (SAMPLE_TYPE=='Slope'    & PARAMETER %in% c('METHOD','PROP','SLOPE_UNITS') & is.na(POINT))==FALSE)
#blank point in habitat
  uucnt=subset(importmaster,SAMPLE_TYPE=='Habitat' & is.na(POINT));uuhab=uucnt[0,]
  if(nrow(uucnt)>0){  
    uucnt=cast(uucnt,'UID  ~ SAMPLE_TYPE')
    uu5=subset(uucnt,Habitat<7)
    importmaster$POINT=ifelse(importmaster$UID %in% uu5$UID & importmaster$SAMPLE_TYPE=='Habitat' & is.na(importmaster$POINT),'0', importmaster$POINT)
    uuhab=subset(uucnt,Habitat>6)
  if(nrow(uuhab)>0 ){   
    View(subset(importmaster,UID %in% uuhab$UID  & importmaster$SAMPLE_TYPE=='Habitat' & is.na(importmaster$POINT)))
    importmaster$POINT=ifelse(importmaster$UID %in% uuhab$UID & toupper(importmaster$SAMPLE_TYPE)=='HABITAT' & is.na(importmaster$POINT),'0.5', importmaster$POINT)
    importmaster$FLAG=ifelse(importmaster$UID %in% uuhab$UID & toupper(importmaster$SAMPLE_TYPE)=='HABITAT' & importmaster$POINT=='0.5','U_POOL',  importmaster$FLAG)
    importmaster$FLAG=ifelse(importmaster$UID %in% uuhab$UID & importmaster$PARAMETER %in% c('PTAILDEP','MAXDEPTH') & importmaster$POINT=='0.5','K_POOL',importmaster$FLAG)
    importmaster$ACTIVE=ifelse(importmaster$UID %in% uuhab$UID & importmaster$PARAMETER %in% c('MAXDEPTH') & importmaster$POINT=='0.5','FALSE',importmaster$ACTIVE)# max depth inactivated so residual depth not accidentally computed, but tail depth could still be used if desired (not used alone in any PIBO stats)
    importmaster$DEPRECATION=ifelse(importmaster$UID %in% uuhab$UID & importmaster$PARAMETER %in% c('MAXDEPTH') & importmaster$POINT=='0.5',as.character(Sys.Date()),importmaster$DEPRECATION)
  }}
  
 #!other data quirks that need global correction in existing data and prevention in future data
 #!double records for CHANDEPTHB and CROSSSECW with the same index (should be cleared up for new xwalk funciton); check other ind duplicates
 #!measurement units for app tweaks
#!different angle, size_cls parameter for 2014 bc of significant protocol change
                                                                                                

importmaster=unique(importmaster)
#importmaster14Jul14clean2=importmaster
 
  
##START comments##

  importmaster$PARAMETER=ifelse(importmaster$PARAMETER =='CAL_REASON','COMMENT_REASON',importmaster$PARAMETER);importmaster$PARAMETER=ifelse(importmaster$PARAMETER =='COMMENTS','COMMENT_FAIL',importmaster$PARAMETER)#!temporary, will be corrected in app by July 2014
  #!need to reconcile assign flags to YSI readings (FIELDMEAS) if calibration failed (like probes, probably do this processing at the end of the season)
  #separate and match flags and comments (abbreviate and number flags (Letter:FieldSuffix:Transect - ex: U_Wid_B), port comments to separate table with flag, match flag to rows based on protocol (xwalk))
  tblCOMMENTStmp=subset(importmaster,subset= substr(PARAMETER,1,nchar('COMMENT'))=='COMMENT');tblCOMMENTStmp$COMMENT=tblCOMMENTStmp$RESULT;#tblCOMMENTStmp$PARAMETER=substr(tblCOMMENTStmp$PARAMETER,nchar('COMMENT_')+1,nchar(tblCOMMENTStmp$PARAMETER))
  tblFLAGStmp=subset(importmaster,subset= substr(PARAMETER,1,nchar('FLAG'))=='FLAG');tblFLAGStmp$PARAMETER=gsub('FLAG_','COMMENT_',tblFLAGStmp$PARAMETER)#tblFLAGStmp$PARAMETER=substr(tblFLAGStmp$PARAMETER,nchar('FLAG_')+1,nchar(tblFLAGStmp$PARAMETER))
  tblFLAGStmp$FLAG=sprintf('%s_%s%s%s',
                           tblFLAGStmp$RESULT,
                           sub('COMMENT_','',tblFLAGStmp$PARAMETER),#substr(tblFLAGStmp$PARAMETER,nchar('COMMENT_')+1,nchar(tblFLAGStmp$PARAMETER)),#tblFLAGStmp$PARAMETER,#
                           ifelse(is.na(tblFLAGStmp$TRANSECT),'',sprintf('_%s',tblFLAGStmp$TRANSECT)),
                           ifelse(is.na(tblFLAGStmp$POINT),'',sprintf('_%s',tblFLAGStmp$POINT)))
  flagCNT=nrow(tblFLAGStmp);flagdupCNT=nrow(unique(cbind(tblFLAGStmp$FLAG,tblFLAGStmp$UID,tblFLAGStmp$TRANSECT)))
  if(flagCNT!=flagdupCNT){sprintf('WARNING! %s identical flags',flagCNT-flagdupCNT)}
  tblCOMMENTStmp=tblCOMMENTStmp[,!(names(tblCOMMENTStmp) %in% c('FLAG','RESULT','IND'))];tblFLAGStmp=tblFLAGStmp[,!(names(tblFLAGStmp) %in% c('RESULT','IND'))];
  tblCOMMENTSin=merge(tblCOMMENTStmp,tblFLAGStmp,all=T)#in theory, shouldn't get any comments without flags and shouldn't get many flags without comments
  tblCOMMENTSin$FLAG=ifelse(is.na(tblCOMMENTSin$FLAG),tblCOMMENTSin$PARAMETER,tblCOMMENTSin$FLAG);tblCOMMENTSin$FLAG=ifelse(tblCOMMENTSin$FLAG=="",tblCOMMENTSin$SAMPLE_TYPE,tblCOMMENTSin$FLAG)
  tblCOMMENTSin$FLAG=gsub('COMMENT_','',tblCOMMENTSin$FLAG)
  flagonlyCNT=nrow(subset(tblCOMMENTSin,is.na(COMMENT)));commentonlyCNT=nrow(subset(tblCOMMENTSin,is.na(FLAG)));sprintf('WARNING! %s Comments without Flags and %s Flags without Comments',commentonlyCNT,flagonlyCNT)
  tblCOMMENTSin$PAGE=tblCOMMENTSin$POINT#!should PAGE (EPA format) be formally switched to point here and in WRSAdb....always 1 in old EPA data
  tblCOMMENTSin$SAMPLE_TYPE=ifelse(tblCOMMENTSin$SAMPLE_TYPE=='FailedSite' & tblCOMMENTSin$FLAG=='FAIL','SampleEvent',tblCOMMENTSin$SAMPLE_TYPE)##odd comment that has the result in one table and comment in another
  #apply comments to master table
  tblCOMMENTmulti=Xwalk(Source='R',Table="tblCOMMENTSin",XwalkName='FMstr',XwalkDirection='_Xwalk',COL=c('PAGE','COMMENT'));
  tblCOMMENTmulti=unique(tblCOMMENTmulti[,!(names(tblCOMMENTmulti) %in% c('IND','RESULT','TABLE'))]);importmaster=importmaster[,!(names(importmaster) %in% c('FLAG'))];importmaster$SAMPLE_TYPE=toupper(importmaster$SAMPLE_TYPE)#tblCOMMENTSin=tblCOMMENTSin[,!(names(tblCOMMENTSin) %in% c('IND'))];
  importmaster=merge(importmaster,tblCOMMENTmulti,all.x=T)#!does this match null transect/point properly?; by default: intersect(names(importmaster),names(tblCOMMENTmulti))
  commentnullCNT=nrow(subset(merge(importmaster,tblCOMMENTmulti,all.y=T),is.na(RESULT)))
  importmaster=subset(importmaster,subset= substr(PARAMETER,1,nchar('COMMENT'))!='COMMENT'& substr(PARAMETER,1,nchar('FLAG'))!='FLAG')#remove Comments and Flags since these have already been extracted
  tblCOMMENTSin=ColCheck(tblCOMMENTSin,setdiff(c(VAR,'COMMENT','TRANSECT',"PAGE"),c('RESULT',"PARAMETER")))#!should PAGE (EPA format) be formally switched to point here and in WRSAdb....always 1 in old EPA data
  importmaster=unique(ColCheck(importmaster,importcols))
  commentCNT=nrow(tblCOMMENTSin);
  if (commentCNT>commentnullCNT){sprintf('%s comments with no result match (null result i.e. a comment was used to indicate missing data)',commentnullCNT)}
  #comment additions
  #flag multiple pools with unassigned points
if(nrow(uuhab)>0 ){
uuhabcom=subset(importmaster,UID %in% uuhab$UID & PARAMETER %in% c('LENGTH','MAXDEPTH') & POINT=='0.5')
  if(nrow(uuhabcom)>0 ){
    uuhabcom$RESULT=ifelse(uuhabcom$PARAMETER=='LENGTH','Multiple pools without assigned number','Max depth omitted because cannot be paired to multiple available pool tail depths.');uuhabcom$PAGE=uuhabcom$POINT;uuhabcom$COMMENT=uuhabcom$RESULT
    uumulti=uuhabcom;uumulti$Name_Xwalk=NA;uumulti$Table_Xwalk=NA;uumulti$Type_Xwalk=NA;uumulti$Parameter_Xwalk=NA;uumulti$Notes=NA;uumulti$PARAMETERMATCH=NA;uumulti=uumulti[,!(names(uumulti) %in% c('IND','RESULT'))]
    tblCOMMENTmulti=rbind(tblCOMMENTmulti, uumulti)
    uuhabcom=unique(uuhabcom[,!(names(uuhabcom) %in% c('IND','POINT','RESULT','PARAMETER'))]);uuhabcom$IND=seq(from=IndMax,to=IndMax+nrow(uuhabcom)-1);IndMax=IndMax+nrow(uuhabcom)
    tblCOMMENTSin=rbind(tblCOMMENTSin, uuhabcom)
  }}
  #copy QA comments
  uuqacom=subset(importmaster, toupper(substr(PARAMETER,1,5)) =='QABYP' )
  uuqacom$PAGE=uuqacom$POINT;uuqacom$COMMENT=uuqacom$RESULT;uuqacom$FLAG=sprintf('QA_%s',substr(uuqacom$SAMPLE_TYPE,1,1))
  uumulti=uuqacom;uumulti=uumulti[,!(names(uumulti) %in% c('IND','RESULT'))]
  tblCOMMENTmulti=rbind(tblCOMMENTmulti, uumulti)
  uuqacom=unique(uuqacom[,!(names(uuqacom) %in% c('IND','POINT','RESULT','PARAMETER'))]);uuqacom$IND=seq(from=IndMax,to=IndMax+nrow(uuqacom)-1);IndMax=IndMax+nrow(uuqacom)
  tblCOMMENTSin=rbind(tblCOMMENTSin, uuqacom)
  
    ##END comments##
  #need to clean up duplicates - issue was with Xwalk, make sure not affecting main body of data (shouldn't bc matching sample_type+parameter)
  #!fix thalweg_inter comments...having problems because of transect conversion? noticed 7/15/2014: commentfail11Jul14=commentfail --> 7/16/14 added to middletran conversion
  #! consider adding first point for comments that don't have a match due to a null parameter (i.e. comment explains why parameter was not collected)
  #! truncate all UIDS to 10 char at the end of ht efield season + fix in app
                                                                                                
  #tblCOMMENTSin14Jul142=tblCOMMENTSin
  #importmaster14Jul14nocomm2=importmaster
  
  #Xwalk all parameters to non-FM names and assign proper Sample_Type (don't think it needs to be assigned earlier)
  #match sample_type for comments (not done earlier because original parameter names need to be retained for comment matching)
  tblCOMMENTmulti=Xwalk(Source='R',Table="tblCOMMENTmulti",XwalkName='FM',XwalkDirection='_Xwalk',COL=c('PAGE','COMMENT')) 
  tblCOMMENTst=unique(ColCheck(tblCOMMENTmulti,c('UID','FLAG','SAMPLE_TYPE')))
  #merge tblCOMMENTSst to tblCOMMENTSin using UID and FLAG and reduce to unique after ColCheck (below)
  tblCOMMENTSin$STold=tblCOMMENTSin$SAMPLE_TYPE;tblCOMMENTSin= tblCOMMENTSin[,!(names( tblCOMMENTSin) %in% c('SAMPLE_TYPE'))]
  tblCOMMENTSin=unique(merge(tblCOMMENTSin, tblCOMMENTst,by=c('UID','FLAG'),all.x=T))
  tblCOMMENTSin=subset(tblCOMMENTSin,SAMPLE_TYPE!='Tracking'|substr(FLAG,1,2)=='QA')
  tblCOMMENTSin=ColCheck(tblCOMMENTSin,setdiff(c(VAR,'COMMENT','TRANSECT',"PAGE"),c('RESULT','POINT',"PARAMETER")))#!should PAGE (EPA format) be formally switched to point here and in WRSAdb....always 1 in old EPA data
   ##match sample_type for main data
  importmaster=Xwalk(Source='R',Table="importmaster",XwalkName='FM',XwalkDirection='_Xwalk')                                                                                              
  importmaster=subset(importmaster,subset=toupper(PARAMETER)!='OMIT'|is.na(PARAMETER))#Omit tracking and other unnecessary fields
  importmaster=ColCheck(importmaster,importcols)                                                                                              
                                                                                                
  

#importmaster14Jul14xwalk2=importmaster;comments14Jul14xwalk2=tblCOMMENTSin
 }#end Else Proceed=2

  
#####################################STOP###########BREATHE...Then#Proceed#With#Imports###############################################################################################
if(PROCEED<3) {print("Data ready for import. Perform any desired checkes and set PROCEED=3 (in console) to continue.")
               }  else{
  tblCOMMENTSin$TRANSECT=ifelse(is.na(tblCOMMENTSin$TRANSECT),"ALL",tblCOMMENTSin$TRANSECT);tblCOMMENTSin$PAGE=ifelse(is.na(tblCOMMENTSin$PAGE),"1",tblCOMMENTSin$PAGE);tblCOMMENTSin=tblCOMMENTSin[with(tblCOMMENTSin,order(IND)),]
  tblPOINTin=subset(importmaster,subset=is.na(POINT)==FALSE & toupper(SAMPLE_TYPE)!='TRACKING' );tblPOINTin$TRANSECT=ifelse(is.na(tblPOINTin$TRANSECT) |toupper(tblPOINTin$TRANSECT)=="NA","0",tblPOINTin$TRANSECT);tblPOINTin=tblPOINTin[with(tblPOINTin,order(IND)),]
  tblTRANSECTin=subset(importmaster,is.na(POINT)==TRUE & is.na(TRANSECT)==FALSE & SAMPLE_TYPE!='Tracking');tblTRANSECTin=tblTRANSECTin[,!(names(tblTRANSECTin) %in% c('POINT'))];tblTRANSECTin=tblTRANSECTin[with(tblTRANSECTin,order(IND)),]
  tblFAILUREin=subset(importmaster,toupper(SAMPLE_TYPE)=='FAILURE'|toupper(PARAMETER)=='PROXIMITY');#unique(tblFAILUREin$PARAMETER)
  tblQAin=subset(importmaster,toupper(SAMPLE_TYPE)=='TRACKING' & toupper(PARAMETER)!='OMIT')#unique(tblQAin$PARAMETER)
  tblVERIFICATIONin=subset(importmaster,toupper(SAMPLE_TYPE)=='VERIF');tblVERIFICATIONin=tblVERIFICATIONin[,!(names(tblVERIFICATIONin) %in% c('POINT','TRANSECT'))];tblVERIFICATIONin=tblVERIFICATIONin[with(tblVERIFICATIONin,order(IND)),]
  tblREACHin=subset(importmaster,is.na(POINT)==TRUE & is.na(TRANSECT)==TRUE & toupper(SAMPLE_TYPE)!='FAILURE' & toupper(SAMPLE_TYPE)!='TRACKING' & SAMPLE_TYPE!='VERIF');tblREACHin=tblREACHin[,!(names(tblREACHin) %in% c('POINT','TRANSECT'))];tblREACHin=tblREACHin[with(tblREACHin,order(IND)),] #any remaining with not in  tblVERIFICATIONin and parameter <> comment/flag
  masterCNT=nrow(importmaster);pointCNT=nrow(tblPOINTin);transectCNT=nrow(tblTRANSECTin);failCNT=nrow(tblFAILUREin);qaCNT=nrow(tblQAin);reachCNT=nrow(tblREACHin);verifCNT=nrow(tblVERIFICATIONin);
  unacctCNT=masterCNT-#total rows expected
    sum(pointCNT,transectCNT,reachCNT,commentCNT,verifCNT,failCNT,qaCNT)-#total rows accounted for in partitioned tables
    (commentCNT-flagonlyCNT-commentonlyCNT)- #double count the overlap between flags and comments
    omitCNT #tracking parameters that were omitted
  sprintf('wARNING! %s rows unaccounted for after table partitioning',unacctCNT)
  #problems to check for again: tblREACH - SLOPE, THAL,FISHCOV,CANCOV,LWD,CROSSSEC, HABITAT, PHOTOS ;tblTRANSECT - photos,CROSSSEC, stability *3 (Bankw; happened in single UID) , Slope
  #View(subset(tblREACHin,SAMPLE_TYPE %in% c('SLOPEW', 'THALW','FISHCOVW','CANCOVW','LWDW','CROSSSECW', 'HABITAT', 'PHOTOS')))
  #View(subset(tblTRANSECTin,SAMPLE_TYPE %in% c('SLOPEW', 'CROSSSECW', 'BANKW', 'PHOTOS')))  
  #aggcnt=aggregate(RESULT~UID + TRANSECT + POINT + SAMPLE_TYPE + PARAMETER,data=tblPOINTin,FUN=length);aggcnt=subset(aggcnt,RESULT>1);View(aggcnt)

  

   
  
  ##if pass (missing, accounted, outlier), migrate to WRSAdb and access db
  
  #import to WRSAdb (SQL server)
  print('Beginning import to SQL server')
  TablesOUT=c('tblVERIFICATIONin','tblCOMMENTSin','tblREACHin','tblTRANSECTin','tblPOINTin')
  TableNAMES=c('tblVERIFICATION','tblCOMMENTS','tblREACH','tblTRANSECT','tblPOINT')
  for (t in 4:length(TablesOUT)) {  
    TBL=eval(parse(text=TablesOUT[t]));dbTable=TableNAMES[t]#tblVerification imported for 14Jul set on 7/30/14
    #INDexist=sqlQuery(wrsa1314,sprintf('select IND from %s',dbTable));TBL=subset(TBL,(IND %in% INDexist$IND)==FALSE)#for troubleshooting and restarting failed imports, ordered by IND at an earlier step for easier monitoring, could consolidate into single line here
    #!tblREACH import struggles alot with CALIB sample_Type...sometimes random, sometimes duplicate, sometimes date or UID errors. systematic problems were corrected for during import, but unsure where the duplicate issue creeps in
    #correction for TBL reach: TBL=unique(TBL);TBL=subset(TBL,(PARAMETER=='CAL_INST_DATE' & FLAG=='F_REASON')==FALSE)
    #!tblpoint sometimes has issues with the deprecation value not being a date for the ones i manually set (maxdepth for habitat), this is simliar to the date issues for calibrations in tblreach
    sqlSave(wrsa1314,dat=TBL,tablename=dbTable,rownames=F, append=TRUE)
  }
  print('End import to SQL server')
  
}#end Else Proceed=3
  
  ##!QA checks moved to DataQA_WRSA  
  if(PROCEED<4) { 
    if(exists("MissingTotals4")){
      tblQAin=merge(tblQAin,MissingTotals4,all=T)#rbind(tblQAin,MissingTotals4)#add percent missing
  } else {print ("Missing Totals need to be run in DataQA_WRSA.R if want to review missing percentages in Access. Otherwise set PROCEED=4 (in console) to continue without Missing Totals.")}
  
  #import to ProbSurveydb (Access)
 #repivot tables going to Access
 #since has to be done on remote desktop, easier to run here, export tables, re-consume and import on remote (remote can't talkt so SQL and Sarah local can't talk to Access)
 tblFAILUREcomments=subset(tblCOMMENTSin,FLAG=='FAIL');tblFAILUREcomments$SAMPLE_TYPE='Failure';tblFAILUREcomments=unique(tblFAILUREcomments);tblFAILUREcomments$PARAMETER='COMMENTS';tblFAILUREcomments$RESULT=tblFAILUREcomments$COMMENT;tblFAILUREcomments=ColCheck(tblFAILUREcomments,colnames(tblFAILUREin))
 pvtFAIL= cast(rbind(tblFAILUREin,tblFAILUREcomments), 'UID ~ PARAMETER',value='RESULT') #! use options to decode VALXSITE subcategories, etc  
 pvtFAIL=pvtFAIL[rowSums(is.na(pvtFAIL)) != ncol(pvtFAIL)-2,]#remove Nulls,
 tblQAin=unique(tblQAin[,!(names(tblQAin) %in% c('IND'))])
 tblQAstat=subset(tblQAin,toupper(PARAMETER)=='STATUS')
 tblQAstatcnt=cast(tblQAstat,'UID + TRANSECT + POINT ~ PARAMETER',value='RESULT' ) 
 if(nrow(tblQAstatcnt)>0){tblQAstatcnt=subset(tblQAstatcnt,select=UID,STATUS==2)
 tblQAin=subset(tblQAin,(toupper(PARAMETER)=='STATUS' & RESULT=='AC' & UID %in% tblQAstatcnt$UID)==FALSE)#eliminate duplicate status - all seem to have an extra "Active" status
 }#tblQAin=subset(tblQAin,(PARAMETER=='PCT_THALW_QA' & UID=='8.22121239495907e+21' & is.na(TRANSECT) & RESULT=='1')==FALSE)#unexplained duplicate in MissingTotals4 from 30 Jul import
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
#!having to manually renumber because of intensfications, consider just setting maxSamp to zero which triggers all samplenubmers to be zero, then if maxsampl>0, run the sequence code
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

  }#end Else Proceed=4
  
  
if (SYNC=='Y'){
  ##!sync metadata between FM and SQL
  tblMetadataRange=read.xlsx("C:\\Users\\Sarah\\Desktop\\NAMCdevelopmentLocal\\tblMetadataRange_20Aug14.xlsx",1)
  tblMetadataRange$SAMPLE_TYPE=tblMetadataRange$tblMetadata..Tbl
  tblMetadataRange$ACTIVE=ifelse(tblMetadataRange$USE=='Y','TRUE','FALSE')
  tblMetadataRange=subset(tblMetadataRange,USE=='Y')
  #!rather than import for exported table, would prefer#odbcConnect() # FM set up to share, but need to setup DSN
  #!sql query to extract tblmetadatarange='select Tbl as SAMPLE_TYPE, * from tblMetadataRange join tblMetadata on tblmetadata.parameter=tblmetadatarange.parameter'
  tblMetadataRange=Xwalk(Source='R',Table="tblMetadataRange",XwalkName='FM',XwalkDirection='_Xwalk',COL=c('STAT','EXPLANATION'))
  tblMetadataRange=tblMetadataRange[,!(names(tblMetadataRange) %in% c('TABLE','POINT','TRANSECT','FLAG','UID'))]
  #!remove final X - probably move the one from importmaster into function
  #!need to check for existing matches, otherwise append OR always append and inactivate (similar to update functions in progress)
  TBL=tblMetadataRange;dbTable='tblMetadataRange'#!setting these variables on the assumption that eventually will be used in for loop for all metadata tables
  sqlSave(wrsa1314,dat=TBL,tablename=dbTable,rownames=F, append=TRUE)
  
  #!tables to do: tblMEtadata, tblMetadataProtocol, Options
  #!Options (have plans to merge and replicate + sync between app (filemaker), WRSAdb (SQL), and ProbSurveyDb(Access))
}