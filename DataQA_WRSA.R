#most run DBpassword_doNotgit.R and DataConsumption_WRSAdb.R setup section first
#must run Project, Year, and Protocol filters in DataConsumption_WRSAdb.R first

#all issues that are found but not dealt with immediately should be recorded in OfficeComments -either on google drive or access database
#all edits should be added to Z:\buglab\Research Projects\BLM_WRSA_Stream_Surveys\Results and Reports\QA\final QC of all sites collected up to Nov 2015\Office_updates.xsls for WRSA data or Z:\buglab\Research Projects\AIM\Analysis\QC\OfficeUpdates for data collected in 2016 and beyond
#follow instructions in Z:\buglab\Research Projects\BLM_WRSA_Stream_Surveys\Technology\WRSA data mananagement page 5 for how to format the data and put it into OfficeUpdates table 
#Jennifer will then copy and paste the edits into the ProbSurvey Access database run the stored export to export a csv. Then run the UpdateDatabase_WRSA.R script and follow its subsequent instructions

#need to run the code below for joins to work
library(plyr)

######################################################################################
#########                          Site Check                                #########
######################################################################################
##First step-do you have all the sites you should?
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR'),Projects=projects,Years=years,Protocols=protocols)
listsites=cast(listsites,'UID~PARAMETER',value='RESULT')


#read in sites from final designations and do a left join to determine if any sampled sites are missing or if sample statuses need to be changed
#if sites are missing check error logs on server or emails and check last modfied date

#do all sites have correct project?
#check all partial sites by running missing data checks below

######################################################################################
########            Comment Check and Office Comments check                 ##########
######################################################################################
#export comments table from SQL ----prefer to do this prior to missing data check because might fill in missing data
    #check for data not entered in fields
    #check for data that needs to be deleted or moved
    #check for data quality issues such as flooding prior to taking water quality
    #check for protocol clarity issues especially dry sites
    #check for dry transects or partial data notes
    #especially review QA comments
    #inactivate resolved or superfluous comments

#review office comments and prioritize anything that might affect below checks----need above site information to link UID to comments and make change


#######################################################################################
########          Preliminary missing data check                            ###########
#######################################################################################
# A preliminary check to make sure all paper field forms were entered, partial sites are flagged and no egregious protocol errors with more than a few indicators with >50% data missing

##missing data parameters
UnionTBL=tblRetrieve(Table='', Years=years, Projects=projects,Protocols=protocols,SiteCodes=sitecodes)
#ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years='years',Protocols='protocols')#Protocols='' brings in failed sites as well

CheckAll='N'#options: 'Y' = Check All Parameters for the protocol; 'N' = Check only Parameters in UnionTBL (i.e. if subsetting UnionTBL to single Table and don't want clutter from parameters not interested in)....this is not done automatically because missing data checks are meant to look for parameters that have ZERO readings for a particular dataset, only use in testing and known scenarios (usually where AllParams='Y')
CommentsCount='N'#'Y' = a comment (as represented by a flag) allows the missing data warning to be ignored; 'N' = missing data is reported regardless and contributes to subsequent percentages. 
MissingXwalk='MissingBackend'; MetricType='Y'#if MetricType=='Y' then groupings will be done by the bin (Type_Xwalk) and the metric type (Indicator, Covariate, QC)

##missing data check
if('POINT' %in% colnames(UnionTBL) ==FALSE){UnionTBL$POINT=NA};if('TRANSECT' %in% colnames(UnionTBL) ==FALSE){UnionTBL$TRANSECT=NA}
Protocols=subset(tblRetrieve(Table='tblVERIFICATION',Parameters='PROTOCOL',UIDS=unique(UnionTBL$UID)),select=c(UID,RESULT))#unique(subset(importmaster,select=RESULT,subset=toupper(PARAMETER)=='PROTOCOL'))
ProtocolsP=unique(Protocols$RESULT)
tblMetadataProtocoltmp=sqlQuery(wrsa1314,sprintf("select * from tblMetadataProtocol"))
emptyFulldataset=tblMetadataProtocoltmp[1,]
emptyFulldataset$TRANSECT=NA;    emptyFulldataset$UID=NA;emptyFulldataset=emptyFulldataset[0,]
for (p in 1:length(ProtocolsP)){
  emptyFulldatasetTmp=emptyFulldataset[0,]
  if(CheckAll=='Y'){PARAMstr=''} else{PARAMstr=sprintf('and PARAMETER in (%s) and SAMPLE_TYPE in (%s)', inLOOP(unique(UnionTBL$PARAMETER)), inLOOP(unique(UnionTBL$SAMPLE_TYPE)))}
  tblMetadataProtocolR=sqlQuery(wrsa1314,sprintf("select * from tblMetadataProtocol where Protocol='%s' and Active='Y' %s",unlist(ProtocolsP[p]),PARAMstr))
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
      reptmp4=subset(reptmp,repcnt>11)#(PARAMETER %in% c('STABLE','COVER','EROSION','LOC','SIZE_CLS'))| (PARAMETER %in% c('WETWID','BARWID') & SAMPLE_TYPE=='THALW'))#parameter subsetting was too unique to 2014 protocol
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
  UIDsADD=unique(subset(UnionTBL,select=UID,subset=UID %in% subset(Protocols,RESULT==ProtocolsP[p])$UID )) #& UID %in% testingUID))
  UIDtmp=emptyFulldatasetTmp
  for (u in 1:nrow(UIDsADD)){  
    UIDtmp$UID=UIDsADD$UID[u]
    emptyFulldataset=rbind(emptyFulldataset,UIDtmp)
  }                         
}

emptytmp=emptyFulldataset#;empty14Jul14=emptyFulldataset

#!method/checking oddities (not sure if these should be handled in emptyFulldataset or MissingCounts)
UnionTBL$TRANSECT=ifelse(UnionTBL$SAMPLE_TYPE %in% c('SLOPEW', 'HABITAT') 
                         & addKEYS(UnionTBL,c('PROTOCOL'))$PROTOCOL %in% c('WRSA14','AK14'),
                         NA,UnionTBL$TRANSECT)#instances where only care if have at least one, but it's recorded as multiple so TRANSECT unable to join in MissingCounts if different
#!posible issues with INCREMENT, but likely warrants database cleanup

#only reporting values with missing counts
MissingCounts=sqldf("select *, case when MissingCount is null then Points else Points-MissingCount end MissingCNT from 
                    (select *, case when Transect is null then 'O' when Transect='' then 'O' else Transect end as TRE from emptyFulldataset where Points>0) as ExpectedCounts
                    outer left join 
                    (select Sample_Type STO, Parameter PO, [UID] as UIDo, case when Transect is null then 'O' when Transect='' then 'O' else Transect end as TR, COUNT(Result) MissingCount, FLAG  from UnionTBL
                    group by Sample_Type, Parameter, [UID], Transect) as ObservedCounts
                    on ExpectedCounts.Sample_Type=ObservedCounts.STO and ExpectedCounts.Parameter=ObservedCounts.PO and ExpectedCounts.TRE=ObservedCounts.TR and ExpectedCounts.UID=ObservedCounts.UIDo
                    where (MissingCount < Points or MissingCount is null) ")
if(CommentsCount=='Y'){
  COMt=unique(ColCheck(tblRetrieve(Table='tblCOMMENTS',UIDS=unique(UnionTBL$UID)),c('SAMPLE_TYPE','TRANSECT','UID','FLAG')))##6692-2898
  FLAGS=unique(subset(UnionTBL,select=c(UID,SAMPLE_TYPE,TRANSECT,FLAG),is.na(FLAG)==FALSE));FLAGS$PRESENT=1#trying to count only comments that don't have a pair = that's why data is missing
  COMt=merge(COMt,FLAGS,all.x=T);COMt=subset(COMt,is.na(PRESENT))
  COMt=unique(ColCheck(COMt,c('SAMPLE_TYPE','TRANSECT','UID')))  
  COMt$TRANSECT=ifelse(COMt$TRANSECT=="ALL","O",COMt$TRANSECT);MissingCounts$TRANSECT=ifelse(is.na(MissingCounts$TRANSECT),"O",MissingCounts$TRANSECT)
  COMt$COMMENTcnt=1
  MissingCounts=merge(MissingCounts,COMt,by=c('SAMPLE_TYPE','TRANSECT','UID'),all.x=T)
} else{MissingCounts$COMMENTcnt=NA}
  
#count number of parameters with missing data
MissingTotals=sqldf('select UID,count(*) ParamCNT from MissingCounts group by UID')


#Xwalk Sample_types before grouping
#MissingCounts$Sample_Type_ORG=MissingCounts$SAMPLE_TYPE; MissingCounts$Parameter_ORG=MissingCounts$PARAMETER#keeping this for peace of mind, may be able to omit
#emptyFulldataset$Sample_Type_ORG=emptyFulldataset$SAMPLE_TYPE; emptyFulldataset$Parameter_ORG=emptyFulldataset$PARAMETER
if (MissingXwalk==''){
  MissingCounts$TABLE=''#;MissingGrouping='SAMPLE_TYPE'
} else{
  MissingCounts=Xwalk(Source='R',XwalkName=MissingXwalk,XwalkDirection='',Table='MissingCounts',COL=colnames(MissingCounts))#,Filter='',UIDS='BLANK',SiteCodes='',Dates='',Years='',Projects='',Protocols='',Parameters='',ALLp='N'){
  emptyFulldataset=Xwalk(Source='R',XwalkName=MissingXwalk,XwalkDirection='',Table='emptyFulldataset',COL=colnames(emptyFulldataset))
  if(MetricType=='Y' & MissingXwalk=='MissingBackend'){
    MissingCounts$SAMPLE_TYPE=sprintf('%s:%s',MissingCounts$SAMPLE_TYPE,MissingCounts$TABLE)
    emptyFulldataset$SAMPLE_TYPE=sprintf('%s:%s',emptyFulldataset$SAMPLE_TYPE,emptyFulldataset$TABLE)
  } #else{MissingGrouping='SAMPLE_TYPE'}#can set custom grouping combinations here as desired for specific xwalks
  
}

#!combine Sample_type and table instead of groupings?
#DONE: need to update MetadataProtocol to account for things that need to contribute to missing data checks: ACTRANSP, etc. ; change SIZE_CLS to SIZE_NUM
#!don't group by parameter(metric name), but join at end
#!fix MissingTotals4 (or did this only matter for paramCNT and commentCNT?); just tell folks this has stopped development unless they would like it



#group by Sample_type + Transect #! likely want a more custom solution with something more akin to QAbins in FM
MissingTotals2=sqldf(sprintf("select UID,SAMPLE_TYPE,TRE as TRANSECT,MC MissCNT, PT ExpectedCNT, cast(ifnull(round(MC/PT,2) ,0) as float) MissingPCT, CC COMMENTcnt from
                     (select UID, SAMPLE_TYPE,count(*) PTC, cast(sum(Points) as float) PT, case when Transect is null then 'O' else Transect end as TRE
                     from emptyFulldataset group by UID, SAMPLE_TYPE, Transect) as ExpectedCounts
                     outer left join 
                     (select UID as UIDo, SAMPLE_TYPE STO,case when Transect is null then 'O' else Transect end as TR, count(*) MCC, cast(sum(MissingCNT) as float) MC , sum(COMMENTcnt) as CC
                     from MissingCounts group by UID, SAMPLE_TYPE, Transect) as ObservedCounts
                     on ExpectedCounts.UID=ObservedCounts.UIDo and ExpectedCounts.Sample_Type=ObservedCounts.STO  and ExpectedCounts.TRE=ObservedCounts.TR 
                     %s",ifelse(CommentsCount=='Y','where COMMENTcnt is null','')))


#group by Sample_type only to get reach totals
MissingTotals3=sqldf("select UID,SAMPLE_TYPE, 'ReachTotal' TRANSECT,sum(MissCNT) MissCNT, sum(ExpectedCNT) ExpectedCNT ,  cast(ifnull(round((cast(sum(MissCNT) as float)/cast(sum(ExpectedCNT) as float) ),2),0) as float) MissingPCT, sum(COMMENTcnt) as COMMENTcnt from MissingTotals2 group by UID, SAMPLE_TYPE")

#main ouput (until param and comments fleshed out more)
if (MissingXwalk==''){
  MissingTotals5=sqldf("select UID, 'REACH' SAMPLE_TYPE,  'O' TRANSECT, ParamCNT,MissCNT,  ExpectedCNT, MissingPCT ,COMMENTcnt from MissingTotals 
                     join 
                     (select UID as UIDm,  sum(MissCNT) MissCNT, sum(ExpectedCNT) ExpectedCNT,sum(MissCNT) /sum(ExpectedCNT)  MissingPCT , sum(COMMENTcnt) as COMMENTcnt from MissingTotals2 group by UID) as Totals
                     on MissingTotals.UID=Totals.UIDm
                     ")
  MissingTotals6=MissingTotals5[,!(names(MissingTotals5) %in% c('ParamCNT'))];MissingTotals7=MissingTotals5;MissingTotals7$MissingPCT=MissingTotals7$ParamCNT;MissingTotals7$SAMPLE_TYPE='Param';MissingTotals7=MissingTotals7[,!(names(MissingTotals7) %in% c('ParamCNT'))]
  MissingTotals4=unique(rbind(MissingTotals2,MissingTotals3,MissingTotals6,MissingTotals7)  );MissingTotals4$RESULT=MissingTotals4$MissingPCT;MissingTotals4$PARAMETER=paste("PCT_",MissingTotals4$SAMPLE_TYPE,"_QA",sep='');MissingTotals4$TRANSECT=ifelse(MissingTotals4$TRANSECT=='O',NA,MissingTotals4$TRANSECT);MissingTotals4$POINT=MissingTotals4$TRANSECT;MissingTotals4=ColCheck(MissingTotals4,c(VAR,'TRANSECT','POINT','PARAMETER','RESULT'))
  print ('ready for use in Access import')
} else{MissingTotals4=unique(rbind(MissingTotals2,MissingTotals3)  )
#! need to fix the revisions to MissingTotals4 before this will work
MissingTotals4=MissingTotals4[,!(names(MissingTotals4) %in% c('POINT'))]
MissingTotalsOUT= addKEYS(cast(subset(MissingTotals4,is.na(UID)==FALSE ), 'UID + TRANSECT  ~ SAMPLE_TYPE',value='MissingPCT' ),Columns=c('SITE_ID','DATE_COL','Protocol')) 
MissingTotalsREACH=subset(MissingTotalsOUT,TRANSECT=='ReachTotal');write.xlsx(MissingTotalsREACH,"MissingDataQCrch_nocom.xlsx")
MissingTotalsTRAN=subset(MissingTotalsOUT,TRANSECT!='ReachTotal');write.xlsx(MissingTotalsTRAN,"MissingDataQCttran_nocom.xlsx")
write.xlsx(MissingCounts,"MissingCounts.xlsx")
}
#combine reach totals
#!revision to MissingTotals4 (once complete, comment out MissingTotals4 and 6 above)
# MissingTotals4=unique(rbind(melt(MissingTotals2,id=c('SAMPLE_TYPE','TRANSECT','UID')),melt(MissingTotals3,id=c('SAMPLE_TYPE','TRANSECT','UID')),melt(MissingTotals5,id=c('SAMPLE_TYPE','TRANSECT','UID'))))
# MissingTotals4$TRANSECT=ifelse(MissingTotals4$variable=='ParamCNT','ReachTotal',MissingTotals4$TRANSECT)
# MissingTotals4=subset(MissingTotals4,variable=='MissingPCT' );MissingTotals4$RESULT=MissingTotals4$value;MissingTotals4$PARAMETER=ifelse(MissingTotals4$variable=='MissingPCT',paste("PCT_",MissingTotals4$SAMPLE_TYPE,"_QA",sep=''),MissingTotals4$variable);MissingTotals4$TRANSECT=ifelse(MissingTotals4$TRANSECT=='O',NA,MissingTotals4$TRANSECT);MissingTotals4$POINT=MissingTotals4$TRANSECT;MissingTotals4=ColCheck(MissingTotals4,c(VAR,'TRANSECT','POINT','PARAMETER','RESULT'))
#! or statement removed from above subset | (variable %in% c('CommentCNT','ParamCNT') & TRANSECT=='ReachTotal')
#!trying to add commentcnt at all levels, but was causing issues in the MissingTotalsOUT pivot and counts seem high and not sure if folks will appreciate
#!paramcnt not working with new melt method

  #print("Warning! The following sites failed missing data checks for the specified number of parameters.")
  #print(MissingTotals5)    
  #checking individual sites#View(subset(MissingCounts,subset=UID==''))#View(subset(importmaster,subset=UID==''))# View(subset(tblCOMMENTSin,subset=UID==''))


#############################################################################
########                         outlier check                 ##############
#############################################################################

##Need to have binMETADATAtemp.csv in your working directory for this script to work. You can get this table from SQL. It is called "tblMETADATAbin".
######need to add IND to output!!!!!!!!!!! in the future

#incoming vs. comparison dataset
UnionTBLsites=unique(UnionTBL$UID)
if(exists('UnionTBLall')){
  UnionTBLsitesALL=unique(UnionTBLall$UID)
  print(sprintf('%s incoming sites will be compared to %s sites in the larger dataset. Review the incoming (UnionTBL) and comparison (UnionTBLall) datasets to confirm before proceeding with outlier checks.',length(UnionTBLsites),length(UnionTBLsitesALL)))
} else {print(sprintf('No larger comparision dataset specified. The incoming dataset (%s sites) will also be used for between site comparison. Set UnionTBLall to a different dataset if desired before proceeding with outlier checks.',length(UnionTBLsites)))
  UnionTBLall=UnionTBL
}

#QA boxplots
give.n <- function(x){return(data.frame(y = max(x)+1, label = paste("n =",length(x))))}#SWJ to do: improve to handle the multiple classes for Categorical 
whisk95 <- function(x) {r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}#supports custom boxplot whiskers (instead of 1.5IQR default)#http://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot
out2SD <- function(x) {
  #subset(x, x < quantile(x)[2] | quantile(x)[4] < x)
  MN=mean(x)
  SD2=2*sd(x)
  subset(x, x < MN-SD2 | MN+SD2 < x)
}#supports custom outliers#http://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot
boxPARAM=function(boxdata,outlierdata,sampsize,facetSTR,titleSTR){
  siteavg=subset(boxdata,subset=STRATATYPE !="UID"  , select=c('UID','STRATATYPE','PARAMRES', 'PARAMCAT'))
  if(grepl('STRATATYPE',facetSTR)){
    siteavg=subset(siteavg,UID==allsites[s])
  } else { siteavg=subset(siteavg,STRATATYPE==typestrata[n])}
  siteavg=aggregate(PARAMRES~PARAMCAT,data=siteavg,FUN='mean')
  boxplot=ggplot(boxdata,aes(y=PARAMRES, x=PARAMCAT,fill=PARAMCAT,colour=PARAMCAT,label=SiteLabelOUT2)) +
  stat_summary(fun.data=whisk95, geom='boxplot',colour='black') + #geom_boxplot(outlier.colour='darkred',outlier.size=10,colour='black') + 
  stat_summary(fun.y=out2SD, geom='point',colour='darkred',size=5,show_guide=F) +
  eval(parse(text=facetSTR)) + #
  geom_hline(aes(yintercept=PARAMRES, colour=PARAMCAT),siteavg,size=1)  + #mark the average for the site
  scale_colour_brewer(drop=FALSE,palette='Set1') + scale_fill_brewer(palette='Set1')+#sync colors between lines and boxplots (especially important for categorical)
  #stat_summary(fun.data =give.n, geom = "text") +
  eval(parse(text=titleSTR)) +
  #geom_text(data=paramQ,aes(label=SiteLabelOUT),show_guide=F,size=3,position= position_jitter(width = 0.5, height=0))+#jitter a little strange, but makes it readable
  #stat_summary(fun.y=out2SD, geom='text',colour='red',show_guide=F) +
  geom_text(show_guide=F,size=3,position= position_jitter(width = 0.15, height=0))+#data=outlierdata,aes(label=SiteLabelOUT2),
  geom_text(aes(label=sprintf('n=%s',SampSize),x=(length(unique(PARAMCAT))/2)+0.5,y=max(PARAMRES)+(max(PARAMRES)/10)),colour='black')+#,x=(length(unique(boxdata$PARAMCAT))/2)+0.5,y=max(boxdata$PARAMRES)+0.25),inherit.aes=FALSE, parse=FALSE)+#annotate("text",x=2,y=max(paramTBL6$PARAMRES)+0.5,label=sprintf('n=%s',paramN$PARAMRES)) +#annotate: n(sites) for strata plots and n(points) for site  (function defined above) #messy for categorical#previous (not working as anticipated, particularly for categorical): #stat_summary(fun.data =give.n, geom = "text") + #function for adding sample size to boxplots #
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank())  #remove x axis 
}# to support similar boxplot structure for stratabox and sitebox
subcol=c('UID','SITE_ID','PARAMETER','STRATATYPE','STRATA','PARAMRES','PARAMCAT','TRANSECT','POINT')
#compile parameter list
dbPARAM=sqlQuery(wrsa1314,"Select SAMPLE_TYPE, PARAMETER, LABEL,VAR_TYPE from tblMETADATA where ACTIVE='TRUE'")#parameter names (SWJ to do: iterate over Sample_Type groups to generate pivots)
params_C=subset(dbPARAM, subset=VAR_TYPE=='CHARACTER')
allparams=unique(paste(UnionTBL$SAMPLE_TYPE,UnionTBL$PARAMETER,sep=" "))#numeric: allparams=c("BANKW INCISED", "BANKW WETWID" )#categorical: allparams=c("CROSSSECW SIZE_CLS","HUMINFLUW WALL")
excludeparams=c("FIELDMEAS DO",'THALW BAR_PRES' ,"THALW BACKWATER",grep("CONSTRAINT",allparams,value=T),grep("VERIF",allparams,value=T),grep("CALIB",allparams,value=T),grep("BERW",allparams,value=T),grep("CHEM",allparams,value=T),grep("PHOTO",allparams,value=T),grep("PACK",allparams,value=T),grep("INVA",allparams,value=T),grep("NOT_COLLECTED",allparams,value=T),"SLOPEW METHOD","FIELDMEAS LOCATION","FIELDMEAS TIME" ,"FIELDMEAS CORRECTED","FIELDMEAS OTH_LOCATION",'CROSSSECW DIST_LB',"FIELDMEAS PROBE_ENDTIME" ,"FIELDMEAS PROBE_ID"       ,"SLOPEW ENDHEIGHT"     ,     "SLOPEW ENDTRAN"  ,   "SLOPEW STARTHEIGHT"        ,    "SLOPEW PROP",   "FIELDMEAS PROBE_STARTTIME",'SLOPEW SLOPE_UNITS','CROSSSECW SUB_5_7','THALW INCREMENT')
combineparams=c("CANCOVERW DENSIOM",'CROSSSECW XSIZE_CLS',grep("LWD",allparams,value=T),grep("HUMINFLU",allparams,value=T),grep("VISRIP",allparams,value=T),grep("FISHCOV",allparams,value=T),grep("ASSESS",allparams,value=T),grep("TORR",allparams,value=T))#need to exclude originals from allparams list and add new names back; some of these may be useable, just want to ponder them a bit more (run a few examples through the existing framework)
#!add Size_Num to combineparams list, use same translation for converting prior to aquamet, revise legal checks for Size_NUM
allparams1=setdiff(allparams,c(excludeparams,combineparams))
UnionTBL2=UnionTBLall#UnionTBL2=subset(UnionTBL,SITE_ID=='EL-LS-8126')
#!the below be redone with Xwalk along with bin?!
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='XSIZE_CLS','SIZE_CLS',UnionTBL2$PARAMETER)#for all our analysis purposes, these are the same
UnionTBL2$PARAMETER=ifelse(UnionTBL2$SAMPLE_TYPE=='LWDW','LWDtally',UnionTBL2$PARAMETER)#xwalk:fmstr#for preliminary analysis purposes, these are the same
UnionTBL2$PARAMETER=ifelse(UnionTBL2$SAMPLE_TYPE=='HUMINFLUW','HumanPresence',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='AGR','AGRicultural',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='IND','INDustrial',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='MAN','MANagement',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='REC','RECreation',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(substr(UnionTBL2$PARAMETER,1,3)=='RES','RESidential',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(UnionTBL2$SAMPLE_TYPE=='TORR' & UnionTBL2$PARAMETER!='TSD011','Torrent',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$SAMPLE_TYPE=ifelse(UnionTBL2$PARAMETER=='VEG_TYPE','VISRIP2W',UnionTBL2$SAMPLE_TYPE)
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='BARE','BARE',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='CANBTRE'|UnionTBL2$PARAMETER=='CANSTRE','CAN_TREE',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='CANVEG'|UnionTBL2$PARAMETER=='UNDERVEG','VEG_TYPE',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='GCNWDY'|UnionTBL2$PARAMETER=='UNDNWDY','NONWOOD',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='GCWDY'|UnionTBL2$PARAMETER=='UNDWDY','WOODY',UnionTBL2$PARAMETER)#xwalk:fmstr
UnionTBL2$PARAMETER=ifelse(UnionTBL2$PARAMETER=='DENSIOM' & UnionTBL2$POINT %in% c('LF','RT'),'DENSIOMbank',ifelse(UnionTBL2$PARAMETER=='DENSIOM' & (UnionTBL2$POINT  %in% c('LF','RT')==FALSE),'DENSIOMcenter',UnionTBL2$PARAMETER))#for preliminary analysis purposes, these need to be divided (and are believed to be separated in aquamet)
combineparamNEW=c('CHEM NTL','CHEM PTL','CANCOVERW DENSIOMbank','CANCOVERW DENSIOMcenter','CROSSSECW SIZE_CLS','LWDW LWDtally','TORR Torrent','ASSESS AGRicultural','ASSESS INDustrial','ASSESS MANagement','ASSESS RECreation','ASSESS RESidential','HUMINFLUW HumanPresence','VISRIPW BARE','VISRIPW CAN_TREE','VISRIP2W VEG_TYPE','VISRIPW NONWOOD','VISRIPW WOODY')##still need to ponder HUMINFLU, VISRIP, FISHCOV, and ASSESS and add back in here
allparams1=union(allparams1,combineparamNEW)#allparams1=allparams1[4:length(allparams1)]
#binned parameters
bin='Y'#'Y' if would like to apply specified binning to results of parameters in binparams
binparams=c("CROSSSECW SIZE_CLS",grep("LWD",allparams1,value=T),grep("CANCOVER",allparams1,value=T),'VISRIP2W VEG_TYPE','TORR Torrent','HUMINFLUW HumanPresence',grep("ASSESS",allparams1,value=T),grep("VISRIP",allparams1,value=T))#also list any parameters that should be treated as categorical that are otherwise in params_N
binMETA=read.csv('binMETADATAtemp.csv')##!feed in from SQL once solified in FM, R, SQL; also used to order categoricals
#set strata for later iteration
typestrata=c('ALL','EcoReg','Size')#must match column names that are created
numstrata=length(typestrata)
UnionTBL2=addKEYS(UnionTBL,'SITE_ID')
UnionTBL2$EcoReg=substr(UnionTBL2$SITE_ID,1,2)#!-- Switch to climatic rather than ecoreg?  ; ; may need to explicitly join an ecoregion column if sitecodes change over different projects and/or to utilize the EPA reference dataset, this works for NRSA only; also needs to be more expandable for additional strata
UnionTBL2$Size=substr(UnionTBL2$SITE_ID,4,5)
UnionTBL2$ALL='ALL'
#! other possible strata: reach width (would need to be calculated), VALXSITE or protocol (especially boatable vs. wadeable)
#this section is highly dependent on WRSA siteID naming structure and GRTS strata
rm(outlierTBL)
for (p in 1:length(allparams1)){#this is a standard loop for iterating, could put it in a function that allows you to plug in a string for the most nested middle
for (p in 2:6){#target specifc parameters during testing or start over mid-process  
  typeparam=strsplit(allparams1[p]," ")
  type=typeparam[[1]][[1]]; param=typeparam[[1]][[2]]
  paramTBL=subset(UnionTBL2,subset=PARAMETER==param & SAMPLE_TYPE==type)#!some where in subsetting and labelling of graphs, parameter was assumed to be unique...it technically is not (i.e. crosssection vs. thalweg depths, boatable) and needs Sample_TYPE in tandem
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
    #set up dataset and outliers for later subsetting by UID and strata in subsequent loops
    paramTBL3$TRANSECT='ALL';paramTBL3$POINT='ALL'
    paramTBL3$POINT=ifelse(paramSTATUS=='CHAR' & paramTBL3$STRATATYPE=='SITE_ID',paramTBL3$IND.y,paramTBL3$POINT)#set up for later N (sample size) use
    paramTBL5=subset(paramTBL3,select=subcol)
    if(paramSTATUS=='NUM'){
      paramTBL$STRATATYPE='UID';paramTBL$STRATA=paramTBL$SITE;paramTBL$PARAMCAT='None'; paramTBL$STRATA=factor(paramTBL$STRATA,levels=unique(paramTBL$STRATA))
      paramTBL6=subset(paramTBL,select=subcol)
      paramTBL6=rbind(paramTBL6,paramTBL5)
      paramN2=aggregate(PARAMRES~STRATATYPE+UID,data=subset(paramTBL6,STRATATYPE=='UID'),FUN='length');colnames(paramN2)=c(colnames(paramN2)[1:2],'SampSizeTMP')
      paramTBL6=merge(paramTBL6,paramN2,by=c('STRATATYPE','UID'),all.x=T)
    } else if(paramSTATUS=='CHAR'){paramTBL6=paramTBL5;paramTBL6$STRATATYPE=ifelse(paramTBL6$STRATATYPE=='SITE_ID','UID',paramTBL6$STRATATYPE);paramTBL6$SampSizeTMP=NA}
    paramTBL6$STRATATYPE=factor(paramTBL6$STRATATYPE,levels=c("UID",typestrata))
    paramTBL6$PARAMCAT=factor(paramTBL6$PARAMCAT)
    #label sample size
    paramN1=aggregate(PARAMRES~STRATATYPE+STRATA+UID,data=subset(paramTBL6,STRATATYPE!='UID'),FUN='length')#to remove PARAMCAT duplicates for CHAR
    paramN1=aggregate(PARAMRES~STRATATYPE+STRATA,paramN1,FUN='length');colnames(paramN1)=c(colnames(paramN1)[1:2],'SampSize')#paramN$STRATATYPE=factor(paramN$STRATATYPE,levels=levels(paramTBL6$STRATATYPE))#might be needed to keep in the same order, unsure
    paramTBL6=merge(paramTBL6,paramN1,by=c('STRATATYPE','STRATA'),all.x=T)
    paramTBL6$SampSizeTMP=ifelse(is.na(paramTBL6$SampSizeTMP),paramTBL6$POINT,paramTBL6$SampSizeTMP);paramTBL6$SampSize=ifelse(is.na(paramTBL6$SampSize),paramTBL6$SampSizeTMP,paramTBL6$SampSize)
    if(paramSTATUS=='CHAR'){paramN$PARAMRES=ifelse(paramN$STRATATYPE=='UID',min(paramTBL6$POINT),round(paramN$PARAMRES/length(unique(paramTBL6$PARAMCAT)),0))}
    #label quantiles with SiteCode
    paramquant=aggregate(PARAMRES~STRATATYPE+PARAMCAT+STRATA,data=paramTBL6,FUN='quantile',probs=c(0.05,0.95),names=FALSE);colnames(paramquant)=c('STRATATYPE','PARAMCAT','STRATA','Quant')
    paramTBL6=merge(paramTBL6,paramquant,by=c('STRATATYPE','PARAMCAT','STRATA'),all.x=T)
    paramTBL6$SiteLabelOUT=ifelse(paramTBL6$PARAMRES<paramTBL6$Quant[,1],paramTBL6$SITE_ID,ifelse(paramTBL6$PARAMRES>paramTBL6$Quant[,2],paramTBL6$SITE_ID,NA))#create a site label if an outlier
    paramTBL6$SiteLabelOUT=ifelse(paramTBL6$STRATATYPE=="UID"& paramSTATUS=='CHAR',NA, ifelse(paramTBL6$STRATATYPE=="UID" & is.na(paramTBL6$SiteLabelOUT)==FALSE,paste(paramTBL6$TRANSECT,paramTBL6$POINT,sep=":"),paramTBL6$SiteLabelOUT))#change site label to transect if raw data
    paramQ=subset(paramTBL6,is.na(SiteLabelOUT)==FALSE)
    #label outliers with SiteCode
    paramoutlrM=aggregate(PARAMRES~STRATATYPE+PARAMCAT+STRATA,data=paramTBL6,FUN='mean');colnames(paramoutlrM)=c('STRATATYPE','PARAMCAT','STRATA','Mean')
    paramoutlrS=aggregate(PARAMRES~STRATATYPE+PARAMCAT+STRATA,data=paramTBL6,FUN='sd');colnames(paramoutlrS)=c('STRATATYPE','PARAMCAT','STRATA','SD')
    paramTBL6=merge(paramTBL6,paramoutlrM,by=c('STRATATYPE','PARAMCAT','STRATA'));paramTBL6=merge(paramTBL6,paramoutlrS,by=c('STRATATYPE','PARAMCAT','STRATA'))
    paramTBL6$SiteLabelOUT2=ifelse(paramTBL6$PARAMRES>(paramTBL6$Mean + (2*paramTBL6$SD)),paramTBL6$SITE_ID,ifelse(paramTBL6$PARAMRES<(paramTBL6$Mean - (2*paramTBL6$SD)),paramTBL6$SITE_ID,NA))#create a site label if an outlier
    paramTBL6$SiteLabelOUT2=ifelse(paramTBL6$STRATATYPE=="UID"& paramSTATUS=='CHAR',NA, ifelse(paramTBL6$STRATATYPE=="UID" & is.na(paramTBL6$SiteLabelOUT2)==FALSE,paste(paramTBL6$TRANSECT,paramTBL6$POINT,sep=":"),paramTBL6$SiteLabelOUT2))#change site label to transect if raw data
    paramTBL6$SiteLabelOUT2=ifelse(is.na(paramTBL6$SiteLabelOUT2),'',paramTBL6$SiteLabelOUT2)
    paramMSD=subset(paramTBL6,is.na(SiteLabelOUT2)==FALSE & SiteLabelOUT2!='');paramMSDpres=nrow(paramMSD); if(paramMSDpres==0){paramMSD=paramTBL6[1,];paramMSD$SiteLabelOUT2=''}#geom_text will fail if no rows are present
    if(paramMSDpres>0){
      if(exists('outlierTBL')) {
        outlierTBL=rbind(outlierTBL,paramMSD)
      } else {outlierTBL=paramMSD}
    }
    allsites=intersect(unique(paramTBL$UID),unique(UnionTBL$UID))#only iterate over sites in the incoming/subset dataset (UnionTBL)
    for (s in 1:length(allsites)){
    #for (s in 1:3){#to test a smaller subset
      stratas=unique(subset(paramTBL6,select=STRATA,subset=UID==allsites[s]))
      paramTBL7=subset(paramTBL6,subset=STRATA %in% stratas$STRATA)
      #generate box plot in ggplot2
    if(max(paramTBL7$UID==allsites[s] & paramTBL7$SiteLabelOUT2!='')==1){#only print plot if it has outliers within the site or when compared to the strata
      sitebox=boxPARAM(boxdata=paramTBL7,sampsize=paramN,facetSTR='facet_grid(.~STRATATYPE)',titleSTR='labs(title=sprintf("SITE- %s (%s) ~ PARAM- %s",unique(subset(boxdata, subset=STRATATYPE=="UID", select=SITE_ID)),allsites[s],param))')
      ggsave(filename=sprintf('%s.jpg',sitebox$labels$title),plot=sitebox)#assign(sitebox$labels$title,sitebox)#save jpeg or assign var (need to refine naming)
      }
      
    }
  
    for (n in 1:numstrata) {#re-enter for loop now that all STRATA are complete and aggregated
    if(nrow(subset(paramMSD,STRATATYPE==typestrata[n]))>0){
      paramTBL4=subset(paramTBL6,subset=STRATATYPE==typestrata[n])
      stratabox=boxPARAM(boxdata=paramTBL4,sampsize=paramN,facetSTR='facet_grid(.~STRATA)',titleSTR='labs (title=sprintf("STRATA- %s ~ PARAM- %s",typestrata[n],param))')
      ggsave(filename=sprintf('%s.jpg',stratabox$labels$title),plot=stratabox)#assign(stratabox$labels$title,stratabox)#save jpeg or # assign(sprintf('box_STRATA_%s_%s',typestrata[n],param),stratabox)
    }
    }
  } }
rm(paramTBL3,paramTBL4,paramTBL6,paramTBL5,paramTBL3a,paramTBL3b)

outlierTBL=unique(subset(outlierTBL,select=c('STRATATYPE','STRATA','SITE_ID','UID','PARAMETER','PARAMCAT','TRANSECT','POINT','PARAMRES','Mean','SD')))
stratat=unique(outlierTBL$STRATATYPE)
for (s in 1:length(stratat)){
  outlierTBLst=subset(outlierTBL,subset=STRATATYPE==stratat[s])
  write.csv(outlierTBLst,file=sprintf('Outliers_2SDmean_%s.csv',stratat[s]))#could export as a single table, but a bit overwhelming
}}
#! also output allparams1 to know what was checked (And which were excluded - excludeparams) --> will be easier once in xwalk
#SWJ to do (2/11/14):
#print cv or other metric as a warning for the spread? compare cv of site to avg cv of all/strata sites? (Scott--> cv only for repeatable data, didn't give alternate spread)
#mimic labelling of site boxplots in strata
#add "all" boxplot in strata

#######################################################################
########               summary stats                       ############
#######################################################################

#retrieve all possible tables by protocol groups and pivot
#for exploratory purposes to review data and determine expected values, not intended to replace modular SQL solutions for multiple tools
#UnionTBLnum_pvtQUANTmean_Site is pretty hard to look at. I prefer to look at UnionTBLnum_pvtSUMMARYn_Site sort by parameter and then sort values to look at min and maxes for each parameter across all sites
#uses tblMETADATA to pull what parameters are in these files so tblMETADATA must be up to date for this to pull newer parameters

tblCOL=c('UID', 'SAMPLE_TYPE','PARAMETER','RESULT','TRANSECT','POINT')
pvtCOL='UID %s ~ SAMPLE_TYPE + PARAMETER';pvtCOLdefault=sprintf(pvtCOL,'');pvtCOL2=sprintf(pvtCOL,'+ TRANSECT + POINT')
AggLevel='Site'#options = Site, All
dbPARAM=sqlQuery(wrsa1314,"Select SAMPLE_TYPE, PARAMETER, LABEL,VAR_TYPE from tblMETADATA where ACTIVE='TRUE'")#parameter names (SWJ to do: iterate over Sample_Type groups to generate pivots)
params_N=subset(dbPARAM, subset=VAR_TYPE=='NUMERIC')
tblNAME='UnionTBLnum'
  if(min(c('SAMPLE_TYPE',tblCOL) %in% colnames(UnionTBL))==1){#if minimum needed columns are present, proceed, otherwise assume it is a pivoted or otherwise human readable table
        #summarized quantitative data (average values per pivot cell which is per site per parameter)
        tblNUM=subset(UnionTBL,subset=PARAMETER %in% params_N$PARAMETER )
        if(nrow(tblNUM)>1){#only assign pivot to variable if not empty and only dive into subsequent if not empty
          if(AggLevel=='Site'){pvtCOL4='UID + SAMPLE_TYPE + PARAMETER ~.'; pvtCOL5='RESULT~UID + SAMPLE_TYPE + PARAMETER'; colUID='tblPVTnSUM2$UID';nameUID=c('UID','SAMPLE_TYPE','PARAMETER','Quant1','Quant2')} else if (AggLevel=='All') {pvtCOL4='SAMPLE_TYPE + PARAMETER ~.';pvtCOL5='RESULT~SAMPLE_TYPE + PARAMETER';colUID='';nameUID=c('SAMPLE_TYPE','PARAMETER','Quant1','Quant2')}
          tblNUM$RESULT=as.numeric(tblNUM$RESULT)
          tblNUM=subset(tblNUM,subset= is.na(RESULT)==FALSE)#apparently not removing NAs during pivot aggregation, so done manually because causing errors - have to do after conversion to number
          tblPVTn=cast(tblNUM, eval(parse(text=pvtCOLdefault)),value='RESULT',fun.aggregate='mean')#pivot reach average by site
          tblPVTnSUM1=cast(tblNUM, eval(parse(text=pvtCOL4)),value='RESULT',fun.aggregate=c(length,mean,median,min,max,sd),fill='NA') # pivot summary stats by all sites combined or individual sites
          tblPVTnSUM2=aggregate(eval(parse(text=pvtCOL5)),data=tblNUM,FUN='quantile',probs=c(0.25,0.75),names=FALSE)
          tblPVTnSUM2=data.frame(cbind(eval(parse(text=colUID)),tblPVTnSUM2$SAMPLE_TYPE,tblPVTnSUM2$PARAMETER,tblPVTnSUM2$RESULT[,1],tblPVTnSUM2$RESULT[,2]));colnames(tblPVTnSUM2)=nameUID
          tblPVTnSUM=merge(tblPVTnSUM1,tblPVTnSUM2,by=setdiff(nameUID,c('Quant1','Quant2')))
          #need to do this by UID for WRSA13 QA duplicate comparison
          assign(sprintf('%s_pvtQUANTmean_%s',tblNAME,AggLevel),tblPVTn)
          assign(sprintf('%s_pvtSUMMARYn_%s',tblNAME,AggLevel),tblPVTnSUM)
      }
    }

#export results
QUANTtbls=c(grep('pvtQUANTmean',ls(),value=T),setdiff(grep('pvtSUMMARYn',ls(),value=T),"pvtSUMMARYn"))
for (t in 1:length(QUANTtbls)){
  write.csv(eval(parse(text=QUANTtbls[t])),sprintf('%s.csv',QUANTtbls[t]))#could merge-pvtQUANTmean_, but I like them grouped by categories
}

#####################################################################################################
#########                             Parameter Specific Checks                             #########
#####################################################################################################

######## GPS ##########
#get all coordinates and site metadata
listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR','Z_DISTANCEFROMX','TRCHLEN','REPEAT_VISIT','SLIDE_YN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
listsites=cast(listsites,'UID~PARAMETER',value='RESULT')

#Check all Z_DISTANCEFROMX to verify within 250 or 500m or allowable distance to be slid
SlideIssues=subset(listsites,as.numeric(Z_DISTANCEFROMX)>0.25)# not working because of "?"
write.csv(SlideIssues,'SlideIssues.csv')

#Check for merged sites
Merge=subset(listsites,REPEAT_VISIT!='N')
write.csv(Merge,'Merge.csv')
#look at FieldTracking or ScoutTracking spreadsheets on the google drive and fill in as needed

#Check that the straight-line distance between BR and TR does not exceed the total reach length (i.e. sinuosity <1 should never happen)
listsites$straightline=acos(sin(as.numeric(listsites$LAT_DD_BR)*3.141593/180)*sin(as.numeric(listsites$LAT_DD_TR)*3.141593/180) + cos(as.numeric(listsites$LAT_DD_BR)*3.141593/180)*cos(as.numeric(listsites$LAT_DD_TR)*3.141593/180)*cos(as.numeric(listsites$LON_DD_TR)*3.141593/180-as.numeric(listsites$LON_DD_BR)*3.141593/180)) * 6371000
listsites$SINUOSITY=as.numeric(listsites$TRCHLEN)/as.numeric(listsites$straightline)
SinuosityCheck=subset(listsites,SINUOSITY<1)
write.csv(SinuosityCheck,'SinuosityCheck.csv')

#After all GPS coordinates check out export coordinates for Ryan Lokteff or GIS tech to compute WestWide bug OE model and EC,TN,TP models
write.csv(listsites,'postseason_site_coordinates.csv')

#get other useful information associated with sites
#eventually read in design table from SQL but for now the table should be read in from the path below to compare original coordinates with those that were collected
designs=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\AIM_DataManagement\\ProjectMngtSystem\\design_table2.csv')
postseasonmetadata=join(listsites,designs, by="SITE_ID", type="left")
#get ecoregional and stream size info for context for values
#designmetadata=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\GRTS_CodeGuidance\\MasterSample\\MasterSampleDraws\\Aquatic\\LoticMasterSample\\Attributed\\LoticMasterSampleAttributedPtsWithHybridEcoregions.csv')
#postseasonmetadata=join(postseason,designmetadata, by="MS_ID", type="left")
#eventually need to edit the read in csv above to reflect the sampled coordinates for future sample draws at the end of the season
postseasonmetadata$CALC_DISTFROMX=acos(sin(as.numeric(postseasonmetadata$LAT_DD)*3.141593/180)*sin(as.numeric(postseasonmetadata$DESIGN_LAT)*3.141593/180) + cos(as.numeric(postseasonmetadata$LAT_DD)*3.141593/180)*cos(as.numeric(postseasonmetadata$DESIGN_LAT)*3.141593/180)*cos(as.numeric(postseasonmetadata$DESIGN_LON)*3.141593/180-as.numeric(postseasonmetadata$LON_DD)*3.141593/180)) * 6371000



####### Bugs #########
#get all bug data
Bugs=tblRetrieve(Parameters=c('JAR_NO','ACTUAL_DATE','AREA_SAMP','TRAN_NUM','SAMPLER','AREA','BUG_METHOD','VALXSITE','PROTOCOL','PROJECT'),Years=years, Projects=projects,Protocol=protocols,SiteCodes=sitecodes)
Bugspvt=addKEYS(cast(Bugs,'UID~SAMPLE_TYPE+PARAMETER',value='RESULT'),c('SITE_ID','DATE_COL'))


#check surber net and mini surber net areas because they were wrong in the app at the begining of 2016 field season
AreaCheck1=subset(Bugspvt,(as.numeric(BERW_AREA_SAMP)!=0.093 & BERW_SAMPLER=='SU'))
AreaCheck2=subset(Bugspvt,(as.numeric(BERW_AREA_SAMP)!=0.0413 & BERW_SAMPLER=='MI'))
AreaCheck3=subset(Bugspvt,(as.numeric(BERW_AREA_SAMP)!=0.093 & BERW_SAMPLER=='KC'))
write.csv(AreaCheck1,'AreaCheck1.csv')
write.csv(AreaCheck2,'AreaCheck2.csv')
write.csv(AreaCheck3,'AreaCheck3.csv')


#check to make sure 8 or 11 TRAN_NUM at all sites
SamplingCheck1=subset(Bugspvt,(BERW_TRAN_NUM<8 & BERW_BUG_METHOD=='TARGETED RIFFLE'))
SamplingCheck2=subset(Bugspvt,(BERW_TRAN_NUM<11 & BERW_BUG_METHOD=='REACH WIDE'))
write.csv(SamplingCheck1,'SamplingCheck1.csv')
write.csv(SamplingCheck2,'SamplingCheck2.csv')

#get data ready to submit via http://www.usu.edu/buglab/SampleProcessing/SampleSubmission/
postseasonmetadata=subset(postseasonmetadata,PROTOCOL!='FAILED')
SubMetadata=c('UID','SITE_ID', 'COUNTY','STREAM_NAME','LAT_DD','LON_DD','PROJECT','STRATUM')
Metadata=postseasonmetadata[SubMetadata]
Bugspvtsub=c('SITE_ID','BERW_JAR_NO','BERW_ACTUAL_DATE','BERW_AREA','BERW_SAMPLER','BERW_BUG_METHOD')
Bugspvt2=Bugspvt[Bugspvtsub]
BugsSubmit=join(Bugspvt2,Metadata, by="SITE_ID")
BugsSubmit$BERW_SAMPLER=ifelse(BugsSubmit$BERW_SAMPLER=='SU'|BugsSubmit$BERW_SAMPLER=='MI',"Surber net",ifelse(BugsSubmit$BERW_SAMPLER=='KC',"Kick net",BugsSubmit$BERW_SAMPLER))
BugsSubmit=BugsSubmit[,c(12,13,1,2,9,10,11,3,5,6,4,7,8)]#fill in desired columns
write.csv(BugsSubmit,'BugsSubmit.csv')
                      
                      
######## WQ checks ########
#corrected for temperature check
WQ2=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED','TEMPERATURE'), Comments='Y', Years=years, Projects=projects,SiteCodes=sitecodes)
WQ1=cast(WQ2,'UID~PARAMETER',value='RESULT')
WQind=cast(WQ2,'UID~PARAMETER',value='IND')
WQ3=addKEYS(merge(WQ1,WQind,by=c('UID'),all=T) ,c('SITE_ID','DATE_COL','CREW_LEADER'))
WQ3.sub=subset(WQ3,CORRECTED.x!='Y')
#write.csv(WQ3.sub,'not_temp_corrected_conduct.csv')

#Chem check the hours prior to freezing
WQtbl=tblRetrieve(Parameters=c('NTL','PTL','TN_PRED','TP_PRED','TIME_UNFROZEN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
WQpvt=addKEYS(cast(WQtbl,'UID~PARAMETER',value='RESULT'),c('SITE_ID','CREW_LEADER'))
WQpvt$OE_TN=WQpvt$NTL-WQpvt$TN_PRED
WQpvt$OE_TP=WQpvt$PTL-WQpvt$TP_PRED
#graph time_unfrozen with raw values and OE values
plot(WQpvt$TIME_UNFROZEN,WQvt$OE_TN)
plot(WQpvt$TIME_UNFROZEN,WQvt$OE_TP) 
                      
#make condition determination and average time_unfrozen in each condition
WQpvt$OE_TNrtg=ifelse(WQpvt$OE_TN <=52.1,'Good',ifelse(WQpvt$OE_TN >114.7, 'Poor','Fair'))
WQpvt$OE_TPrtg=ifelse(WQpvt$OE_TP <=9.9,'Good',ifelse(WQpvt$OE_TP >21.3, 'Poor','Fair'))
ConditionCheck1=aggregate(TIME_UNFROZEN~OE_TNrtg, data=WQpvt, FUN=mean)
ConditionCheck2=aggregate(TIME_UNFROZEN~OE_TPrtg, data=WQpvt, FUN=mean) 
                      
#view any typical values violations for Conductivity and PH
Conduct=addKEYS(tblRetrieve(Comments='Y',Parameters=c('CONDUCTIVITY'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes),c('SITE_ID'))
ConductQuestions=subset(Conduct,RESULT<100 | RESULT>600)# review comments for any sites flagged here 
postseasonmetadata_ecoregion=postseasonmetadata[,c('UID','ECO_10')]
ConductQuestions=join(ConductQuestions,postseasonmetadata_ecoregion, by="UID",type="left")
write.csv(ConductQuestions,'ConductQuestions.csv')
PH=addKEYS(tblRetrieve(Comments='Y',Parameters=c('PH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes),c('SITE_ID'))
PHQuestions=subset(PH,RESULT<6 | RESULT>9)# review comments for any sites flagged here
PHQuestions=join(PHQuestions,postseasonmetadata_ecoregion, by="UID",type="left")
write.csv(PHQuestions,'PHQuestions.csv')

#compare any questionable values to ecoregional EPA data
#should automate this process so that only values that fall outside this range get flagged ---need to join the ecoregion data to the design table; can't remember where all that site metadata from the master sample ended up
  #could pull code from condition determinations to do that but this code will be pretty complex....for now just leave as a manual task
read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\EPA_WQ_typical_values.csv')

#instrument check
#pull all data for instrument if values still not resolved after looking at ecoregional values # not as pertinent this year and in the future because the same instrument will be used for any given proejct and confounded with ecoregional differences
WQ2=tblRetrieve(Parameters=c('CONDUCTIVITY','PH','CAL_INST_ID'), Comments='Y', Years=years, Projects=projects)
WQ1=addKEYS(cast(WQ2,'UID~PARAMETER',value='RESULT')  ,c('SITE_ID','DATE_COL','CREW_LEADER'))
WQ1=subset(WQ1,CAL_INST_ID=='')# fill in data of interest here

                      
####### incision and bank height ############
#cross checks implemented in app so just check extreme values, outliers (above), and for unit issues
heights=addKEYS(tblRetrieve(Parameters=c('INCISED','BANKHT'),Years=years, Projects=projects,SiteCodes=sitecodes),c('SITE_ID','DATE_COL','CREW_LEADER'))
HeightCheck1=subset(heights,RESULT>1.5|RESULT<0.1)
View(HeightCheck1)
write.csv(HeightCheck1,'HeightCheck1.csv')
                      
                      
#######   width  #########
#cross checks implemented in app and extreme values hard to catch/ not so important so just check for outliers(above), and for protocol issues related to dry sites                       
Depths=tblRetrieve(Parameters=c('DEPTH'),Years=years, Projects=projects,SiteCodes=sitecodes)
Depths=subset(Depths,RESULT==0 & POINT==1)# query wetted width for these transects and UIDs in SQL and check if it is 0 and TRANDRY='Y' 
write.csv(Depths,'Depths.csv')
#do the opposite query
Widths=tblRetrieve(Parameters=c('WETWID','BANKWID','TRANDRY'),Years=years, Projects=projects,SiteCodes=sitecodes)                      
pvtWidths=cast(Widths,'UID+TRANSECT~PARAMETER',value='RESULT')
Widths=subset(Widths,RESULT==0)# query the corresponding thalweg depths in SQL if present and the VALXSITE to make sure it is INTWADE                      
write.csv(Widths,'Widths.csv')#something is wrong because this is aggregating things to counts
#dry transects
DryCheck=subset(pvtWidths,(WETWID!=0 & TRANDRY=='Y')|(WETWID==0 & TRANDRY=='N')|(WETWID==0 & is.na(TRANDRY)=='TRUE'))#likely needs tweaking                      
write.csv(DryCheck,'DryCheck.csv')   

# #interrupted flow checks---easier to do directly in SQL so see check_interrupted_sites SQL script
# Interrupt=tblRetrieve(Parameters=c('VALXSITE'),Years=years, Projects=projects,SiteCodes=sitecodes)
# Interrupt=subset(Interrupt,RESULT=='INTWADE')
# DryTran=tblRetrieve(Parameters=c('TRANDRY'),Years=years, Projects=projects,SiteCodes=sitecodes)
# Interrupt_CHECK=join(Interrupt,DryTran, by=c('UID'))
# DryTran=subset(DryTran,RESULT=='Y')
                      
####### substrate #######
#get sediment data
Sediment=tblRetrieve(Parameters=c('SIZE_CLS','XSIZE_CLS'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
Sed2014=tblRetrieve(Parameters=c('SIZE_NUM','LOC'),Projects=projects,Years=years,Protocols=protocols)
A_Sed2014=cast(Sed2014,'UID+TRANSECT+POINT~PARAMETER', value='RESULT')

##check % of paricles that were "other" categories
#2013 data
Sediment_OT=subset(Sediment,RESULT=="OT"| RESULT=="WD")
Sediment_SED=subset(Sediment,RESULT!="OT"& RESULT!="WD")
Sed_OT=setNames(count(Sediment_OT,"UID"),c('UID','countOT'))
Sed_SED=setNames(count(Sediment_SED,"UID"),c('UID','countSED'))
Sed_count=merge(Sed_OT,Sed_SED,by='UID',all=T)
Sed_count$PCT=Sed_count$countOT/(Sed_count$countOT+Sed_count$countSED)*100#if greater than 10% look to see what the OT is
write.csv(Sed_count,'OtherCount2013.csv')
#2014+ data
Sed2014_OT=subset(Sed2014,RESULT==0 & PARAMETER=='SIZE_NUM')
Sed2014_SED=subset(Sed2014, RESULT!=0 & PARAMETER=='SIZE_NUM')
A_Sed2014_OT=setNames(cast(Sed2014_OT,'UID~PARAMETER', value='RESULT',fun=length),c('UID','countOT'))
A_Sed2014_SED=setNames(cast(Sed2014_SED,'UID~PARAMETER', value='RESULT',fun=length),c('UID','countSED'))
A_Sed2014_count=merge(A_Sed2014_OT,A_Sed2014_SED,by='UID',all=T)
A_Sed2014_count$PCT=A_Sed2014_count$countOT/(A_Sed2014_count$countOT+A_Sed2014_count$countSED)*100#if greater than 10% look to see what the OT is
write.csv(A_Sed2014_count,'OtherCount2014.csv')

#check sample sizes and that bed and bank protocols were followed                       
C_Sed2014=A_Sed2014[!A_Sed2014$LOC== "BANK", ]
Nbed_Sed2014pvt=aggregate(.~UID, data=C_Sed2014, length)#number of bed pebbles
Nbed_Sed2014pvt=setNames(Nbed_Sed2014pvt[,c(1,5)],c("UID","nbed"))
Nall_Sed2014pvt=setNames(cast(Sed2014,'UID~PARAMETER',value='RESULT',fun=length),c("UID","nLOC","nall"))#number of all collected pebbles
Nall_Sed2014pvt=Nall_Sed2014pvt[,c(1,3)]
sample_size=addKEYS(join(Nbed_Sed2014pvt,Nall_Sed2014pvt, by="UID"),c('SITE_ID'))                      
write.csv(sample_size,'sed_sample_size.csv')

###### Angle  ########                      
#outlier checks (above) and check for missing SLANT to check if angle was being calculated in app properly
Angle=tblRetrieve(Parameters=c('ANGLE180','SLANT'),Years=years, Projects=projects,SiteCodes=sitecodes)
pvtAngle=addKEYS(cast(Angle,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'), c('SITE_ID','DATE_COL','CREW_LEADER'))                     
AngleCheck1=subset(pvtAngle, is.na(SLANT=='TRUE'))                      
AngleCheck2=subset(pvtAngle, SLANT=='OB' & as.numeric(ANGLE180)<90)  
write.csv(AngleCheck1,'AngleCheck1.csv')
write.csv(AngleCheck2,'AngleCheck2.csv')
                 
#######  Riparian Veg  ########
#overstory >100% check
riparian1=tblRetrieve(Parameters=c('CANBTRE','CANSTRE'),Years=years, Projects=projects,SiteCodes=sitecodes)
riparian1PVT=cast(riparian1,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian1PVT_IND=cast(riparian1,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian1pvt=merge(riparian1PVT,riparian1PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian1PVTsub=subset(riparian1pvt,(CANBTRE.x==4 & CANSTRE.x>2) | (CANSTRE.x==4 & CANBTRE.x>2))

#middlestory >100% check
riparian2=tblRetrieve(Parameters=c('UNDNWDY','UNDWDY'),Years=years, Projects=projects,SiteCodes=sitecodes)
riparian2PVT=cast(riparian2,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian2PVT_IND=cast(riparian2,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian2pvt=merge(riparian2PVT,riparian2PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian2PVTsub=subset(riparian2pvt,(UNDNWDY.x==4 & UNDWDY.x>2) | (UNDWDY.x==4 & UNDNWDY.x>2))

#understory >100% check
riparian3=tblRetrieve(Parameters=c('GCNWDY','GCWDY','BARE'),Years=years, Projects=projects,SiteCodes=sitecodes)
riparian3PVT=cast(riparian3,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian3PVT_IND=cast(riparian3,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian3pvt=merge(riparian3PVT,riparian3PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian3PVTsub=subset(riparian3pvt,(GCNWDY.x==4 & GCWDY.x>2) | (GCWDY.x==4 & GCNWDY.x>2) | (GCNWDY.x==4 & BARE.x>2) | (BARE.x==4 & GCNWDY.x>2) | (BARE.x==4 & GCWDY.x>2) | (GCWDY.x==4 & BARE.x>2))

#veg type missing check
#canopy
riparian4=tblRetrieve(Parameters=c('CANVEG','CANBTRE','CANSTRE'),Years=years, Projects=projects,SiteCodes=sitecodes)
riparian4PVT=cast(riparian4,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian4pvtsub=subset(riparian4PVT,CANVEG=='N' & CANBTRE>0 & CANSTRE>0)

riparian4PVT_IND=cast(riparian4,'UID+TRANSECT+POINT~PARAMETER',value='IND')
riparian4pvt=merge(riparian4PVT,riparian4PVT_IND,by=c('UID','TRANSECT','POINT'),all=T)
riparian4pvtsub2=subset(riparian4pvt,CANVEG.x!='N' & CANBTRE.x==0 & CANSTRE.x==0)

#middle
riparian5=tblRetrieve(Parameters=c('UNDERVEG','UNDNWDY','UNDWDY'),Years=years, Projects=projects,SiteCodes=sitecodes)
riparian5PVT=cast(riparian5,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
riparian5pvtsub=subset(riparian5PVT,UNDERVEG=='N' & UNDNWDY>0 & UNDWDY>0)
                      
                      
                      
###### Thalweg   #########

#Custom thalweg missing data because number of station is dynamic
#run thalweg_completion_check SQL query currently under JC Projects folder locally but want to migrate to R or to a view in SQL
tbl=tblRetrieve(Parameters=c('DEPTH'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
tbl.2=tblRetrieve(Parameters=c('NUM_THALWEG'),Project=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
tbl3=cast(tbl.2,'UID~PARAMETER',value='RESULT',mean)
tbl.PVT=addKEYS(cast(tbl,'UID~PARAMETER',value='RESULT'),c('SITE_ID'))# count is default
thalweg.missing=merge(tbl.PVT,tbl3, by='UID')
thalweg.missing$PctComplete=thalweg.missing$DEPTH/(thalweg.missing$NUM_THALWEG*10)*100
write.csv(thalweg.missing,'thalweg.missing.csv')

#Increment cross-validation WRSA checks
incrementcheck=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT','RCHWIDTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
incrsub=subset(incrementcheck,UID!='1500BC4F-C9B3-4FFC-9639-B5054B0FCD62')#UID:10383  IND 4849393 needs to be deactivated for this to work
incrementPVT=cast(incrsub,'UID~PARAMETER',value='RESULT')
incrementsub=subset(incrementPVT,TRCHLEN/0.01!=INCREMENT)#couldn't get this to work so checked this manually in excel and also checked to make sure that RCHWIDTH*40=TRCHLEN and for RCHWIDTH<2.5 INCREMENT=1 and for RCHWIDTH>2.5<4 INCREMENT=1.5
write.csv(incrementPVT,'incrementPVT.csv')
#weridinc=tblRetrieve(Parameters=c('INCREMENT'),UIDS='11852')
                      
#thalweg checks####couldn't get this to work so did cell referencing in excel to interpolate between values with 1 missing value inbetween
thalweg<-tblRetrieve(Parameters='DEPTH',Projects=projects,Years=years, Protocols=protocols)
thalweg_depth<-subset(thalweg,SAMPLE_TYPE!='CROSSSECW')
thalweg_depth_pvt<-cast(thalweg_depth,'UID+TRANSECT~POINT', value='RESULT')
thalweg_depth_pvt_order<-thalweg_depth_pvt[with(thalweg_depth_pvt, order(1,29))]
thaleg_depth_NA<-thalweg_depth_pvt [is.na(thalweg_depth_pvt$'1')==TRUE,c(1:2,4)]

#check too deep variable for any depths that need trig 
toodeep=tblRetrieve(Comments='Y',Parameters=c('TOODEEP','DEPTH_ANGLE','DEPTH'),Projects=projects,Years=years, Protocols=protocols,SiteCodes=sitecodes)
pvttoodeep=cast(toodeep,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
TooDeepCheck=subset(pvttoodeep,TOODEEP=='Y')
toodeepcomments=join(TooDeepCheck)

#thalweg depth/ width EPA check
#wading sites-----------not curenly working depths not being joined properly
depthcheck=tblRetrieve(Parameters=c('DEPTH'), Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
depthcheck.sub=subset(depthcheck,SAMPLE_TYPE!='CROSSSECW')
pvtdepthcheck=cast(depthcheck.sub,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
pvtdepthcheck.sub=subset(pvtdepthcheck,POINT=='1')#1 for 2016 and 0 for pre-2016
width=tblRetrieve(Parameters=c('WETWID'),Projects=projects, Years=years,Protocols=protocols)
check=join_all(list(pvtdepthcheck.sub,width),by=c('UID','TRANSECT'))
check$RESULT=as.numeric(check$RESULT)
check$DEPTH=as.numeric(check$DEPTH)
odd_ratio=addKEYS(subset(check,RESULT/(DEPTH/100)>50|RESULT/(DEPTH/100)<1),c('SITE_ID'))
#boating sites
odd_ratio=subset(check,RESULT/(DEPTH)>50|RESULT/(DEPTH)<1)

                 
                 
#########  slope   ################                    
Slope=tblRetrieve(Parameters=c('AVGSLOPE','SLPRCHLEN','TRCHLEN','PARTIAL_RCHLEN','POOLRCHLEN','SLOPE_COLLECT','PCT_GRADE','VALXSITE'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)                 
pvtSlope=addKEYS(cast(Slope,'UID~PARAMETER',value='RESULT'), c('SITE_ID','CREW_LEADER'))                
SlopeCheck1=subset(pvtSlope,as.numeric(PCT_GRADE)>14|as.numeric(PCT_GRADE)<1)
SlopeCheck2=subset(pvtSlope,SLOPE_COLLECT=='PARTIAL'|SLOPE_COLLECT=='NO SLOPE')
Pass=tblRetrieve(Parameters=c('ENDTRAN'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)                 
SlopeCheck=subset(Pass,TRANSECT>2)# if more than 2 passes need to manually check which ones to average
#avgslope does not get computed in the app if the passes are not within 10%.... for those passes manually average and flag as not within 10%?                
# sites with pct_grade==0 are not within 10%
NOT10PER=subset(pvtSlope,PCT_GRADE=='0')
# for any sites that failed the within 10% check, see idividual passes below
IndividualSlope=tblRetrieve(Parameters=c('SLOPE','STARTHEIGHT','ENDHEIGHT'),Projects=projects, Years=years,Protocols=protocols,SiteCodes=sitecodes)
pvtIndividualSlope=addKEYS(cast(IndividualSlope,'UID+TRANSECT~PARAMETER',value='RESULT', fun=sum),c('SITE_ID','CREW_LEADER'))#note Transect=Pass
#pvtIndividualSlope=addKEYS(cast(IndividualSlope,'UID+TRANSECT+POINT~PARAMETER',value='RESULT'),c('SITE_ID','CREW_LEADER'))#note Transect=Pass
#still need to check a site with 3 passes to make sure Reid averaged slope properly

                 
#########  pools   ################                                           
#getting all pool data for a specfic set of sites---not collecting one of the parameters below anymore
pools<-addKEYS(tblRetrieve(Parameters=c('HABTYPE','FORMATION','PTAILDEP','MAXDEPTH','LENGTH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes, Comments='Y'),c('SITE_ID'))
pvtpools=cast(pools,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
write.csv(pvtpools,'pvtpools.csv')#short and look for min and max and 0 data or unit issues

# flow and collected checks
PoolCollect<-tblRetrieve(Parameters=c('POOL_COLLECT','VALXSITE','POOLRCHLEN','TRCHLEN','SLOPE_COLLECT','SLPRCHLEN'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes, Comments='Y')
pvtPoolCollect=addKEYS(cast(PoolCollect,'UID~PARAMETER',value='RESULT'),c('SITE_ID','CREW_LEADER'))
pvtPoolCheck=subset(pvtPoolCollect,VALXSITE=='INTWADE'|POOL_COLLECT=='NF'|POOL_COLLECT=='NC')#needs tweaking
write.csv(pvtPoolCheck,'poolNCNFCheck.csv')

#check sum of lengths not > than total reach length  
pool_length<-tblRetrieve(Parameters=c('LENGTH'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes, Comments='Y')
pool_length$RESULT=as.numeric(pool_length$RESULT)
pvtpools1=cast(pool_length,'UID~PARAMETER',value='RESULT',fun=sum) 
reach_length=tblRetrieve(Parameters=c('POOLRCHLEN'),Projects=projects,SiteCodes=sitecodes)
pvtpools2=cast(reach_length,'UID~PARAMETER',value='RESULT') 
poolsmerge<-merge(pvtpools1,pvtpools2,by=c('UID'),all=T)
pool_great_100<-subset(poolsmerge,LENGTH>POOLRCHLEN)


#########  photos  ###################
#use to check any questionable values such as bankfull widths and heights                      
Photo=tblRetrieve(Parameters=c('PHOTO_ID','PHOTO_DESCRIP','PHOTO_TYPE','ROD','DIRECTION','COMMENT'),Projects=projects,Years=years,SiteCodes=sitecodes)                   
pvtPhotos=cast(Photo,'UID+POINT~PARAMETER',value='RESULT')


############################################################################################################################
#Final pivot checks on all indicators to make sure no duplicate data exists from data edits---copy over from indicator calc
#run summary data and sort again to make sure no wacky values
#pay close attention to sample sizes when running indicators....should probably eventually run missing data checks this way rather than the knarly script above                 





##########################################################################################################################
################## Old checks ############################################################################################
##########################################################################################################################

##legal value checks
#low-high pairs
LowHigh=c("MIN,MAX","QLOWI,QHIGHI","QLOWR,QHIGHR");#original FM string: "QLOWI,QHIGHI¶QLOWR,QHIGHR¶MIN,MAX"
UnionTBLstat=UnionTBL
#random quirks to ignore in legal checks
UnionTBLstat=subset(UnionTBLstat,(PARAMETER=='SIZE_CLS' & is.na(as.numeric(RESULT)))==FALSE)#remove text Size_CLS  from 2013 (eventually will not be needed)
UnionTBLstat=subset(UnionTBLstat,(PARAMETER=='SIZE_CLS' & RESULT=='0')==FALSE)#remove wood/other SIZE_CLS particles so don't fail the check
#loop over pairs
for (p in 1:length(LowHigh)){
  StatPair=strsplit(LowHigh[p],",")
  StatLow=StatPair[[1]][1]
  StatHigh=StatPair[[1]][2]
  StatValues="Select Sample_Type, Parameter, Stat, Result from tblMetadataRange where ACTIVE='TRUE' and STAT='%s' and Protocol='WRSA14'"#! protocol determination should be dynamic!! currently there are only values for WRSA14
  Low=sqlQuery(wrsa1314,sprintf(StatValues,StatLow))
  High=sqlQuery(wrsa1314,sprintf(StatValues,StatHigh))
  LowHighJoin=sqldf("select * from UnionTBLstat 
                    join (select Sample_Type as ST, Parameter as PM, Stat as LowStat,Result as LowResult from Low) L on UnionTBLstat.Sample_Type=L.ST and UnionTBLstat.Parameter=L.PM
                    join (select Sample_Type as ST, Parameter as PM, Stat as HighStat,Result as HighResult from High) H on UnionTBLstat.Sample_Type=H.ST and UnionTBLstat.Parameter=H.PM
                    ")
  LowHighFail=sqldf("select UID,Transect, Point, IND, Sample_Type,Parameter, 
                    Result,LowResult,HighResult,LowStat,HighStat
                    from LowHighJoin
                    where Result<LowResult or Result>HighResult")
  if(p==1){LowHighFailOUT=LowHighFail} else{LowHighFailOUT=rbind(LowHighFailOUT,LowHighFail)}
}
LowHighFailOUT=addKEYS(LowHighFailOUT,c('SITE_ID','DATE_COL'))#JC added to aid in QC process. However, it has duplicate siteid and date columns at the momment, so not very pretty at the moment
write.csv(LowHighFailOUT,'LegalChecks.csv')

####################################################################################################################
#slope checks
#!compare to GIS
###---Connected Slope passes with no gaps---###
SlopeTran1=tblRetrieve(Parameters=c('ENDTRAN'),Years=c('2014','2015','2016'))#WRSA protocol
SlopeTran2=tblRetrieve(Parameters=c('SLOPE'),Years=c('2013'))#NRSA protocol
SlopeTran2$PARAMETER='ENDTRAN'#could choose to add this in database
SlopeTran2$PointTMP=SlopeTran2$POINT
SlopeTran2$POINT=SlopeTran2$TRANSECT
SlopeTran2$TRANSECT=SlopeTran2$PointTMP;SlopeTran2=SlopeTran2[,!(names(SlopeTran2) %in% c('PointTMP'))]
tran=c('A','B','C','D','E','F','G','H','I','J','K')
for (t in 1:nrow(SlopeTran2)){
  SlopeTran2$RESULT[t]=tran[1+grep(SlopeTran2$POINT[t],tran)]#could choose to add this in database
}
SlopeTran=rbind(SlopeTran1,SlopeTran2)
SlopeTran$Start=SlopeTran$POINT;SlopeTran$Stop=SlopeTran$RESULT;SlopeTran=SlopeTran[,!(names(SlopeTran) %in% c('POINT','RESULT'))]
SlopeTran=SlopeTran[,!(names(SlopeTran) %in% c('SAMPLE_TYPE','PARAMETER','IND','ACTIVE','FLAG','OPERATION','INSERTION','DEPRECATION','REASON'))]#!not necessary, just easier to see when clean
NotTran=subset(SlopeTran, nchar(Start)>1|nchar(Stop)>1)
if(nrow(NotTran)>0){print('WARNING: some transects are not single letter transect names. Review and correct'); View(NotTran)}
St=c('Start','Stop')
SlopeUID=unique(subset(SlopeTran,select=c(UID,TRANSECT)));SlopeUIDmulti=SlopeUID[0,]
for (u in 1:nrow(SlopeUID)){
  SlopeU=subset(SlopeTran,UID==SlopeUID$UID[u] & TRANSECT==SlopeUID$TRANSECT[u]);Urow=nrow(SlopeU);SlopeU=unique(SlopeU);if(Urow!=nrow(SlopeU)){SlopeUIDmulti=rbind(SlopeUIDmulti,unique(subset(SlopeU,select=c(UID,TRANSECT))))}
  SlopeConnect=SlopeU[1,];SlopeConnect$StartL0=SlopeConnect$Start;SlopeConnect$StartR0=SlopeConnect$Start;SlopeConnect$StopL0=SlopeConnect$Stop;SlopeConnect$StopR0=SlopeConnect$Stop
  FinalR=SlopeConnect$Stop;FinalL=SlopeConnect$Start
  if(nrow(SlopeU)==1){SlopeMatch=SlopeU[0,]
  } else{
    SlopeMatch=SlopeU[2:nrow(SlopeU),];
  }
  if(u==1){SlopeConnectFail=SlopeConnect[1,];SlopeConnectFail$UID=NA;SlopeConnectPass=SlopeConnect[1,];SlopeConnectPass$UID=NA;SlopeMatchFail=SlopeMatch[1,];SlopeMatchFail$UID=NA}
  SlopeMatch=SlopeMatch[,!(names(SlopeMatch) %in% c('UID','TRANSECT'))];
  for (m in 1:nrow(SlopeMatch)){
    if(nrow(SlopeMatch)>0){
      SlopeMatchL=SlopeMatch; names(SlopeMatchL)[names(SlopeMatchL) %in% St] <- sprintf('%sL%s',St,m)
      SlopeMatchR=SlopeMatch; names(SlopeMatchR)[names(SlopeMatchR) %in% St] <- sprintf('%sR%s',St,m)
      SlopeConnect=sqldf(sprintf('select * from SlopeConnect left join SlopeMatchL on ltrim(rtrim(upper(SlopeConnect.StartL%s)))=ltrim(rtrim(upper(SlopeMatchL.StopL%s))) left join SlopeMatchR on ltrim(rtrim(upper(SlopeConnect.StopR%s)))=ltrim(rtrim(upper(SlopeMatchR.StartR%s)))',m-1,m,m-1,m))
      StartR=unlist(subset(SlopeConnect,select=sprintf('StartR%s',m)));StartL=unlist(subset(SlopeConnect,select=sprintf('StartL%s',m)));StopR=unlist(subset(SlopeConnect,select=sprintf('StopR%s',m)))
      FinalR=ifelse(is.na(StopR),FinalR,StopR);FinalL=ifelse(is.na(StartL),FinalL,StartL)
      SlopeMatch=subset(SlopeMatch,(Start %in% c(StartL,StartR))==FALSE)
    }}
  SlopeConnect$FinalR=FinalR;SlopeConnect$FinalL=FinalL
  namesSlope=union(names(SlopeConnectFail),names(SlopeConnect))
  if (nrow(SlopeMatch)>0){
    SlopeConnectFail=rbind(ColCheck(SlopeConnectFail,namesSlope),ColCheck(SlopeConnect,namesSlope));
    SlopeMatchTMP=SlopeMatch;SlopeMatchTMP$UID=SlopeUID$UID[u];SlopeMatchTMP$TRANSECT=SlopeUID$TRANSECT[u]
    SlopeMatchFail=rbind(SlopeMatchFail,SlopeMatchTMP)
  } else {SlopeConnectPass=rbind(ColCheck(SlopeConnectPass,namesSlope),ColCheck(SlopeConnect,namesSlope))}
}
SlopeConnectFail=subset(SlopeConnectFail,is.na(UID)==F);SlopeConnectPass=subset(SlopeConnectPass,is.na(UID)==F);SlopeMatchFail=subset(SlopeMatchFail,is.na(UID)==F)
#!Warnings
SlopeConnectPassEndFail=subset(SlopeConnectPass,((toupper(gsub(" ","",FinalL))=='A'& toupper(gsub(" ","",FinalR))=='K')|(toupper(gsub(" ","",FinalL))=='K'& toupper(gsub(" ","",FinalR))=='A'))==FALSE)
SlopeConnectPassEndPass=sqldf('select s1.* from SlopeConnectPass as s1 left join SlopeConnectPassEndFail as s2 on s1.UID=s2.UID and s1.TRANSECT=s2.TRANSECT where s2.UID is null')
SlopeConnectPassCNT=sqldf('select UID, Count(*) as CNT from SlopeConnectPassEndPass group by UID');SlopeConnectPass2Pass=subset(SlopeConnectPassCNT,CNT>1);SlopeConnectPass2Fail=subset(SlopeConnectPassCNT,CNT<2)
print(sprintf('CONGRATULATIONS! %s sites with 2 successful Slope Passes!',nrow(SlopeConnectPass2Pass)))
if(nrow(SlopeConnectPassEndFail)>0){print('WARNING: Some slope passes do not start at A or end at K. Summary data at in SlopeConnectPassEndFail. Examine raw data in detail.');View(subset(SlopeConnectPassEndFail,select=c('UID','TRANSECT','FinalL','FinalR')))}
if(nrow(SlopeConnectPass2Fail)>0){print('WARNING: Some sites have only one connected Slope Pass. This may be because the data has already been cleaned to remove the 2nd pass if within 10%.');print(SlopeConnectPass2Fail)}
if(nrow(SlopeConnectFail)>0){print('WARNING: Some slope passes had gaps and could not be connected for the entire reach. Summary data in SlopeConnectFail and SlopeMatchFail. Examine raw data in detail.');View(subset(SlopeConnectFail,select=c('UID','TRANSECT','FinalL','FinalR'))); View(SlopeMatchFail)}
if(nrow(SlopeUIDmulti)>0){print('WARNING: Duplicate transects within same slope pass. Examine raw data in detail.');print(SlopeUIDmulti)}


SlopeSlope=tblRetrieve(Parameters=c('SLOPE','PROP','METHOD','SLOPE_UNITS','ENDTRAN'),Years=c('2014','2015','2016'))
SlopeSlope=subset(SlopeSlope,substr(SAMPLE_TYPE,1,5) =='SLOPE')
####---SLOPE METHOD CHECK---##
SlopeDiffMethod=subset(SlopeSlope,PARAMETER %in% c('PROP','METHOD','SLOPE_UNITS') & (RESULT %in% c('100','TR','CM'))==F)
if(nrow(SlopeDiffMethod)>0){print('WARNING: Slopes with non-standard methods. Expected methods are PROP=100, Method=TR (transit) and Units=CM (centimeters)');View(SlopeDiffMethod)}
####---SLOPE 10% CHECK---##
Slope2=subset(SlopeSlope,UID %in% SlopeConnectPass2Pass$UID & PARAMETER=='SLOPE');Slope2$RESULT=as.numeric(Slope2$RESULT)
Slope2sum=sqldf('select UID,TRANSECT, SUM(RESULT) as SlopeSum from Slope2 group by UID, TRANSECT')
Slope2sum=cast(Slope2sum,'UID~TRANSECT',value='SlopeSum')
SlopePass1=abs(Slope2sum[2]);SlopePass2=abs(Slope2sum[3]);
Slope2sum$TenPCT=ifelse((SlopePass2>(SlopePass1+(SlopePass1*0.1))) | (SlopePass2<(SlopePass1-(SlopePass1*0.1))),'FAIL10%','PASS10%')#! app version is more iterative to check all passes -> could either apply here OR have app flag the slope that passes
Slope2pass=subset(Slope2sum,TenPCT=='PASS10%' & is.na(Slope2sum[4]));Slope2fail=subset(Slope2sum,TenPCT=='FAIL10%' | is.na(Slope2sum[4])==F)
if(nrow(Slope2fail)>0){print('WARNING: Pass 1 and 2 were not within 10%% OR if within 10% additional passes were present. Reconcile manually.');View(Slope2fail);View(subset(Slope2,UID %in% Slope2fail$UID ))}#print raw and summmed data for failed
SlopeManuallyApproveTran1=c('21013264668376829952','5280846501466459068066440','933116886431965036742642','313491917404832989706088','57724867494140169944648','548616094584309022720','6057255640464259809280','21766046479553159758484','30503742404793951322602')
if(nrow(Slope2pass)>0){print(sprintf('CONGRATULATIONS! Pass 1 met 10%% match requirements for %s sites. Inactivate Pass 2 in WRSAdb (csv exported).',nrow(Slope2pass)));write.csv(subset(SlopeSlope,TRANSECT>1 & UID %in% c(Slope2pass$UID,SlopeManuallyApproveTran1)),'SlopesToInactivate.csv')}#export 2nd passes to inactivate (eventually connect to UpdateDB.R once screened)
#change externally: 
#keep Tran2 active, Tran 3 inactive: #subset(SlopeSlope,UID %in% c('81353742941924589578','719428245490921504768','716024640500735016960') & TRANSECT!=2)
#inactivate tran 1: 716024640500735016960 # reason: ignore this pass #use above subset
#keep Tran3, omit all previous #subset(SlopeSlope,UID %in% c('863868431598454964244') & TRANSECT!=3)
#keep Tran1, Point k (not a)  #subset(SlopeSlope,UID %in% c('37519940409184604912244', '47271381432878896252680') & (TRANSECT!=1|POINT=='a'))
#keep Tran1, Point a (not k)  #subset(SlopeSlope,UID %in% c('8692216624269710244040620') & (TRANSECT!=1|POINT=='k'))
#makes no sense and no comments to clarify 87601876754740660818804248 #View(cast(subset(SlopeSlope,UID=='87601876754740660818804248'),'UID+TRANSECT+POINT~PARAMETER',value='RESULT'))

##############################################################################################
#only works right after running old csv import script and even then I don't think it was correct
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
                      
###########################################################################################################                      
#####Additional QC Checks
#bank cross-validation WRSA checks
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects=c('NORCAL','2015ProtocolOverlap','AKEFO'),Years=c('2013','2014','2015'))
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'),Projects=c('NORCAL','2015ProtocolOverlap','AKEFO'),Years=c('2013','2014','2015'))
#widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects=c('WRSA','NV','GSENM','COPLT'),Years=c('2015'))
#widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects=c('WRSA','NV','GSENM','COPLT'),Years=c('2015'))
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
wnPVTIND=cast(widhgt,'UID+TRANSECT~PARAMETER',value='IND')     
tranPVT=addKEYS(merge(whPVT,bnkPVT,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
rawwhPVT=addKEYS(merge(whPVT,wnPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
colnames(rawwhPVT)<-c('UID','TRANSECT','BANKHT','BANKWID','BARWID','INCISED','WETWID','BANKHT_IND','BANKWID_IND','BARWID_ID','INCISED_IND','WETWID_ID','DATE_COL','SITE_ID')

bankhtcheck=subset(rawwhPVT,BANKHT>INCISED|BANKHT>1.5)#!possible crossvalidation rule to scan for#no bank heights showed up in the legal value or outlier check so wanted to check units
wetwidthchecks=subset(rawwhPVT,WETWID>BANKWID)
write.csv(bankhtcheck,'bankhtincisedcheck_31Aug2015.csv')
write.csv(wetwidthchecks,'wetwidthchecks_31Aug2015.csv')
write.csv(rbind(widhgt2,banks),'WidthHeightRaw_31Aug2015.csv')#need raw output to get IND values

#getting raw bank data for a few problem sites
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects='AKEFO',Years=c('2015'),SiteCodes=c('AF-LS3-9172','AF-SS1-9146','AA-STR-0013'))
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects='AKEFO',Years=c('2015'),SiteCodes=c('AF-LS3-9172','AF-SS1-9146','AA-STR-0013'))
#widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects='WRSA',Years=c('2013','2014'),SiteCodes=c('MN-SS-1133','MP-SS-2091','MS-SS-3126','XE-RO-5081','XE-SS-5105','XS-SS-6135', 'OT-LS-7001',  'OT-LS-7012',  'MP-SS-2080',	'XE-SS-5150',	'MS-LS-3026',	'OT-LS-7019',	'OT-SS-7133'))
#widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects='WRSA',Years=c('2013','2014'),SiteCodes=c('MN-SS-1133','MP-SS-2091','MS-SS-3126','XE-RO-5081','XE-SS-5105','XS-SS-6135', 'OT-LS-7001',  'OT-LS-7012',	'MP-SS-2080',	'XE-SS-5150',	'MS-LS-3026',	'OT-LS-7019',	'OT-SS-7133'))
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
wnPVTIND=cast(widhgt,'UID+TRANSECT~PARAMETER',value='IND')     
rawwhPVT=addKEYS(merge(whPVT,wnPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
colnames(rawwhPVT)<-c('UID','TRANSECT','BANKHT','BANKWID','BARWID','INCISED','WETWID','BANKHT_IND','BANKWID_IND','BARWID_ID','INCISED_IND','WETWID_ID','DATE_COL','SITE_ID')
write.csv(rawwhPVT,'problem_sites_cross_valid_bank.csv')

#third bank parameter check on select UIDs based off of summary
widhgt=addKEYS(tblRetrieve(Parameters=c('BANKHT'), Projects='WRSA',Years=c('2013','2014'),UIDS=c(10376,10381,13517,11833,12717,11847,12648,11836)),c('SITE_ID','DATE_COL'))
widhgt.sub=addKEYS(TBLout,c('SITE_ID','DATE_COL'))

tblRetrieve(Parameters='ANGLE180', SiteCodes='XN-RO-4085')
#check 0 substrate flagged in legal values
substratecheck=addKEYS(tblRetrieve(Parameters=c('SIZE_NUM'),Projects='WRSA',Years=c('2013','2014'),Protocols=c('NRSA13','WRSA14'), Comments='Y'), c('SITE_ID','DATE_COL'))
zerosubstrate=subset(substratecheck, RESULT==0)
write.csv(zerosubstrate,'zerosubstrate.csv')

#second round cross validation checks
#checked bht and bankwidth 1st round checks again and did not find any values that still needed to be changed
incisedhtcheck=subset(rawwhPVT,INCISED>1.5)#many came up but no unit errors obvious
barwidthchecks=subset(rawwhPVT,BARWID>WETWID)#none found

banks=tblRetrieve(Parameters=c('ANGLE','UNDERCUT'), Projects='WRSA',Years=c('2013','2014'))
banksnum=subset(banks,is.na(as.numeric(RESULT))==F);banksnum$RESULT=as.numeric(banksnum$RESULT)
bnkPVT=cast(banks,'UID+TRANSECT~PARAMETER+POINT',value='RESULT')   
bnkPVTIND=cast(banks,'UID+TRANSECT~PARAMETER+POINT',value='IND')  
rawwhPVT=addKEYS(merge(bnkPVT,bnkPVTIND,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
undercut_checks=subset(rawwhPVT,UNDERCUT_LF.x>1|UNDERCUT_RT.x>1)
write.csv(undercut_checks,'undercut_checks.csv')#many units issues                     
                      

#check on duplicate wetted widths from field forms
##crossvalidation/business rules
##! store and dynamically compose validation rules 
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
                                        
####################################################################################################
#Jennifer's attempt to get indicator outliers and boxplots
#QA boxplots
give.n <- function(x){return(data.frame(y = max(x)+1, label = paste("n =",length(x))))}#SWJ to do: improve to handle the multiple classes for Categorical 
whisk95 <- function(x) {r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
                        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
                        r
}#supports custom boxplot whiskers (instead of 1.5IQR default)#http://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot
out2SD <- function(x) {
  #subset(x, x < quantile(x)[2] | quantile(x)[4] < x)
  MN=mean(x)
  SD2=2*sd(x)
  subset(x, x < MN-SD2 | MN+SD2 < x)
}#supports custom outliers#http://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot
boxPARAM=function(boxdata,outlierdata,sampsize,facetSTR,titleSTR){
  siteavg=subset(boxdata,subset=STRATATYPE !="UID"  , select=c('UID','STRATATYPE','PARAMRES', 'PARAMCAT'))
  if(grepl('STRATATYPE',facetSTR)){
    siteavg=subset(siteavg,UID==allsites[s])
  } else { siteavg=subset(siteavg,STRATATYPE==typestrata[n])}
  siteavg=aggregate(PARAMRES~PARAMCAT,data=siteavg,FUN='mean')
  boxplot=ggplot(boxdata,aes(y=PARAMRES, x=PARAMCAT,fill=PARAMCAT,colour=PARAMCAT,label=SiteLabelOUT2)) +
    stat_summary(fun.data=whisk95, geom='boxplot',colour='black') + #geom_boxplot(outlier.colour='darkred',outlier.size=10,colour='black') + 
    stat_summary(fun.y=out2SD, geom='point',colour='darkred',size=5,show_guide=F) +
    eval(parse(text=facetSTR)) + #
    geom_hline(aes(yintercept=PARAMRES, colour=PARAMCAT),siteavg,size=1)  + #mark the average for the site
    scale_colour_brewer(drop=FALSE,palette='Set1') + scale_fill_brewer(palette='Set1')+#sync colors between lines and boxplots (especially important for categorical)
    #stat_summary(fun.data =give.n, geom = "text") +
    eval(parse(text=titleSTR)) +
    #geom_text(data=paramQ,aes(label=SiteLabelOUT),show_guide=F,size=3,position= position_jitter(width = 0.5, height=0))+#jitter a little strange, but makes it readable
    #stat_summary(fun.y=out2SD, geom='text',colour='red',show_guide=F) +
    geom_text(show_guide=F,size=3,position= position_jitter(width = 0.15, height=0))+#data=outlierdata,aes(label=SiteLabelOUT2),
    geom_text(aes(label=sprintf('n=%s',SampSize),x=(length(unique(PARAMCAT))/2)+0.5,y=max(PARAMRES)+(max(PARAMRES)/10)),colour='black')+#,x=(length(unique(boxdata$PARAMCAT))/2)+0.5,y=max(boxdata$PARAMRES)+0.25),inherit.aes=FALSE, parse=FALSE)+#annotate("text",x=2,y=max(paramTBL6$PARAMRES)+0.5,label=sprintf('n=%s',paramN$PARAMRES)) +#annotate: n(sites) for strata plots and n(points) for site  (function defined above) #messy for categorical#previous (not working as anticipated, particularly for categorical): #stat_summary(fun.data =give.n, geom = "text") + #function for adding sample size to boxplots #
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank())  #remove x axis 
}# to support similar boxplot structure for stratabox and sitebox
subcol=c('UID','SITE_ID','PARAMETER','STRATATYPE','STRATA','PARAMRES','PARAMCAT','TRANSECT','POINT')
#compile parameter list
dbPARAM=list(colnames(indicators))
params_C=subset(dbPARAM, subset=VAR_TYPE=='CHARACTER')
allparams=unique(paste(UnionTBL$SAMPLE_TYPE,UnionTBL$PARAMETER,sep=" "))#numeric: allparams=c("BANKW INCISED", "BANKW WETWID" )#categorical: allparams=c("CROSSSECW SIZE_CLS","HUMINFLUW WALL")
#!add Size_Num to combineparams list, use same translation for converting prior to aquamet, revise legal checks for Size_NUM
allparams1=setdiff(allparams,c(excludeparams,combineparams))
UnionTBL2=UnionTBLall#UnionTBL2=subset(UnionTBL,SITE_ID=='EL-LS-8126')
#set strata for later iteration
typestrata=c('ALL','EcoReg','Size')#must match column names that are created
numstrata=length(typestrata)
UnionTBL2=addKEYS(UnionTBL,'SITE_ID')
UnionTBL2$EcoReg=substr(UnionTBL2$SITE_ID,1,2)#!-- Switch to climatic rather than ecoreg?  ; ; may need to explicitly join an ecoregion column if sitecodes change over different projects and/or to utilize the EPA reference dataset, this works for NRSA only; also needs to be more expandable for additional strata
UnionTBL2$Size=substr(UnionTBL2$SITE_ID,4,5)
UnionTBL2$ALL='ALL'
#! other possible strata: reach width (would need to be calculated), VALXSITE or protocol (especially boatable vs. wadeable)
#this section is highly dependent on WRSA siteID naming structure and GRTS strata
rm(outlierTBL)
for (p in 1:length(allparams1)){#this is a standard loop for iterating, could put it in a function that allows you to plug in a string for the most nested middle
  for (p in 2:6){#target specifc parameters during testing or start over mid-process  
    typeparam=strsplit(allparams1[p]," ")
    type=typeparam[[1]][[1]]; param=typeparam[[1]][[2]]
    paramTBL=subset(UnionTBL2,subset=PARAMETER==param & SAMPLE_TYPE==type)#!some where in subsetting and labelling of graphs, parameter was assumed to be unique...it technically is not (i.e. crosssection vs. thalweg depths, boatable) and needs Sample_TYPE in tandem
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
      #set up dataset and outliers for later subsetting by UID and strata in subsequent loops
      paramTBL3$TRANSECT='ALL';paramTBL3$POINT='ALL'
      paramTBL3$POINT=ifelse(paramSTATUS=='CHAR' & paramTBL3$STRATATYPE=='SITE_ID',paramTBL3$IND.y,paramTBL3$POINT)#set up for later N (sample size) use
      paramTBL5=subset(paramTBL3,select=subcol)
      if(paramSTATUS=='NUM'){
        paramTBL$STRATATYPE='UID';paramTBL$STRATA=paramTBL$SITE;paramTBL$PARAMCAT='None'; paramTBL$STRATA=factor(paramTBL$STRATA,levels=unique(paramTBL$STRATA))
        paramTBL6=subset(paramTBL,select=subcol)
        paramTBL6=rbind(paramTBL6,paramTBL5)
        paramN2=aggregate(PARAMRES~STRATATYPE+UID,data=subset(paramTBL6,STRATATYPE=='UID'),FUN='length');colnames(paramN2)=c(colnames(paramN2)[1:2],'SampSizeTMP')
        paramTBL6=merge(paramTBL6,paramN2,by=c('STRATATYPE','UID'),all.x=T)
      } else if(paramSTATUS=='CHAR'){paramTBL6=paramTBL5;paramTBL6$STRATATYPE=ifelse(paramTBL6$STRATATYPE=='SITE_ID','UID',paramTBL6$STRATATYPE);paramTBL6$SampSizeTMP=NA}
      paramTBL6$STRATATYPE=factor(paramTBL6$STRATATYPE,levels=c("UID",typestrata))
      paramTBL6$PARAMCAT=factor(paramTBL6$PARAMCAT)
      #label sample size
      paramN1=aggregate(PARAMRES~STRATATYPE+STRATA+UID,data=subset(paramTBL6,STRATATYPE!='UID'),FUN='length')#to remove PARAMCAT duplicates for CHAR
      paramN1=aggregate(PARAMRES~STRATATYPE+STRATA,paramN1,FUN='length');colnames(paramN1)=c(colnames(paramN1)[1:2],'SampSize')#paramN$STRATATYPE=factor(paramN$STRATATYPE,levels=levels(paramTBL6$STRATATYPE))#might be needed to keep in the same order, unsure
      paramTBL6=merge(paramTBL6,paramN1,by=c('STRATATYPE','STRATA'),all.x=T)
      paramTBL6$SampSizeTMP=ifelse(is.na(paramTBL6$SampSizeTMP),paramTBL6$POINT,paramTBL6$SampSizeTMP);paramTBL6$SampSize=ifelse(is.na(paramTBL6$SampSize),paramTBL6$SampSizeTMP,paramTBL6$SampSize)
      if(paramSTATUS=='CHAR'){paramN$PARAMRES=ifelse(paramN$STRATATYPE=='UID',min(paramTBL6$POINT),round(paramN$PARAMRES/length(unique(paramTBL6$PARAMCAT)),0))}
      #label quantiles with SiteCode
      paramquant=aggregate(PARAMRES~STRATATYPE+PARAMCAT+STRATA,data=paramTBL6,FUN='quantile',probs=c(0.05,0.95),names=FALSE);colnames(paramquant)=c('STRATATYPE','PARAMCAT','STRATA','Quant')
      paramTBL6=merge(paramTBL6,paramquant,by=c('STRATATYPE','PARAMCAT','STRATA'),all.x=T)
      paramTBL6$SiteLabelOUT=ifelse(paramTBL6$PARAMRES<paramTBL6$Quant[,1],paramTBL6$SITE_ID,ifelse(paramTBL6$PARAMRES>paramTBL6$Quant[,2],paramTBL6$SITE_ID,NA))#create a site label if an outlier
      paramTBL6$SiteLabelOUT=ifelse(paramTBL6$STRATATYPE=="UID"& paramSTATUS=='CHAR',NA, ifelse(paramTBL6$STRATATYPE=="UID" & is.na(paramTBL6$SiteLabelOUT)==FALSE,paste(paramTBL6$TRANSECT,paramTBL6$POINT,sep=":"),paramTBL6$SiteLabelOUT))#change site label to transect if raw data
      paramQ=subset(paramTBL6,is.na(SiteLabelOUT)==FALSE)
      #label outliers with SiteCode
      paramoutlrM=aggregate(PARAMRES~STRATATYPE+PARAMCAT+STRATA,data=paramTBL6,FUN='mean');colnames(paramoutlrM)=c('STRATATYPE','PARAMCAT','STRATA','Mean')
      paramoutlrS=aggregate(PARAMRES~STRATATYPE+PARAMCAT+STRATA,data=paramTBL6,FUN='sd');colnames(paramoutlrS)=c('STRATATYPE','PARAMCAT','STRATA','SD')
      paramTBL6=merge(paramTBL6,paramoutlrM,by=c('STRATATYPE','PARAMCAT','STRATA'));paramTBL6=merge(paramTBL6,paramoutlrS,by=c('STRATATYPE','PARAMCAT','STRATA'))
      paramTBL6$SiteLabelOUT2=ifelse(paramTBL6$PARAMRES>(paramTBL6$Mean + (2*paramTBL6$SD)),paramTBL6$SITE_ID,ifelse(paramTBL6$PARAMRES<(paramTBL6$Mean - (2*paramTBL6$SD)),paramTBL6$SITE_ID,NA))#create a site label if an outlier
      paramTBL6$SiteLabelOUT2=ifelse(paramTBL6$STRATATYPE=="UID"& paramSTATUS=='CHAR',NA, ifelse(paramTBL6$STRATATYPE=="UID" & is.na(paramTBL6$SiteLabelOUT2)==FALSE,paste(paramTBL6$TRANSECT,paramTBL6$POINT,sep=":"),paramTBL6$SiteLabelOUT2))#change site label to transect if raw data
      paramTBL6$SiteLabelOUT2=ifelse(is.na(paramTBL6$SiteLabelOUT2),'',paramTBL6$SiteLabelOUT2)
      paramMSD=subset(paramTBL6,is.na(SiteLabelOUT2)==FALSE & SiteLabelOUT2!='');paramMSDpres=nrow(paramMSD); if(paramMSDpres==0){paramMSD=paramTBL6[1,];paramMSD$SiteLabelOUT2=''}#geom_text will fail if no rows are present
      if(paramMSDpres>0){
        if(exists('outlierTBL')) {
          outlierTBL=rbind(outlierTBL,paramMSD)
        } else {outlierTBL=paramMSD}
      }
      allsites=intersect(unique(paramTBL$UID),unique(UnionTBL$UID))#only iterate over sites in the incoming/subset dataset (UnionTBL)
      for (s in 1:length(allsites)){
        #for (s in 1:3){#to test a smaller subset
        stratas=unique(subset(paramTBL6,select=STRATA,subset=UID==allsites[s]))
        paramTBL7=subset(paramTBL6,subset=STRATA %in% stratas$STRATA)
        #generate box plot in ggplot2
        if(max(paramTBL7$UID==allsites[s] & paramTBL7$SiteLabelOUT2!='')==1){#only print plot if it has outliers within the site or when compared to the strata
          sitebox=boxPARAM(boxdata=paramTBL7,sampsize=paramN,facetSTR='facet_grid(.~STRATATYPE)',titleSTR='labs(title=sprintf("SITE- %s (%s) ~ PARAM- %s",unique(subset(boxdata, subset=STRATATYPE=="UID", select=SITE_ID)),allsites[s],param))')
          ggsave(filename=sprintf('%s.jpg',sitebox$labels$title),plot=sitebox)#assign(sitebox$labels$title,sitebox)#save jpeg or assign var (need to refine naming)
        }
        
      }
      
      for (n in 1:numstrata) {#re-enter for loop now that all STRATA are complete and aggregated
        if(nrow(subset(paramMSD,STRATATYPE==typestrata[n]))>0){
          paramTBL4=subset(paramTBL6,subset=STRATATYPE==typestrata[n])
          stratabox=boxPARAM(boxdata=paramTBL4,sampsize=paramN,facetSTR='facet_grid(.~STRATA)',titleSTR='labs (title=sprintf("STRATA- %s ~ PARAM- %s",typestrata[n],param))')
          ggsave(filename=sprintf('%s.jpg',stratabox$labels$title),plot=stratabox)#assign(stratabox$labels$title,stratabox)#save jpeg or # assign(sprintf('box_STRATA_%s_%s',typestrata[n],param),stratabox)
        }
      }
    } }
  rm(paramTBL3,paramTBL4,paramTBL6,paramTBL5,paramTBL3a,paramTBL3b)
  
  outlierTBL=unique(subset(outlierTBL,select=c('STRATATYPE','STRATA','SITE_ID','UID','PARAMETER','PARAMCAT','TRANSECT','POINT','PARAMRES','Mean','SD')))
  stratat=unique(outlierTBL$STRATATYPE)
  for (s in 1:length(stratat)){
    outlierTBLst=subset(outlierTBL,subset=STRATATYPE==stratat[s])
    write.csv(outlierTBLst,file=sprintf('Outliers_2SDmean_%s.csv',stratat[s]))#could export as a single table, but a bit overwhelming
  }}

indicators=read.csv('IndicatorCheck_4Dec2015.csv')
str(indicators)
a=aggregate()

for (n in 1:length(indicators))######{mean(indicators$n)}
{paste("sub",n)=subset(indicators,ECOREGION==n)}

LIST=list()
for (n in 2:ncol(indicators))
{
 LIST[(n)]=mean(indicators[,n])
}
ecoregions=c("XN","XS","MN","MS","XE","OT","MP")
for (n in 1:length(ecoregions))
{subset=indicators$ECOREGION[n,]}
boxplot(indicators$PH_CHECK)}
boxplot()

