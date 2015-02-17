#temporary update method
#updates R table, not underlying database

#export from Access using SavedExport Export-Office_Updates3
UpdatesTBL=read.csv('Office_Updates.csv')
#UpdatesTBL=read.csv('revisons_deletions_SRM.csv')
#IND is required!!! all "TBD" IND will be ignored, all blank IND will be added as a new row
UpdatesTBL=subset(UpdatesTBL,UPDATE=='' & IND !='TBD')


#enter the name of the table you want to update
TBL='UnionTBL'#TBL='XwalkUnion'
TBLout=eval(parse(text=TBL))
#testing, need some IND matches # intersect(unique(UpdatesTBL$IND),unique(TBLout$IND))#UpdatesTBL[485,]$IND= head(unique(TBLout$IND))[1]#UpdatesTBL[486,]$IND= head(unique(TBLout$IND))[2]#subset(TBLout,IND==2104713)
#match IND rows
UpdatesTBLind=subset(UpdatesTBL,is.na(IND)==FALSE & IND!='')
UpdatesTBLind= UpdatesTBLind[,(names(UpdatesTBLind) %in% c('IND','RESULT'))]
colIND='IND'#colIND=intersect(colnames(TBLout),colnames(UpdatesTBL));colIND=setdiff(colIND, c('REASON','INSERTION','FLAG','RESULT'))# decided to make the union looser with just IND especially given UID matching issues (permanent swaps will flag lack of matches for the colRM fields too)
TBLout=merge(TBLout,UpdatesTBLind,colIND,all.x=T)
TBLout$RESULT=ifelse(is.na(TBLout$RESULT.y  ) ,TBLout$RESULT.x,TBLout$RESULT.y)  #if nervous, stop to verify which result will be used                                                                  
TBLout=ColCheck(TBLout,c(VAR,'PARAMETER','RESULT','TRANSECT','POINT'))
#append blank IND
UpdatesTBLnew=ColCheck(subset(UpdatesTBL,is.na(IND) | IND==''),colnames(TBLout));UpdatesTBLnew$INSERTION=as.POSIXct(Sys.Date())
TBLout=rbind(TBLout,UpdatesTBLnew)
#DONE!

#JC troubleshooting###
widhgt=TBLout
UnionTBL=TBLout

#######JC temporary SRM workaround
TBLout=Incision
TBLout=merge(TBLout,UpdatesTBL,by='IND',all.x=T)
TBLout$RESULT=ifelse(is.na(TBLout$RESULT.y  ) ,TBLout$RESULT.x,TBLout$RESULT.y)  #if nervous, stop to verify which result will be used                                                                  
TBLout.sub=subset(TBLout, is.na(INACTIVATE)|INACTIVATE=='0')
Incision=TBLout.sub

###############################
assign(TBL,TBLout)

DEVELOPMENT='N'#update query still in development

if(DEVELOPMENT=='Y'){

if(sessionInfo()$R.version$major==2){
  library('RODBC')
  probsurv14=odbcConnect("ProbSurveyDB")#have to run on remote desktop (gisUser3) or machine which has 64bit R and 64bit Access
  UpdatesTBL=sqlQuery(probsurv14,'select * from Office_Updates where Update is null'
                      #!set Update field to today())
} else {setwd('C:\\Users\\Sarah\\Desktop\\NAMCdevelopmentLocal\\WRSA')#setwd('M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Field Work\\Post Sample\\iPad backup\\AccessImport');
        UpdatesTBL=read.csv('Office_Updates.csv')#export from SavedExport Export-Office_Updates3
        UpdatesTBL=subset(UpdatesTBL,UPDATE=='')
        print('Set UPDATE field in ProbSurveyDB (Access) Office_UPDATE to today to indicate that the update was performed.')
}

#test scenarios
#ID3 ; 667-670= match with IND
#change location (uid/transect/point) NOT result --> handle manually bc usually an odd situation where crew or import goofed up due to a misunderstanding
#ID0 = Comment (example of a comment manually inserted + flag update: IND 2390646 in tblReach and tblCommment insertion: IND=4804552)
#ID399-435=TBD IND (need match) # i=359; i=373, i=368 (368 is nonexact UID match)
#ID4 - 12 (and many more) = new insertions, no IND match anticipated
#ID543-604 = Inactivate only
#ID 458-479 = already updated #DONE

#Xwalk to determine table location
UpdatesTBL=Xwalk(Source='R',Table="UpdatesTBL",XwalkName='WRSA',XwalkDirection='',COL=c('INACTIVATE','INITIAL'))
UpdatesTBL=addKEYS(UpdatesTBL,Columns=c('SITE_ID','DATE_COL'))

#look for possible matches if IND missing
#!if no IND match (or TBD), provide potential matches (find matches in SQL SERVER based on UID, SAMPLE_TYPE, PARAMETER, TRANSECT, POINT)
INDnonexist=subset(UpdatesTBL,(IND=='' | IND=='TBD'|IND=='0') & TABLE!='TBLCOMMENTS')
matchTally=0
for (i in 1:nrow(INDnonexist)){
  match=sqlQuery(wrsa1314,sprintf("SELECT * %s %s, IND as existingIND, RESULT as existingRESULT  from  %s where left(cast(UID as nvarchar),10)='%s'  %s  %s  and SAMPLE_TYPE='%s' and PARAMETER='%s'  ",ifelse(INDnonexist$TRANSECT[i]=='',",'' as TRANSECT",''),ifelse(INDnonexist$POINT[i]=='',",'' as POINT",''),INDnonexist$TABLE[i],substr(INDnonexist$UID[i],1,10),ifelse(INDnonexist$TRANSECT[i]=='','',sprintf("and TRANSECT='%s'",INDnonexist$TRANSECT[i])),ifelse(INDnonexist$POINT[i]=='','',sprintf("and POINT='%s'",INDnonexist$POINT[i])),INDnonexist$SAMPLE_TYPE[i],INDnonexist$PARAMETER[i]))
  matchCNT=nrow(match)
    if(matchCNT>0){
    if(matchTally==0){
      matches=match[0,]
    }
    match$MatchCount=matchCNT
    matchTally=matchTally+1
    #export and review method
    matches=rbind(matches,match)
# possible interactive method
#                     print('Possible match based on point and parameter information: ');print(match)
#                     print(sprintf('Proposed change is to: %s',INDnonexist$RESULT[i]))
#                     print('Do you accept this match?')#!need to pause code here, in the meantime, exists loop# proceed=invisible(readline(prompt="Enter Y to accept and N to decline. Then press [enter] to continue.  "))
#                    if(match$UID!=INDnonexist$UID[i]){
#                       match=addKEYS(match,Columns=c('SITE_ID','DATE_COL'))
#                       print(sprintf('UID match based on 1st 10 characters, not full UID (Original: %s:%s:%s vs. Match: %s:%s:%s)',INDnonexist$UID[i],INDnonexist$SITE_ID[i],INDnonexist$DATE_COL[i],match$UID,match$SITE_ID,match$DATE_COL))
#                     }
#                     if(exists('accept')){
#                       if(accept=='Y'){
#                         INDnonexist$IND[i]=match$IND#set IND if match found so UPDATE can set OPERATION correctly
#                       }
#                       rm(accept)
#                     } else {print("Set accept='Y' or accept='N'")}
  } 
}
####RESUME HERE TO merge MATCHES back to INDnonexist, then need to append to INDexist if existing IND found
matches=ColCheck(matches,c('UID','TRANSECT','POINT','SAMPLE_TYPE','PARAMETER','existingIND','existingRESULT','MatchCount'))
sprintf('%s potential index matches were made. To confirm the match, update IND and omit duplicates (MatchCount) in the original file before proceeding and reimport UpdatesTBL. Indicate that this is done by changing proceedNONexist to Y. If old rows are not properly linked and invalidated, duplicate data and persistence of the error will result.',nrow(matches))#, nrow(subset(INDnonexist,IND!='' & IND!='TBD')))
INDnonexist$TRANSECT=ifelse(INDnonexist$TRANSECT=='',NA,INDnonexist$TRANSECT);INDnonexist$POINT=ifelse(INDnonexist$POINT=='',NA,INDnonexist$POINT)
INDnonexistCHECK=merge(INDnonexist,matches,all.x=T)#INDnonexist2=merge(matches,INDnonexist2,all.x=T) #to backcheck matches
View(INDnonexistCHECK)


proceedNONexist='N'#proceedNONexist='Y'

if (proceedNONexist=='Y'){
##DEPRECATE and INACTIVATE matching rows
INDexist=subset(UpdatesTBL,IND!='' & IND!='TBD')
for (i in 1:nrow(INDexist)){
  sqlQuery(wrsa1314,sprintf("UPDATE %s set DEPRECATION='%s', ACTIVE='FALSE',OPERATION='OD',REASON='%s' where IND=%s",INDexist$TABLE[i],Sys.Date(),paste(INDexist$INITIAL[i],INDexist$REASON[i],sep=': '),INDexist$IND[i]))#set DEPRECATION to today #set ACTIVE to FALSE #set OPERATION to "OD" (original deprecated)
  #! add old reason too if present!! need to decide if reason given to deprecated or updated row
  #sqlUpdate(wrsa1314,dat=,tablename=,REASON=, index=IND)
}

FLAGexist=subset(UpdatesTBL,RESULT == 'FLAG')#flag added to unchanged result
for (i in 1:nrow(FLAGexist)){
  sqlQuery(wrsa1314,sprintf("UPDATE %s set FLAG='%s' where IND=%s",INDexist$TABLE[i],INDexist$FLAG[i],INDexist$IND[i]))#set DEPRECATION to today #set ACTIVE to FALSE #set OPERATION to "OD" (original deprecated)
}

#! flag if value not different from original
##insert UPDATED rows
#Omit rows that are only for deletion
UPDATE=subset(UpdatesTBL,(subset=INACTIVATE=='0'|INACTIVATE=='FALSE') & RESULT != 'FLAG')#Inactive is 0 in csv import, but might be FALSE if copy/paste or direct from Access via ODBC
#match existing Access fields to SQL server fields
UPDATE$REASON=paste(UPDATE$INITIAL,UPDATE$REASON,sep=': ')#! add old reason too if present!! #set REASON to REASON + initials
UPDATE$ACTIVE='TRUE'#set ACTIVE to TRUE
UPDATE$INSERTION=Sys.Date()#set INSERTION=today
UPDATE$DEPRECATION='9999-12-31 00:00:00.000'#set DEPRECATION='9999-12-31 00:00:00.000' (default)
UPDATE$OPERATION=ifelse(UPDATE$IND=='','I','U')#set OPERATION to "U" if update or "I" if new insertion (based on presence of IND)
UPDATE=UPDATE[,!(names(UPDATE) %in% c('IND'))];UPDATE=ColCheck(UPDATE,c('IND',names(UPDATE)))#remove old IND and set IND to next available IndMax via ColCheck
#loop over tables to append
UPDATEtables=unique(UPDATE$TABLE)
for (t in 1:nrow(UPDATEtables)){
  TBL=subset(UPDATE,TABLE==UPDATEtables[t])#testing: TBL=TBL[4:6,]
  colnamesTBL=sqlQuery(wrsa1314,sprintf("select top 1 * from %s",UPDATEtables[t]));TBL=ColCheck(TBL,names(colnamesTBL))#column names must match exactly
  sqlSave(wrsa1314,dat=TBL,tablename=UPDATEtables[t],rownames=FALSE,append=TRUE)#SWJ 2/16/15: crashing on sqlSave in R (no SQL error returned) for unknown reasons
}


print('Set UPDATE field in ProbSurveyDB (Access) Office_UPDATE to today to indicate that the update was performed.')
}} else {print('If IND was updated to consider potential matches, change proceedNONexist to Y. Otherwise run the INDnonexist block of code and resolve potential matches.')}