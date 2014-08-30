
###pull in ValXsite
SiteWeights=addKEYS(tblRetrieve(Table='',Parameters='VALXSITE',ALLp=AllParam,UIDS=UIDs,ALL=AllData,
                    Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,
                    Protocols=''),#forcing all protocols despite global DataConsumption settings to pull in Failed sites
        c('SITE_ID','DATE_COL','PROJECT'))

###Final Designation reconcilation###
#reconcilation was (will be) done manually for norcal 2013-4, the following needs to be implemented for a more streamlined process:
#! 1.  query used to check for norcal missing valxsite, not sure how to replicate using tblRetrieve
# select * from  (select * from tblverification where PARAMETER='project' and RESULT like '%cal%') as pr
# left join (select * from tblverification where tblverification.PARAMETER='valxsite') as vx on pr.UID=vx.UID
#! 2. also not sure how to automatically bring in Access Sample_Tracking which will identify missing VALXSITE from sites pending final designation (i.e. office omissions that need to be entered into the database, some of which may be resolved by the end of the project and may have multipile records for a single site)
#probsurv14=odbcConnect("ProbSurveyDB")#have to run on remote desktop (gisUser3) or machine which has 64bit R and 64bit Access
#SampleTracking=sqlQuery(probsurv14,sprintf('select * from SampleTracking where Year(SampleDate) in (%s) ',inLOOP(years)))#!tblRetrieve should  be expanded to retrieve from Access too??! because here, I'm essentially reconstructing tblRetrieve/UIDselect for a particular query; would need to add a Source Parameter (WRSAdb vs. ProbSurvey_DB) and add key fields to SampleTracking (i.e. formal project rather than prefix of siteCode or hitch)
#! 3. any missing VALXSITE or changed FinalDesignations should go to Office_Updates in Access and be updated via UpdateDatabase_WRSA.R; avoid duplicates for the same site code especially for failed sites (i.e. if visit 1 failed and visit 2 was successful, insert/update VALXSITE from visit 2; if visit 1 was "temporary inaccessible" and visit 2 was "permenatently inaccessible", determine what the primary reason for not getting to the site was, keeping in mind that in the subsequent translation code, it really boils down to 4 categories (uneval, sucessful, inaccessible (i.e. unknown), and non-target) )


###translate to EvalStatus###
##previous Access iif statements (which also handle duplicate reconciled sites)
#EvalStatus_Target: IIf([NonTarget]=Yes,"NT",IIf(IsNull([minofScoutDate]),"NN",IIf((([minofunsuccessreason]=[maxofunsuccessreason]) Or ([minofunsuccessreason]=62 And [maxofunsuccessreason]=63)) And ([minofunsuccessreason] In (62,63)) And IsNull([sampledate]),"NT",IIf([office_site].[unsuccessReason] In (62,63),"NT",IIf((([minofunsuccessreason] In (65,67)) Or ([minofunsuccessreason]<>[maxofunsuccessreason])) And IsNull([sampledate]) And IsNull([office_site].[unsuccessreason]),"UNK","TS")))))
#EvalStatus_Access: IIf([EvalStatus_Target]="NT","NT",IIf([SampleEvent].[ProbeReachID] Is Not Null,"TS",IIf((([minofunsuccessreason]=[maxofunsuccessreason]) And [minofunsuccessreason] In (60,61)) Or [office_site].[unsuccessreason] In (60,61),"IA",[EvalStatus_Target])))
##Site Status Codes stored in ProbSurvey_DB.accdb Options Category=EvalStatus
# TS: Target Sampled
# NT: Non-Target (dry, too shallow,...)
# NN: Not Needed (not evaluated)
# IA: Inaccessible - any type of Access Issue (landowner, gate, distance, etc), i.e. assumed to still be in target pop, but use as a measure of uncertainty (X% unknown) - combines EMAP "Land Owner Denied" and "Access" (LD/PB) categories
#!IA --> TS for reweighting, but that and any other categories can be used to separate in bar charts

TS_VALXSITE=c('WADEABLE','BOATABLE','PARBYWADE', 'PARBYBOAT','INTWADE', 'INTBOAT', 'ALTERED')
NT_VALXSITE=c('DRYVISIT', 'DRYNOVISIT', 'WETLAND', 'MAPERROR', 'IMPOUNDED', 'TIDAL','NT') 
IA_VALXSITE=c('OTHER_NST','NOTBOAT','NOTWADE', 'OTHER_NOACCESS','NOACCESS', 'INACCPERM','INACCTEMP')
SiteWeights$EvalStatus=ifelse(SiteWeights$RESULT %in% TS_VALXSITE, 'TS',ifelse(SiteWeights$RESULT %in% IA_VALXSITE, 'IA',ifelse(SiteWeights$RESULT %in% NT_VALXSITE, 'NT','NN')))
UNKeval=subset(SiteWeights,EvalStatus=='NN'); if(nrow(UNKeval)>0){print('The VALXSITE for the following sites was not recognized. Please reconcile, or it will be assumed to be unevaluated (NN)');print(UNKeval)}

#mimic UTBLM script (UTblmGRTS.r) that utilizes spsurvey and included the design code as well
#!merge SiteWeights to original design file or frame metadata to get original weights (see ProbSurveydb query Stats_Weights_Sites and/or table GRTS_Design)
#! NN (not evaluated sites get omitted, do they even need to be brought back in?) if so, need to assign null EvalStatus to be NN