###------------------------------------------------pull in relevant SiteInfo------------------------------------------------###
siteeval=tblRetrieve(Table='',Parameters=c('VALXSITE','STRATUM','MDCATY','WGT'),UIDS=UIDs,ALL=AllData,
                    Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,
                    Protocols='')#forcing all protocols despite global DataConsumption settings to pull in Failed sites
siteeval=addKEYS(cast(siteeval,'UID~PARAMETER',value='RESULT'),c('SITE_ID','DATE_COL','PROJECT','VISIT_NO','LAT_DD','LON_DD'))
#View(siteeval) or otherwise check for data gaps and expected # of sites

###Final Designation reconcilation###
#reconcilation was done manually for norcal 2013-4 (took approx 1 hour), the following needs to be implemented for a more streamlined process:
#!prep: for all sites (sampled/visited and failed), qa lat/long (add intended location if failed site), make sure minimum parameters are present (even for "NN" and "NSF" and "UNK" sites!): 'SITE_ID','DATE_COL','VALXSITE','STRATUM','MDCATY','WGT' ,'PROJECT','VISIT_NO','LAT_DD','LON_DD','PROTOCOL'
#! 1.  query used to check for norcal missing valxsite, not sure how to replicate using tblRetrieve
# select * from  (select * from tblverification where PARAMETER='project' and RESULT like '%cal%') as pr
# left join (select * from tblverification where tblverification.PARAMETER='valxsite') as vx on pr.UID=vx.UID
#! 2. also not sure how to automatically bring in Access Sample_Tracking which will identify missing VALXSITE from sites pending final designation (i.e. office omissions that need to be entered into the database, some of which may be resolved by the end of the project and may have multipile records for a single site)
#probsurv14=odbcConnect("ProbSurveyDB")#have to run on remote desktop (gisUser3) or machine which has 64bit R and 64bit Access
#SampleTracking=sqlQuery(probsurv14,sprintf('select * from SampleTracking where Year(SampleDate) in (%s) ',inLOOP(years)))#!tblRetrieve should  be expanded to retrieve from Access too??! because here, I'm essentially reconstructing tblRetrieve/UIDselect for a particular query; would need to add a Source Parameter (WRSAdb vs. ProbSurvey_DB) and add key fields to SampleTracking (i.e. formal project rather than prefix of siteCode or hitch)
#SampleTrackingNEW=sqlQuery(probsurv14,sprintf('select uid, sitecode,streamname,sample_date,sampleable,reason1,finaldesignation, samplecrew,evalstatus from SampleTracking where UID not in (%s) or UID is null',inLOOP(UIDSexist))) #select samples not in the database
#SampleTrackingFINAL=sqlQuery(probsurv14,sprintf('select uid, sitecode,streamname,sample_date,sampleable,reason1,finaldesignation, samplecrew,evalstatus from SampleTracking where sampleable<>finaldesignation and (UID not in (%s) or UID is null)',inLOOP(UIDSexist))) #select samples not in the database
#for sites with no WRSAdb record, minimum parameters are: SITE_ID,LOC_NAME,DATE_COL,VALXSITE,PROJECT,Protocol 
#!unpivot from SampleTrackingNEW to get above parameters, add new uid, then sqlsave; also update the new uid to access sample_tracking
#! 3. any missing VALXSITE or changed FinalDesignations should go to Office_Updates in Access and be updated via UpdateDatabase_WRSA.R; avoid duplicates for the same site code especially for failed sites (i.e. if visit 1 failed and visit 2 was successful, insert/update VALXSITE from visit 2; if visit 1 was "temporary inaccessible" and visit 2 was "permenatently inaccessible", determine what the primary reason for not getting to the site was, keeping in mind that in the subsequent translation code, it really boils down to 4 categories (uneval, sucessful, inaccessible (i.e. unknown), and non-target) )
#questions to resolve
#!merge SiteWeights to original design file or frame metadata to get original weights (see ProbSurveydb query Stats_Weights_Sites and/or table GRTS_Design)
#! NN (not evaluated sites get omitted, do they even need to be brought back in?) if so, need to assign null EvalStatus to be NN

###------------------------------------------------translate to EvalStatus------------------------------------------------##
##previous Access iif statements (which also handle duplicate reconciled sites)
#EvalStatus_Target: IIf([NonTarget]=Yes,"NT",IIf(IsNull([minofScoutDate]),"NN",IIf((([minofunsuccessreason]=[maxofunsuccessreason]) Or ([minofunsuccessreason]=62 And [maxofunsuccessreason]=63)) And ([minofunsuccessreason] In (62,63)) And IsNull([sampledate]),"NT",IIf([office_site].[unsuccessReason] In (62,63),"NT",IIf((([minofunsuccessreason] In (65,67)) Or ([minofunsuccessreason]<>[maxofunsuccessreason])) And IsNull([sampledate]) And IsNull([office_site].[unsuccessreason]),"UNK","TS")))))
#EvalStatus_Access: IIf([EvalStatus_Target]="NT","NT",IIf([SampleEvent].[ProbeReachID] Is Not Null,"TS",IIf((([minofunsuccessreason]=[maxofunsuccessreason]) And [minofunsuccessreason] In (60,61)) Or [office_site].[unsuccessreason] In (60,61),"IA",[EvalStatus_Target])))
##Site Status Codes stored in ProbSurvey_DB.accdb Options Category=EvalStatus
# TS: Target Sampled
# NT: Non-Target (dry, too shallow,...)
# NN: Not Needed (not evaluated)
# IA: Inaccessible - any type of Access Issue (landowner, gate, distance, etc), i.e. assumed to still be in target pop, but use as a measure of uncertainty (X% unknown) - combines EMAP "Land Owner Denied" and "Access" (LD/PB) categories
#!IA --> TS for reweighting, but that and any other categories can be used to separate in bar charts
#!migrate to "translation" table/function planned for Size_NUM, LWD, etc. binning
TS_VALXSITE=c('WADEABLE','BOATABLE','PARBYWADE', 'PARBYBOAT','INTWADE', 'INTBOAT', 'ALTERED')
NT_VALXSITE=c('DRYVISIT', 'DRYNOVISIT', 'WETLAND', 'MAPERROR', 'IMPOUNDED', 'TIDAL','NT') 
IA_VALXSITE=c('OTHER_NST','NOTBOAT','NOTWADE', 'OTHER_NOACCESS','NOACCESS', 'INACCPERM','INACCTEMP')#! should we be more careful to distinguish IA sites that had physical barriers (i.e. bushes) but were clearly target vs. streams we never saw which are truly unknown - either way, they are assumed TS, but need to determine if IA vs. UNK is important for our tabulations (see section 3 of spsurvey/GRTS practitioner guide); if long strings of IA with no subsequent TS, then set to NN. Tony lumps IA into a broader "UNK"
NN_VALXSITE=c('NSF',"NN")# not needed because not in sample frame (ex: 2013-15 canals omitted a priori) or unevaluated
siteeval$EvalStatus=ifelse(siteeval$VALXSITE %in% TS_VALXSITE, 'TS',ifelse(siteeval$VALXSITE %in% IA_VALXSITE, 'IA',ifelse(siteeval$VALXSITE %in% NT_VALXSITE, 'NT',ifelse(siteeval$VALXSITE %in% NN_VALXSITE, 'NN','UNK'))))
UNKeval=subset(siteeval,EvalStatus=='UNK'); if(nrow(UNKeval)>0){print('The VALXSITE for the following sites was not recognized. Please reconcile, or it will be assumed to be unevaluated (UNK)');print(UNKeval)}
omitUIDanalysis=c(11799)#11799=omit NorCal random reference site
siteeval=subset(siteeval,(UID %in% omitUIDanalysis)==FALSE)
siteeval=subset(siteeval,is.na(VISIT_NO) | (VISIT_NO==2)==FALSE)#omit repeat QA visits

###------------------------------------------------assign weights------------------------------------------------###
#read in the target population
#!this is most recent stuff from Tony, should also reconcile with UTBLM adjusted weights (though these were a bit different due to  segment vs. point selection)
#load("C:/Users/Sarah/Desktop/NAMCdevelopmentLocal/WRSA/GRTS_NHDworkspace.RData")#takes a while to read in att, so saved to workspace
#att="N:\\GIS\\Projects\\NRSA\\BLM_NRSA.dbf"#att = Tony (BLM_NRSA NHD)
#att2="N:\\GIS\\GIS_Stats\\Streams\\NHD4GRTS7.dbf"# att2 = Lokteff NHD (updated with correct south dakota; supposed to move location ("N:\GIS\Projects\NHD_GRTS_Process\NHD_AllStreams_Classification.dbf") and get a better name soon!!), worried about StrCalc used to liberally to denote braided channels resulting in too many unconnected and arid streams being omitted
att3=read.dbf('\\\\share2.bluezone.usu.edu\\miller\\GIS\\Projects\\CAblm\\Olsen_EPA_Design\\OriginalDesign\\SWJ_TargetPopClip\\NHD4GRTS7_NorCalFO.dbf') #
att3$mdcaty=ifelse(att3$StreamOr_1<3,"SmallStreams",ifelse(att3$StreamOr<5,"LargeStreams","RiversOther"))
att3$stratum=ifelse(att3$FO=='Surprise','Surprise Field Office',ifelse(att3$FO=='EagleLake','Eagle Lake Field Offfice',ifelse(att3$FO=='Alturas','Alturas Field Office',NA)))
att3$stratum=ifelse(is.na(att3$ALT),att3$stratum,ifelse(att3$ALT=='HomeCamp','Surprise Field Office Home Camp',ifelse(att3$ALT=='TwinPeaks','Eagle Lake Field Office Twin Peaks',att3$stratum)))
#framesum=att #tony original #slop in nhd line splitting since pulling in all of BLM_NRSA, not a clip
framesum=subset(att3,nonBLMstrm==0 & Wtrbody==0 & Intermitt==0)#replicate EPA exclusions (BLM only, not ArtificialPath in waterbody, not intermittent fcode)
#framesum=subset(att3,GRTS_Remov==0)#singlehandedly remove braided (Sc=0), canals, and epa omissions, and non-blm
framesum=subset(framesum,Canal==0)#exclude canals and pipelines
framesum=subset(framesum,StrCalcRem==0)#exclude  streamcalc0 (braided, unconnected --> concerned applies to liberally, seems to affect streams EPA has with positive stream order but we have as 0...perhaps differences in NHDplus version?) #matches with EPA layer in upper right section, but not middle left section; EPA layer is not effectively removing truely braided systems (37.850,-107.572), which this method does
#framesum=subset(framesum,StreamOr_1<5)#exclude RiverOther


# recalculate stream length in field office regions
framesize <- aggregate(framesum$LengthKM2,list(paste(framesum$stratum,framesum$mdcaty, sep="_")),sum);#att3#tmp <- tapply(framesum$LengthKM2,list(paste(framesum$stratum,framesum$mdcaty, sep="_")),sum)#att3
names(framesize)=c('wgt_cat','wgt'); frmszARRY=array(framesize$wgt,dimnames=list(framesize$wgt_cat))
#tmp <- tapply(framesum$LENGTHKM, list(paste(framesum$BLM_UNIT, framesum$DES_NRSA14, sep="_")), sum)#att
#tmp[is.na(tmp)] <- 0
#round(addmargins(tmp), 2)
#framesize <- round(addmargins(tmp), 2)


# read in evaluation information
#SWJ read in from SQL instead (above)
#siteeval <-read.csv('\\\\share2.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\GRTS\\PostSampleDesignAdjustments\\FieldOffice_DesignStatus_Final_23Sept.csv')
#siteeval$STRATUM=siteeval$stratum; siteeval$MDCATY=siteeval$mdcaty; siteeval$WGT=siteeval$wgt;siteeval$SITE_ID=siteeval$siteID #set all variables to upper case if reading in from csv
addmargins(table(siteeval$STRATUM, siteeval$EvalStatus))#addmargins(table(siteeval$stratum, siteeval$EvalStatus_ARO))
addmargins(table(siteeval$MDCATY, siteeval$EvalStatus))#addmargins(table(siteeval$mdcaty, siteeval$EvalStatus_ARO))


# look at design weights
#siteeval=merge(siteeval,framesize,by=c('wgt_cat'))
# tmp <- tapply(siteeval$wgt, list(siteeval$STRATUM, siteeval$mdcaty, siteeval$panel), sum)
# tmp[is.na(tmp)] <- 0
# round(addmargins(tmp), 1)


# adjust weights by field office strata and mdcaty
# create weight adjustment category variable
siteeval$wgt_cat <- factor(paste(siteeval$STRATUM, siteeval$MDCATY, sep="_"))
summary(siteeval$wgt_cat) # look at all categories

siteeval$Wgt_Final <- adjwgt(sites=siteeval$EvalStatus != "NN", 
                             wgt=as.numeric(siteeval$WGT),
                             wtcat=siteeval$wgt_cat, framesize=frmszARRY)

sum(siteeval$Wgt_Final)

write.csv(siteeval,'AdjustedWeights.csv');View(siteeval)

#go to SpSurvey_ExtentEstimates.R for extent estimates
#source('SpSurvey_ExtentEstimates.R')
#then go to ExtentFigures.R for extent estimates
#source('ExtentFigures.R')

