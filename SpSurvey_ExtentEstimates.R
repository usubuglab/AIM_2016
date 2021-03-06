#run SpSurvey_DesignWeights.R first!
#source('SpSurvey_DesignWeights.R')

##-------------------------------------------Input Prep for cat.analysis()------------------------------------------###
###-------SUBPOPULATIONS-------------###
#!NorCal specific!! need to make more broad and dynamic for WRSA!
# create field office variable or other subpopulations of interest for extent estimates
# siteeval$Field_Office <- as.factor(siteeval$STRATUM)
# levels(siteeval$Field_Office) <- list(Alturas_Field_Office="Alturas Field Office",
#                                       Eagle_Lake_Field_Office=c("Eagle Lake Field Offfice", 
#                                                                 "Eagle Lake Field Office Twin Peaks"),
#                                       Surprise_Field_Office=c("Surprise Field Office",
#                                                               "Surprise Field Office Home Camp")
# )
#2016 Start# 
#siteeval=read.csv('AdjustedWeights_2016TRY2.csv')
#siteeval=read.csv('AdjustedWeights_2016_Try1_Run2.csv')
siteeval=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Idaho\\Statewide 2016\\Analysis\\Weights_ExtentEstimates\\AdjustedWeights_IdahoState2017_13April2017.csv')
siteeval=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Analysis\\Weights_ExtentEstimates\\AdjustedWeights_GSENM2016_formated.csv')
siteeval=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Wyoming\\Rawlins\\Analysis\\Weights\\AdjustedWeights_Rawlins_NHD_19Jan2018.csv')


#siteeval$SITE_ID=siteeval$Sitecode
#siteeval$Field_Office <- as.factor("Smoke Creek Watershed")
#siteeval$STRATUM=siteeval$Field_Office
#2016 stop#

#subpopuations do NOT need to be in the original design. Others that were considered and investigated in preliminary analysis in UTBLM:
# ,StreamOrder=SiteInfo$StreamOrder, #low sample sizes (see warnings in cat.analysis)
# Ecoregion=SiteInfo$EcoregionIII_Name #low sample sizes (see warnings in cat.analysis)
#EPAeco=SiteInfo$EPAhybridECO

#siteeval$ReportingUnit1 <- as.factor(siteeval$ReportingUnit1)

siteeval$ReportingUnit1 <- as.factor(siteeval$FieldOffice)


#set up subpopulations for use in cat.analysis
subpopCON=data.frame(siteID=siteeval$SITE_ID,
                     ReportingUnit1=rep("IdahoStatewide", nrow(siteeval))
                     #ReportingUnit2=siteeval$ReportingUnit2,
                     #ReportingUnit3=siteeval$ReportingUnit3
)


# siteeval$CLIMATE <- as.factor(siteeval$CLIMATE)
# 
# 
# #set up subpopulations for use in cat.analysis
# subpopCON=data.frame(siteID=siteeval$SITE_ID,
#                         Westwide=rep("Westwide", nrow(siteeval)),
#                         Climate=siteeval$CLIMATE,
#                         Strata=siteeval$STRATUM
#                         )
#2016 start
#subpopCON=data.frame(siteID=siteeval$SITE_ID,
#                     Strata=siteeval$STRATUM
#)
#2016 stop

###-------Coordinates for variance estimates-------------###
## Need equal area coordinates for variance estimation 
#! as of Sept 2014, coordinates have NOT been QA'd besides a quick comparison to original GRTS!!!
#  (uses x-site coords when available, design coords otherwise) --> SWJ: I still don't understand why coordinates are important for getting variance estimates
UIDs=siteeval$UID
SQLsiteeval=tblRetrieve(Parameters=c('LAT_DD','LON_DD'),UIDS=UIDs)
SQLsiteeval=setNames(cast(SQLsiteeval,'UID~PARAMETER',value='RESULT'),c("UID","LAT_DD","LON_DD"))
siteeval=join(siteeval,SQLsiteeval, by="UID",type="left",match="first")
siteeval$LAT_DD=ifelse(is.na(siteeval$AnalysisDesignation)!="TS",siteeval$DESIGN_LAT,siteeval$LAT_DD)# changed on 1-18-18 from using eval lat long to design lat long for NT or IA sites
siteeval$LON_DD=ifelse(is.na(siteeval$AnalysisDesignation)!="TS",siteeval$DESIGN_LON,siteeval$LON_DD)# changed on 1-18-18 from using eval lat long to design lat long for NT or IA sites

tmp <- marinus(as.numeric(siteeval$LAT_DD), as.numeric(siteeval$LON_DD))
siteeval$xcoord <- tmp[,'x']
siteeval$ycoord <- tmp[,'y']
#! NorCal - why do sites 8487 and 8503 have the same coordinates?! they don't in the design file, but do in GRTS_SITEINFO and WRSAdb

##TNT designation
siteeval$TNT <-siteeval$AnalysisDesignation
#siteeval$TNT <-siteeval$EvalStatus
levels(siteeval$TNT ) <- list(Target=c("TS", "UNK", "IA"),
                              NonTarget="NT",
                              NotNeeded=c("NN"))


###-------"Design" files (sites and weights)-------------###
#2016# NorCal Smoke Creek WS assessment:
# siteeval$xcoord=siteeval$MidpointLo
# siteeval$ycoord=siteeval$MidpointLa
# siteeval$VALXSITE=siteeval$FinalDes

sitesCON=data.frame(siteID=siteeval$SITE_ID,
                    Use=(siteeval$AnalysisDesignation !="NN")# revision approved via email with Tony Olsen Feb 2014#old pre-2014 version:# Use=(SiteInfo$EvalStatus_Target=="TS")# 
                    #Use=(SiteInfo$SampleDate!='' &
                    #format( as.Date(SiteInfo$SampleDate, format="%m/%d/%Y"),'%Y')==2011)#Temporary! Until 2012 bugs and WQ complete
)

designCON=data.frame(siteID=siteeval$SITE_ID,
                  xcoord=siteeval$xcoord,   
                  ycoord=siteeval$ycoord,
                  wgt=siteeval$Wgt_Final#not a separate condition weight (according to Tony Olsen, this was only done to help things sum in a very particular study)#cat.analysis won't run with 0 and NA weights (error: "Weights Must Be Positive") even though none of these are flagged with a TRUE in use in sitesCON
                  #swgt#must also set sizeweight=T in cat.analysis() #used in UTBLM to scale from segments to lengthKM; if I recall correctly, it forces Estimate.U to be a particular value
                  #stratum#not sure why this was specified in UTBLM, should be able to use subpopCON
)


###-------Indicators and Condition Ratings-------------###
#accepts PIVOT or FLAT format, read in test sets to understand structure
#UID is CRITICAL for linking regardless of the format
#! eventually read straight from aquamet or from previous aquamet run stored in WRSAdb (note, aquamet is "flat" and original ResponseInfo was pivotted)
######UT BLM examples###########
#PIVOT
#ResponseInfo=read.csv('\\\\share2.bluezone.usu.edu\\miller\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\GRTSweights\\Stats_Metrics_4Feb2014.csv');ResponseInfo$UID=siteeval$UID[1:nrow(ResponseInfo)];ResponseInfo$DATE_COL=siteeval$DATE_COL[1:nrow(ResponseInfo)]#(UID randomly assigned for testing)#UTBLM OE scores updated 2/4/14 for all>1="good" and no NA for low split counts (most become poor); Access query "Stats_MetricsOutputCompileForR" #previous inputs: Stats_Metrics_30Oct12
#FLAT
#ResponseInfo=read.csv('\\\\share2.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\AquametTEST\\metsSubstrateCharacterization.csv')

######NorCal examples#########
#########################PIVOT
#Below line removes three outliers from all indicators (12453,EL-SS-8124;11777,AR-SS-8017;12476,SU-SS-8322)
#ResponseInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\ExtentEstimates\\NorCal_ExtEst_Input_ReduceAllIndicators.csv')
#Below line uses only 67 sites for MMI but 70 sites for all 
#ResponseInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\ExtentEstimates\\NorCal_ExtEst_Input_ReduceMMI.csv')

#2016 NorCal ELFO WS assessment#
#ResponseInfo=read.csv('C:\\Users\\Nicole\\Desktop\\NorCal_SmokeCreekWSassessment\\SmokeCreek_ExtEst_Input.csv')

#######SRM input file
#ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\ResponseInfo.csv')

######WRSA file
#ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\IndicatorCondECO10_18March2016.csv')

#This is NorCal specific and SHOULD be commented out. 
#This code looks for the UIDs that start with the first three numbers in the input file and replace with the actual UID
#However, if this code does not work or gives errors for ANY lines view the ResponseInfo and check that the first 3 values of the UID are the same as rounding CAN occur while reading the csv to R
#NEEDS to be commented out in case the WRSA UIDs start with any of the same 3 values. 
# ResponseInfo[grep('^172.*?',ResponseInfo$UID),'UID']='1719845190'
# ResponseInfo[grep('^171.*?',ResponseInfo$UID),'UID']='1719845190'
# ResponseInfo[grep('^228.*?',ResponseInfo$UID),'UID']='2282844338'
# ResponseInfo[grep('^535.*?',ResponseInfo$UID),'UID']='5353841249'
# ResponseInfo[grep('^746.*?',ResponseInfo$UID),'UID']='7458804319'
# ResponseInfo[grep('^745.*?',ResponseInfo$UID),'UID']='7458804319'
# ResponseInfo[grep('^164.*?',ResponseInfo$UID),'UID']='1644494991'
# ResponseInfo[grep('^210.*?',ResponseInfo$UID),'UID']='2101326466'
# ResponseInfo[grep('^163.*?',ResponseInfo$UID),'UID']='1632243014'
# ResponseInfo[grep('^522.*?',ResponseInfo$UID),'UID']='5223418430'
# ResponseInfo[grep('^991.*?',ResponseInfo$UID),'UID']='9906345146'
# ResponseInfo[grep('^990.*?',ResponseInfo$UID),'UID']='9906345146'
# ResponseInfo[grep('^524.*?',ResponseInfo$UID),'UID']='5250818050'
# ResponseInfo[grep('^525.*?',ResponseInfo$UID),'UID']='5250818050'
# ResponseInfo[grep('^742.*?',ResponseInfo$UID),'UID']='7417360746'
# ResponseInfo[grep('^741.*?',ResponseInfo$UID),'UID']='7417360746'
# ResponseInfo[grep('^313.*?',ResponseInfo$UID),'UID']='3134919174'
# ResponseInfo[grep('^312.*?',ResponseInfo$UID),'UID']='3134919174'
# ResponseInfo[grep('^449.*?',ResponseInfo$UID),'UID']='4501931467'
# ResponseInfo[grep('^450.*?',ResponseInfo$UID),'UID']='4501931467'


# #FLAT
# #ResponseInfo=read.csv('\\\\share2.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\AquametTEST\\Test2\\10-08input_ToDelete\\metsAquamet_2014-10-02.csv')
# 
# #determine if Pivotting needed
# aquametSTR=subset(count(colnames(ResponseInfo) %in% c('METRIC','RESULT')),x==TRUE)
# if(nrow(aquametSTR)>0){#if in aquamet format
#   ResponseInfo$METRIC=toupper(ResponseInfo$METRIC)
#   ResponseInfo=cast(ResponseInfo,'UID~METRIC',value='RESULT')
# } else if(ncol(ResponseInfo)<10) {print('There are not many columns. Pivot the data if in Flat format!')#or just very few columns
#                                  } else {print('There are many columns. Data is assumed to be pivoted with indicator and matching condition names as columns.')}

#wrsa
#ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Idaho\\Statewide 2016\\Analysis\\Weights_ExtentEstimates\\IndicatorCond_20April2018.csv')
#ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Analysis\\Weights_ExtentEstimates\\IndicatorsCond_GrandStaircase_18April2017revisedsed.csv')
#ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\IndicatorsCond_29April2016.csv')
#ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\IndicatorsCond_revised_wq_bugs_thresh_example.csv')
ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Wyoming\\Rawlins\\Analysis\\Condition_Rinput_16Oct2019.csv')
ResponseInfo=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Idaho\\Statewide 2016\\Analysis\\Weights_ExtentEstimates\\IndicatorCond_13Nov2018_NA_RR.csv')


ResponseInfo$OErtg=ifelse(ResponseInfo$COUNT<200 & ResponseInfo$OErtg!="Good",NA,ResponseInfo$OErtg)
ResponseInfo$OErtg=ifelse(ResponseInfo$MODELTEST=='Fail',NA,ResponseInfo$OErtg)
##exclude QC sites---dont need to worry about it because it is filtered
#ResponseInfo=subset(ResponseInfo,UID!=12457& UID!=12422& UID!=	12714& UID!=	13550& UID!=	11787& UID!=	13527& UID!=	9779832504& UID!=	13518& UID!=	13539& UID!=	8497901114& UID!=	2772740176& UID!=	3833994365& UID!=	7194282454& UID!=	9846034316& UID!=	7977571143& UID!=	4943503766& UID!=	6152206654& UID!=	6964535047& UID!=	7746712455& UID!=	2956707014& UID!=	4324237804& UID!=	4197418344& UID!=	8537408400& UID!=	4116634326& UID!=	2109978745)
ResponseInfo=IndicatorsCond
#ResponseInfo=Indicators

#merge site info to indicator info
#merge site info to indicator info by the column heading that matches between the two files (In WRSA it was UID). 
#If you would like to specify which columns are used to merge then use: SiteInfo=merge(siteeval,ResponseInfo,by.x="Sitecode", by.y="SiteCode",all.x=T)
#Where merge(first file x, second file y, by.x="Column name from first file x", by.y="column name from second file y", all.x=T to keep all records from file x)
SiteInfo=merge(siteeval,ResponseInfo, by.x="UID", by.y="UID",all.x=T)
#SiteInfo=merge(siteeval,ResponseInfo,by=intersect(colnames(siteeval),colnames(ResponseInfo)),all.x=T)


#add ratings if not done externally (standard process TBD, could adapt from Stats_Thresholds method in ProbSurveyDB.accdb)
#note the use of 'rtg' suffix on the indicator name (this is required structure for subsequent code to work)
#dummies for test set:
##SiteInfo$PCT_SAFNrtg=ifelse(SiteInfo$PCT_SAFN>50,"Poor",'Good');SiteInfo$LSUB_DMMrtg=ifelse(SiteInfo$PCT_SAFN>50,"Poor",'Good')
##SiteInfo$OE=ifelse(SiteInfo$VALXSITE=='WADEABLE',rnorm(nrow(SiteInfo),mean=1,sd=1),NA);SiteInfo$OErtg=ifelse(SiteInfo$OE<.5,"Poor",ifelse(SiteInfo$OE<.75 & SiteInfo$OE>.5,'Fair','Good'))


###-------Indicator Setup-------------###
##potential variables:
#str(SiteInfo)
##variable selection:
#########2016_AIM ################
selectVARauto='N'; selectVARchoice=ifelse(selectVARauto=='Y','AllVar','CustomVar')#automatically select all variables
extentVAR=c('TNT','VALXSITE2')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
#extentVAR=c('TNT','EvalStatus','VALXSITE_CHECK')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
#extentVAR=c('TNT','EvalStatus','VALXSITE2')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
responseVAR=c('OE')# Input here should be bug model
#WY
stressorsVAR=c("OE_TN","OE_TP","OE_EC","PH_CHECK","allPCT_SAFN2","LINCIS_H","XCDENBK","XFC_NAT","BnkCover_StabErosional","XCMG")#NOT stressorsVAR=c('MMI')   ####'C1WM100','PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
#GrandStaircase
stressorsVAR=c("OE_TN","OE_TP","OE_EC","PH_CHECK","allPCT_SAFN2","LINCIS_H","XCDENBK","XFC_NAT","INVASIVE_MACRO","BnkCover_StabErosional","XCMG")#NOT stressorsVAR=c('MMI')   ####'C1WM100','PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
#Idaho
#stressorsVAR=c("OE_TN","OE_TP","OE_EC","PH_CHECK","allPCT_SAFN2","LINCIS_H","XCDENBK","BnkCover_StabErosional","Temp")#NOT stressorsVAR=c('MMI')  "XFC_NAT" ####'PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)

#########WRSA_SFS ################
# selectVARauto='N'; selectVARchoice=ifelse(selectVARauto=='Y','AllVar','CustomVar')#automatically select all variables
# extentVAR=c('TNT','EvalStatus','VALXSITE')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
# responseVAR=c('OE')# Input here should be bug model
# stressorsVAR=c("OE_TN","OE_TP","OE_EC","PH_CHECK","PCT_SAFN","XFC_NAT","LINCIS_H","XCDENBK","INVASIVE_MACRO")#NOT stressorsVAR=c('MMI')   ####'PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
#"OE_less100","OE_50_100","BnkCover_StabErosional","XEMBED","XCMG"
#########UT BLM examples###########
# selectVARauto='N'; selectVARchoice=ifelse(selectVARauto=='Y','AllVar','CustomVar')#automatically select all variables
# extentVAR=c('TNT','EvalStatus','VALXSITE')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
# responseVAR=c('OE')
# stressorsVAR=c('PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
#########NorCal examples###########
#responseVAR=c("NV_MMI")# Input here should be bug model
#stressorsVAR=c("NV_Invasives","OE_TN","OE_TP","OE_Conduct","PH_CHECK","BnkStability_BLM_CHECK","PCT_SAFN_CHECK","XCMG_CHECK","XFC_NAT_CHECK","LINCIS_H_CHECK","xcdenmid_CHECK","INVASIVE_MACRO")#,"XEMBED_CHECK")#NOT stressorsVAR=c('MMI')   ####'PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
#"XGB_CHECK",
#To remove MMI from "Poor"/stressorgraphs Use code below
#responseVAR=c("OE_TN")# Input here is bogus
#stressorsVAR=c("NV_Invasives","OE_TP","OE_Conduct","PH_CHECK","BnkStability_BLM_CHECK","PCT_SAFN_CHECK","XCMG_CHECK","XFC_NAT_CHECK","LINCIS_H_CHECK","xcdenmid_CHECK")#,"XEMBED_CHECK")#NOT stressorsVAR=c('MMI')   ####'PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
##########SRM ###################
# selectVARauto='N'; selectVARchoice=ifelse(selectVARauto=='Y','AllVar','CustomVar')#automatically select all variables
# extentVAR=c('EvalStatus2')#TNT#NOT extentVAR=c('MMI','trial','EvalStatus','VALXSITE')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
# responseVAR=c('OE') #Input here should be Bug model e.g., MMI or OE
# stressorsVAR=c("PH_CHECK","XCMG_CHECK","XFC_NAT_CHECK","LINCIS_H_CHECK","XEMBED_CHECK","OE_Conduct","OE_TN","OE_TP","Invasives","xcdenmid_CHECK")#NOT stressorsVAR=c('MMI')   ####'PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
#save previous variable lists here:
#initial run variables (default): c('TotalHA','RIPARIAN',"EC","TN","TP",'MWMT','PCT_SAFN')
#Scott November 2012 figures: c('EC','TP','TN','RIPARIAN','MWMT','PCT_SAFN')

omitVAR=c('LRBS_bw5');omitVARrtg=sprintf('%srtg',omitVAR)#These metrics need additional input from phil kaufman before they are usable. Also, they are crashing the code anyways..not sure why these variables aren't working in access/R, but LRBS_bw5 is crashing cat.analysis with "Estimates cannot be calculated since the vector of categorical variable values is empty."
if(selectVARauto=='Y'){
  variablesrtg=names(ResponseInfo)[grep('rtg',names(ResponseInfo))]
  variables=sub('rtg','',variablesrtg)
  stressorsVAR=setdiff(variables, responseVAR)
  varWRNmsg=''
} else {variables=c(responseVAR,stressorsVAR);variablesrtg=sprintf('%srtg',variables)
        varWRNmsg='Specify the desired variables in the lists responseVAR, extentVAR, and stressorsVAR.\n Then rerun this section of code.'
}
print(sprintf('%s\n Default response variables (responseVAR) currently includes:%s.
              Default extent variable (extentVAR) currently is:%s.
              Default stressor variables (stressorsVAR) currently includes:%s.
              Manually omitted variables (omitVAR) include:%s'
              ,varWRNmsg
              ,response=paste(responseVAR,collapse=", ")
              ,extent=paste(extentVAR,collapse=", ")
              ,stress=paste(variables,collapse=", ")
              ,omit=paste(omitVAR,collapse=", ")
))
variablesrtg=setdiff(variablesrtg,omitVARrtg);variables=setdiff(variables,omitVAR)#temporary, prefer to resolv LRBS issue
stressorsVAR=setdiff(stressorsVAR,omitVAR)

###converting variable names to intelligible names
#this MUST be kept up-to-date even though auto variables are generated, otherwise the variables will be called 'UNK' in the figures
#required format is [AccessVariableName] (exact capitalization matching) + name (all lower case); for example: EC is the variable name in access, thus the variable must be called "ECname"; look at the object variables to see these names
#the asterisk indictates 2012 only
#ECname='Conductivity'; TPname='Phosphorus';MWMTname='Max Temp.'; TNname='Nitrogen'; InvasivesYNname='Invasives';OEname='O/E';
#NorCal
# NV_MMIname='Nevada MMI';
# NV_Invasivesname='Benthic Invasives'; OE_TNname='Total Nitrogen';OE_TPname='Total Phosphorus';OE_Conductname='Conductivity';PH_CHECKname='pH';
# BnkStability_BLM_CHECKname='Bank Stability';PCT_SAFN_CHECKname='% Fine Sediment';XCMG_CHECKname='Riparian Complexity';xcdenmid_CHECKname='Riparian Canopy Cover';
# #XGB_CHECKname='Bare Ground';
# XFC_NAT_CHECKname='Instream Complexity';LINCIS_H_CHECKname='Floodplain Connectivity'




#XEMBED_CHECKname='Embeddedness';OEname='Biological Condition'; Invasivesname='Benthic Invasives;#SRM

#AIM 2016
OE_TNname='Total Nitrogen';OE_TPname='Total Phosphorus';OE_ECname='Specific Conductance';PH_CHECKname='pH';
BnkCover_StabErosionalname='Bank Stability and Cover';allPCT_SAFN2name='% Fine Sediment';XCMGname='Vegetative Complexity';XCDENBKname='% Bank Overhead Cover';
XFC_NATname='Instream Habitat Complexity';LINCIS_Hname='Floodplain Connectivity';
XEMBEDname='Embeddedness';OEname='Biological Condition'; INVASIVE_MACROname='Benthic Invasives'
OE_less100name='OE_less100'; OE_50_100name='OE_50_100'; Tempname='Mean August Temperature'


# #WRSA SFS
# OE_TNname='Total Nitrogen';OE_TPname='Total Phosphorus';OE_ECname='Conductivity';PH_CHECKname='pH';
# BnkCover_StabErosionalname='Bank Stability';allPCT_SAFN2name='% Fine Sediment';XCMGname='Riparian Complexity';XCDENBKname='Riparian Canopy Cover';
# XFC_NATname='Instream Complexity';LINCIS_Hname='Floodplain Connectivity';PCT_SAFNname = 'Fines'
# XEMBEDname='Embeddedness';OEname='Biological Condition'; INVASIVE_MACROname='Benthic Invasives'
# OE_less100name='OE_less100'; OE_50_100name='OE_50_100'

#Old
TotalHAname='Habitat'; RIPARIANname='Riparian Alt.'; AugSTname= 'Stream Temp.'; SummerSTname= 'Stream Temp (Sum)';
C1WM100name='LWD*';XCDENMIDname = 'Canopy*' ; PCT_SAFNname = 'Fines'; LBFXWRatname='Flood Inundation*' ; Stab2name='Bank Stability'; MMIname='MMI';trialname='trial'
#EPA Statistical summary: LBFXWRat is an index of streamside flood inundation potential. A high value of LBFXWRat indicates that a stream or river has very unconstrained access to the valley flood plain and has flood flows sufficiently large to do so.
#brokered through varConvert function stored in FNC_tblRetrievePVT.R


#2016 NorCalWS assessment
#Should be moved to a more appropriate place, temporary for now. 
# Not Sure this is needed. catdata$siteID=catdata$SITE_ID
#SiteInfo$NV_MMIrtg=SiteInfo$NV_MMI_Cond_2
#SiteInfo$OE_Conductrtg=SiteInfo$OE_ECrtg
#SiteInfo$BnkStability_BLM_CHECKrtg=SiteInfo$BnkStability_Erosionalrtg


###-------------------------------------------Run Categorical Analysis------------------------------------------###
catdata=subset(SiteInfo, select=c('SITE_ID',extentVAR,variablesrtg))

###########################------SRM input for figures---------------------------###########################################
# sitesCON=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\sitesCON.csv')
# subpopCON=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\subpopCON.csv')
# designCON=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\designCON.csv')
# catdata=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\catdata.csv')
# rm(sitesCON,subpopCON,designCON,catdata)
############################################################################################################################
subpopCON=subpopCON[,c(1,2)]
results.cat <- cat.analysis(sites = sitesCON, 
                            subpop = subpopCON, 
                            design = designCON,
                            data.cat = catdata, 
                            #sizeweight=T,#used in UTBLM to scale from segments to lengthKM
                            #? What is the difference between Local and SRS for the vartype parameter? in warnings after running the function, it is switching to SRS anyways bc of low sample sizes in Stream Order and Ecoreg! need clarification on how this treats variables differently (EMAP used Local for Extent and SRS for condition)
                            #vartype = "Local",
                            conf = 90)
ParameterSampleSizes=subset(results.cat,subset=Subpopulation=='IdahoStatewide' & Category=='Total');print(ParameterSampleSizes)#samplesize is NResp
#ParameterSampleSizes=subset(results.cat,subset=Subpopulation=='Westwide' & Category=='Total');print(ParameterSampleSizes)#samplesize is NResp

write.csv(results.cat,'ExtentEstimates_ID_Statewide20April2018.csv');View(results.cat)

#old code to force popsize scaling; Tony doesn't typically recommend using and was more necessary for UTBLM segments (not KM)
#popsizeCON=list("Utah"=c("C"=1600,"G"=1100,"W"=400,"Y"=900),"Districts"=list("C"=c("C"=1600),"G"=c("G"=1100),"W"=c("W"=400),"Y"=c("Y"=900)))
#popsizeKM=sum(subset(utbDFO, select=SUM_SUM_SU, subset=STREAMORDE %in% c(2,3,4,5)))#this would be a cleaner method, because only want utblm popsize estimates, but mimic Tony's archaic code bc the grts functions are so specific
# frame_km2 <- tapply(TargetInfo$TotalStreamKM, list(TargetInfo$Stratum, TargetInfo$MDCATY), sum)
# frame_km2[is.na(frame_km2)] <- 0
# frame_km2 <- addmargins(frame_km2)
# popsize_km2 <- list(Utah=frame_km2[,"Sum"]["Sum"], 
#                     Districts=as.list(frame_km2[,"Sum"][1:4]))
# results.cat.EXT <- cat.analysis(sites = sitesCON, 
#                                 subpop = subpopCON, 
#                                 design = subset(designCON,select=c(siteID,swgt,wgt,xcoord,ycoord)),
#                                 data.cat = subset(catdata,select=c('SiteID','EvalStatus_Target')), 
#                                 sizeweight=T,
#                                 popsize=popsize_km2,#popsize = popsizeCON,# 2/14/14 - Tony suggests ONLY using for Target vs. NonTarget extent estimates (not within further subcategories or metrics)
#                                 #? What is the difference between Local and SRS for the vartype parameter? in warnings after running the function, it is switching to SRS anyways bc of low sample sizes in Stream Order and Ecoreg! need clarification on how this treats variables differently (EMAP used Local for Extent and SRS for condition)
#                                 #vartype = "Local",
#                                 conf = 95)




