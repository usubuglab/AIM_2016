#run SpSurvey_DesignWeights.R first!
#source('SpSurvey_DesignWeights.R')


###-------------------------------------------Input Prep for cat.analysis()------------------------------------------###
###-------SUBPOPULATIONS-------------###
#!NorCal specific!! need to make more broad and dynamic for WRSA!
# create field office variable or other subpopulations of interest for extent estimates
siteeval$Field_Office <- as.factor(siteeval$STRATUM)
levels(siteeval$Field_Office) <- list(Alturas_Field_Office="Alturas Field Office",
                                      Eagle_Lake_Field_Office=c("Eagle Lake Field Offfice", 
                                                                "Eagle Lake Field Office Twin Peaks"),
                                      Surprise_Field_Office=c("Surprise Field Office",
                                                              "Surprise Field Office Home Camp")
)
#subpopuations do NOT need to be in the original design. Others that were considered and investigated in preliminary analysis in UTBLM:
# ,StreamOrder=SiteInfo$StreamOrder, #low sample sizes (see warnings in cat.analysis)
# Ecoregion=SiteInfo$EcoregionIII_Name #low sample sizes (see warnings in cat.analysis)
#EPAeco=SiteInfo$EPAhybridECO

#set up subpopulations for use in cat.analysis
subpopCON=data.frame(siteID=siteeval$SITE_ID,
                        All_Offices=rep("All Field Offices", nrow(siteeval)),
                        Field_Offices=siteeval$Field_Office,
                        Strata=siteeval$STRATUM
                        )

###-------Coordinates for variance estimates-------------###
## Need equal area coordinates for variance estimation 
#! as of Sept 2014, coordinates have NOT been QA'd besides a quick comparison to original GRTS!!!
#  (uses x-site coords when available, design coords otherwise) --> SWJ: I still don't understand why coordinates are important for getting variance estimates
tmp <- marinus(as.numeric(siteeval$LAT_DD), as.numeric(siteeval$LON_DD))
siteeval$xcoord <- tmp[,'x']
siteeval$ycoord <- tmp[,'y']
#! NorCal - why do sites 8487 and 8503 have the same coordinates?! they don't in the design file, but do in GRTS_SITEINFO and WRSAdb


###-------"Design" files (sites and weights)-------------###
sitesCON=data.frame(siteID=siteeval$SITE_ID,
                    Use=(siteeval$EvalStatus !="NN")# revision approved via email with Tony Olsen Feb 2014#old pre-2014 version:# Use=(SiteInfo$EvalStatus_Target=="TS")# 
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
#PIVOT
#ResponseInfo=read.csv('\\\\share2.bluezone.usu.edu\\miller\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\GRTSweights\\Stats_Metrics_4Feb2014.csv');ResponseInfo$UID=siteeval$UID[1:nrow(ResponseInfo)];ResponseInfo$DATE_COL=siteeval$DATE_COL[1:nrow(ResponseInfo)]#(UID randomly assigned for testing)#UTBLM OE scores updated 2/4/14 for all>1="good" and no NA for low split counts (most become poor); Access query "Stats_MetricsOutputCompileForR" #previous inputs: Stats_Metrics_30Oct12
#FLAT
ResponseInfo=read.csv('\\\\share2.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\AquametTEST\\metsSubstrateCharacterization.csv')
#determine if Pivotting needed
aquametSTR=subset(count(colnames(ResponseInfo) %in% c('METRIC','RESULT')),x==TRUE)
if(nrow(aquametSTR)>0){#if in aquamet format
  ResponseInfo$METRIC=toupper(ResponseInfo$METRIC)
  ResponseInfo=cast(ResponseInfo,'UID~METRIC',value='RESULT')
} else if(ncol(ResponseInfo)<10) {print('There are not many columns. Pivot the data if in Flat format!')#or just very few columns
                                  } else {print('There are many columns. Data is assumed to be pivoted with indicator and matching condition names as columns.')}
#merge site info to indicator info
SiteInfo=merge(siteeval,ResponseInfo,by=intersect(colnames(siteeval),colnames(ResponseInfo)),all.x=T)
#add ratings if not done externally (standard process TBD, could adapt from Stats_Thresholds method in ProbSurveyDB.accdb)
#note the use of 'rtg' suffix on the indicator name (this is required structure for subsequent code to work)
#dummies for test set:
##SiteInfo$PCT_SAFNrtg=ifelse(SiteInfo$PCT_SAFN>50,"Poor",'Good');SiteInfo$LSUB_DMMrtg=ifelse(SiteInfo$PCT_SAFN>50,"Poor",'Good')
##SiteInfo$OE=ifelse(SiteInfo$VALXSITE=='WADEABLE',rnorm(nrow(SiteInfo),mean=1,sd=1),NA);SiteInfo$OErtg=ifelse(SiteInfo$OE<.5,"Poor",ifelse(SiteInfo$OE<.75 & SiteInfo$OE>.5,'Fair','Good'))


###-------Indicator Setup-------------###
##potential variables:
#str(SiteInfo)
##variable selection:
selectVARauto='N'; selectVARchoice=ifelse(selectVARauto=='Y','AllVar','CustomVar')#automatically select all variables
extentVAR=c('TNT','EvalStatus','VALXSITE')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
responseVAR=c('OE')
stressorsVAR=c('PCT_SAFN','LSUB_DMM')#UTBLM final list: stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
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
ECname='Conductivity'; TPname='Phosphorus';MWMTname='Max Temp.'; TNname='Nitrogen'; InvasivesYNname='Invasives';OEname='O/E';
TotalHAname='Habitat'; RIPARIANname='Riparian Alt.'; AugSTname= 'Stream Temp.'; SummerSTname= 'Stream Temp (Sum)';
C1WM100name='LWD*';XCDENMIDname = 'Canopy*' ; PCT_SAFNname = 'Fines'; LBFXWRatname='Flood Inundation*' ; Stab2name='Bank Stability'
#EPA Statistical summary: LBFXWRat is an index of streamside flood inundation potential. A high value of LBFXWRat indicates that a stream or river has very unconstrained access to the valley flood plain and has flood flows sufficiently large to do so.
#brokered through varConvert function stored in FNC_tblRetrievePVT.R



###-------------------------------------------Run Categorical Analysis------------------------------------------###
catdata=subset(SiteInfo, select=c('SITE_ID',extentVAR,variablesrtg))

results.cat <- cat.analysis(sites = sitesCON, 
                            subpop = subpopCON, 
                            design = designCON,
                            data.cat = catdata, 
                            #sizeweight=T,#used in UTBLM to scale from segments to lengthKM
                            #? What is the difference between Local and SRS for the vartype parameter? in warnings after running the function, it is switching to SRS anyways bc of low sample sizes in Stream Order and Ecoreg! need clarification on how this treats variables differently (EMAP used Local for Extent and SRS for condition)
                            #vartype = "Local",
                            conf = 95)

ParameterSampleSizes=subset(results.cat,subset=Subpopulation=='All Field Offices' & Category=='Total');print(ParameterSampleSizes)#samplesize is NResp

write.csv(results.cat,'ExtentEstimates.csv');View(results.cat)

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



