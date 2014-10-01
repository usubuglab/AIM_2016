# File: UTblmTest.R
# Programmer: Tony Olsen; Modified by Sarah W. Judson
# Date: April 17, 2005; Modified 4/5/2011; Updated with Analysis 10/26/2012

#path=readline("Enter File Path. 
#              If Script and Inputs are in same location, leave blank and press Enter.
#              ")
#won't pause like python raw_input

##PLEASE FILL IN FILE LOCATION PATH
path='SWJ'
WeightsPath=ifelse(path=='SWJ','\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\GRTSweights\\',path)
setwd(WeightsPath)

#Global Figure parameters#
axissize=2#cex
GFPcol=c('firebrick','gold','lightgreen')#c('red','yellow','green')#fix to be global (category can be fed into to only generate relevant colors), needs to be in this order for current code to color good, fair, poor correctly


#######
# Load sp library
#######
# Load psurvey.design library
#######
#install.packages("sp") 
#install.packages("spsurvey")
library (sp)
library (spsurvey)

#######  Read in dbf file to get attributes
# stream network is in Albers projection. See projection file

  
##District    - - FINAL with FO additions to perennial streams
# 30 per district
##from output, figure out how many fall in each ecoregion
#Panel
##year 1: 20
##year 2: 10
##year 3: 10 (may become year 2 oversample)
##oversample: 10 (for year 1)
#Weighted by stream order proportion stream km (see StreamSummary)

##Sample as points to prevent Resampling
utbDFO <- read.dbf("OrigPlusFOinputACTUAL9Jun2011_3")
#notes about final layer:
# streams: perennial segments merged by stream order (2nd through 5th) on blm land  (may be non contigous land ownership)
#debated eliminating <200 meter segments, but I believe we opted to keep them (maybe with the qualifier that they had to have multiple pieces)...in the field we ran into reach length issues
# midpoint taken of each target stream + order; crew allowed to sample anywhere on segment

names(utbDFO) <- tolower(names(utbDFO))
head(utbDFO)
utbDFO
str(utbDFO)     #450 distinct streams #619 segments

# create Strahler categories for equal probability design
utbDFO$strahcat <- as.factor(utbDFO$streamorde)
levels(utbDFO$strahcat) <- list('1st'='1', '2nd'='2', '3rd'='3','4th'='4','5th'='5','Other'=c('6','7','8','9','10','','0') )

#district
 utbDFO$District <- as.factor( utbDFO$first_firs)
levels(utbDFO$District) <- list(Color=c('UTC00000','UT030000'), Green='UTG00000',Canyon='UTY00000',West='UTW00000')         

PaneldsgnDFO<-list(Color=list(panel=c(Year1=20,Year2=10, Year3=10),seltype='Unequal',caty.n=c('2nd'=9,'3rd'=15,'4th'=7,'5th'=9),over=10),
Green=list(panel=c(Year1=20,Year2=10, Year3=10),seltype='Unequal',caty.n=c('2nd'=8,'3rd'=12,'4th'=12,'5th'=8),over=10),   #only 9 available 5th order streams even though they made up for the bulk of the stream km
Canyon=list(panel=c(Year1=20,Year2=10, Year3=10),seltype='Unequal',caty.n=c('2nd'=19,'3rd'=15,'4th'=6),over=10),
West=list(panel=c(Year1=20,Year2=10, Year3=10),seltype='Unequal',caty.n=c('2nd'=22,'3rd'=14,'4th'=3, '5th'=1),over=10))


sample(100000000,1) # run to get random seed
dsgntime <- proc.time()

PanelsitesDFO <- grts(design=PaneldsgnDFO,
                   DesignID='UNEQUAL',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape='OrigPlusFOinputACTUAL9Jun2011_3',      
                   att.frame=utbDFO,
                   stratum='District',
                   mdcaty='strahcat',
                   prjfilename='OrigPlusFOinputACTUAL9Jun2011_3',
                   out.shape='OrigPlusFOinputACTUAL9Jun2011_3_GRTSselect'
                   )
warnings()
  dsgntime <- ( proc.time() - dsgntime )/60  # time in minutes to complete design
dsgntime

addmargins(table( PanelsitesDFO$panel,  PanelsitesDFO$mdcaty, PanelsitesDFO$stratum) )
  
  
  
##ANALYSIS##

#Site Status Codes (EPA)
# TS: Target Sampled
# LD: landowner Denied Access
# PB: Physically Inaccessible
# NT: Non-Target (dry, too shallow,...)
# NS: Not Sampled
# NN: Not Needed (not evaluated)
#Site Status Code (NAMC)
# IA - any type of Access Issue (landowner, gate, distance, etc), i.e. assumed to still be in target pop, but use as a measure of uncertainty (X% unknown) - combines EMAP "Land Owner Denied" and "Access" categories
 

####Adjust Weights#### 
##INPUTS
#ToDo! could odbc import from Access
#ToDo! 26 Oct 2012 export is still missing last 2 sample sites for 2012
#to get estimates of target vs. non target change the two instances of EvalStatus_Target=="TS" to EvalStatus_Target!"NN"
TargetInfo=read.csv(sprintf('%sStats_Weights_Frame_26Oct12.csv',WeightsPath))#Access query "Stats_Weights_Frame"
#!redundant w TargetInfo #DesignInfo=read.csv(sprintf('%sGRTS_Design_9Jun11.csv',WeightsPath))#Access table "GRTS_design"
SiteInfo=read.csv(sprintf('%sStats_Weights_Sites_13Mar13.csv',WeightsPath))#Access query "Stats_Weights_Sites"
#
#
##Weight Data prep##
compareWEIGHTS='Y'
relevantUT=subset(SiteInfo, subset=(EvalStatus_Target!="NN"))#eliminate unvisited sites (assumed to still be in Target Pop)
sitesUT=rep(TRUE, nrow(relevantUT))#revision approved via email with Tony Olsen Feb 2014 (do not omit Non-Target for weight adjustment) #old method: pre-2014 # sitesUT=ifelse(relevantUT$EvalStatus_Target=="TS",TRUE,FALSE)
wgtUTseg=relevantUT$OriginalWeight
wtcatUTdistrict=relevantUT$BLMDistrict#rationale: unit of management inference, lumping of small district + stream orders would cause most groups to approach the strata level 
framesizeUTstrata=unclass(aggregate(TargetInfo$NumSegments, by=list(TargetInfo$Stratum),FUN='sum'))
framesizeUTsegDIST=framesizeUTstrata$x
names(framesizeUTsegDIST)=framesizeUTstrata$Group.1
#Calculate adjusted weights                         
adjwgtUTsegDIST=adjwgt(sitesUT,wgtUTseg,wtcatUTdistrict,framesizeUTsegDIST)
#Join results to the main table                        
relevantUT=cbind(relevantUT,adjwgtUTsegDIST)
#Compare alternate weight methods#                         
if(compareWEIGHTS=='Y'){##other weight strata level comparisions
#level: strata (district) + stream order (issues: small sample sizes in many classes which could significantly affect weight; solution: combine small categories by nearest stream order; problem: differential between strata and for most strata, it ends up combining the majority of categories)
wtcatUT=relevantUT$WGT_CAT  
framesizeUTseg=TargetInfo$NumSegments
names(framesizeUTseg)=TargetInfo$WTCAT
adjwgtUTseg=adjwgt(sitesUT,wgtUTseg,wtcatUT,framesizeUTseg)#as of Feb 2014, Tony rescinded and thinks we should use this for adjusted weights
relevantUT=cbind(relevantUT,adjwgtUTseg)
#level: whole target population (issues: not specific enough, does not account for differential loss between districts)
wtcatUTall=rep("All",nrow(relevantUT))
framesizeUTallseg=sum(TargetInfo$NumSegments)
names(framesizeUTallseg)="All"
adjwgtUTallseg=adjwgt(sitesUT,wgtUTseg,wtcatUTall,framesizeUTallseg)#subtract non-target from Target Pop as a whole
relevantUT=cbind(relevantUT,adjwgtUTallseg)
#could also add the stream KM weights back in and compare results, but Tony Olsen advised that the more proper way to scale to stream KM is using size_weight in cat.analysis
}

#

adjwgtSTRING=names(relevantUT)[breaks=grep('adjwgt',names(relevantUT))]#OR# adjwgtSTRING=subset(names(relevantUT),subset=substr(names(relevantUT),1,6) %in% 'adjwgt'==TRUE)#OR 
SiteInfo=merge(SiteInfo,subset(relevantUT,
                              select=c('Sitecode',adjwgtSTRING)),
                              all.x=T,
                              by="Sitecode") 
Weight_Extent='adjwgtUTseg'#final adjusted weight to use #previously adjwgtUTsegDIST, nowadjwgtUTseg as of Feb 2014, bc Tony rescinded and thinks we should use adjwgtUTseg for adjusted weights
SiteInfo$adjwgtEXT= SiteInfo[,names(SiteInfo)==Weight_Extent]

#Checks on adjusted weights
SiteInfoNoNull=subset(SiteInfo,subset=is.na(adjwgtEXT)==F & EvalStatus_Access=='TS')#EvalStatus_Target=='TS'
TargetPopCompare=aggregate(x=SiteInfoNoNull[,(names(SiteInfoNoNull) %in% adjwgtSTRING | names(SiteInfoNoNull) %in% c('OriginalWeight','Model')) ], 
                           by=list(SiteInfoNoNull$BLMDistrict,SiteInfoNoNull$StreamOrder), FUN='sum',na.action=F)
rm(SiteInfoNoNull)
TargetPopCompare$CountSeg=TargetPopCompare$Model/6
names(TargetPopCompare)[1:2]=c('Stratum','StreamOrder'); TargetInfo$StreamOrder=TargetInfo$MDCATY
TargetPopCompare=merge(TargetPopCompare,TargetInfo,by=c('Stratum','StreamOrder'))#TargetPopCompare=merge(TargetPopCompare,framesizeUTstrata,by='Group.1')#
#??not sure how to use this data (Tony said to compare Dist+SO weighting vs. District weighting to see the effect on TargetPop) -- but am I comparing correctly and what differences are significant?
#??also if use sum for all TS EvalStatus_Target, then it will always sum to the original target population of the stratum used...so how does it actually yield the adjusted target population as in Ator? I think this might be accomplished by only using the sampled streams (TS in EvalStatus_Access)


## Need equal area coordinates for variance estimation ## EPA annotation
#  (uses x-site coords when available, design coords otherwise) ## EPA annotation
tmp <- marinus(SiteInfo$Lat, SiteInfo$Long)
SiteInfo$xmarinus <- tmp[,'x']
SiteInfo$ymarinus <- tmp[,'y']
  


##Condition Estimates##
ResponseInfo=read.csv(sprintf('%sStats_Metrics_4Feb2014.csv',WeightsPath))#OE scores updated 2/4/14 for all>1="good" and no NA for low split counts (most become poor); Access query "Stats_MetricsOutputCompileForR" #previous inputs: Stats_Metrics_30Oct12
#ResponseInfo$SampleDate=paste(ResponseInfo$SampleDate, " 0:00",sep="")
#CAUTION!: Access randomly changes the date format out depending on whether the code is imported or copy/pasted...keep an eye on this! otherwise merge won't work
#SampleDate cut out of merge because of consistent matching problems with various formats...probereachid should be unique for each sampling of that site
#duplicateCheck=min(unique(ResponseInfo$ProbeReachID)==ResponseInfo$ProbeReachID)==1#if true, then no duplicate ProbeReachIDs (Sampling Event) as expected
SiteInfo=merge(SiteInfo,ResponseInfo,by=c("ProbeReachID","Sitecode","BLMDistrict","StreamOrder"),all.x=T)
#SiteInfo=subset(SiteInfo,subset=ProbeReachID!='')#Temporary! bc results.cat misbehaving (all the sudden started caring about duplicates...will matter once want to add % unassessed)
QAeliminate=c('111_29112012','229_29112012')#could filter out in access once determine how QA are stored
#may need to also eliminate ,'5022_2692011' - was creating duplicates in Access as of 1/3/2013 for unknown reasons
SiteInfo=subset(SiteInfo,subset=ProbeReachID %in% QAeliminate==F)
#SiteInfo=subset(SiteInfo,subset= format(as.Date(SiteInfo$SampleDate.x, format="%m/%d/%Y"),'%Y')==2012)#compare 2012 only
SiteInfo$EvalStatus_Specific=ifelse(is.na(SiteInfo$UnsuccessReason),SiteInfo$MinOfUnsuccessReason,SiteInfo$UnsuccessReason)


SiteInfo$SiteID=ifelse(SiteInfo$ProbeReachID=='',as.character(SiteInfo$Sitecode),as.character(SiteInfo$ProbeReachID))

sitesCON=data.frame(siteID=SiteInfo$SiteID,
                 Use=(SiteInfo$EvalStatus_Target!="NN")# revision approved via email with Tony Olsen Feb 2014#old pre-2014 version:# Use=(SiteInfo$EvalStatus_Target=="TS")# 
                    #Use=(SiteInfo$SampleDate!='' &
                 #format( as.Date(SiteInfo$SampleDate, format="%m/%d/%Y"),'%Y')==2011)#Temporary! Until 2012 bugs and WQ complete
                      )
  
subpopCON=data.frame(siteID=SiteInfo$SiteID,
                 Utah=rep('Utah', nrow(SiteInfo)),
                 Districts=SiteInfo$BLMDistrict
                # ,StreamOrder=SiteInfo$StreamOrder, #low sample sizes (see warnings in cat.analysis)
                # Ecoregion=SiteInfo$EcoregionIII_Name #low sample sizes (see warnings in cat.analysis)
                     #EPAeco=SiteInfo$EPAhybridECO
                    )  

designCON=data.frame(siteID=SiteInfo$SiteID, 
                       stratum=SiteInfo$BLMDistrict, 
                       swgt=SiteInfo$BLMlengthKM,
                       wgt=ifelse(is.na(SiteInfo$adjwgtEXT)|SiteInfo$adjwgtEXT==0,1,SiteInfo$adjwgtEXT),#not a separate condition weight (according to Tony Olsen, this was only done to help things sum in a very particular study)
                          #cat.analysis won't run with 0 and NA weights (error: "Weights Must Be Positive") even though none of these are flagged with a TRUE in use in sitesCON
                          #checks that zero/NA weights aren't being used: 
                          #subset(SiteInfo, subset=(adjwgtEXT==0 | is.na(adjwgtEXT)) & SiteInfo$EvalStatus_Target=='TS')
                          #merge(sitesCON,designCON, by=c('siteID'))
                       xcoord=SiteInfo$xmarinus,   ycoord=SiteInfo$ymarinus)


#Variable Setup #
##potential variables:
#str(ResponseInfo)
##variable selection:
selectVARauto='N'; selectVARchoice=ifelse(selectVARauto=='Y','AllVar','CustomVar')#automatically select variables
responseVAR=c('OE')
extentVAR=c('EvalStatus_Access','EvalStatus_Target','EvalStatus_Specific')#Extent Estimate added here since weights the same (rather than running cat.analysis twice)
stressorsVAR=c('InvasivesYN','EC','TP','TN','AugST','LBFXWRat','C1WM100','XCDENMID','Stab2','PCT_SAFN')#must be Access names with a matching 'rtg' variable: to view, str(ResponseInfo)
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

#converting variable names to intelligible names
#this MUST be kept up-to-date even though auto variables are generated, otherwise the variables will be called 'UNK' in the figures
#required format is [AccessVariableName] (exact capitalization matching) + name (all lower case); for example: EC is the variable name in access, thus the variable must be called "ECname"; look at the object variables to see these names
#the asterisk indictates 2012 only
ECname='Conductivity'; TPname='Phosphorus';MWMTname='Max Temp.'; TNname='Nitrogen'; InvasivesYNname='Invasives';OEname='O/E';
TotalHAname='Habitat'; RIPARIANname='Riparian Alt.'; AugSTname= 'Stream Temp.'; SummerSTname= 'Stream Temp (Sum)';
C1WM100name='LWD*';XCDENMIDname = 'Canopy*' ; PCT_SAFNname = 'Fines'; LBFXWRatname='Flood Inundation*' ; Stab2name='Bank Stability'
#EPA Statistical summary: LBFXWRat is an index of streamside flood inundation potential. A high value of LBFXWRat indicates that a stream or river has very unconstrained access to the valley flood plain and has flood flows sufficiently large to do so.

NAMESlist=sub('name','',ls()[grep('name',ls())])#ls call must be outside, otherwise it finds nothing withing the function
print(sprintf('Variables missing presentation names are: %s', temp=paste(setdiff(variables,NAMESlist),collapse=',')))

varConvert=function(x){ #for use within figures to convert to variable names
          presentationNAMES=character()
          for (v in 1:length(variables)){
           if(variables[[v]] %in% NAMESlist) {varNAME=eval(parse(text=sprintf('%sname',variables[[v]])))} else{varNAME='TBD1'}
            presentationNAMES=append(presentationNAMES,varNAME)}
          abc=list(variables, sprintf('%srtg',variables), presentationNAMES)# consider using hash package http://cran.r-project.org/web/packages/hash/
          y=character()
          for (j in 1:length(x)){
            y2=NA
          for (i in 1:length(variables)){
           if(x[[j]]==abc[[1]][[i]] | x[[j]]==abc[[2]][[i]]) {y2=abc[[3]][[i]]} 
           else if(i==length(variables) & is.na(y2)) {y2='TBD2'}
          }
          y=append(y,y2)} 
          list(names=y, color=rainbow(length(y))) }

#Categorical Analysis#
catdata=subset(SiteInfo, select=c('SiteID',extentVAR,variablesrtg))
#popsizeCON=list("Utah"=c("C"=1600,"G"=1100,"W"=400,"Y"=900),"Districts"=list("C"=c("C"=1600),"G"=c("G"=1100),"W"=c("W"=400),"Y"=c("Y"=900)))
#popsizeKM=sum(subset(utbDFO, select=SUM_SUM_SU, subset=STREAMORDE %in% c(2,3,4,5)))#this would be a cleaner method, because only want utblm popsize estimates, but mimic Tony's archaic code bc the grts functions are so specific
frame_km2 <- tapply(TargetInfo$TotalStreamKM, list(TargetInfo$Stratum, TargetInfo$MDCATY), sum)
frame_km2[is.na(frame_km2)] <- 0
frame_km2 <- addmargins(frame_km2)
popsize_km2 <- list(Utah=frame_km2[,"Sum"]["Sum"], 
                    Districts=as.list(frame_km2[,"Sum"][1:4]))
results.cat.EXT <- cat.analysis(sites = sitesCON, 
                            subpop = subpopCON, 
                            design = subset(designCON,select=c(siteID,swgt,wgt,xcoord,ycoord)),
                            data.cat = subset(catdata,select=c('SiteID','EvalStatus_Target')), 
                            sizeweight=T,
                            popsize=popsize_km2,#popsize = popsizeCON,# 2/14/14 - Tony suggests ONLY using for Target vs. NonTarget extent estimates (not within further subcategories or metrics)
                            #? What is the difference between Local and SRS for the vartype parameter? in warnings after running the function, it is switching to SRS anyways bc of low sample sizes in Stream Order and Ecoreg! need clarification on how this treats variables differently (EMAP used Local for Extent and SRS for condition)
                            #vartype = "Local",
                            conf = 95)

results.cat <- cat.analysis(sites = sitesCON, 
    			subpop = subpopCON, 
					design = designCON,
  				data.cat = catdata, 
          sizeweight=T,
         #? What is the difference between Local and SRS for the vartype parameter? in warnings after running the function, it is switching to SRS anyways bc of low sample sizes in Stream Order and Ecoreg! need clarification on how this treats variables differently (EMAP used Local for Extent and SRS for condition)
					#vartype = "Local",
					conf = 95)

ParameterSampleSizes=subset(results.cat,subset=Subpopulation=='Utah' & Category=='Total')#samplesize is NResp
#need to figure out the "#Error" coming from Access for Transect (Incision, RBS, BF:Wet) OR at least change them to NA manually or in R

##EXTENT FIGURES##
#Export='PNL'#options: 'PNG' (exported png); 'PNL' (saved to workspace for later panelling) ## not working as anticipated
ScaleTYPE='Percent'#options: Percent, Absolute (meaning Percentage (Segments or StreamKM same) or StreamKM )
SubpopTypes=unique(results.cat$Type)#SubpopTypes=c('Districts','Utah')
SubpopSort='N'#TO DO! if SubpopSort='Y', will sort each subpopulation based on its own order (may need additional improvement for matching RelExtentPoor to RelRisk...probably saving variableORDER with a district speficic name and then calling via eval at relrisk)
IndicatorInclude='N'# set to "Y" for UTBLM 2014 report in which OE is treated as an equal metric, not the main response variable
#keep an eye on LBFXWRat, was previously sorting incorrectly in the figure generation
for (s in 1:length(SubpopTypes)){#Temporary! only all and district - length(SubpopTypes)
  SubpopStrata=as.character(unclass(unique(subset(results.cat,select=Subpopulation, subset=Type==SubpopTypes[[s]])))[[1]])
  for (t in 1:length(SubpopStrata)){
    ExtentSuffix=sprintf('%s-%s_%s_SFS',SubpopTypes[[s]],SubpopStrata[[t]],ScaleTYPE)
    if(IndicatorInclude=='Y') {BarDataPoor=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]]  & Category=='Poor')
                              } else if (IndicatorInclude=='N'){BarDataPoor=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]]  & Category=='Poor'& Indicator %in% sprintf('%srtg',responseVAR) ==FALSE)}
    if (ScaleTYPE=='Percent'){ BarDataPoor$X= BarDataPoor$Estimate.P; 
                               BarDataPoor$UConf= BarDataPoor$UCB95Pct.P; BarDataPoor$LConf= BarDataPoor$LCB95Pct.P;
                               XmaxPoor=100#setting at 100 sometimes causes problems with large error bars
          } else{ BarDataPoor$X= BarDataPoor$Estimate.U; 
                  BarDataPoor$UConf= BarDataPoor$UCB95Pct.U; BarDataPoor$LConf= BarDataPoor$LCB95Pct.U
                  XmaxPoor=sum(BarDataPoor$X)
                  }
          #code repeated from below, consolidate
    if(s==1 & length(SubpopStrata)==1){#set variable order for remainder
    BarDataPoor=BarDataPoor[with(BarDataPoor,order(X)),]
    variableORDER=data.frame(cbind(Indicator=as.character(BarDataPoor$Indicator), NumericOrder=seq(1,nrow(BarDataPoor))),stringsAsFactors = F)
    variableORDER$StressorV=as.character(sub('rtg','',BarDataPoor$Indicator)); variableORDER$NumericOrder=as.numeric(variableORDER$NumericOrder)
    varFIG=varConvert(variableORDER$Indicator)
    variableORDER=data.frame(cbind(variableORDER,varFIG)); variableORDER$color=as.character(variableORDER$color)
    }
    BarDataPoor=merge(variableORDER,BarDataPoor, by="Indicator",all.x=T)
    BarDataPoor=BarDataPoor[with(BarDataPoor,order(NumericOrder)),]
    if (s!=1 & SubpopSort=='Y'){BarDataPoor=BarDataPoor[with(BarDataPoor,order(X)),]}
    png(sprintf('ExtentPOOR_%s-%s.png',ExtentSuffix,selectVARchoice),width=800,height=700, bg='transparent')
          par(mar = c(2, 1,2 ,0)  ,oma = c(2,7, 1, 2),cex=axissize)   
          #modeled after Fig 23 - http://www.epa.gov/owow/streamsurvey/pdf/WSA_Assessment_May2007.pdf
          #Category barchart: http://www.epa.gov/nheerl/arm/orpages/streamorimpair.htm (cleaner examples in EMAP report)    
        BarEXTp=barplot( BarDataPoor$X,xlim=c(0,XmaxPoor),#FIX! Note 1157 total stream km seems low  (adjwgt adds to 3305, original was **)
                          xlab=ScaleTYPE,
                           names.arg= BarDataPoor$names,horiz=T,
                          col=BarDataPoor$color,las=1) #Temporary! make color global and assign specfically to variables
         title(sprintf('Extent Poor\n%s: %s',SubpopTypes[[s]],SubpopStrata[[t]])
                       ,cex=.1) 
        mtext(ifelse(ScaleTYPE=='Percent','% of Stream KM','Stream KM'),side=1,line=2,cex=axissize)#not sure why xlab is not working in barplot
        BarDataPoor$UConf=ifelse(is.na(BarDataPoor$UConf),0,BarDataPoor$UConf) ; BarDataPoor$X=ifelse(is.na(BarDataPoor$X),0,round(BarDataPoor$X,1))     
        arrows(x0=BarDataPoor$LConf,x1=BarDataPoor$UConf,y0=BarEXTp,length=.1,angle=90,code=3)#use Conf or StErr? why are upper limits so much higher?
        text(y=BarEXTp,x=BarDataPoor$UConf-1, cex=.5,labels=sprintf('%s%s',BarDataPoor$X,ifelse(ScaleTYPE=='Percent','%','')),pos=4,srt=360)#Replace labels with % stream  (from Cell Proportion) 
        graphics.off()
    variablesUSE=c(variablesrtg,extentVAR)#variablesUSE=c(extentVAR,variablesrtg[[1]])#
    for (i in 1:length(variablesUSE)){
      varNAME=sub('rtg','',variablesUSE[[i]])
      BarData=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]] & Indicator==variablesUSE[[i]]  & Category!='Total')
        #ToDO! add in non assessed from Total before removing total
        if(variablesUSE[[i]]!=extentVAR){
          CATcheck=nrow(BarData)
          if(CATcheck<3){#if category missing
            for(f in 1:(3-CATcheck)) {
              BarData=rbind(BarData,0)
              BarData$Category[[(CATcheck+f)]]=ifelse('Fair' %in% BarData$Category==F,'Fair', ifelse('Good' %in% BarData$Category==F,"Good",'Poor'))
          }}
          BarData=BarData[with(BarData,order(Category)),]
          BarData=BarData[with(BarData,order(Category <- c('Good','Fair','Poor'),decreasing=T)),] #sort (very fickle, hence below warning)
          WARNsort=ifelse(BarData$Category[[1]]=='Poor' & BarData$Category[[3]]=='Good','',print(sprintf('SORT incorrect-%s-%s-%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]])))#I think this is corrected with the double sort, but left in just to be safe
        }
        if (ScaleTYPE=='Percent'){BarData$X=BarData$Estimate.P;
                                  BarData$UConf=BarData$UCB95Pct.P;BarData$LConf=BarData$LCB95Pct.P;
                                  Xmax=100
          } else{BarData$X=BarData$Estimate.U;
                 BarData$UConf=BarData$UCB95Pct.U;BarData$LConf=BarData$LCB95Pct.U;
                 Xmax=round(max(results.cat$UCB95Pct.U[results.cat$Indicator!='EvalStatus_Access']),-3)  #the highest upper confidence rounded to the nearest 1000 (actual adjusted pop size is 2500 down from the original 3300)
                 }
      #custom sort order for access #BarData=BarData[with(BarData,order(Estimate.U)),] 
      #manual Xmax for access# Xmax=3000
        png(sprintf('ExtentBAR_%s_%s.png', varNAME,ExtentSuffix),width=1000,height=700, bg='transparent',pointsize = 24)#if(Export=='PNG') {}#temporarily wrapped in if, but recordplot alternative did not work
          par(mar = c(1.5, 1,3 ,1 )  ,oma = c(2, 3, 2, 2),cex=axissize)   
          #modeled after Fig 23 - http://www.epa.gov/owow/streamsurvey/pdf/WSA_Assessment_May2007.pdf
          #Category barchart: http://www.epa.gov/nheerl/arm/orpages/streamorimpair.htm (cleaner examples in EMAP report)    
      BarEXT=barplot( BarData$X,xlim=c(0,Xmax),
                          xlab=ScaleTYPE,
                           names.arg= BarData$Category,horiz=T,col=GFPcol,las=1)
        # title(sprintf('Extent: %s\n%s: %s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]])    ,cex=.5, line=1) 
      mtext(ifelse(ScaleTYPE=='Percent','% of Stream KM','Stream KM'),side=1,line=2,cex=axissize)#not sure why xlab is not working in barplot
      arrows(x0=BarData$LConf,x1=BarData$UConf,y0=BarEXT,length=.1,angle=90,code=3)#use Conf or StErr? why are upper limits so much higher?
          text(y=BarEXT,x=BarData$UConf, cex=.5,labels=sprintf('%s%s',round(BarData$X,1),ifelse(ScaleTYPE=='Percent','%','')),pos=4,srt=360)#Replace labels with % stream  (from Cell Proportion) 
          text(y=0,x=0,WARNsort,cex=5,col='purple')
      # if(Export=='PNL') { assign(sprintf('Extent%s%s%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]]),recordPlot())  }#temporarily wrapped in if, but recordplot alternative did not work
      graphics.off()
#         png(sprintf('ExtentPIE_%s_%s.png', varNAME,ExtentSuffix),width=800,height=600)
#            par(mar = c(1.5, 1,3 ,1 ),cex=axissize)
#           #modelled after: RelRisk Pie: http://www.epa.gov/nheerl/arm/orpages/strmorchembiol.htm
#           pie(BarData$X,
#                 labels=sprintf('%s%s', round(BarData$X,1),ifelse(ScaleTYPE=='Percent','%',''))
#                 , main=sprintf('Extent: %s\n%s: %s', varNAME,SubpopTypes[[s]],SubpopStrata[[t]])
#                 ,clockwise=T
#                 ,col=GFPcol
#                 )
#           legend(x="bottomright",cex=.75,legend=rev(c(BarData$Category)),fill=rev(GFPcol))
#           text(y=0,x=0,WARNsort,cex=5,col='purple')
#             #legend placement is fickle! x=-.5 also works for bottom right and is unrelated to png width
#         graphics.off()
    #could do more complex bars that stack by SubpopStrata (i.e. district)
}}}


#Panelling
#specify custom groupings
#for the number of groups, 
#create X number of columns for the length of the group
#create Y number of row for the number of strata (intended use = all of UT on top, districts below - but make flexible for potential future consumption for WRSA by ecoregion)
#maps yes/no for 1st column
#in original extent figures, have option for toggling titles on or off (want them off for panelling, but keep them on until confident panelling working)
#ExtentBAR_PCT_SAFN_Districts-Y_Percent.png #strata
#ExtentBAR_PCT_SAFN_Utah-Utah_Percent #all
#ExtentPIE_C1WM100_Districts-Y_Percent.png #additional var
#"M:\buglab\Research Projects\UT_BLM_Prob_Baseline\Analyses\Maps\Shaded_West.jpg" #strata map
#"M:\buglab\Research Projects\UT_BLM_Prob_Baseline\Analyses\Maps\Shaded_AllUT.jpg" #all map


library(jpeg)
#specify maps
SubPop_names=list('Utah'='UTAH','Y'='Canyon','C'='Color','G'='Green','W'='West')#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
SubPop_names2=list('UTAH'='UTAH','Canyon'='Canyon','Color'='Color','Green'='Green','West'='West')#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
mapWD="M:\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\Maps\\"
mapFILES=c("Shaded_AllUT",'Shaded_canYon','Shaded_Color','Shaded_Green','Shaded_West')#specify in the desired order; if large number or standard name or iterating over different strata, could extract automatically using read.files()
mapNAMES=as.character(unlist(SubPop_names))
mapLEN=length(mapFILES)
#compile map panel and save
jpeg('MapStack_Districts.jpeg',width=1000,height=(1000*mapLEN)+(100*mapLEN))
par(mar = c(0, 0, 0, 0),oma=c(20,0,20,0),#spacing of components
    mfcol=c(mapLEN,1))#vertical panelling
for (m in 1:mapLEN){
  tmp <- readJPEG(sprintf("%s%s.jpg",mapWD,mapFILES[m]))
  plot(c(0, 10), c(0,10), type="n",
       bty="n",  xlab='',ylab='',yaxt ="n", xaxt = "n")
  mtext(text=mapNAMES[m],cex=8, font=2,line=-8)
  rasterImage(tmp, 1, 1, 9, 9)
}
#save jpeg
graphics.off()
#bring map panel back in and panel with the extent figures live (readJPEG is very slow) 
mapSTACK <- readJPEG('MapStack_Districts.jpeg')



varGRPS=c('WQ','Bio','Rip','Sub')
#need to consolidate with "presentation names" and "varConvert" above
WQ=list('ECrtg'='Conductivity','TPrtg'='Phosphorus','TNrtg'='Nitrogen','AugSTrtg'='Temperature')##WQ=c('ECrtg','TPrtg','TNrtg','AugSTrtg')#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
Bio=list('OErtg'='OE')#could add additional metrics that are relevant in the random forest modelling
Rip=list('Stab2rtg'='Bank Stability','XCDENMIDrtg'='Canopy Cover','LBFXWRatrtg'='Floodplain Connectivity')
Sub=list('PCT_SAFNrtg'='% Fine Sediment','C1WM100rtg'='LWD')
#labeller function for ggplot2
facet_names=c(WQ,SubPop_names,SubPop_names2,Bio,Sub,Rip,Phys)#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
labelALL <- function(variable, value){
    #labelNAME=ifelse(is.null(unlist(facet1_names[as.character(value)])),list(value),facet1_names[as.character(value)])#this isn't quite solving the subpopulation two name issue, so I've cheated and concatenated two subpop lists into facet1 - - the function works by itself but not in labeller for an unknown reason, would be nice to fix it to have a universal labeller function that reads from lists or uses the value if no match is found
  labelNAME=facet_names[as.character(value)]  
  return(paste(labelNAME,"\n",sep=''))

}


library(ggplot2)
library(grid)
library(gridExtra)

TextSize=100
LineSize=5
LegendLocation='bottom'


###SFS settings (UT only, no maps)
#SubPop_names=list('Utah'='UTAH'); SubPop_names2=list('UTAH'='UTAH');SubpopTypes='Utah'
#varGRPS=c('WQ','Bio','Phys')
#Phys=list('Stab2rtg'='Bank Stability','XCDENMIDrtg'='Canopy','LBFXWRatrtg'='Floodplain','PCT_SAFNrtg'='% Fines','C1WM100rtg'='LWD')
#facet_names=c(WQ,SubPop_names,SubPop_names2,Bio,Phys)
#LegendLocation='none'

varLEN=length(varGRPS)
for (g in 1:varLEN){
  varLIST=eval(parse(text=varGRPS[g]))
  variablesUSE=names(varLIST)
BarDataGG=subset(results.cat,subset= Indicator %in% variablesUSE & Category!='Total' & Type %in% SubpopTypes)
BarDataGG$Color=as.factor(ifelse(BarDataGG$Category=='Good','lightgreen',ifelse(BarDataGG$Category=='Fair','gold','firebrick')))
BarDataGG$Category=factor(BarDataGG$Category,               levels = c('Poor','Fair','Good'))#BarDataGG$Order=ifelse(BarDataGG$Category=='Good',3,ifelse(BarDataGG$Category=='Fair',2,1))
BarDataGG$Subpop=factor(SubPop_names[as.character(BarDataGG$Subpopulation)],levels=as.character(unlist(SubPop_names)))
  outputwidth=length(varLIST)+1
  rc=ggplot(BarDataGG, aes(x=Category,y=Estimate.P,fill=Color))
rc= rc + geom_bar(stat = "identity")   + scale_fill_identity(labels=c('Most',"Moderate","Least"),guide=guide_legend(size=50,title='Condition',reverse=T,direction='horizontal')) + 
    coord_flip() + facet_grid(. +  Subpop ~ Indicator ,labeller=labelALL) + #using labeller now, but still need to replace the variable to support ordering by factor; previous: would prefer to use ggplot labeller rather than new columns, but works very inconsistently and doesn't sort - see label_WQ
    labs(y='% of Stream Length',x='') +
    geom_errorbar(aes(ymax = UCB95Pct.P, ymin =LCB95Pct.P), width = .3,colour='black',size=LineSize) + #run to here to see fully labelled panels before stripping off
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),#get rid of horizontal grid and axis
          panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),# element_line(size=LineSize/2),
          axis.line=element_line(size = LineSize/2,colour='black'),axis.ticks.x = element_line(size = LineSize/2,colour='black'),axis.ticks.length = unit( LineSize/2, "lines"),axis.text.x=element_text(colour='black'),#xaxis settings (visible)
          text=element_text(size =TextSize),
          plot.background = element_blank(), panel.background = element_blank(),#along with bg='transparent' in png device call via ggsave, this is needed to get a blank background for overlays
          #axis.text.x = element_text(size=TextSize/2),axis.title.=element_text(size =TextSize),
          strip.text.y= element_blank(),strip.background= element_blank(),#strip.text.x= element_text(size =TextSize),#discard default facet (panel) labels
          panel.margin = unit(LineSize, "lines"),plot.margin = unit(c(7,10,5,0), "lines"),#spacing between panels (not controllable differentially by x and y) and around plot
          legend.position=LegendLocation #legend location - can specify precise coordinates or 'none' to remove
          #base_size = 50#text size - ggplots have trouble scaling proportionally when exported#base_size seems to be deprecated, try rel
          ) 
     ggsave(plot=rc,filename=sprintf('ExtentPanel_%s.png',varGRPS[g]), bg="transparent", units='in', width=8*outputwidth,height=15*length(unique(BarDataGG$Subpopulation)))
#    jpeg(sprintf('MapBarCombine%s.jpeg',varGRPS[g]),width=(1000* outputwidth),height=(1000*mapLEN)+(150*mapLEN))#width=(5*(varLEN+1)),height=(5*mapLEN),units='in',res=100)#width=(1000*(varLEN))+(100),height=(1000*mapLEN)+(100*mapLEN)
#    grid.arrange(rasterGrob(mapSTACK) #from package gridExtra
#                 , rc, widths=c(1/ outputwidth,length(varLIST)/outputwidth), nrow=1)
#    graphics.off()
 }
##SWJ TO DO:
###standardize ggplot 2 width feeding into grid.arrange so not so variable by # of columns #DONE
###resize ggplot text: base_size or rel or size (think I dealt with this in fgd too)#DONE
###Custom variable labels#DONE
###strip good/fair/poor axis#DONE
###only vertical lines in background#DONE
###squish grid arrange and/or try with ggplot custom annotation #http://stackoverflow.com/questions/13299496/reduce-space-between-grid-arrange-plots#DONE
###move ecoregion labels to maps? #DONE
### add percent stream length at the bottom#DONE

#Eval status graph in progress 2/4/2014 - need to define better with Scott
#revised 2/13/2014 - run "ExtentBAR" manually (approx line 365, plus previous lines feeding in) with title commented out and manual Xmax and Sorting as noted in comments
#Eval_Status -- INcluding Non-target and therefore including original weights
ExtentSub=subset(SiteInfo,subset=EvalStatus_Access !='NN')#needs to be revised to utilize results.cat.EXTENT
#do we want to partition into inaccessible (safety/physical) and access denied (landowner) -- i assume the blm cares about the later
ExtentSub$ScaledKM=ExtentSub$OriginalWeight*ExtentSub$BLMlengthKM#is the correct way to scale to KM with the original weights? should one of the sp functions be used?
sum(ExtentSub$ScaledKM)#for report --original target (SiteInfo with NN=4016 (not sure if all oversamp should have been included), with NonEval removed=3455) - 5200 stated in report
ext=ggplot(ExtentSub, aes(x=EvalStatus_Access,y=ScaledKM))#,fill=Color))
ext= ext + geom_bar(stat = "identity")   +  coord_flip()
#are error bars estimated from the % unevaluated? --> EPA Fig 4 has 95% error bars; or should we run through results.cat with original weight to obtain similar estimates
#report in percent or absolute (km), EPA does the latter
#SIsub=subset(SiteInfo,select=c(Sitecode,MinOfUnsuccessReason,Options_OptionText,MaxOfUnsuccessReason,Options_1_OptionText,OmitOverride1,OmitReason,NonTarget1,NonTargetReason,UnsuccessReason,FinalDetermNotes,EvalStatus_Target,EvalStatus_Access,EvalStatus_2012,EvalStatus_2012Resamp))
# #weights 
## original target population == 3305 (according to input shapefile, being careful to only include stream order 1-5)
# #adjusted with non-target included
# Type Subpopulation         Indicator Category NResp  Estimate.P StdError.P  LCB95Pct.P UCB95Pct.P  Estimate.U StdError.U   LCB95Pct.U UCB95Pct.U
# 1        Utah          Utah EvalStatus_Target       NT    73  19.1416361  2.8886690  13.4799489  24.803323  583.748575  85.184827  416.7893814  750.70777
# 2        Utah          Utah EvalStatus_Target       TS   114  80.8583639  2.8886690  75.1966766  86.520051 2465.878794 235.036856 2005.2150207 2926.54257
# 3        Utah          Utah EvalStatus_Target    Total   187 100.0000000  0.0000000 100.0000000 100.000000 3049.627368 229.991458 2598.8523937 3500.40234
# #adjusted as normal (used in analysis) with non-target excluded
# Type Subpopulation         Indicator Category NResp  Estimate.P StdError.P  LCB95Pct.P UCB95Pct.P  Estimate.U StdError.U   LCB95Pct.U UCB95Pct.U
# 1        Utah          Utah EvalStatus_Access       IA    33  27.5330485  4.8712194  17.9856339  37.080463  994.147920 182.016235  637.4026559 1350.89318
# 2        Utah          Utah EvalStatus_Access       TS    81  72.4669515  4.8712194  62.9195369  82.014366 2616.596168 310.452828 2008.1198071 3225.07253
# 3        Utah          Utah EvalStatus_Access    Total   114 100.0000000  0.0000000 100.0000000 100.000000 3610.744089 289.103097 3044.1124315 4177.37575
# #without weight adjustment, which even for percents did incorporate stream km
# Type Subpopulation         Indicator Category NResp Estimate.P StdError.P  LCB95Pct.P UCB95Pct.P Estimate.U StdError.U   LCB95Pct.U UCB95Pct.U
# 1        Utah          Utah EvalStatus_Access       IA    33  34.398733   4.358929  25.8553895  42.942077 143.786705 20.2900142 104.01900799 183.554402
# 2        Utah          Utah EvalStatus_Access       TS    81  65.601267   4.358929  57.0579229  74.144610 274.213295 19.1752313 236.63053214 311.796058
# 3        Utah          Utah EvalStatus_Access    Total   114 100.000000   0.000000 100.0000000 100.000000 418.000000 16.7987406 385.07507344 450.924927


#?will viewports or grid.arrange work with base graphics? --> answer for WRSA boxplots

##grid attempt
# someText <- paste("A panel of text", "produced using", "raw grid code", "that could be used", "to describe","he plot", "to the right.", sep = "\n")
# grid.rect(gp = gpar(lty = "dashed"))
# pushViewport(viewport(layout = grid.layout(1, 2, widths = unit.c(unit(1, "strwidth", someText) , unit(2, "cm"), unit(1, "null")))))
# pushViewport(viewport(layout.pos.col = 1))
# grid.rect(gp = gpar(fill = "light grey"))
# grid.text(someText, x = unit(1, "cm"), y = unit(1, "npc") - unit(1, "inches"), just = c("left", "top"))
# popViewport()
# pushViewport(viewport(layout.pos.col = 2))
# #replayPlot(ExtentTest1)
# plot(rc,vp=2)
# popViewport(2)


##split screen attempt
# split.screen(rbind(c(0.1,0.292,0.1, 0.98), c(0.312, 0.95, 0.1, 0.98)))
# screen(1)
# par(mar = c(0, 0, 0, 0))
# plot(1:30, rnorm(30), xaxs = "i", ylim = c(-3, 3), xaxt = "n")
# axis(1, at = seq(0, 30, 20))
# 
# screen(2)
# rc

 


## attempt to use lattice
# library(lattice)
# attach(results.cat)
# barchart(X~Category|Subpopulation*Indicator, 
#          main="Scatterplots by Cylinders and Gears", 
#          ylab="Miles per Gallon", xlab="Car Weight")

##attempt to use base graphics
# par(mfrow=c(mapLEN,varLEN),mar = c(0, 2,0 ,0),oma = c(2, 3, 2, 2))#par(mar = c(1.5, 1,3 ,1 )  ,oma = c(2, 3, 2, 2),cex=axissize)  
# #copied from EXTENT figures (would have preferred to store the figures as objects and then just call them here, but couldn't easily find a solution)
# #consider wrapping BarData and BarEXT into a function since repeated (would need toggling options for axis names and titles)
# 
# for (g in 1:varLEN){
#   variablesUSE=eval(parse(text=varGRPS[g]))
#   track=0
# 
#   for (s in 1:length(SubpopTypes)){
#   SubpopStrata=as.character(unclass(unique(subset(results.cat,select=Subpopulation, subset=Type==SubpopTypes[[s]])))[[1]])
#   for (t in 1:length(SubpopStrata)){
#     for (i in 1:length(variablesUSE)){
#       #replayPlot(eval(parse(text=sprintf('Extent%s%s%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]]))))#unfortunately, doesn't adopt new par settings for panelling
#       varNAME=sub('rtg','',variablesUSE[[i]])
#       BarData=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]] & Indicator==variablesUSE[[i]]  & Category!='Total')
#       #ToDO! add in non assessed from Total before removing total
#       if(variablesUSE[[i]]!=extentVAR){
#         CATcheck=nrow(BarData)
#         if(CATcheck<3){#if category missing
#           for(f in 1:(3-CATcheck)) {
#             BarData=rbind(BarData,0)
#             BarData$Category[[(CATcheck+f)]]=ifelse('Fair' %in% BarData$Category==F,'Fair', ifelse('Good' %in% BarData$Category==F,"Good",'Poor'))
#           }}
#         BarData=BarData[with(BarData,order(Category)),]
#         BarData=BarData[with(BarData,order(Category <- c('Good','Fair','Poor'),decreasing=T)),] #sort (very fickle, hence below warning)
#         WARNsort=ifelse(BarData$Category[[1]]=='Poor' & BarData$Category[[3]]=='Good','',print(sprintf('SORT incorrect-%s-%s-%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]])))#I think this is corrected with the double sort, but left in just to be safe
#       }
#       if (ScaleTYPE=='Percent'){BarData$X=BarData$Estimate.P;
#                                 BarData$UConf=BarData$UCB95Pct.P;BarData$LConf=BarData$LCB95Pct.P;
#                                 Xmax=100
#       } else{BarData$X=BarData$Estimate.U;
#              BarData$UConf=BarData$UCB95Pct.U;BarData$LConf=BarData$LCB95Pct.U;
#              Xmax=round(max(results.cat$UCB95Pct.U[results.cat$Indicator!='EvalStatus_Access']),-3)  #the highest upper confidence rounded to the nearest 1000 (actual adjusted pop size is 2500 down from the original 3300)
#       }
#       #modeled after Fig 23 - http://www.epa.gov/owow/streamsurvey/pdf/WSA_Assessment_May2007.pdf
#       #Category barchart: http://www.epa.gov/nheerl/arm/orpages/streamorimpair.htm (cleaner examples in EMAP report)    
#       track=track+1
#       BarEXT=barplot( BarData$X,xlim=c(0,Xmax),
#                      xaxt='n',yaxt='n',#replaced xlab with no axes
#                      names.arg= BarData$Category,horiz=T,col=GFPcol,las=1)
#       if(track<=varLEN) axis(3, label=FALSE,outer=TRUE)
#       #box()
#       axis(2,lwd.ticks=0,labels=FALSE); axis(4,lwd.ticks=0,labels=FALSE)
#       if(track>(varLEN * (mapLEN-1))) {axis(1)}
#       #title(sprintf('Extent: %s\n%s: %s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]]),cex=.5, line=1) #remove once formatting complete
#       arrows(x0=BarData$LConf,x1=BarData$UConf,y0=BarEXT,length=.1,angle=90,code=3)#use Conf or StErr? why are upper limits so much higher?
#       #attempt to compensate for high percents getting knocked off page (but EPA figures don't even have the annotations)
#       # if(BarData$UConf>80){labARROWx=ifelse(BarData$UConf>80,90,BarData$UConf); #labARROWy=ifelse(BarData$UConf>80,BarEXT+.25,BarEXT)
#       #  } else {}   labARROWx=BarData$UConf; #labARROWy=BarEXT
#       #          }#Replace labels with % stream  (from Cell Proportion) 
#       #labARROWy=BarEXT
#       #text(y=labARROWy,x=labARROWx, cex=.5,labels=sprintf('%s%s',round(BarData$X,1),ifelse(ScaleTYPE=='Percent','%','')),pos=4,srt=360)
#    }
#   }
# }
# }
#         
  

##RELATIVE RISK + Figures##
samplesUSEDprep=subset(catdata,subset=SiteID!="" & SiteID!="163") # & ((substr(catdata$SiteID,nchar(as.character(catdata$SiteID)),nchar(as.character(catdata$SiteID)))!="2" )) )
 #Temporary! remove & ((substr...)) in both above and below , temporary to eliminate 2012 bc OE and WQ results not in (accidentally gets rid of 122)
sitesUSEDprep=subset(designCON,subset=siteID!="" & siteID!="163")# & ((substr(designCON$siteID,nchar(as.character(designCON$siteID)),nchar(as.character(designCON$siteID)))!="2" )) )
##check probeReachID match order between two sets
#loop over subpop too? not as straightforward as cat.analysis, but would just require a join
##especially add districts!!
districts=c('All','C','Y','G','W')
variableORDER=subset(variableORDER,subset=Indicator %in% sprintf('%srtg',responseVAR) ==FALSE)#OMIT OE from variable order since previously 
#Lump Fair into Good (not recommended by EPA, makes more sense for regulatory inclu comparing to DEQ)
lumpFAIR='Y'#options 'Y' or 'N' (caps, representing yes/no)
for (r in 1:length(responseVAR)){
  for (d in 1:length(districts)) { #make more dyanmic for all subpop, but this is immediate need
   if (districts[[d]]=='All'){sitesUSED=sitesUSEDprep; samplesUSED=samplesUSEDprep
  } else{sitesUSED=subset(sitesUSEDprep,subset=stratum==districts[[d]])
         samplesUSED=subset(samplesUSEDprep,subset=SiteID %in% as.character(sitesUSED$siteID))}
    for (v in 1:length(stressorsVAR)){
      samplesUSEDv=samplesUSED;sitesUSEDv=sitesUSED
      samplesUSEDv$X=as.character(unclass(subset(samplesUSEDv,select=sprintf('%srtg',responseVAR[[r]])))[[1]])
      samplesUSEDv$Y=as.character(unclass(subset(samplesUSEDv,select=sprintf('%srtg',stressorsVAR[[v]])))[[1]])
      if (lumpFAIR=='N') {
        samplesUSEDv=subset(samplesUSEDv, subset=(X!="Fair"  & Y!="Fair"))
        sitesUSEDv=subset(sitesUSED,subset=siteID %in% samplesUSEDv$SiteID)
        fairname=''
             } else{
           samplesUSEDv$X=ifelse(samplesUSEDv$X=="Poor","Poor","Good")
           samplesUSEDv$Y=ifelse(samplesUSEDv$Y=="Poor","Poor","Good")
           fairname='incluFAIR'
          } 
      RelRiskSuffix=sprintf('%s_%s',districts[[d]],fairname)
      if (nrow(samplesUSED)>1){
      results.relrisk=relrisk.est(response=samplesUSEDv$X, 
                                  #relrisk function only suggests two categories (Good and Poor); hence the ifelse lumping fair into good
                                stressor=samplesUSEDv$Y,
                                response.levels=c("Poor","Good"),  stressor.levels=c("Poor",  "Good"), 
                                wgt=sitesUSEDv$wgt,
                                xcoord=sitesUSEDv$xcoord, ycoord=sitesUSEDv$ycoord
                                )
      #ToDo! finish append results (odd format, some tables, some single values)
      ResponseV=responseVAR[[r]]; StressorV=stressorsVAR[[v]];DistrictV=districts[[d]]
      RelRisk=results.relrisk$RelRisk;LConf=results.relrisk$ConfLimits[[1]];UConf=ifelse(is.na(results.relrisk$ConfLimits),NA,results.relrisk$ConfLimits[[2]])
      if(r==1 & v==1 & d==1) {  options(stringsAsFactors = FALSE)
                         RelRiskOUT=data.frame(ResponseV,StressorV,DistrictV,
                                                RelRisk,LConf,UConf)    
                          } else {RelRiskOUT=rbind(RelRiskOUT,c(ResponseV,StressorV,DistrictV,RelRisk,LConf,UConf))}
      #would like table call to be more dynamic (not indexed)
      GoodMATCH=results.relrisk$CellProportions[[2,2]]#Agreement: Not Impaired
      PoorMATCH=results.relrisk$CellProportions[[1,1]] #Agreement: Impaired
      PoorRESPONSEonly=results.relrisk$CellProportions[[1,2]]
      PoorSTRESSORonly=results.relrisk$CellProportions[[2,1]]
      percentVALUES=c(GoodMATCH,PoorMATCH, PoorRESPONSEonly,PoorSTRESSORonly)
      colorVALUES=c('green','red','yellow','orange') #setup like FGD (control BW vs. color globally)
      png(sprintf('%s_%s_%s.png',responseVAR[[r]],stressorsVAR[[v]],RelRiskSuffix),width=800,height=600)
         par(mar = c(1.5, 1,3 ,1 ),cex=axissize)
        #modelled after: RelRisk Pie: http://www.epa.gov/nheerl/arm/orpages/strmorchembiol.htm
        pie(percentVALUES,
              labels=sprintf('%s%s', round(100*percentVALUES,2),'%')
              , main=sprintf('%s : %s\n%s\nRelRisk: %s',responseVAR[[r]],stressorsVAR[[v]],districts[[d]],round(RelRisk,2))
              ,clockwise=T
              ,col=colorVALUES 
              )
        legend(x="bottomright",cex=.5,legend=c('Agreement: Not Impaired','Agreement: Impaired','Response Impaired','Stressor Impaired'),fill=colorVALUES)
          #legend placement is fickle! x=-.5 also works for bottom right and is unrelated to png width
      graphics.off()
    }
    options(stringsAsFactors = TRUE)
    RelRiskOUT$RelRisk=as.numeric(RelRiskOUT$RelRisk);RelRiskOUT$UConf=as.numeric(RelRiskOUT$UConf);RelRiskOUT$LConf=as.numeric(RelRiskOUT$LConf)
    #! figure out how to handle NA (one variable has only Good or only Poor) so the var included in chart - the rarer scenario is perfect concordance (denominator=0) which only happens at the district level
    RelRiskCHART=subset(RelRiskOUT,subset=is.na(RelRisk)==F & DistrictV==districts[[d]])#Temporary! get rid of NA (bc won't plot) -- too correct, see handling of NA in Extent Bar
    #RelRiskCHART=RelRiskCHART[with(RelRiskCHART,order(RelRisk)),] #OLD #sort by RelRisk
    RelRiskCHART=merge(variableORDER,RelRiskCHART, by="StressorV",all.x=T)
    RelRiskCHART=RelRiskCHART[with(RelRiskCHART,order(NumericOrder)),]
    RelRiskCHART=unique(RelRiskCHART)#! very random issue with TotalHA appearing twice for All
    RelRiskCHART$LConf=ifelse(is.na(RelRiskCHART$LConf),0,RelRiskCHART$LConf);RelRiskCHART$UConf=ifelse(is.na(RelRiskCHART$UConf),0,RelRiskCHART$UConf)#got one random 0 relrisk; causes problems with xlim, can likely remove once xlim solidified
      if(nrow(RelRiskCHART)>0){
      png(sprintf('RelRisk_%s-%s.png',RelRiskSuffix,selectVARchoice),width=800,height=700, bg='transparent') 
      par(mar = c(1.5, 1,2 ,1 )  ,oma = c(2,7,1,1),cex=axissize)   
    #modeled after Fig 23 - http://www.epa.gov/owow/streamsurvey/pdf/WSA_Assessment_May2007.pdf
      BarRR=barplot( RelRiskCHART$RelRisk,xlim=c(0,max(RelRiskCHART$UConf)+1),#?should scale be the same between all graphs?
                     names.arg=RelRiskCHART$names,horiz=T,main=sprintf('Relative Risk: %s\n%s',responseVAR[[r]],districts[[d]])
                     ,col=as.character(RelRiskCHART$color),las=1) #Temporary! make color global and assign specfically to variables
      mtext('Relative Risk',side=1,line=2,cex=axissize)#not sure why xlab is not working in barplot
      arrows(x0=RelRiskCHART$LConf,x1=RelRiskCHART$UConf,length=.1,y0=BarRR,angle=90,code=3)#use Conf or StErr? why are upper limits so much higher?
      abline(v=1,lty=2)
      #text(y=BarRR,x=RelRiskCHART$UConf, labels=round(RelRiskCHART$RelRisk,2),pos=4,srt=360)#Replace labels with % stream  (from Cell Proportion) 
    graphics.off()
      }
   }}}


contdata=subset(SiteInfo, select=c('SiteID',variables))#responseVAR
results.cont<- cont.analysis(sites = sitesCON, 
      		subpop = subpopCON, 
					design = designCON,
          data.con=contdata)
#ToDo! CDF charts -cdfplot.fcn()
#ToDo! Compare 2 CDFs using fx cdf.test() (see ARM website, similar to relrisk but for continuous)

##CSV output##
outputs=c('results.cat:Results_Categorical_ConditionSum',#outputVariable:outputFilename
          'RelRiskOUT:Results_Categorical_RelRisk',
          'results.cont$CDF:Results_Continuous_CDF',
          'results.cont$Pct:Results_Continuous_Percent')
for (o in 1:length(outputs)){
  out=strsplit(outputs[[o]],":")
  write.table(eval(parse(text=(out[[1]][1]))), file=sprintf("%s_%s.csv",out[[1]][2],Sys.Date()), sep=",", row.names=FALSE)
}

####END ANALYSIS####
##Method checks##
##fines##
Fines=subset(ResponseInfo,is.na(PCT_SAFNrtg)==F);FinesPool=subset(Fines,FineMethod=="Pool");FinesTran=subset(Fines,FineMethod=="Tran")
FinesX=c('Fines$D84_woBed','FinesPool$PercentFinesAVG','FinesTran$PercentFinesAVG'); FinesY=c('Fines$PCT_SAFN','FinesPool$PCT_SAFN','FinesTran$PCT_SAFN')
for(x in 1:length(FinesX)){
  FinesXsel=eval(parse(text=FinesX[[x]]));FinesYsel=eval(parse(text=FinesY[[x]]))
  plot(FinesXsel,FinesYsel, xlab=FinesX[[x]], ylab=FinesY[[x]])
  mtext(paste('Pearson: ', cor(FinesXsel,FinesYsel)),side=3)
}

Sites2Methods=subset(Fines,MTHDCNT==2,select=c('ProbeReachID','Sitecode'))
#Reach 4051: Pool method (n=30, % Fines= 0.275); Tran method (n=9, % Fines=0.441)
#Reach 4161_2592011: Pool method (n=1, % Fines= 0.18); Tran method (n=10, % Fines=0.354)

#section commented out with triple quotes#

"""
##extent combined into condition
#Extent estimates of site categories (access issues vs. candidate/scouted sites)
sitesEXT=data.frame(SiteInfo$Sitecode,
                 Use=(SiteInfo$EvalStatus_Target=='TS')) #need to eliminate sites with zero weight, otherwise later steps crash (hence different weights for extent and condition)
                 #Use=rep(TRUE,nrow(SiteInfo)))#use all sites
                               ##so, the R code is telling me that sites that get sampled get a lower weight (EPA=condition success) than the actual population success (EPA=extent weight); so how did they get weights for the non-evaluated sites? (for uncertainty); and why can't you apply population weights (i.e. successful scouts) to the metrics?
subpopEXT=data.frame(SiteID=SiteInfo$Sitecode, 
                 Utah=rep('Utah', nrow(SiteInfo)),
                 Districts=SiteInfo$BLMDistrict,
                 StreamOrder=SiteInfo$StreamOrder
                  #Ecoregion=#ToDo! This is in response, not siteinfo
                    )
                 #? Can these be different than original stratum? If so, how? I.E. could we add Ecoregion which isn't nested within District (the original statrum)?'
                 
## Need equal area coordinates for variance estimation ## EPA annotation
#  (uses x-site coords when available, design coords otherwise) ## EPA annotation
tmp <- marinus(SiteInfo$Lat, SiteInfo$Long)
SiteInfo$xmarinus <- tmp[,'x']
SiteInfo$ymarinus <- tmp[,'y']
  
designEXT=data.frame(siteID=SiteInfo$Sitecode, 
                       #? is this just District (stratum) or District + Stream Order (weight cat? (original stratum was District, but was weighted by stream order)
                       stratum=SiteInfo$BLMDistrict,
                       #?Why are weights for extent and condition different?
    	                 wgt=ifelse(is.na(SiteInfo$adjwgtEXT),0,SiteInfo$adjwgtEXT),
                       #! how are the coordinates used? What kind of spatial analysis is being done at this point?
			                 xcoord=SiteInfo$xmarinus,
			                 ycoord=SiteInfo$ymarinus)
	
  
#Extent Estimate#
catdataEXT=data.frame(siteID=SiteInfo$Sitecode,response=SiteInfo$EvalStatus_Access)
  
popstatus.ext <- cat.analysis(sites = sitesEXT, 
  				subpop = subpopEXT, 
					design = designEXT,
  				data.cat = catdataEXT, 
					#vartype = "Local", #! need clarification on how this treats variables differently
					conf = 95)
 

##weights notes, prior to consultation with Tony Olsen (11/30/12)
##Data prep##
relevantUT=subset(SiteInfo, subset=(EvalStatus_Target!="NN"))#eliminate unvisited sites (assumed to still be in Target Pop)
#?? Or, should we assign NN to "TRUE" to be able to get an extent/status estimate for them?#relevantUT=SiteInfo # plus next annotation for sitesUT #when done this way, sites appropriately add to target pop size BUT should add to adjusted pop size (original size - weight OR streamkm); if only TS considered, then weight higher than original!
sitesUT=ifelse(relevantUT$EvalStatus_Target=="TS",TRUE,FALSE)# sitesUT=ifelse(relevantUT$EvalStatus_Target!="NT",TRUE,FALSE) 
sitesUTsamp=ifelse(relevantUT$SampleDate=="",FALSE,TRUE)
wgtUTseg=relevantUT$OriginalWeight
TargetInfo$WeightScaleSTR=(TargetInfo$OriginalWeight * TargetInfo$TotalStreamKM)/TargetInfo$NumSegments #logic NumSegments/Weight=SampleSize --> solve for x in equivalency conversion: Total(seg)/Weight(seg)=Total(km)/Weight(km) #since we selected the number of segments based on % stream KM desired, then the inclusion probability (inverse of weight) selected by segment can be scaled by the TotalStreamKM
#alternative calc: 
#Alternative 1: scale segments by 200 m and divide by total stream km (then get inverse of the probability) #logic for NOT using:  but, all GRTS designs (linear or finite) have a design panel that only specifies the number of streams you want pulled out, not streamKM of them
#Alternative 2: should stream km weight be assigned at the site level (Weight * StreamKM) so longer streams contribute more to the total ##logic for NOT using: original weight is created based on whole target pop, not knowledge of individual segments or their lengths....linear resource designs assign points randomly anywhere, the stream km and 200 m are only used to determine total population and therefore the needed sample size --> NOT done in Wadeable streams, the same weight is applied to all in the same stratum, but is a larger absolute value because of stream km
TargetInfo$WGT_CAT=TargetInfo$WTCAT
relevantUT=merge(relevantUT,TargetInfo,by="WGT_CAT")
wgtUTstr= relevantUT$WeightScaleSTR
wtcatUT=relevantUT$WGT_CAT  
wtcatUTall=rep("All",nrow(relevantUT))
#switch from finite resource (segment midpoint) to linear resource (stream km)
framesizeUTseg=TargetInfo$NumSegments;framesizeUTstr=TargetInfo$TotalStreamKM
names(framesizeUTseg)=TargetInfo$WTCAT;names(framesizeUTstr)=TargetInfo$WTCAT
framesizeUTallseg=sum(TargetInfo$NumSegments);framesizeUTallstr=sum(TargetInfo$TotalStreamKM)
names(framesizeUTallseg)="All";names(framesizeUTallstr)=TargetInfo$WTCAT="All"
#
##Various adjusted weight methods## 
#Extent Weights#
adjwgtUTseg=adjwgt(sitesUT,wgtUTseg,wtcatUT,framesizeUTseg)#subtract non-target from specific stratum + stream order; weights adjusted to number of segments
adjwgtUTstr=adjwgt(sitesUT,wgtUTstr,wtcatUT,framesizeUTstr)#subtract non-target from specific stratum + stream order; seg scaled to stream km
#? SWJ 10/31: Should weights be scaled by the total population (Wetlands example (R code available), perhaps Indiana), stratum (Ator?), or category (stratum + stream order (weighted category))
#TOlson 11/5: Weight adjustment: adjustment should at a minimum include the categories and strata used in the design. It should also reflect how sites were replaced if that differed from what was used in the design. The adjustment categories can also be done to reflect the stream km in the sample frame that you want to make sure are matched. So at a minimum would adjust by BLM district since likely want the weights to sum up to the stream length in a district. Also likely that you would prefer to have weights sum up to stream length by strahler order categories within a district. Issue that can arise is that you may only have a few sites within these categories and it is preferred that have 5-10 sites evaluated within a category. If that does not happen, then may want to collapse some strahler order categories.
#?? SWJ 11/5: only category with less than 5-10 is W4 and W5, should these be collapsed even though all others had sufficient segments?
##?? SWJ 11/5: should stream weights simply be stream KM?? just the total pop changes?
#We have been assuming weight is by each unique class (stratum + stream order), or should it simply be total pop (which would assume sites are unsuccessful at same rate across total target pop, which is not the case in ours...we lose many more southern streams to intermittency)
#adjwgtUTstrata=NA#substract by strata (perhaps like Ator et al.)
adjwgtUTallseg=adjwgt(sitesUT,wgtUTseg,wtcatUTall,framesizeUTallseg)#subtract non-target from Target Pop as a whole
adjwgtUTallstr=adjwgt(sitesUT,wgtUTstr,wtcatUTall,framesizeUTallstr)#subtract non-target from Target Pop as a whole;  seg scaled to stream km
#
#Condition Weight#
adjwgtUTsegSAMP=adjwgt(sitesUTsamp,wgtUTseg,wtcatUT,framesizeUTseg)
adjwgtUTstrSAMP=adjwgt(sitesUTsamp,wgtUTstr,wtcatUT,framesizeUTstr)
#
#Join results to the main table 
relevantUT=cbind(relevantUT,adjwgtUTseg)
relevantUT=cbind(relevantUT,adjwgtUTstr)
relevantUT=cbind(relevantUT,adjwgtUTallseg)
relevantUT=cbind(relevantUT,adjwgtUTallstr)
relevantUT=cbind(relevantUT,adjwgtUTsegSAMP)
relevantUT=cbind(relevantUT,adjwgtUTstrSAMP)
SiteInfo=merge(SiteInfo,subset(relevantUT,
                              select=c('Sitecode','adjwgtUTseg','adjwgtUTstr','adjwgtUTallseg','adjwgtUTallstr','adjwgtUTsegSAMP','adjwgtUTstrSAMP')),
                              all.x=T,
                              by="Sitecode") 
Weight_Extent='adjwgtUTstr'#final adjusted weight to use
SiteInfo$adjwgtEXT= SiteInfo[,names(SiteInfo)==Weight_Extent]
Weight_Condition=Weight_Extent#'adjwgtUTsegSAMP' #Temporary! wait until recieve feedback on extent weights, then do the same weighting type for condition
#TOlson 11/5: Weights should be based on all evaluated sites and that is the only set of weights. Want the weights to sum to the total stream length in the sample frame. This enables one to estimate the target population of stream length.
SiteInfo$adjwgtCON= SiteInfo[,names(SiteInfo)==Weight_Condition]

                                  
#Ator Example
Ator=read.csv(sprintf('%sAtor_Table2.csv',WeightsPath))
sites=NA;wgt=NA;cat=NA;sitesSamp=NA
for (a in 1:nrow(Ator)) {
  wgt=append(wgt,rep(Ator$OriginalWeight[[a]],Ator$TotalRecon[[a]]))
  cat=append(cat,rep(sprintf('%s.%s',Ator$Subregion[[a]],Ator$Category[[a]]),Ator$TotalRecon[[a]]))
  if (Ator$TotalRecon[[a]]==Ator$TrueCnt[[a]]){
  sites=append(sites,rep("TRUE",Ator$TotalRecon[[a]]))}
  else {sites=append(sites,rep("TRUE",Ator$TrueCnt[[a]]))
        sites=append(sites,rep("FALSE",(Ator$TotalRecon[[a]]-Ator$TrueCnt[[a]])))
        }
  if (Ator$TotalRecon[[a]]==Ator$SampleCnt[[a]]){
  sitesSamp=append(sitesSamp,rep("TRUE",Ator$TotalRecon[[a]]))}
  else {sitesSamp=append(sitesSamp,rep("TRUE",Ator$SampleCnt[[a]]))
        sitesSamp=append(sitesSamp,rep("FALSE",(Ator$TotalRecon[[a]]-Ator$SampleCnt[[a]])))
        }}
sitesA=data.frame(sites,wgt,cat,sitesSamp)
sitesA$sitesRecon=TRUE
sitesA=sitesA[2:nrow(sitesA),]
sitesA$sites=as.logical(sitesA$sites);sitesA$sitesSamp=as.logical(sitesA$sitesSamp)
rm(sites,wgt,cat)
CatPop=Ator$Population; names(CatPop)=sprintf('%s.%s',Ator$Subregion,Ator$Category)
sitesA$cat2=substr(sitesA$cat,1,1)
StrataPop=aggregate(Population~Subregion,data=Ator,FUN=sum)
StrataPop=StrataPop$Population;names(StrataPop)=seq(1,7)
sitesA$cat3='All'
TotalPop=sum(Ator$Population);names(TotalPop)='All'
adjwgtATORcat=adjwgt(sitesA$sites,sitesA$wgt,sitesA$cat,CatPop)#higher than Ator recon result
adjwgtATORstrat=adjwgt(sitesA$sites,sitesA$wgt,sitesA$cat2,StrataPop)#higher than Ator recon result #this is also what the paper seems to suggest in the equations
adjwgtATORstratRecon=adjwgt(sitesA$sitesRecon,sitesA$wgt,sitesA$cat2,StrataPop)#matches Ator (all Recon sites=True, assumes no Non-Target), despite Table 3
#?how come this did not incorporate non-target sites? this suggests that the recon weight is simply a re-weighting based on the candidate sites provided by the original GRTS design + any additional sites (i.e. the extra sites for subregion 1, category 4)
adjwgtATORstratSamp=adjwgt(sitesA$sitesSamp,sitesA$wgt,sitesA$cat2,StrataPop)#higher than Ator sample result, presumably because of how recon weight factored in
#?should we apply additional recon weight as in Ator?
adjwgtATORall=adjwgt(sitesA$sites,sitesA$wgt,sitesA$cat3,TotalPop)#higher than Ator recon result (closest)
#Indiana Example
#!would need original design info (framesize)
"""