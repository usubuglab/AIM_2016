#install.packages("M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\aquamet_1.0.zip")
# install.packages(c('Hmisc','foreach','reshape'))#dependent packages
library(aquamet)#need R 2... version, not 3.0 (available on remote desktop)

sink("NRSAmetrics.txt")
cat("NRSA 2008-2009 Physical Habitat Metrics\n")

#SWJ to do: test all functions for boatable (not yet verified bc no example data); for example, need to convert depth to pole/sonar
#! to troubleshoot use getAnywhere() to view raw code of function

#options(stringsAsFactors=T)#revert this option to default; aquamet handles factor conversions to character
readEXTERNAL='N'#set to 'Y' to read in EPA or other provided files (i.e. when using remote desktop), otherwise set to 'N' and it will query directly from WRSAdb.SQL based on defaults specified in "DataConsumption_WRSAdb.R"
writeEXTERNAL='Y'#set to 'Y' to produce csv files of inputs, metrics, and 2nd tier calcs; useful if have to run in other version of R on remote desktop
#wd="\\\\share2.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\AquametTEST\\Test2";setwd(wd)

files <- c("tblBANKGEOMETRY2", "tblCHANCOV2", "tblCHANDEPTH2",##SWJ: none of these tables are in the 2013 output, can cross walk if given corresponding tables
           "tblCHANNELCHAR2", "tblCHANNELCROSSSECTION2", "tblCHANNELGEOMETRY2",##SWJ: no Channel Geometry
           "tblFISHCOVER2", "tblINVASIVELEGACY2", "tblLITTORAL2",##SWJ: no Littoral table (might be boatable only)
           "tblTHALWEG2", "tblVISITS2", "tblVISRIP2", "tblWOOD2")##SWJ: no Wood table anymore...in Channel
tables <- c("bankgeometry", "channelcover", "chandepth", "channelchar",
            "channelcrosssection", "channelgeometry", "fishcover",
            "invasivelegacy", "littoral", "thalweg", "visits", "visrip", "wood")

if(readEXTERNAL=='N'){
#WRSA data conversion
#assumes DB connection remains open
#XwalkUnion=tblRetrieve(Table='',Parameters='',ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
XwalkUnion=tblRetrieve(Table='',Parameters='',Projects='AKEFO',Years='2015')
#XwalkUnion=tblRetrieve(Table='',Parameters='',Projects='WRSA', Protocols=c('NRSA13','WRSA14')) #ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)#XwalkUniontmp=XwalkUnion
#protocol and parameter conversions #! these conversions also need to be done when relaying back to EPA
XwalkUnion=bintranslate(Table='XwalkUnion',ST='CROSSSECW',PM='SIZE_NUM')
#!should undercut (PIBO>EPA) be translated? otherwise non-existent for 2014
#!should we randomly subset pebbles and embeddness to lower EPA amounts?
#!slope handling TBD#Hmm...need to check my thinking and reconcile with aquamet code, but with the EPA method, straight length (RUN) = reach length because slope was taken transect to transect (so distance between transects is standard and additive); with the PIBO method, straight length must be determined by BR and TR coordinates, because crews could skip over transects at will. This also makes slopes that end early (did not go all the way from A to K) more difficult to estimate. Not sure if that makes sense in typing, but my questions with slope are increasing rather than decreasing and though the field method was simpler, the data handling is more complex with this method. 
#convert angle
XwalkUnion$RESULT=ifelse(XwalkUnion$PARAMETER=='ANGLE180',180-as.numeric(XwalkUnion$RESULT),XwalkUnion$RESULT)
#change parameter names
XwalkUnion=Xwalk(XwalkName='Aquamet1',Table='XwalkUnion',Source='R',XwalkDirection='')#!need to formally omit unused parameters and track down unknowns to see how they are used in aquamet (i.e. Assessment, etc)
#collapse LWD
XwalkLWDsub=subset(XwalkUnion,PARAMETER %in% c('DXDSL','DSDSL','DMDSL','DLDSL','WXDSL','WSDSL','WMDSL','WLDSL'));XwalkLWDsub$RESULT=as.numeric(XwalkLWDsub$RESULT);
XwalkLWDagg=data.frame(cast(XwalkLWDsub,'UID+TRANSECT+POINT+TABLE+SAMPLE_TYPE+PARAMETER ~ .',value='RESULT',fun.aggregate=sum));XwalkLWDagg$RESULT=XwalkLWDagg$X.all.;XwalkLWDagg=ColCheck(XwalkLWDagg,colnames(XwalkUnion))#  228284433826712128 B
XwalkNOlwd=subset(XwalkUnion,(IND %in% XwalkLWDsub$IND)==FALSE)
XwalkUnion=rbind(XwalkLWDagg,XwalkNOlwd); rm(XwalkLWDsub); rm(XwalkLWDagg);rm(XwalkNOlwd)#XwalkUnionclean=XwalkUnion
}

for(i in 1:length(tables)) {
   cat("\n\n", tables[i], ":\n\n", sep="")
   if(readEXTERNAL=='Y'){TBLtmp=eval(parse(text=paste("read.csv('", files[i], ".csv')", sep="")))
   }  else {TBLtmp=subset(XwalkUnion,toupper(TABLE)==toupper(files[i]))
        if(writeEXTERNAL=='Y'){write.csv(TBLtmp,file=sprintf('%s.csv', files[i]),row.names=FALSE)}
      }
   assign(tables[i],TBLtmp)
   eval(parse(text=paste("print(head(", tables[i], "))", sep="")))
}

#slight structure tweaks
#!need to be careful with factor handling as in thalweg$TRANSECT and thalweg$STATION, especially in ifelse statements
#SWJ: consider rolling into Xwalk metadata
channelcover$TRANSDIR=as.character(channelcover$POINT) # how to include this in the Xwalk table in SQL WRSAdb?
channelcrosssection$TRANSDIR=as.character(channelcrosssection$POINT)
visrip$TRANSDIR=as.character(visrip$POINT)
visits=addKEYS(visits,c('SITE_ID','DATE_COL','VISIT_NO','VALXSITE','XSTATUS','LOC_NAME'));visits$SITE_CLASS="PROB"#write.csv(visits,"tblVISITS2.csv"); visits=read.csv("tblVISITS2.csv")
if('VALXSITE' %in% colnames(visits)==FALSE){visits$VALXSITE=visits$RESULT[visits$PARAMETER=='VALXSITE']}
visits$PROTOCOL=visits$VALXSITE#what was this added for?
#for running ChannelMorphology
bankgeometry$TRANSDIR=as.character(bankgeometry$POINT)
bankgeometry$TRANSDIR=ifelse(is.na(bankgeometry$TRANSDIR)|bankgeometry$TRANSDIR=='',"NONE",as.character(bankgeometry$TRANSDIR))
thalweg$UNITS=ifelse(thalweg$PARAMETER=='DEPTH',"CM","")#can only be assumed to be cm for wadeable, the boatable protocol allows FT or CM
bankgeometry$UNITS=ifelse(bankgeometry$PARAMETER=='ANGLE',"DEGREES","M")#need to sync this with tblMETADATA eventually

#for SlopeBearing (also used in residual pool)
thalweg$STATION=as.character(thalweg$POINT); thalweg$STATION=ifelse(thalweg$STATION=='ALL'|thalweg$STATION==''|is.na(thalweg$STATION),0,thalweg$STATION);thalweg$STATION=as.numeric(thalweg$STATION)#slope bearing has a very specific filter for station 0, which is now "ALL" 
#thalweg$STATION=0#Station and Transect were given for old slope method which seems odd, just trying to push the function to work
thalweg$UNITS=ifelse(thalweg$PARAMETER=='BARWIDTH'|thalweg$PARAMETER=='INCREMNT'|thalweg$PARAMETER=='REACHLENGTH'|thalweg$PARAMETER=='WETWIDTH',"M",
                    ifelse(thalweg$PARAMETER=='DEPTH',"CM", "NONE"))#can only be assumed to be cm for wadeable sites for which unique unit values were checked
thalweg$TRANSECT=as.factor(ifelse(thalweg$PARAMETER=='INCREMNT',"A",as.character(thalweg$TRANSECT)))
#visits$UNITS='CM'#what was this added for?
#add zero bearings to force slopeBearing function
TRANhold=unique(paste(as.character(channelgeometry$UID) , as.character(channelgeometry$TRANSECT)))
UIDhold=data.frame(UID=substr(TRANhold,1,nchar(TRANhold)-2),SAMPLE_TYPE='SLOPEW',TRANSECT=substr(TRANhold,nchar(TRANhold),nchar(TRANhold)),POINT='0',PARAMETER='BEARING',RESULT=0);UIDhold=ColCheck(UIDhold,colnames(channelgeometry))
channelgeometry=rbind(channelgeometry,UIDhold)
channelgeometry$TRANSECT=ifelse(is.na(as.numeric(channelgeometry$TRANSECT))==FALSE,as.character(channelgeometry$POINT),as.character(channelgeometry$TRANSECT))#!temporary fix for 2014 slopes, these need additional modification both for skipping transects and because someimtes the crew listed the starting transect as "landmark1", etc.
print('WARNING: Sites with questionable slope data are being omitted!')
channelgeometry=subset(channelgeometry,substr(UID,1,5) %in% c(11625,11626,16322,16444,17198,21013,22828,31349,45019,52508,53538,74173,74588,99063)==FALSE )#omit suspected problems for NorCal so subsequent metrics not produced
#add dummy boatable data to avoid crash in residual pools
ACTRANSPhold=UIDhold[1,];ACTRANSPhold$PARAMETER='ACTRANSP'
DISThold=UIDhold[1,];DISThold$PARAMETER='DISTANCE'
ACDThold=rbind(ACTRANSPhold,DISThold);ACDThold$RESULT='1';ACDThold$UID='123456789'
channelgeometry=rbind(channelgeometry,ACDThold);
#other channel geo modifications
channelgeometry$UNITS=ifelse(channelgeometry$PARAMETER=='SLOPE',"CM",#can't assume, need to pivot from slope_units
                            # ifelse(channelgeometry$PARAMETER=='DISTANCE',"M",#this is to help push a boatable site through
                             ifelse(channelgeometry$PARAMETER=='PROP',"PERCENT","NONE"))
channelgeometry$LINE=NA;channelgeometry$TRANLINE='NONE'#channelgeometry$TRANLINE=ifelse(channelgeometry$PARAMETER=='DISTANCE',"MID",'NONE') #appears to be a dummy variable populated by ifelse statements within the function - used by boatable protocol perhaps?
channelgeometry$METHOD='TR'#need to get this from pivoting the method parameter or addkeys(but not in VERIF) (can't assume for boatable)
channelgeometry$BANK='NONE'#get from point (can't assume for boatable)
#remove extra columns (need to do for all tables, most critical for bankgeom)>>use ColCheck
bankgeometry=subset(bankgeometry,select=-c(IND, ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON,TABLE))
thalweg=subset(thalweg,select=-c(IND, ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON,TABLE))
bankgeometry=subset(bankgeometry,select=-c(POINT)) #remove point since it was redistributed to appropriate legacy columns
thalweg=subset(thalweg,select=-c(POINT)) #remove point since it was redistributed to appropriate legacy column


cat("\n\ngpsBasedCalculations_asOf201203008:\n\n")
metsoutgisCalcs <- read.csv("gpsBasedCalculations_asOf201203008.csv")##SWJ: need to know how to generate this, until understood, commented out of functions
head(gisCalcs)
#not provided with example data set
#Ryan lokteff adapted the EPA script M:\GIS\GIS_Stats\Slope\Python\Slope_Endpoints1.py

cat("\n\nLittoral Depth:\n\n")#pending boatable data
metsoutLittoralDepth <- metsLittoralDepth(chandepth)

metsoutSlopeBearing <- metsSlopeBearing(thalweg, channelgeometry, visits)#, gisCalcs) #DONE, but be cautious with slopes #gisCalcs allows null # not working without bearing #can force with 0 bearings does not change metrics except xbearing and sinu --> make sure these aren't consumed in other functions that use channelgeometry #bearings isn't affecting any other outputs (residual pools, general, bed stability)

metsoutResidualPools <- metsResidualPools(thalweg, channelgeometry, visits)#, gisCalcs) #DONE, but be cautious with slopes

metsoutBedStability <- metsBedStability(bankgeometry, thalweg, visits, channelgeometry,   channelcrosssection, littoral, wood, fishcover)#DONE, but be cautious with slopes#, gisCalcs)

metsoutGeneral <- metsGeneral(thalweg, channelgeometry)#working, but odd values coming out for reachlength still >> trouble shoot thalweg$STATION (currently set to numeric); seems to be good >> next up:  cdData data subset driving the station count which results in 0 for many NRSA sites which don't include all the "No"s for side channel, etc. >> cdData <- subset(indat, PARAMETER %in% c("ACTRANSP", "DISTANCE", "INCREMNT", "SIDCHN", "OFF_CHAN", "REACHLENGTH"))
rlen=subset(metsoutGeneral,METRIC=='reachlen'); rlen$VALUEl=rlen$RESULT-(rlen$RESULT*.1); rlen$VALUEh=rlen$RESULT+(rlen$RESULT*.1)#10% bounds
thalCheck=tblRetrieve(Parameters='TRCHLEN',UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
thalCheck=merge(rlen,thalCheck,intersect(setdiff(colnames(metsoutGeneral),'RESULT'),colnames(thalCheck))),all.x=T)
thalCheck$check=ifelse(thalCheck$RESULT>thalCheck$VALUEl & thalCheck$RESULT<thalCheck$VALUEh,'OK','X');thalCheck=subset(thalCheck,check=='X')
if(nrow(thalCheck)>0){print("WARNING! Calculated reach length (METRIC=='reachlen') significantly different from field recorded Reach Length (PARAMETER=='TRCHLEN'). Confirm correct INCREMENT parameter.")}

metsoutLargeWoody <- metsLargeWoody(thalweg, channelgeometry, bankgeometry, wood, visits)#DONE, but concerned about reachlen value coming out of metsGeneral

metsoutBankMorphology <- metsBankMorphology(bankgeometry, visits)#DONE SWJ

metsoutCanopyDensiometer <- metsCanopyDensiometer(channelcover)#done SWJ

metsoutChannelChar <- metsChannelChar(bankgeometry, channelchar)##SWJ in progress (DONE for wadeable) - channelchar parameters and bankgeo (boatable) parameters matched but not run for boatable - bankgeo only seems to be incorporated if boatable, otherwise only populated by channel constraint; WRSA/NRSA tables that boatable bankgeo data will be provided in is unknown (recorded as "UNK" in tblXWalk)

metsoutChannelMorphology <- metsChannelMorphology(bankgeometry, thalweg, visits)# DONE SWJ for wadeable; needs particular attention when boatable and alternate slope methods added (BANK/METHOD;step through source code to verify use of multiple DEPTHS and WETWID)

metsoutFishCover <- metsFishCover(fishcover, visits)#done SWJ

metsoutHumanInfluence <- metsHumanInfluence(visrip)#done SWJ

metsoutRiparianVegetation <- metsRiparianVegetation(visrip)#done SWJ

metsoutSubstrateCharacterization <- metsSubstrateCharacterization(channelcrosssection,   thalweg, littoral) #done SWJ (for wadeable, boatable not yet verified bc no example data)

metsoutSubstrateEmbed <- metsSubstrateEmbed(channelcrosssection)#done SWJ 

metsoutChannelHabitat <- metsChannelHabitat(thalweg)#!unresolved, not relevant to NAMC since CHANUNCD not collected in 2014

metsoutInvasiveSpecies <- metsInvasiveSpecies(invasivelegacy)#not relevant to NAMC

metsoutLegacyTree <- metsLegacyTree(invasivelegacy)#not relevant to NAMC

sink()

#combine all metric files
outls=ls()[grep('metsout',ls())]
for (m in 1:length(outls)){
  TBLtmp=eval(parse(text=outls[m]))
  if(m==1){METmaster=TBLtmp[0,]}
  METmaster=rbind(METmaster,TBLtmp)
}
if(writeEXTERNAL=='Y'){write.csv(METmaster,file=sprintf('metsAquamet_%s.csv',Sys.Date()),row.names=FALSE)}

#Bring in aquamet results to do second tier calcs
if(readEXTERNAL=='Y'){
METfiles=list.files(wd, pattern='mets*.')
for(i in 1:length(METfiles)) {
  print(cat("\n\n", METfiles[i], ":\n\n", sep=""))
  assign('METtmp', read.csv(METfiles[i]))
  print(head(METtemp))
  if(i==1){METmaster=METtmp} else {METmaster=rbind(METmaster,METtmp)}
}}

METmaster$RESULT=as.numeric(METmaster$RESULT)
unique(METmaster$METRIC)
if(exists('indicators')){
  print("WARNING: The following core indicators (as recorded in the MissingBackend Xwalk) were not found in the AQUAMET output:")
  print(setdiff(toupper(indicators),toupper(unique(METmaster$METRIC))))
} else {print('Core indicators unknown.')}
#variables to omit: xbearing, sinu  (bc bearing not collected, values are dummy)
#write to csv as one mondo file, or don't even write out in the first place, but compile via rbinds after all functions called; use some kind of grep on ls


#QR1
QRmetTIER2=subset(METmaster, subset=METRIC %in% c('w1_hall','xcmgw','xcdenbk')) #pivot or merge? if the metrics get stored in the database (with a timestamp), then switch to pivot, but merge for now
QRmetTIER2=cast(QRmetTIER2, UID ~ METRIC, value='RESULT', fun.aggregate=mean)#fun.aggregate=count #count to make sure only 1 record per pivot cell
QRmetTIER2$QRDIST1=1/ (1+QRmetTIER2$w1_hall)
QRmetTIER2$QRVeg2= 0.1 + (0.9*(QRmetTIER2$xcdenbk/100))
QRmetTIER2$QRVeg1=ifelse(QRmetTIER2$xcmgw<=2.00, .1+(.9 * (QRmetTIER2$xcmgw/2.00)),1)
QRmetTIER2$QR1=((QRmetTIER2$QRVeg1)*(QRmetTIER2$QRVeg2)*(QRmetTIER2$QRDIST1))^0.333
QRmetTIER2$METRIC='QR1';QRmetTIER2$RESULT=QRmetTIER2$QR1#name and save the desired metric(s), copy table and name with metTIER2 if want to save multiple submetrics

#post-processing to do for remaining core indicators:
##LINCIS_H from xinc_h (log)
##LRBS from ?? (determine which variant)
##LDVRP100 from RP100 (log and ??)
##LBFWD_RAT from bfwd_rat (log)
##NTL, PTL, Conductivity (+ correction), Turbidity, pH, temperature from raw data
##CVDPTH from ?? (not in NRSA_documentation that came with aquamet package)
##concerned about reachlen used by C1WM100 (see notes for thalCheck variable above)
##custom (non-aquamet): OE, INVASIVES, POOLHABITAT, BANKSTAB, BLM_QR1 


#combine all 2nd tier calcs and append to METmaster
outls=ls()[grep('metTIER2',ls())]
for (m in 1:length(outls)){
  TBLtmp=eval(parse(text=outls[m]))
  TBLtmp=ColCheck(TBLtmp,colnames(METmaster))#assumes metric(s) was named and saved in the METRIC-RESULT format at the end of each calc
  METmaster=rbind(METmaster,TBLtmp)
}
if(writeEXTERNAL=='Y'){write.csv(METmaster,file=sprintf('metsAquamet_%s.csv',Sys.Date()),row.names=FALSE)}


#####JC pivot check 
pivot.METmaster=cast(METmaster, 'UID~METRIC',value='RESULT')


#xcmg check
tempMETS=subset(METmaster, subset=METRIC %in% c('xcmg','xcl','xcs','xmh','xgw','xgh','xmw')) 
tempMETS=cast(tempMETS,UID ~ METRIC, value='RESULT', fun.aggregate=mean)
tempMETS$xcmg2=tempMETS$xcl+tempMETS$xcs+tempMETS$xmw+tempMETS$xgh+tempMETS$xgw+tempMETS$xmh

#Nicole TO DO
#LRBS: Need to do some work here.  Put off until after 7 Feb.Need geometric mean substrate diameter, and several metrics that need to be looked into.

#LINCS
##Log(Average incision height-average bankfull height+0.1)
###First need to get the avg incision and bankful heights. 
####Subset the data in the tblTRANSECT table so that I only get information for INCISED and BANKHT
LINCIS_H_subset=subset(tblTRANSECT, subset= PARAMETER=="BANKHT"|PARAMETER=="INCISED", select=c(UID, PARAMETER,RESULT))
View(LINCIS_H_subset)
write.csv(LINCIS_H_subset, file="LINCIS_H_Process2.csv")
LINCIS_H_Process2 <- read.csv("C:/Users/Nicole/Desktop/TrialForGIT/WRSA/LINCIS_H_Process2.csv")
View(LINCIS_H_Process2)
LINCIS_H_Means=cast(LINCIS_H_Process2, UID~PARAMETER, mean)
LINCIS_H_Means
### Now I need to: log(XINC_H-XBKF_H+0.1) Maybe do one logged and one not? Why +0.1?? Where XINC_H is mean incision height and XBKF_H mean bankfull height
