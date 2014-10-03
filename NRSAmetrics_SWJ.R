#install.packages("M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\aquamet_1.0.zip")
# install.packages(c('Hmisc','foreach','reshape'))#dependent packages
library(aquamet)#need R 2... version, not 3.0 (available on remote desktop)

sink("NRSAmetrics.txt")
cat("NRSA 2008-2009 Physical Habitat Metrics\n")

#SWJ to do: test all functions for boatable (not yet verified bc no example data)

readEXTERNAL='N'#set to 'Y' to read in EPA or other provided files, otherwise set to 'N' and it will query directly from WRSAdb.SQL based on defaults specified in "DataConsumption_WRSAdb.R"
writeEXTERNAL='N'#set to 'Y' to produce csv files of inputs, metrics, and 2nd tier calcs; useful if have to run in other version of R on remote desktop
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
XwalkUnion=tblRetrieve(Table='',Parameters='',ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)#XwalkUniontmp=XwalkUnion
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
#SWJ: consider rolling into Xwalk metadata
channelcover$TRANSDIR=channelcover$POINT # how to include this in the Xwalk table in SQL WRSAdb?
channelcrosssection$TRANSDIR=channelcrosssection$POINT
visrip$TRANSDIR=visrip$POINT
visits$VALXSITE=visits$RESULT[visits$PARAMETER=='VALXSITE']
#for running ChannelMorphology
bankgeometry$TRANSDIR=bankgeometry$POINT
bankgeometry$TRANSDIR=ifelse(is.na(bankgeometry$TRANSDIR),"NONE",bankgeometry$TRANSDIR)
#visits$PROTOCOL=visits$VALXSITE#what was this added for?
thalweg$UNITS=ifelse(thalweg$PARAMETER=='DEPTH',"CM","")#can only be assumed to be cm for wadeable, the boatable protocol allows FT or CM
bankgeometry$UNITS=ifelse(bankgeometry$PARAMETER=='ANGLE',"DEGRES","M")#need to sync this with tblMETADATA eventually

#for SlopeBearing (also used in residual pool)
thalweg$STATION=thalweg$POINT; thalweg$STATION=ifelse(thalweg$POINT=='ALL',0,thalweg$STATION)#slope bearing has a very specific filter for station 0, which is now "ALL" 
#thalweg$STATION=0#Station and Transect were given for old slope method which seems odd, just trying to push the function to work
thalweg$UNITS=ifelse(thalweg$PARAMETER=='BARWIDTH'|thalweg$PARAMETER=='INCREMNT'|thalweg$PARAMETER=='REACHLENGTH'|thalweg$PARAMETER=='WETWIDTH',"M",
                    ifelse(thalweg$PARAMETER=='DEPTH',"CM", "NONE"))#can only be assumed to be cm for wadeable sites for which unique unit values were checked
#visits$UNITS='CM'#what was this added for?
#add zero bearings to force slopeBearing function
TRANhold=unique(paste(as.character(channelgeometry$UID) , as.character(channelgeometry$TRANSECT)))
UIDhold=data.frame(UID=substr(TRANhold,1,5),SAMPLE_TYPE='SLOPEW',TRANSECT=substr(TRANhold,7,8),POINT=0,PARAMETER='BEARING',RESULT=0);UIDhold=ColCheck(UIDhold,colnames(channelgeometry))
channelgeometry=rbind(channelgeometry,UIDhold)
channelgeometry$UNITS=ifelse(channelgeometry$PARAMETER=='SLOPE',"CM",#can't assume, need to pivot from slope_units
                            # ifelse(channelgeometry$PARAMETER=='DISTANCE',"M",#this is to help push a boatable site through
                             ifelse(channelgeometry$PARAMETER=='PROP',"PERCENT","NONE"))
channelgeometry$LINE=NA;channelgeometry$TRANLINE='NONE'#channelgeometry$TRANLINE=ifelse(channelgeometry$PARAMETER=='DISTANCE',"MID",'NONE') #appears to be a dummy variable populated by ifelse statements within the function - used by boatable protocol perhaps?
channelgeometry$METHOD='TR'#need to get this from pivoting the method parameter (can't assume for boatable)
#remove extra columns (need to do for all tables, most critical for bankgeom)
bankgeometry=subset(bankgeometry,select=-c(IND, ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON))
bankgeometry=subset(bankgeometry,select=-c(POINT)) #remove point since it was redistributed to appropriate legacy columns


cat("\n\ngpsBasedCalculations_asOf201203008:\n\n")
metsoutgisCalcs <- read.csv("gpsBasedCalculations_asOf201203008.csv")##SWJ: need to know how to generate this, until understood, commented out of functions
head(gisCalcs)
#not provided with example data set
#Ryan lokteff adapted the EPA script M:\GIS\GIS_Stats\Slope\Python\Slope_Endpoints1.py


metsoutBankMorphology <- metsBankMorphology(bankgeometry, visits)#DONE SWJ

cat("\n\nBed Stability:\n\n")#!unresolved
metsoutBedStability <- metsBedStability(bankgeometry, thalweg, visits, channelgeometry,   channelcrosssection, littoral, wood, fishcover)#, gisCalcs)

metsoutCanopyDensiometer <- metsCanopyDensiometer(channelcover)#done SWJ

metsoutChannelChar <- metsChannelChar(bankgeometry, channelchar)##SWJ in progress (DONE for wadeable) - channelchar parameters and bankgeo (boatable) parameters matched but not run for boatable - bankgeo only seems to be incorporated if boatable, otherwise only populated by channel constraint; WRSA/NRSA tables that boatable bankgeo data will be provided in is unknown (recorded as "UNK" in tblXWalk)

cat("\n\nChannel Habitat:\n\n")#!unresolved
metsoutChannelHabitat <- metsChannelHabitat(thalweg)

cat("\n\nChannel Morphology:\n\n") #in Progress SWJ #works with example data# - major format issues, awaiting input from Curt - needs particular attention when boatable added (step through source code to verify use of multiple DEPTHS and WETWID)
metsoutChannelMorphology <- metsChannelMorphology(bankgeometry, thalweg, visits)

metsoutFishCover <- metsFishCover(fishcover, visits)#done SWJ

metsoutGeneral <- metsGeneral(thalweg, channelgeometry)#DONE SWJ

metsoutHumanInfluence <- metsHumanInfluence(visrip)#done SWJ

cat("\n\nInvasive Species:\n\n")#not relevant to NAMC
metsoutInvasiveSpecies <- metsInvasiveSpecies(invasivelegacy)

cat("\n\nLarge Woody Debris:\n\n") #!unresolved: should work with example data set once Visits fixed
metsoutLargeWoody <- metsLargeWoody(thalweg, channelgeometry, bankgeometry, wood, visits)

cat("\n\nLegacy Riparian Trees:\n\n")#not relevant to NAMC
metsoutLegacyTree <- metsLegacyTree(invasivelegacy)

cat("\n\nLittoral Depth:\n\n")#pending boatable data
metsoutLittoralDepth <- metsLittoralDepth(chandepth)

cat("\n\nResidual Pools:\n\n")# was working now crashing on slope bearing
metsoutResidualPools <- metsResidualPools(thalweg, channelgeometry, visits)#, gisCalcs)

metsoutRiparianVegetation <- metsRiparianVegetation(visrip)#done SWJ


cat("\n\nSlope and Bearing:\n\n")#SWJ in progress- in conjuction with Residual pools #works with example data set once Visits fixed# (##the additional message seems to have been corrected by adding the column VALXSITE to visits based on records present in Littoral to demarcate Boatable)
metsoutSlopeBearing <- metsSlopeBearing(thalweg, channelgeometry, visits)#, gisCalcs) #gisCalcs allows null
# not working without bearing
#can force with 0 bearings does not change metrics except xbearing and sinu --> make sure these aren't consumed in other functions that use channelgeometry
#bearings isn't affecting any other outputs (residual pools, general, bed stability)

metsoutSubstrateCharacterization <- metsSubstrateCharacterization(channelcrosssection,   thalweg, littoral) #done SWJ (for wadeable, boatable not yet verified bc no example data)

metsoutSubstrateEmbed <- metsSubstrateEmbed(channelcrosssection)#done SWJ 

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
  cat("\n\n", METfiles[i], ":\n\n", sep="")
  eval(parse(text=paste(METfiles[i], " <- read.csv('", METfiles[i], "', row.names=1)", sep="")))
  eval(parse(text=paste("print(head(", METfiles[i], "))", sep="")))
  if(i==1){METmaster=eval(parse(text=METfiles[i]))} else {METmaster=rbind(METmaster,eval(parse(text=METfiles[i])))}
}}

METmaster$RESULT=as.numeric(METmaster$RESULT)
unique(METmaster$METRIC)
#variables to omit: xbearing, sinu  (bc bearing not collected)
#write to csv as one mondo file, or don't even write out in the first place, but compile via rbinds after all functions called; use some kind of grep on ls

##QR1
QRmetTIER2=subset(METmaster, subset=METRIC %in% c('w1_hall','xcmgw','xcdenbk')) #pivot or merge? if the metrics get stored in the database (with a timestamp), then switch to pivot, but merge for now
QRmetTIER2=cast(QRmetTIER2, UID ~ METRIC, value='RESULT', fun.aggregate=mean)#fun.aggregate=count #count to make sure only 1 record per pivot cell
QRmetTIER2$QRDIST1=1/ (1+QRmetTIER2$w1_hall)
QRmetTIER2$QRVeg2= 0.1 + (0.9*(QRmetTIER2$xcdenbk/100))
QRmetTIER2$QRVeg1=ifelse(QRmetTIER2$xcmgw<=2.00, .1+(.9 * (QRmetTIER2$xcmgw/2.00)),1)
QRmetTIER2$QR1=((QRmetTIER2$QRVeg1)*(QRmetTIER2$QRVeg2)*(QRmetTIER2$QRDIST1))^0.333
QRmetTIER2$METRIC='QR1';QRmetTIER2$RESULT=QRmetTIER2$QR1#name and save the desired metric(s), copy table and name with metTIER2 if want to save multiple submetrics

##LINCIS_H

#combine all 2nd tier calcs and append to METmaster
outls=ls()[grep('metTIER2',ls())]
for (m in 1:length(outls)){
  TBLtmp=eval(parse(text=outls[m]))
  TBLtmp=ColCheck(TBLtmp,colnames(METmaster))#assumes metric(s) was named and saved in the METRIC-RESULT format at the end of each calc
  METmaster=rbind(METmaster,TBLtmp)
}
if(writeEXTERNAL=='Y'){write.csv(METmaster,file=sprintf('metsAquamet_%s.csv',Sys.Date()),row.names=FALSE)}


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
