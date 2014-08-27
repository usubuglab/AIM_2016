#install.packages("M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\aquamet_1.0.zip")

# install.packages(c('Hmisc','foreach','reshape'))#dependent packages
library(aquamet)#need R 2... version, not 3.0

sink("NRSAmetrics.txt")
cat("NRSA 2008-2009 Physical Habitat Metrics\n")

wd="M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\AquametTEST"
setwd(wd)

#SWJ to do: at the end, consume the metrics outputs back in and pivot them for readability and prepare filtering them for figure processing

#WRSA data conversion
#assumes DB connection remains open
tblx=Xwalk(XwalkName='Aquamet1',source='SQL',XwalkDirection='')#Norcal inputs:,Years=c('2013','2014'),Projects='NorCal')

#!parameters that will need to be modified from 2014 protocol changes: size_cls, angle, width/height units

files <- c("tblBANKGEOMETRY2", "tblCHANCOV2", "tblCHANDEPTH2",##SWJ: none of these tables are in the 2013 output, can cross walk if given corresponding tables
           "tblCHANNELCHAR2", "tblCHANNELCROSSSECTION2", "tblCHANNELGEOMETRY2",##SWJ: no Channel Geometry
           "tblFISHCOVER2", "tblINVASIVELEGACY2", "tblLITTORAL2",##SWJ: no Littoral table (might be boatable only)
           "tblTHALWEG2", "tblVISITS2", "tblVISRIP2", "tblWOOD2")##SWJ: no Wood table anymore...in Channel

#filesout=unique(XwalkUnion$Table_Xwalk)#use existing files list which will then make dummy files for all anticipated files
for (f in 1:length(files)){
  tblout=subset(XwalkUnion,subset=Table_Xwalk==files[f])
  write.csv(tblout[-1],file=sprintf('%s.csv', files[f]))
}
#SWJ to do: could bypass file export and go straight to files variable as established by EPA, need to change all metrics inputs which call csv
#SWJ to do: test all functions for boatable (not yet verified bc no example data)

tables <- c("bankgeometry", "channelcover", "chandepth", "channelchar",
            "channelcrosssection", "channelgeometry", "fishcover",
            "invasivelegacy", "littoral", "thalweg", "visits", "visrip", "wood")

for(i in 1:length(tables)) {
   cat("\n\n", tables[i], ":\n\n", sep="")
   eval(parse(text=paste(tables[i], " <- read.csv('", files[i], ".csv', row.names=1)", sep="")))
   eval(parse(text=paste("print(head(", tables[i], "))", sep="")))
}

#slight tweaks
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
UIDhold=data.frame(UID=substr(TRANhold,1,5),SAMPLE_TYPE='SLOPEW',TRANSECT=substr(TRANhold,7,8),POINT=0,PARAMETER='BEARING',RESULT=0,FLAG=NA,IND=NA,ACTIVE=NA,OPERATION=NA,INSERTION=NA,DEPRECATION=NA,REASON=NA)
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
gisCalcs <- read.csv("gpsBasedCalculations_asOf201203008.csv")##SWJ: need to know how to generate this
head(gisCalcs)
#not provided with example data set
#Ryan lokteff adapted the EPA script M:\GIS\GIS_Stats\Slope\Python\Slope_Endpoints1.py

cat("\n\nBank Morphology:\n\n")#DONE SWJ
BankMorphology <- metsBankMorphology(bankgeometry, visits)
print(head(BankMorphology))
write.csv(BankMorphology, "metsBankMorphology.csv")


cat("\n\nBed Stability:\n\n")
BedStability <- metsBedStability(bankgeometry, thalweg, visits, channelgeometry,   channelcrosssection, littoral, wood, fishcover)#, gisCalcs)
print(head(BedStability))
write.csv(BedStability, "metsBedStability.csv")

cat("\n\nCanopy Densiometer:\n\n")#done SWJ
CanopyDensiometer <- metsCanopyDensiometer(channelcover)
print(head(CanopyDensiometer))
write.csv(CanopyDensiometer, "metsCanopyDensiometer.csv")

cat("\n\nChannel Characteristic:\n\n")##SWJ in progress (DONE for wadeable) - channelchar parameters and bankgeo (boatable) parameters matched but not run for boatable - bankgeo only seems to be incorporated if boatable, otherwise only populated by channel constraint; WRSA/NRSA tables that boatable bankgeo data will be provided in is unknown (recorded as "UNK" in tblXWalk)
ChannelChar <- metsChannelChar(bankgeometry, channelchar)
print(head(ChannelChar))
write.csv(ChannelChar, "metsChannelChar.csv")


cat("\n\nChannel Habitat:\n\n")
ChannelHabitat <- metsChannelHabitat(thalweg)
print(head(ChannelHabitat))
write.csv(ChannelHabitat, "metsChannelHabitat.csv")

cat("\n\nChannel Morphology:\n\n") #in Progress SWJ #works with example data# - major format issues, awaiting input from Curt - needs particular attention when boatable added (step through source code to verify use of multiple DEPTHS and WETWID)
ChannelMorphology <- metsChannelMorphology(bankgeometry, thalweg, visits)
print(head(ChannelMorphology))
write.csv(ChannelMorphology, "metsChannelMorphology.csv")

cat("\n\nFish Cover:\n\n")#done SWJ
FishCover <- metsFishCover(fishcover, visits)
print(head(FishCover))
write.csv(FishCover, "metsFishCover.csv")

cat("\n\nGeneral:\n\n")#DONE SWJ
General <- metsGeneral(thalweg, channelgeometry)
print(head(General))
write.csv(General, "metsGeneral.csv")

cat("\n\nHuman Influence:\n\n")#done SWJ
HumanInfluence <- metsHumanInfluence(visrip)
print(head(HumanInfluence))
write.csv(HumanInfluence, "metsHumanInfluence.csv")

cat("\n\nInvasive Species:\n\n")#not relevant to NAMC
InvasiveSpecies <- metsInvasiveSpecies(invasivelegacy)
print(head(InvasiveSpecies))
write.csv(InvasiveSpecies, "metsInvasiveSpecies.csv")

cat("\n\nLarge Woody Debris:\n\n") #works with example data set once Visits fixed
LargeWoody <- metsLargeWoody(thalweg, channelgeometry, bankgeometry, wood, visits)
print(head(LargeWoody))
write.csv(LargeWoody, "metsLargeWoody.csv")


cat("\n\nLegacy Riparian Trees:\n\n")#not relevant to NAMC
LegacyTree <- metsLegacyTree(invasivelegacy)
print(head(LegacyTree))
write.csv(LegacyTree, "metsLegacyTree.csv")

cat("\n\nLittoral Depth:\n\n")#pending boatable data
LittoralDepth <- metsLittoralDepth(chandepth)
print(head(LittoralDepth))
write.csv(LittoralDepth, "metsLittoralDepth.csv")

cat("\n\nResidual Pools:\n\n")#DONE SWJ (wadeable only)
ResidualPools <- metsResidualPools(thalweg, channelgeometry, visits)#, gisCalcs)
print(head(ResidualPools))
write.csv(ResidualPools, "metsResidualPools.csv")

cat("\n\nRiparian Vegetation:\n\n")#done SWJ
RiparianVegetation <- metsRiparianVegetation(visrip)
print(head(RiparianVegetation))
write.csv(RiparianVegetation, "metsRiparianVegetation.csv")

cat("\n\nSlope and Bearing:\n\n")#SWJ in progress- in conjuction with Residual pools #works with example data set once Visits fixed# (##the additional message seems to have been corrected by adding the column VALXSITE to visits based on records present in Littoral to demarcate Boatable)
SlopeBearing <- metsSlopeBearing(thalweg, channelgeometry, visits)#, gisCalcs) #gisCalcs allows null
print(head(SlopeBearing))
write.csv(SlopeBearing, "metsSlopeBearing.csv")
# not working without bearing
#can force with 0 bearings does not change metrics except xbearing and sinu --> make sure these aren't consumed in other functions that use channelgeometry
#bearings isn't affecting any other outputs (residual pools, general, bed stability)


cat("\n\nSubstrate Characterization:\n\n") #done SWJ (for wadeable, boatable not yet verified bc no example data)
SubstrateCharacterization <- metsSubstrateCharacterization(channelcrosssection,   thalweg, littoral)
print(head(SubstrateCharacterization))
write.csv(SubstrateCharacterization, "metsSubstrateCharacterization.csv")

cat("\n\nSubstrate Embeddedness:\n\n")#done SWJ 
SubstrateEmbed <- metsSubstrateEmbed(channelcrosssection)#SWJ: Input = Embeddedness
print(head(SubstrateEmbed))
write.csv(SubstrateEmbed, "metsSubstrateEmbed.csv")

sink()


#Bring in aquamet results to do second tier calcs
METfiles=list.files(wd, pattern='mets*.')
for(i in 1:length(METfiles)) {
  cat("\n\n", METfiles[i], ":\n\n", sep="")
  eval(parse(text=paste(METfiles[i], " <- read.csv('", METfiles[i], "', row.names=1)", sep="")))
  eval(parse(text=paste("print(head(", METfiles[i], "))", sep="")))
  if(i==1){METmaster=eval(parse(text=METfiles[i]))} else {METmaster=rbind(METmaster,eval(parse(text=METfiles[i])))}
}

METmaster$RESULT=as.numeric(METmaster$RESULT)
unique(METmaster$METRIC)
#variables to omit: xbearing, sinu  (bc bearing not collected)

##QR1
QRmets=subset(METmaster, subset=METRIC %in% c('w1_hall','xcmgw','xcdenbk')) #pivot or merge? if the metrics get stored in the database (with a timestamp), then switch to pivot, but merge for now
QRmets=cast(QRmets, UID ~ METRIC, value='RESULT', fun.aggregate=mean)#fun.aggregate=count #count to make sure only 1 record per pivot cell
QRmets$QRDIST1=1/ (1+QRmets$w1_hall)
QRmets$QRVeg2= 0.1 + (0.9*(QRmets$xcdenbk/100))
QRmets$QRVeg1=ifelse(QRmets$xcmgw<=2.00, .1+(.9 * (QRmets$xcmgw/2.00)),1)
QRmets$QR1=((QRVeg1)*(QRVeg2)*(QRDIST1))^0.333


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
LINCIS_H=subset(tblTRANSECT, subset= PARAMETER=="BANKHT"|PARAMETER=="INCISED")
trycast=cast(LINCIS_H, UID~PARAMETER, mean)#This gives the below error
#Using REASON as value column.  Use the value argument to cast to override this choice
trycast=cast(LINCIS_H, UID~PARAMETER, mean, value=RESULT)# Trying to tell it which column to take the mean of. Also gives an error. See below. Also tried with simgle and double quotes
#Error in cast(LINCIS_H, UID ~ PARAMETER, mean, value = RESULT) : object 'RESULT' not found
#Try trycast=cast(LINCIS_H, UID~PARAMETER, fun.aggregate='RESULT', mean)
