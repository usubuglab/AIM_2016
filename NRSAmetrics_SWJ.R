#install.packages("M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\aquamet_1.0.zip")

# install.packages(c('Hmisc','foreach','reshape'))#dependent packages
library(aquamet)#need R 2... version, not 3.0

sink("NRSAmetrics.txt")
cat("NRSA 2008-2009 Physical Habitat Metrics\n")

setwd("M:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\AquametTEST")

#SWJ to do: at the end, consume the metrics outputs back in and pivot them for readability and prepare filtering them for figure processing

#WRSA data conversion
#assumes DB connection remains open

XwalkUnion=sqlQuery(wrsa1314,
"
select  XwalkTBL.Table_Xwalk, UID, SAMPLE_TYPE=case when XwalkTBL.Type_Xwalk='' then XwalkTBL.Type2 else XwalkTBL.Type_Xwalk end, TRANSECT, POINT,XwalkTBL.Parameter_Xwalk as PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
from (
  select  UID, SAMPLE_TYPE, TRANSECT, POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
  from tblPOINT
  union
  select   UID, SAMPLE_TYPE, TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON  
  from tbltransect
  union
  select   UID, SAMPLE_TYPE, cast(Null as nvarchar(5)) TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
  from tblreach
  union
  select UID, SAMPLE_TYPE, cast(Null as nvarchar(5)) TRANSECT, cast(Null as nvarchar(5)) POINT,PARAMETER,RESULT,FLAG,IND,ACTIVE,OPERATION,INSERTION,DEPRECATION,REASON 
  from tblverification
) UnionTBL
JOIN (select *, left(SAMPLE_TYPE,len(SAMPLE_TYPE)-1) as Type2 from tblXWALK where Name_XWALK='Aquamet1') XwalkTBL on UnionTBL.PARAMETER= XwalkTBL.PARAMETER and UnionTBL.SAMPLE_TYPE= XwalkTBL.Type2
where ACTIVE='TRUE'
")#SWJ to do: clean redundancy in this query up (i.e. the UID... string is repeated), also make Name_XWALK dynamic
#SWJ to do: could do a similar case statment for parameter and only store non-matching in tblXWALK (though it is nice to see verified matches)


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
visits$PROTOCOL=visits$VALXSITE
thalweg$UNITS=ifelse(thalweg$PARAMETER=='DEPTH',"CM","")#can only be assumed to be cm for wadeable, the boatable protocol allows FT or CM
bankgeometry$UNITS=""
#for SlopeBearing (also used in residual pool)
thalweg$STATION=0#Station and Transect were given for old slope method which seems odd, just trying to push the function to work
thalweg$UNITS.SLOPE=ifelse(thalweg$PARAMETER=='SLOPE',"CM","NONE")#can only be assumed to be cm for wadeable sites for which unique unit values were checked
visits$UNITS='CM'
channelgeometry$UNITS=ifelse(channelgeometry$PARAMETER=='SLOPE'|channelgeometry$PARAMETER=='INCREMNT',"CM","NONE")#SWJ - is increment the actual slope values?
channelgeometry$LINE=NA #appears to be a dummy variable populated by ifelse statements within the function

cat("\n\ngpsBasedCalculations_asOf201203008:\n\n")
gisCalcs <- read.csv("gpsBasedCalculations_asOf201203008.csv")##SWJ: need to know how to generate this
head(gisCalcs)

cat("\n\nBank Morphology:\n\n")
BankMorphology <- metsBankMorphology(bankgeometry, visits)
print(head(BankMorphology))
write.csv(BankMorphology, "metsBankMorphology.csv")

cat("\n\nBed Stability:\n\n")
BedStability <- metsBedStability(bankgeometry, thalweg, visits, channelgeometry,
   channelcrosssection, littoral, wood, fishcover, gisCalcs)
print(head(BedStability))
write.csv(BedStability, "metsBedStability.csv")

cat("\n\nCanopy Densiometer:\n\n")#done SWJ
CanopyDensiometer <- metsCanopyDensiometer(channelcover)
print(head(CanopyDensiometer))
write.csv(CanopyDensiometer, "metsCanopyDensiometer.csv")

cat("\n\nChannel Characteristic:\n\n")
ChannelChar <- metsChannelChar(bankgeometry, channelchar)
print(head(ChannelChar))
write.csv(ChannelChar, "metsChannelChar.csv")

cat("\n\nChannel Habitat:\n\n")
ChannelHabitat <- metsChannelHabitat(thalweg)
print(head(ChannelHabitat))
write.csv(ChannelHabitat, "metsChannelHabitat.csv")

cat("\n\nChannel Morphology:\n\n") #in Progress SWJ - major format issues, awaiting input from Curt - needs particular attention when boatable added (step through source code to verify use of multiple DEPTHS and WETWID)
ChannelMorphology <- metsChannelMorphology(bankgeometry, thalweg, visits)
print(head(ChannelMorphology))
write.csv(ChannelMorphology, "metsChannelMorphology.csv")

cat("\n\nFish Cover:\n\n")#done SWJ
FishCover <- metsFishCover(fishcover, visits)
print(head(FishCover))
write.csv(FishCover, "metsFishCover.csv")

cat("\n\nGeneral:\n\n")#SWJ in progress - hard to tell if lots of variables are missing for channelgeometry or which variables belong to thalweg or channelgeometry in the old structure or if the protocols are very distinct btwn wade and boat
#had to manually run the source code for metsGeneral() and run metsGeneral.1() without the shell function in order to eliminate the line "actransp <- subset(actransp, PARAMETER %in% c("ACTRANSP", "DISTANCE"))" because otherwise that would drop "SIDCHN" - not sure if this metric is only intended for the boatable protocol which has actransp
General <- metsGeneral(thalweg, channelgeometry)
print(head(General))
write.csv(General, "metsGeneral.csv")

cat("\n\nHuman Influence:\n\n")#done SWJ
HumanInfluence <- metsHumanInfluence(visrip)
print(head(HumanInfluence))
write.csv(HumanInfluence, "metsHumanInfluence.csv")

cat("\n\nInvasive Species:\n\n")#not relevant to NAM
InvasiveSpecies <- metsInvasiveSpecies(invasivelegacy)
print(head(InvasiveSpecies))
write.csv(InvasiveSpecies, "metsInvasiveSpecies.csv")

cat("\n\nLarge Woody Debris:\n\n")
LargeWoody <- metsLargeWoody(thalweg, channelgeometry, bankgeometry, wood,
   visits)
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

cat("\n\nResidual Pools:\n\n")#SWJ in progress - stuck on "LINE" column which is no longer in NRSA - need to determine what happens with null vs. actual values for gisCalcs (slope and reachlenght)
ResidualPools <- metsResidualPools(thalweg, channelgeometry, visits, gisCalcs)
print(head(ResidualPools))
write.csv(ResidualPools, "metsResidualPools.csv")

cat("\n\nRiparian Vegetation:\n\n")#done SWJ
RiparianVegetation <- metsRiparianVegetation(visrip)
print(head(RiparianVegetation))
write.csv(RiparianVegetation, "metsRiparianVegetation.csv")

cat("\n\nSlope and Bearing:\n\n")#SWJ in progress- in conjuction with Residual pools
SlopeBearing <- metsSlopeBearing(thalweg, channelgeometry, visits)#, gisCalcs) #gisCalcs allows null
print(head(SlopeBearing))
write.csv(SlopeBearing, "metsSlopeBearing.csv")

cat("\n\nSubstrate Characterization:\n\n") #done SWJ (for wadeable, boatable not yet verified bc no example data)
SubstrateCharacterization <- metsSubstrateCharacterization(channelcrosssection,
   thalweg, littoral)
print(head(SubstrateCharacterization))
write.csv(SubstrateCharacterization, "metsSubstrateCharacterization.csv")

cat("\n\nSubstrate Embeddedness:\n\n")#SWJ in progress - seems to have run, but had no final results
SubstrateEmbed <- metsSubstrateEmbed(channelcrosssection)#SWJ: Input = Embeddedness
print(head(SubstrateEmbed))
write.csv(SubstrateEmbed, "metsSubstrateEmbed.csv")

sink()
