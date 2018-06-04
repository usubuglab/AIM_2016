# need to deal with Xparameters and run all parameters through metadata table to make sure valid


#tblreach
assessment=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblreach\\tblASSESSMENT_RDec 09 2016.csv')
meltassessment=melt(assessment,id=c("UID","SAMPLE_TYPE"),na.rm=TRUE)
meltassessmentsub=meltassessment[281:638,]

fieldchem=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblreach\\tblFIELD_RDec 09 2016.csv')
meltfieldchem=melt(fieldchem,id=c("UID","SAMPLE_TYPE"),na.rm=TRUE)
meltfieldchemsub=meltfieldchem[536:1367,]

labresults=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblreach\\prepdata for import\\PTL.csv')

samples=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblreach\\tblSAMPLES_RDec 09 2016.csv')
samples=melt(samples,id=c("UID","SAMPLE_TYPE"),na.rm=TRUE)
samplessub=subset(samples,SAMPLE_TYPE=='BERW'|SAMPLE_TYPE=='CHEM')
samplessub=samplessub[546:926,]
samplessub=subset(samplessub,value!='')

tblreach=setNames(rbind(meltassessmentsub,meltfieldchemsub,labresults,samplessub),c("UID","SAMPLE_TYPE","PARAMETER","RESULT"))
tblreach=subset(tblreach,!(PARAMETER %in% c('DO','DO_CALIBRATION_VALUE','DO_CALIBRATION_UNITS','DO_DISPLAYED_VALUE','PH_QCS_TRUE','TEMP_THERMOMETER')))
write.csv(tblreach,'tblreach.csv')

# DO=unique(tblreach[grep('^DO',tblreach$PARAMETER),3])
# tblreach=subset(tblreach,!(PARAMETER %in% DO))


#tblverification
verification=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblverification\\tblVERIFICATION_RDec 09 2016.csv')
meltverification=melt(verification,id=c("UID","SAMPLE_TYPE"),na.rm=TRUE)
meltverificationsub=meltverification[70:1463,]
meltverificationsub=subset(meltverificationsub,value!='')

crew=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblverification\\tblCREW_RDec 09 2016.csv')
meltcrew=melt(crew,id=c("UID","SAMPLE_TYPE"),na.rm=TRUE)
meltcrewsub=meltcrew[277:730,]
meltcrewsub=subset(meltcrewsub,value!='')

chancross1=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblpoint\\tblCHANCROSSSEC_RDec 09 2016.csv')
chancross1=subset(chancross1,TRANSDIR=='ALL')
meltchancross1=melt(chancross1,id=c("UID","SAMPLE_TYPE","TRANSECT"),na.rm=TRUE)
meltchancross1=meltchancross1[3247:3786,]# need to subset further to get just one value per site
meltchancross1=subset(meltchancross1,TRANSECT=='A')
meltchancross1=meltchancross1[,c(1,2,4,5)]

tblverification=setNames(rbind(meltverificationsub,meltcrewsub,meltchancross1),c("UID","SAMPLE_TYPE","PARAMETER","RESULT"))
tblverification=subset(tblverification, PARAMETER!='SIG_VER' & PARAMETER!='SKETCH' & PARAMETER!='FISH_TAXONOMIST' & PARAMETER!='NAME4' & PARAMETER!='NAME5')
write.csv(tblverification,'tblverification.csv')

#tbltransect
channel=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tbltransect\\tblCHANNEL_Dec 09 2016.csv')
meltchannel=melt(channel,id=c("UID","SAMPLE_TYPE","TRANSECT"),na.rm=TRUE)
meltchannelsub=meltchannel[8871:30549,]
meltchannelsub=subset(meltchannelsub,value!='')
unique=unique(meltchannelsub$variable)
unique
#slope is an issue- clinometer per transect and need to take a weighted average at a transect; can't replicate EPA slope values
slope=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tbltransect\\tblSLOPE_RDec 09 2016.csv')
slope=subset(slope,is.na(SLOPE)==FALSE)
slope$SLOPE=slope$SLOPE*slope$PROP/100
sumslope=aggregate (SLOPE~UID+TRANSECT,data=slope, FUN=sum)
sumslope=aggregate(SLOPE~UID, data=sumslope, FUN=mean)
meltsumslope=melt(sumslope,id=c("UID"),na.rm=TRUE)
meltsumslope$SAMPLE_TYPE=rep("SLOPEW",32)
meltsumslope$PARAMETER=rep("PCT_GRADE",32)
meltsumslope=meltsumslope[,c(1,4,5,3)]
write.csv(meltsumslope,'slope.csv')

tbltransect=meltchannelsub
write.csv(tbltransect,'tbltransect.csv')

#tblpoint
chancross=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblpoint\\tblCHANCROSSSEC_RDec 09 2016.csv')
chancross=subset(chancross,TRANSDIR!='ALL')
meltchancross=melt(chancross,id=c("UID","SAMPLE_TYPE","TRANSECT","TRANSDIR"),na.rm=TRUE)
meltchancrosssub=meltchancross[18288:30078,]
meltchancrosssub=setNames(meltchancrosssub,c("UID","SAMPLE_TYPE","TRANSECT","POINT","PARAMETER","RESULT"))

chanrip=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblpoint\\tblCHANRIP_RDec 09 2016.csv')
meltchanrip=melt(chanrip,id=c("UID","SAMPLE_TYPE","TRANSECT","BANK"),na.rm=TRUE)
meltchanripsub=meltchanrip[36586:66880,]
meltchanripsub=setNames(meltchanripsub,c("UID","SAMPLE_TYPE","TRANSECT","POINT","PARAMETER","RESULT"))
  
thalweg=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblpoint\\tblTHALWEG_RDec 09 2016.csv')
thalweg=thalweg[,c(2,7:11,14,16,17)]
meltthalweg=melt(thalweg,id=c("UID","SAMPLE_TYPE","TRANSECT","STATION"),na.rm=TRUE) #wetwidth needs removed # what is reachlength? 
meltthalweg=subset(meltthalweg,value!='')
meltthalweg=setNames(meltthalweg,c("UID","SAMPLE_TYPE","TRANSECT","POINT","PARAMETER","RESULT"))

wetwidth=read.csv('Z:\\buglab\\Research Projects\\AIM\\Raw_Data\\NPRA data import\\tblpoint\\tblTHALWEG_RDec 09 2016.csv')
wetwidth=wetwidth[,c(2,7:9,12,18)]
meltwetwidth=melt(wetwidth,id=c("UID","SAMPLE_TYPE","TRANSECT","STATION"),na.rm=TRUE)
meltwetwidth=subset(meltwetwidth,STATION!='0')
meltwetwidth=setNames(meltwetwidth, c("UID","SAMPLE_TYPE","TRANSECT","POINT","PARAMETER","RESULT"))

tblpoint=rbind(meltchancrosssub,meltchanripsub,meltthalweg,meltwetwidth)
write.csv(tblpoint,'tblpoint.csv')
