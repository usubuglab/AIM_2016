#################################################################################################
#                                 Metadata                                                      #
#################################################################################################
###EPA aquamet code modified by Jennifer Courtwright
#Created on April 4, 2017

#successfully replicated this code by hand in excel following the methods on pg. 56(38) of Kaufmann et al. 1999
#excel example can be found at "Z:\buglab\Research Projects\AIM\Analysis\Indicator_Scoping\IndicatorMetaData\pool calc.xlsx"
#this was only replicated for a complete site without missing data
#notes on conversations with Phil about how to handle missing data can be found at "Z:\buglab\Research Projects\AIM\Analysis\Indicator_Scoping\Questions for phil 3-20-16 related to pools.docx"
#this file also contains correspondence with Curt about how aquamet handles missing data
#most importantly we identified a critical error in aquamet slope code which is used as an input into this residual pool code
#aquamet calculates slope using the equation % slope = 360/2*pi *tan(rise/run)... this equation makes no sense and is an error

#In general any depression in the bed is counted as a pool after correcting for the downward slope of the stream bed
#therefore, the EPAs metrics are a true measurement of water depths if the stream is not flowing
#This is usefull for bed hederogenity but is not useful biologically and so a depth reiquirement should be imposed for a biologically relevant metric of pools

#The following is taken from Kaufmann et al. 1999 and describes how residual depths and pools are determined
#the downstream riffle crest (control point) of each residual pool in the longitudinal
#thalweg depth profile is detected iteratively, by scanning the sequence of thalweg depths for
#a value that exceeds the previous (downstream) depth by an amount determined using
#Stack’s correction angle equation, as follows: 
#   depth(i) > depth(i-1) + {[0.12 + 0.25(slope)] × interval} (4)
#        where:
#         depth(i) is the measured thalweg depth at the ith point in the upstream
#             direction along the sampled reach. Depth(1), used for the first
#             measured depth(i = 1), is taken to be the thalweg depth mean
#             minus the thalweg depth standard deviation, to allow detection of
#             the first pool in those cases where a sampled reach begins in a pool;
#         'i' is the thalweg depth measurement index, starting at 1 and continuing
#             to the upstream end of the reach, generally either 100 or 150;
#         slope is the mean water surface slope of the reach, and
#         interval is the distance between consecutive measurement points.

#Once this riffle crest is detected, the residual pool continues upstream until it's
#calculated residual surface intersects the thalweg bed surface. This point is detected by
#scanning the sequence of thalweg depths until the following relation is true:
#    depth(e) # depth(b) + {[0.12 + 0.25(slope)] × [interval × (e - b)]}; (5)
#         where:
#             depth(b) is the water depth at the estimated downstream riffle crest defining this
#                  particular residual pool;
#             depth(e) is the measured thalweg depth at the estimated upstream end of the
#                  residual pool. It represents the point at which the residual surface intersects
#                  the streambed. This point may or may not be the downstream control point
#                  of the next upstream residual pool;
#             slope is the mean water surface slope of the reach;
#                  interval is the distance between consecutive measurement points;
#             b is the value of the index i at which the downstream starting point of the pool
#                  was detected using Equation 4;
#             e is the value of the index i at which this relation holds, marking the detected
#                  upstream end of the residual pool.

#The residual depth is defined as the difference between the measured thalweg depth
#and the calculated residual surface depth, as shown in Equation 6:
#        resd(i) = depth(i) - {depth(b) + [0.12 + 0.25(slope)][interval × (e - b)]} (6)
#           where:
#              resd(i) is the residual depth at the ith measurement point along the sample reach.

#Additional resources on this method are Robison and Kaufmann (1994) and Robison (1998) but the Kaufmann et al. 1999 paper was the most helpful for replicating aquamet code

#################################################################################################################
#                code to calculate residual depths starts here                                                #
#################################################################################################################

library(aquamet)#need R 2... version, not 3.0 (available on remote desktop)

#Get thalweg, channelgeometry, visits objects with raw thalweg data for input into function metsResidualPools
#copied from NRSA metrics_SWJ.R this can eventually be paired down to only include data needed for these functions
files <- c("tblBANKGEOMETRY2", "tblCHANCOV2", "tblCHANDEPTH2",##SWJ: none of these tables are in the 2013 output, can cross walk if given corresponding tables
           "tblCHANNELCHAR2", "tblCHANNELCROSSSECTION2", "tblCHANNELGEOMETRY2",##SWJ: no Channel Geometry
           "tblFISHCOVER2", "tblINVASIVELEGACY2", "tblLITTORAL2",##SWJ: no Littoral table (might be boatable only)
           "tblTHALWEG2", "tblVISITS2", "tblVISRIP2", "tblWOOD2")##SWJ: no Wood table anymore...in Channel
tables <- c("bankgeometry", "channelcover", "chandepth", "channelchar",
            "channelcrosssection", "channelgeometry", "fishcover",
            "invasivelegacy", "littoral", "thalweg", "visits", "visrip", "wood")
XwalkUnion=tblRetrieve(Table='',Parameters='',Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
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
for(i in 1:length(tables)) {
  cat("\n\n", tables[i], ":\n\n", sep="")
  if(readEXTERNAL=='Y'){TBLtmp=eval(parse(text=paste("read.csv('", files[i], ".csv')", sep="")))
  }  else {TBLtmp=subset(XwalkUnion,toupper(TABLE)==toupper(files[i]))
           if(writeEXTERNAL=='Y'){write.csv(TBLtmp,file=sprintf('%s.csv', files[i]),row.names=FALSE)}
  }
  assign(tables[i],TBLtmp)
  eval(parse(text=paste("print(head(", tables[i], "))", sep="")))
}
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


#Other inputs
#input into metsResidualPools
gisCalcs = NULL
#input into metsResidualPools.1
writeIntermediateFiles = TRUE
oldeMethods = FALSE
minSampPct = 50 #This is the % of thalweg data needed to calculate metrics. EPA sets this at 85% but Phil said 50% was ok to match rest of our data


#metsResidualPools
  cat("Residual Pools calculations:\n")
  intermediateMessage(".1 Convert factors to character variables.", 
                      loc = "end")
  thalweg <- convert_to_char(thalweg)
  channelgeometry <- convert_to_char(channelgeometry)
  visits <- convert_to_char(visits)
  if (!is.null(gisCalcs)) 
    gisCalcs <- convert_to_char(gisCalcs)
  intermediateMessage(".2 Subset the data frames.", loc = "end")
  thal <- subset(thalweg, PARAMETER %in% c("BARWIDTH", "BAR_PRES", 
                                           "CHANUNCD", "DEPTH", "DEP_POLE", "DEP_SONR", "INCREMNT", 
                                           "POOLFMCD", "REACHLENGTH", "SEDIMENT", "OFF_CHAN", "WETWIDTH"))
  actransp <- subset(channelgeometry, PARAMETER %in% c("ACTRANSP", 
                                                       "DISTANCE"))
  intermediateMessage(".3 Call the metsSlopeBearing function.", 
                      loc = "end")
  #read slopes in from an external file formatted with 3 columns(UID,METRIC(xslope),RESULT(calculated slope value in %))
  slopes=read.csv('poolcompareslope.csv')
  intermediateMessage(".4 Set protocols.", loc = "end")
  protocols <- siteProtocol(unique(thal$UID), visits)
  intermediateMessage(".5 Call function metsResidualPools.1.", 
                      loc = "end")
  mets <- metsResidualPools.1(thal, actransp, slopes, protocols)
  row.names(mets) <- 1:nrow(mets)
  intermediateMessage("Done.", loc = "end")
  return(mets)



#metsResidualPools.1
#function (thal, actransp, slopes, protocols, writeIntermediateFiles = FALSE, 
#          oldeMethods = FALSE) 

  thal <- subset(thal, TRANSECT %in% LETTERS)
  tt <- dfWiden(actransp, c("UID", "TRANSECT"), "PARAMETER", 
                "RESULT")
  tt$RESULT <- ifelse(is.na(tt$ACTRANSP), tt$DISTANCE, tt$ACTRANSP)
  tt$PARAMETER <- "ACTRANSP"
  tt$ACTRANSP <- NULL
  tt$DISTANCE <- NULL
  actransp <- tt
  thalSeries <- metsResidualPools.dataOrganization(thal, actransp, 
                                                   slopes)
  if (writeIntermediateFiles == TRUE) {
    write.csv(thalSeries, "intermediate_thalSeries.csv")
  }
  residualSeries <- metsResidualPools.dimensions(thalSeries, 
                                                 protocols, minSampPct = 85, oldeMethods = oldeMethods)
  if (writeIntermediateFiles == TRUE) {
    write.csv(residualSeries, "intermediate_residualSeries.csv")
  }
  poolSeries <- metsResidualPools.poolCharacteristics(residualSeries)
  if (writeIntermediateFiles == TRUE) {
    write.csv(poolSeries, "intermediate_poolSeries.csv")
  }
  wide <- metsResidualPools.siteSummaries(poolSeries, residualSeries, 
                                          protocols)
  if (writeIntermediateFiles == TRUE) {
    write.csv(wide, "pvtpoolmetrics.csv")
  }
  mets <- dfLengthen(wide, "UID", "METRIC", "RESULT", names(wide)[!(names(wide) %in% 
                                                                      "UID")])
  if (writeIntermediateFiles == TRUE) {
    write.csv(mets, "intermediate_mets.csv")
  }


###################################################################################################################
#Should only need to run the above  2 sets of code but can run the following functions line by line without boating to troubleshoot protocol issues or any other issues

#metsResidualPools.dataOrganization
#function (thal, actransp, slopes) 

  wadeable <- subset(thal, SAMPLE_TYPE == "PHAB_THALW")
  if (nrow(wadeable) == 0) {
    rawStreams <- NULL
  }
  else {
    rawData <- subset(wadeable, PARAMETER == "DEPTH", select = c(UID, 
                                                                 TRANSECT, STATION, RESULT))
    rawData <- rename(rawData, "RESULT", "DEPTH")
    rawData$DEPTH <- as.numeric(rawData$DEPTH)/100
    nSta <- nWadeableStationsPerTransect(rawData)
    nSta <- first(nSta[order(nSta$UID, nSta$TRANSECT), ], 
                  "UID", "first.UID")
    nSta$startLOC <- NA
    for (i in 1:nrow(nSta)) {
      nSta[i, ]$startLOC <- ifelse(nSta[i, ]$first.UID == 
                                     TRUE | i == 1, 0, nSta[i - 1, ]$startLOC + nSta[i - 
                                                                                       1, ]$nSta)
    }
    rawData <- merge(rawData, nSta, by = c("UID", "TRANSECT"), 
                     all.x = TRUE)
    rawData$LOC <- rawData$startLOC + rawData$STATION + 1
    rawData$LOC <- as.integer(rawData$LOC)
    rawData <- subset(rawData, select = -c(nSta, first.UID, 
                                           startLOC))
    wadeable <- wadeable[order(wadeable$UID, wadeable$TRANSECT, 
                               wadeable$STATION), ]
    incremnt <- first(subset(wadeable, PARAMETER == "INCREMNT"), 
                      "UID", "firstRow")
    incremnt <- subset(incremnt, firstRow == TRUE, select = c(UID, 
                                                              RESULT))
    incremnt <- rename(incremnt, "RESULT", "INCREMNT")
    rawStreams <- merge(rawData, incremnt, by = "UID")
    rawStreams$INCREMNT <- as.numeric(rawStreams$INCREMNT)
    rm(rawData, nSta, wadeable, incremnt)
  }
  boatable <- subset(thal, SAMPLE_TYPE == "PHAB_THAL")
  if (nrow(boatable) == 0) {
    rawRivers <- NULL
  }
  else {
    rawData <- subset(boatable, PARAMETER %in% c("DEP_SONR", 
                                                 "DEP_POLE"), select = c(UID, TRANSECT, STATION, RESULT, 
                                                                         UNITS))
    rawData$RESULT <- as.numeric(ifelse(rawData$RESULT == 
                                          ".", NA, rawData$RESULT))
    rawData$DEPTH <- NA
    rawData$DEPTH <- ifelse(rawData$UNITS == "M", rawData$RESULT, 
                            ifelse(rawData$UNITS == "FT", rawData$RESULT * 0.3048, 
                                   NA))
    rawData <- subset(rawData, select = -c(UNITS, RESULT))
    rawData <- first(rawData[order(rawData$UID, rawData$TRANSECT, 
                                   rawData$STATION), ], "UID", "firstUID")
    rawData$revLOC <- NA
    rl <- NA
    for (i in 1:nrow(rawData)) {
      rl <- ifelse(rawData$firstUID[i], 1, rl + 1)
      rawData$revLOC[i] <- rl
    }
    rawData <- merge(rawData, aggregate(list(maxLOC = rawData$revLOC), 
                                        list(UID = rawData$UID), max, na.rm = TRUE), by = "UID")
    rawData$LOC <- rawData$maxLOC - rawData$revLOC + 1
    rawData$firstUID <- NULL
    rawData$revLOC <- NULL
    rawData$maxLOC <- NULL
    lastSta <- aggregate(list(lastSta = rawData$STATION), 
                         list(UID = rawData$UID, TRANSECT = rawData$TRANSECT), 
                         max, na.rm = TRUE)
    rawRivers <- merge(rawData, lastSta, by = c("UID", "TRANSECT"), 
                       all.x = TRUE)
    rawRivers <- merge(rawRivers, actransp[c("UID", "TRANSECT", 
                                             "RESULT")], by = c("UID", "TRANSECT"), all.x = TRUE)
    rawRivers$INCREMNT <- as.numeric(rawRivers$RESULT)/(rawRivers$lastSta + 
                                                          1)
    rawRivers <- subset(rawRivers, select = -c(lastSta, RESULT))
    rm(boatable, rl, lastSta, rawData)
  }
  if (is.null(rawStreams) & is.null(rawRivers)) {
    return("There are no wadeable nor boatable reaches in the data")
  }
  else {
    thalSeries <- rbind(rawStreams, rawRivers)
  }
  slopes <- subset(slopes, METRIC == "xslope")
  slopes$stackSlope <- 0.12 + 0.25 * as.numeric(slopes$RESULT)
  thalSeries <- merge(thalSeries, slopes[c("UID", "stackSlope")], 
                      by = "UID", all.x = TRUE)
  dMean <- aggregate(list(dMean = thalSeries$DEPTH), list(UID = thalSeries$UID), 
                     mean, na.rm = TRUE)
  dStdev <- aggregate(list(dStdev = thalSeries$DEPTH), list(UID = thalSeries$UID), 
                      sd, na.rm = TRUE)
  thalSeries <- thalSeries[order(thalSeries$UID, thalSeries$LOC), 
                           ]
  nicks <- subset(first(thalSeries, "UID", "first"), first == 
                    "TRUE")
  nicks <- merge(nicks, dMean, by = "UID", all.x = TRUE)
  nicks <- merge(nicks, dStdev, by = "UID", all.x = TRUE)
  nicks$LOC <- as.integer(0)
  nicks$DEPTH <- nicks$dMean - nicks$dStdev
  nicks$DEPTH <- ifelse(nicks$DEPTH < 0, 0, nicks$DEPTH)
  thalSeries <- rbind(thalSeries, nicks[names(thalSeries)])
  thalSeries <- thalSeries[order(thalSeries$UID, thalSeries$LOC), 
                           ]
  rownames(thalSeries) <- NULL
  thalSeries$STATION <- as.integer(thalSeries$STATION)
  return(thalSeries)




#metsResidualPools.dimensions
#function (thalSeries, thalProtocol, minSampPct = 85, oldeMethods = FALSE) 

  intermediateMessage("starting metsResidualPools.dimensions", 
                      loc = "start")
  thalSeries <- first(thalSeries[order(thalSeries$UID, thalSeries$LOC), 
                                 ], "UID", "siteStart")
  thalSeries$resDepth <- as.numeric(NA)
  thalSeries$resArea <- as.numeric(NA)
  thalSeries$resLength <- as.numeric(NA)
  thalSeries$poolID <- as.numeric(NA)
  rpSeriesSplit <- split(thalSeries, thalSeries$UID)
  r <- foreach(uid = names(rpSeriesSplit), .combine = rbind) %do% 
{
  thisSeries <- rpSeriesSplit[[uid]]
  for (i in 1:nrow(thisSeries)) {
    if (thisSeries[i, ]$siteStart) {
      poolBaseDepth <- thisSeries[i, ]$DEPTH
      poolID <- 0
      poolLen <- 0
      inPool <- FALSE
      pp <- subset(protocols, UID == thisSeries[i, 
                                                   ]$UID)
      if (nrow(pp) == 1) {
        isWadeable <- pp$PROTOCOL == "WADEABLE"
      }
      else {
        isWadeable <- TRUE
      }
    }
    else {
      thisSeries[i, ]$resLength <- thisSeries[i, 
                                              ]$INCREMNT * (thisSeries[i, ]$LOC - thisSeries[i - 
                                                                                               1, ]$LOC)
      poolLen <- poolLen + thisSeries[i, ]$resLength
      if (oldeMethods & isWadeable & thisSeries[i - 
                                                  1, ]$siteStart) {
        poolLen <- ifelse(is.na(thisSeries[i, ]$INCREMNT), 
                          NA, 0)
      }
      thisSeries[i, ]$resDepth <- thisSeries[i, ]$DEPTH - 
        (poolBaseDepth + poolLen * thisSeries[i, 
                                              ]$stackSlope/100)
      if (is.na(thisSeries[i, ]$resDepth)) {
        thisSeries[i, ]$resLength <- NA
      }
      else if (thisSeries[i, ]$resDepth > 0) {
        if (!inPool) {
          inPool <- TRUE
          poolID <- poolID + 1
        }
        thisSeries[i, ]$resArea <- thisSeries[i, 
                                              ]$resDepth * thisSeries[i, ]$resLength
        if (oldeMethods & isWadeable & thisSeries[i - 
                                                    1, ]$siteStart) 
          thisSeries[i, ]$resArea <- 0
        thisSeries[i, ]$poolID <- poolID
      }
      else {
        thisSeries[i, ]$resDepth <- 0
        thisSeries[i, ]$resArea <- 0
        thisSeries[i, ]$resLength <- 0
        thisSeries[i, ]$poolID <- 0
        inPool <- FALSE
        poolBaseDepth <- thisSeries[i, ]$DEPTH
        poolLen <- 0
      }
    }
  }
  thisSeries
}
  thalSeries <- r
  intermediateMessage(".cleaning up")
  tt <- aggregate(list(nExpected = thalSeries$LOC), list(UID = thalSeries$UID), 
                  max, na.rm = TRUE)
  mm <- aggregate(list(nMissing = thalSeries$DEPTH), list(UID = thalSeries$UID), 
                  function(x) {
                    sum(is.na(x))
                  })
  pp <- aggregate(list(nPresent = thalSeries$LOC), list(UID = thalSeries$UID), 
                  count)
  tt <- merge(tt, merge(mm, pp, by = "UID"), by = "UID")
  tt$sampPct <- 100 * (tt$nPresent - tt$nMissing)/tt$nExpected
  tt$keep <- (tt$sampPct >= minSampPct)
  thalSeries <- merge(thalSeries, subset(tt, select = c(UID, 
                                                        keep)), by = "UID")
  thalSeries$resDepth <- ifelse(thalSeries$keep, thalSeries$resDepth, 
                                NA)
  thalSeries$resArea <- ifelse(thalSeries$keep, thalSeries$resArea, 
                               NA)
  thalSeries$resLength <- ifelse(thalSeries$keep, thalSeries$resLength, 
                                 NA)
  thalSeries$poolID <- ifelse(thalSeries$keep, thalSeries$poolID, 
                              NA)
  thalSeries$keep <- NULL
  intermediateMessage(". Finished.", loc = "end")
  return(thalSeries)


#metsResidualPools.poolCharacteristics
#function (poolDims) 
  intermediateMessage("starting metsResidualPools.poolCharacteristics", 
                      loc = "start")
  poolar <- aggregate(list(poolar = poolDims$resArea), list(UID = poolDims$UID, 
                                                            poolID = poolDims$poolID), sum, na.rm = TRUE)
  intermediateMessage(".1")
  poolen <- aggregate(list(poolen = poolDims$resLength), list(UID = poolDims$UID, 
                                                              poolID = poolDims$poolID), sum, na.rm = TRUE)
  intermediateMessage(".2")
  mindep <- aggregate(list(mindep = poolDims$resDepth), list(UID = poolDims$UID, 
                                                             poolID = poolDims$poolID), min, na.rm = TRUE)
  xdep <- aggregate(list(xdep = poolDims$resDepth), list(UID = poolDims$UID, 
                                                         poolID = poolDims$poolID), mean, na.rm = TRUE)
  rpvdep <- aggregate(list(rpvdep = poolDims$resDepth), list(UID = poolDims$UID, 
                                                             poolID = poolDims$poolID), sd, na.rm = TRUE)
  rpmxdep <- aggregate(list(rpmxdep = poolDims$resDepth), list(UID = poolDims$UID, 
                                                               poolID = poolDims$poolID), max, na.rm = TRUE)
  meddep <- aggregate(list(meddep = poolDims$resDepth), list(UID = poolDims$UID, 
                                                             poolID = poolDims$poolID), median, na.rm = TRUE)
  dep25 <- aggregate(list(dep25 = poolDims$resDepth), list(UID = poolDims$UID, 
                                                           poolID = poolDims$poolID), quantile, probs = 0.25, na.rm = TRUE, 
                     type = 2)
  dep75 <- aggregate(list(dep75 = poolDims$resDepth), list(UID = poolDims$UID, 
                                                           poolID = poolDims$poolID), quantile, probs = 0.75, na.rm = TRUE, 
                     type = 2)
  intermediateMessage(".3")
  poolCharacteristics <- merge(poolar, poolen, by = c("UID", 
                                                      "poolID"), all = TRUE)
  poolCharacteristics <- merge(poolCharacteristics, mindep, 
                               by = c("UID", "poolID"), all = TRUE)
  poolCharacteristics <- merge(poolCharacteristics, xdep, by = c("UID", 
                                                                 "poolID"), all = TRUE)
  poolCharacteristics <- merge(poolCharacteristics, rpvdep, 
                               by = c("UID", "poolID"), all = TRUE)
  poolCharacteristics <- merge(poolCharacteristics, rpmxdep, 
                               by = c("UID", "poolID"), all = TRUE)
  poolCharacteristics <- merge(poolCharacteristics, meddep, 
                               by = c("UID", "poolID"), all = TRUE)
  poolCharacteristics <- merge(poolCharacteristics, dep25, 
                               by = c("UID", "poolID"), all = TRUE)
  poolCharacteristics <- merge(poolCharacteristics, dep75, 
                               by = c("UID", "poolID"), all = TRUE)
  poolCharacteristics <- subset(poolCharacteristics, poolID != 
                                  0)
  intermediateMessage(". Finished.", loc = "end")
  return(poolCharacteristics)

  
#metsResidualPools.siteSummaries

#function (poolInfo, stationInfo, protocols) 
  intermediateMessage(" starting metsResidualPools.siteSummaries ", 
                      loc = "start")
  rpxlen <- aggregate(list(rpxlen = poolInfo$poolen), list(UID = poolInfo$UID), 
                      mean, na.rm = TRUE)
  rpvlen <- aggregate(list(rpvlen = poolInfo$poolen), list(UID = poolInfo$UID), 
                      sd, na.rm = TRUE)
  rpmxlen <- aggregate(list(rpmxlen = poolInfo$poolen), list(UID = poolInfo$UID), 
                       max, na.rm = TRUE)
  totplen <- aggregate(list(totplen = poolInfo$poolen), list(UID = poolInfo$UID), 
                       sum, na.rm = TRUE)
  intermediateMessage(".1")
  rpxdep <- aggregate(list(rpxdep = stationInfo$resDepth), 
                      list(UID = stationInfo$UID), function(x) {
                        mean(ifelse(x == 0, NA, x), na.rm = TRUE)
                      })
  rpxdep$rpxdep <- ifelse(rpxdep$UID %in% subset(protocols, 
                                                 PROTOCOL %in% "WADEABLE")$UID, rpxdep$rpxdep * 100, rpxdep$rpxdep)
  rpvdep <- aggregate(list(rpvdep = stationInfo$resDepth), 
                      list(UID = stationInfo$UID), function(x) {
                        sd(ifelse(x == 0, NA, x), na.rm = TRUE)
                      })
  rpvdep$rpvdep <- ifelse(rpvdep$UID %in% subset(protocols, 
                                                 PROTOCOL %in% "WADEABLE")$UID, rpvdep$rpvdep * 100, rpvdep$rpvdep)
  rpmxdep <- aggregate(list(rpmxdep = stationInfo$resDepth), 
                       list(UID = stationInfo$UID), function(x) {
                         max(ifelse(is.na(x), -Inf, x), na.rm = TRUE)
                       })
  rpmxdep$rpmxdep <- ifelse(rpmxdep$rpmxdep == -Inf, NA, rpmxdep$rpmxdep)
  rpmxdep$rpmxdep <- ifelse(rpmxdep$UID %in% subset(protocols, 
                                                    PROTOCOL %in% "WADEABLE")$UID, rpmxdep$rpmxdep * 100, 
                            rpmxdep$rpmxdep)
  rpgt50 <- aggregate(list(rpgt50 = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                      function(x) {
                        sum(x > 0.5)
                      })
  rpgt75 <- aggregate(list(rpgt75 = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                      function(x) {
                        sum(x > 0.75)
                      })
  rpgt100 <- aggregate(list(rpgt100 = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                       function(x) {
                         sum(x > 1)
                       })
  rpgt05 <- aggregate(list(rpgt05 = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                      function(x) {
                        sum(x > 0.05)
                      })
  rpgt05x <- aggregate(list(rpgt05x = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                       function(x) {
                         mean(ifelse(x > 0.05, x, NA), na.rm = TRUE)
                       })
  rpgt05x$rpgt05x <- ifelse(rpgt05x$UID %in% subset(protocols, 
                                                    PROTOCOL %in% "WADEABLE")$UID, rpgt05x$rpgt05x * 100, 
                            rpgt05x$rpgt05x)
  rpgt10 <- aggregate(list(rpgt10 = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                      function(x) {
                        sum(x > 0.1)
                      })
  rpgt10x <- aggregate(list(rpgt10x = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                       function(x) {
                         mean(ifelse(x > 0.1, x, NA), na.rm = TRUE)
                       })
  rpgt10x$rpgt10x <- ifelse(rpgt10x$UID %in% subset(protocols, 
                                                    PROTOCOL %in% "WADEABLE")$UID, rpgt10x$rpgt10x * 100, 
                            rpgt10x$rpgt10x)
  rpgt20 <- aggregate(list(rpgt20 = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                      function(x) {
                        sum(x > 0.2)
                      })
  rpgt20x <- aggregate(list(rpgt20x = poolInfo$rpmxdep), list(UID = poolInfo$UID), 
                       function(x) {
                         mean(ifelse(x > 0.2, x, NA), na.rm = TRUE)
                       })
  rpgt20x$rpgt20x <- ifelse(rpgt20x$UID %in% subset(protocols, 
                                                    PROTOCOL %in% "WADEABLE")$UID, rpgt20x$rpgt20x * 100, 
                            rpgt20x$rpgt20x)
  intermediateMessage(".2")
  rpxarea <- aggregate(list(rpxarea = poolInfo$poolar), list(UID = poolInfo$UID), 
                       mean, na.rm = TRUE)
  rpvarea <- aggregate(list(rpvarea = poolInfo$poolar), list(UID = poolInfo$UID), 
                       sd, na.rm = TRUE)
  rpmxar <- aggregate(list(rpmxar = poolInfo$poolar), list(UID = poolInfo$UID), 
                      max, na.rm = TRUE)
  areasum <- aggregate(list(areasum = poolInfo$poolar), list(UID = poolInfo$UID), 
                       sum, na.rm = TRUE)
  intermediateMessage(".3")
  tt <- stationInfo[order(stationInfo$UID, stationInfo$LOC), 
                    ]
  tt <- last(tt, "UID", "lastStation")
  tt <- subset(tt, LOC > 0 & !lastStation)
  reachlen <- aggregate(list(reachlen = tt$INCREMNT), list(UID = tt$UID), 
                        sum, na.rm = TRUE)
  rp100 <- merge(areasum, reachlen, by = "UID")
  rp100$rp100 <- ifelse(is.na(rp100$reachlen), NA, ifelse(rp100$reachlen == 
                                                            0, NA, 100 * rp100$areasum/rp100$reachlen))
  rp100 <- subset(rp100, select = c(UID, rp100))
  intermediateMessage(".4")
  lengthMets <- merge(rpxlen, rpvlen, by = "UID", all = TRUE)
  lengthMets <- merge(lengthMets, rpmxlen, by = "UID", all = TRUE)
  lengthMets <- merge(lengthMets, totplen, by = "UID", all = TRUE)
  depthMets <- merge(rpxdep, rpvdep, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpmxdep, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt50, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt75, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt100, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt05, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt05x, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt10, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt10x, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt20, by = "UID", all = TRUE)
  depthMets <- merge(depthMets, rpgt20x, by = "UID", all = TRUE)
  areaMets <- merge(rpxarea, rpvarea, by = "UID", all = TRUE)
  areaMets <- merge(areaMets, rpmxar, by = "UID", all = TRUE)
  areaMets <- merge(areaMets, areasum, by = "UID", all = TRUE)
  mets <- merge(lengthMets, depthMets, by = "UID", all = TRUE)
  mets <- merge(mets, areaMets, by = "UID", all = TRUE)
  mets <- merge(mets, rp100, by = "UID", all = TRUE)
  intermediateMessage(".  Done.", loc = "end")

  