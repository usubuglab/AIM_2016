pg 35 of kaufmann 1999 paper Robison and Kaufmann (1994)
and Robison (1998) describe the rapid approach in detail

Stack (1989), to best match residual pool areas based on surveys
of bed elevation:
  Correction angle of residual surface = 0.12 + 0.25(slope)


depth(i) > depth(i-1) + {[0.12 + 0.25(slope)] × interval} (4)
where:
  depth(i) is the measured thalweg depth at the ith point in the upstream
direction along the sampled reach. Depth(1), used for the first
measured depth(i = 1), is taken to be the thalweg depth mean
minus the thalweg depth standard deviation, to allow detection of
the first pool in those cases where a sampled reach begins in a
pool;
'i' is the thalweg depth measurement index, starting at 1 and continuing
to the upstream end of the reach, generally either 100 or 150;
slope is the mean water surface slope of the reach, and
interval is the distance between consecutive measurement points.


gisCalcs = NULL
slopes=read.csv('poolcompareslope.csv')
writeIntermediateFiles = TRUE
oldeMethods = FALSE

> getAnywhere(metsResidualPools)
A single object matching ‘metsResidualPools’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (thalweg, channelgeometry, visits, gisCalcs = NULL) 
{
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
  slopes <- metsSlopeBearing(thalweg, channelgeometry, visits, 
                             gisCalcs)
  slopes <- subset(slopes, METRIC %in% c("xslope", "vslope"))
  intermediateMessage(".4 Set protocols.", loc = "end")
  protocols <- siteProtocol(unique(thal$UID), visits)
  intermediateMessage(".5 Call function metsResidualPools.1.", 
                      loc = "end")
  mets <- metsResidualPools.1(thal, actransp, slopes, protocols)
  row.names(mets) <- 1:nrow(mets)
  intermediateMessage("Done.", loc = "end")
  return(mets)
}
<bytecode: 0x0dc4b0bc>
  <environment: namespace:aquamet>


A single object matching ‘metsResidualPools.1’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (thal, actransp, slopes, protocols, writeIntermediateFiles = FALSE, 
          oldeMethods = FALSE) 
{
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
    write.csv(poolSeries, "intermediate_poolSeriesAK.csv")
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
  return(mets)
}

> getAnywhere(metsResidualPools.dataOrganization)
A single object matching ‘metsResidualPools.dataOrganization’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (thal, actransp, slopes) 
{
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
}
<bytecode: 0x0d1dcc80>
  <environment: namespace:aquamet>



> getAnywhere(metsResidualPools.dimensions)
A single object matching ‘metsResidualPools.dimensions’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (thalSeries, thalProtocol, minSampPct = 0, oldeMethods = FALSE) 
{
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
}
<bytecode: 0x0d6acb2c>
  <environment: namespace:aquamet>

  > getAnywhere(metsResidualPools.poolCharacteristics)
A single object matching ‘metsResidualPools.poolCharacteristics’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (poolDims) 
{
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
}
<bytecode: 0x0cfab640>
  <environment: namespace:aquamet>
  
  > getAnywhere(metsResidualPools.siteSummaries)
A single object matching ‘metsResidualPools.siteSummaries’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (poolInfo, stationInfo, protocols) 
{
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
  return(mets)
}
<bytecode: 0x0d7066e4>
  <environment: namespace:aquamet>
  