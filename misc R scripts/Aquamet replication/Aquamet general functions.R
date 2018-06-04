> getAnywhere(metsGeneral.1)
A single object matching ‘metsGeneral.1’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (indat) 
{
  intermediateMessage("General mets ", loc = "start")
  cdData <- subset(indat, PARAMETER %in% c("ACTRANSP", "DISTANCE", 
                                           "INCREMNT", "SIDCHN", "OFF_CHAN", "REACHLENGTH"))
  sidec <- subset(cdData, PARAMETER %in% c("SIDCHN", "OFF_CHAN") & 
                    RESULT %in% c("Y", "N", NA) & !(TRANSECT %in% c("XA", 
                                                                    "XB", "XC", "XD", "XE", "XF", "XG", "XH", "XI", "XJ", 
                                                                    "XK")))
  intermediateMessage(".1")
  ps <- summaryby(sidec, "count", "pct_side")
  tyout <- aggregate(list(typesum = sidec$RESULT), list(UID = sidec$UID), 
                     function(x) {
                       sum(x == "Y", na.rm = TRUE)
                     })
  ps <- merge(ps, tyout, by = "UID", all.x = TRUE)
  ps$RESULT <- (ps$typesum/ps$RESULT) * 100
  ps <- ps[c("UID", "METRIC", "RESULT")]
  wdata <- subset(cdData, SAMPLE_TYPE == "PHAB_THALW" & PARAMETER == 
                    "INCREMNT")
  sc <- NULL
  if (nrow(wdata) > 0) {
    wdata <- unique(wdata[c("UID", "TRANSECT")])
    wdata$RESULT <- ifelse(wdata$TRANSECT %in% c("XA", "XB", 
                                                 "XC", "XD", "XE", "XF", "XG", "XH", "XI", "XJ", "XK"), 
                           1, 0)
    sc <- summaryby(wdata, "sum", "sidecnt")
  }
  incr <- subset(cdData, SAMPLE_TYPE == "PHAB_THALW" & PARAMETER == 
                   "INCREMNT" & TRANSECT == "A" & STATION == 0, select = c(UID, 
                                                                           RESULT))
  rlw <- NULL
  if (nrow(incr) > 0) {
    w2 <- nWadeableStationsPerTransect(cdData)
    transpc <- merge(incr, w2, by = c("UID"), all.x = TRUE)
    transpc$transpc <- (as.numeric(transpc$RESULT) * transpc$nSta)
    transpc$RESULT <- ifelse(transpc$transpc <= 0, NA, transpc$transpc)
    rlw <- summaryby(transpc, "sum", "reachlen")
    rlw <- merge(rlw, incr, by = "UID", all.x = TRUE, suffix = c(".transpcTot", 
                                                                 ".incremnt"))
    rlw$RESULT <- rlw$RESULT.transpcTot - as.numeric(rlw$RESULT.incremnt)
    rlw <- rlw[c("UID", "METRIC", "RESULT")]
  }
  bdata <- subset(cdData, PARAMETER %in% c("ACTRANSP", "DISTANCE"))
  rl <- NULL
  if (nrow(bdata) > 0) {
    actransp <- subset(bdata, PARAMETER %in% c("ACTRANSP") & 
                         !is.na(RESULT))
    distance <- subset(bdata, PARAMETER %in% c("DISTANCE") & 
                         !is.na(RESULT) & paste(UID, TRANSECT) %nin% paste(actransp$UID, 
                                                                           actransp$TRANSECT))
    bdata <- rbind(actransp, distance)
    bdata$RESULT <- as.numeric(bdata$RESULT)
    bdata$nSta <- 1
    bdata$RESULT <- ifelse(bdata$RESULT <= 0, NA, bdata$RESULT)
    rl <- summaryby(bdata, "sum", "reachlen")
  }
  intermediateMessage(".2")
  mets <- rbind(ps, sc, rlw, rl)
  mets$RESULT <- ifelse(mets$RESULT == "NaN", NA, mets$RESULT)
  intermediateMessage(".Done", loc = "end")
  return(mets)
}
<bytecode: 0x3e5183a4>
  <environment: namespace:aquamet>
  > getAnywhere(nWadeableStationsPerTransect)
A single object matching ‘nWadeableStationsPerTransect’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (thal) 
{
  thal <- subset(thal, TRANSECT %in% LETTERS[1:11])
  staLast <- aggregate(list(staLast = thal$STATION), list(UID = thal$UID, 
                                                          TRANSECT = thal$TRANSECT), max, na.rm = TRUE)
  staMode <- aggregate(list(staLastMode = staLast$staLast), 
                       list(UID = staLast$UID), modalvalue)
  staModeCount <- aggregate(list(staModeCount = staLast$staLast), 
                            list(UID = staLast$UID), modalCount)
  tt <- merge(staLast, staMode, by = "UID")
  tt <- merge(tt, staModeCount, by = "UID")
  tt$lastSta <- ifelse(tt$staModeCount > 6, ifelse(tt$TRANSECT %in% 
                                                     LETTERS[1:10], ifelse(tt$staLast < tt$staLastMode, tt$staLastMode, 
                                                                           tt$staLast), 0), tt$staLast)
  tt$nSta <- tt$lastSta + 1
  tt <- subset(tt, select = c(UID, TRANSECT, nSta))
  return(tt)
}
<bytecode: 0x3e4c614c>
  <environment: namespace:aquamet>
  > View(cdData)