A single object matching ‘metsSubstrateCharacterization.1’ was found
It was found in the following places
package:aquamet
namespace:aquamet
with value

function (df1, df2, df3) 
{
  intermediateMessage("Substrate Characterization", loc = "start")
  tt <- textConnection("class min     max\n                    RS 4000    8000\n                    RR 4000    8000\n                    RC 4000    8000\n                    BH 4000    8000\n                    XB 1000    4000\n                    SB  250    1000\n                    BL  250    4000\n                    CB   64     250\n                    GC   16      64\n                    GF    2      16\n                    GR    2      64\n                    SA    0.06    2\n                    FN    0.001   0.06\n                    HP 4000    8000\n#                    HP   NA      NA\n                    WD   NA      NA\n                    OT   NA      NA\n                ")
  subsInfo <- read.table(tt, header = TRUE, stringsAsFactors = FALSE)
  close(tt)
  subsInfo$diam <- NA
  for (s in 1:nrow(subsInfo)) {
    subsInfo[s, ]$diam = gmean(c(subsInfo[s, ]$min, subsInfo[s, 
                                                             ]$max))
  }
  subsInfo$lmin = log10(subsInfo$min)
  subsInfo$lmax = log10(subsInfo$max)
  subsInfo$lDiam <- log10(subsInfo$diam)
  wadeableAllTwoBoulderClasses <- c("RS", "RR", "RC", "XB", 
                                    "SB", "CB", "GC", "GF", "SA", "FN", "HP", "WD", "OT")
  wadeableMobileTwoBoulderClasses <- c("XB", "SB", "CB", "GC", 
                                       "GF", "SA", "FN", "WD", "OT")
  wadeableMeasurableTwoBoulderClasses <- c("XB", "SB", "CB", 
                                           "GC", "GF", "SA", "FN")
  wadeableAllOneBoulderClass <- c("RS", "RR", "RC", "BL", "CB", 
                                  "GC", "GF", "SA", "FN", "HP", "WD", "OT")
  wadeableMobileOneBoulderClass <- c("BL", "CB", "GC", "GF", 
                                     "SA", "FN", "WD", "OT")################nor
  wadeableNumericTwoBoulderClasses <- c("RS", "RR", "RC", "XB", 
                                        "SB", "CB", "GC", "GF", "SA", "FN", "HP")
  boatableAllThalwegClasses <- c("BH", "BL", "CB", "GR", "SA", 
                                 "FN", "OT")
  boatableNumericThalwegClasses <- c("BH", "BL", "CB", "GR", 
                                     "SA", "FN")
  boatableLittoralClasses <- c("RS", "RR", "XB", "SB", "CB", 
                               "GC", "GF", "SA", "FN", "HP", "WD", "OT", "BL", "OM", 
                               "RC")
  intermediateMessage(".1")
  mets <- NULL
  if (nrow(as.data.frame(df1)) > 0) {
    ldBugmm <- merge(df1, subset(subsInfo, class %in% wadeableAllTwoBoulderClasses, 
                                 select = c(class, lDiam)), by.x = "RESULT", by.y = "class", 
                     all.x = TRUE)
    ldBug1mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                          quantile, 0.16, na.rm = TRUE, names = FALSE, type = 2)
    ldBug2mm <- rename(ldBug1mm, "x", "RESULT")
    ldBug2mm$METRIC <- "lsub2d16"
    ldBug3mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                          quantile, 0.25, na.rm = TRUE, names = FALSE, type = 2)
    ldBug4mm <- rename(ldBug3mm, "x", "RESULT")
    ldBug4mm$METRIC <- "lsub2d25"
    ldBug5mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                          quantile, 0.5, na.rm = TRUE, names = FALSE, type = 2)
    ldBug6mm <- rename(ldBug5mm, "x", "RESULT")
    ldBug6mm$METRIC <- "lsub2d50"
    ldBug7mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                          quantile, 0.75, na.rm = TRUE, names = FALSE, type = 2)
    ldBug8mm <- rename(ldBug7mm, "x", "RESULT")
    ldBug8mm$METRIC <- "lsub2d75"
    ldBug9mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                          quantile, 0.84, na.rm = TRUE, names = FALSE, type = 2)
    ldBug10mm <- rename(ldBug9mm, "x", "RESULT")
    ldBug10mm$METRIC <- "lsub2d84"
    ldBug11mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                           mean, na.rm = TRUE)
    ldBug12mm <- rename(ldBug11mm, "x", "RESULT")
    ldBug12mm$METRIC <- "lsub2dmm"
    ldBug13mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                           sd, na.rm = TRUE)
    ldBug14mm <- rename(ldBug13mm, "x", "RESULT")
    ldBug14mm$METRIC <- "lsubd2sd"
    ldBug15mm <- aggregate(ldBugmm$lDiam, list(UID = ldBugmm$UID), 
                           iqr)
    ldBug16mm <- rename(ldBug15mm, "x", "RESULT")
    ldBug16mm$METRIC <- "lsub2iqr"
    intermediateMessage(".2")
    ldBugtt <- merge(df1, subset(subsInfo, class %in% wadeableMobileTwoBoulderClasses, 
                                 select = c(class, diam, lDiam)), by.x = "RESULT", 
                     by.y = "class", all.x = TRUE)
    ldBug11tt <- aggregate(ldBugtt$lDiam, list(UID = ldBugtt$UID), 
                           mean, na.rm = TRUE)
    intermediateMessage(".3")
    ldBug11tt$dgm <- 10^ldBug11tt$x
    ldBug11tt <- rename(ldBug11tt, "x", "lsub2dmm_nor")
    ldBug12tt <- reshape(ldBug11tt, idvar = c("UID"), direction = "long", 
                         varying = names(ldBug11tt)[names(ldBug11tt) != "UID"], 
                         times = names(ldBug11tt)[names(ldBug11tt) != "UID"], 
                         v.names = "RESULT", timevar = "METRIC")
    row.names(ldBug12tt) <- NULL
    ldBug13tt <- aggregate(ldBugtt$lDiam, list(UID = ldBugtt$UID), 
                           sd, na.rm = TRUE)
    ldBug14tt <- rename(ldBug13tt, "x", "RESULT")
    ldBug14tt$METRIC <- "lsubd2sd_nor"
    dBug11tt <- aggregate(ldBugtt$diam, list(UID = ldBugtt$UID), 
                          mean, na.rm = TRUE)
    dBug12tt <- rename(dBug11tt, "x", "RESULT")
    dBug12tt$METRIC <- "sub2dmm_nor"
    dBug13tt <- aggregate(ldBugtt$diam, list(UID = ldBugtt$UID), 
                          sd, na.rm = TRUE)
    dBug14tt <- rename(dBug13tt, "x", "RESULT")
    dBug14tt$METRIC <- "subd2sd_nor"
    intermediateMessage(".4")
    streamld <- rbind(ldBug2mm, ldBug4mm, ldBug6mm, ldBug8mm, 
                      ldBug10mm, ldBug12mm, ldBug14mm, ldBug16mm, ldBug12tt, 
                      ldBug14tt, dBug12tt, dBug14tt)
    interpdata <- subset(df1, RESULT %in% wadeableMeasurableTwoBoulderClasses)
    measurable <- rename(subset(subsInfo, class %in% wadeableMeasurableTwoBoulderClasses, 
                                select = c(class, lmin, lmax)), c("class", "lmin", "lmax"), c("CLASS", "min", "max"))
    c16 <- interpolatePercentile(interpdata, "RESULT", 16, 
                                 "lsub2d16inor", measurable)
    c50 <- interpolatePercentile(interpdata, "RESULT", 50, 
                                 "lsub2d50inor", measurable)
    c84 <- interpolatePercentile(interpdata, "RESULT", 84, 
                                 "lsub2d84inor", measurable)
    c16$d16 <- 10^(c16$lsub2d16inor)
    c50$d50 <- 10^(c50$lsub2d50inor)
    c84$d84 <- 10^(c84$lsub2d84inor)
    calcs <- merge(c16, merge(c50, c84, by = "UID", all = TRUE), 
                   by = "UID", all = TRUE)
    calcs <- reshape(calcs, idvar = c("UID"), direction = "long", 
                     varying = names(calcs)[names(calcs) != "UID"], times = names(calcs)[names(calcs) != 
                                                                                           "UID"], v.names = "RESULT", timevar = "METRIC")
    row.names(calcs) <- NULL
    intermediateMessage(".5")
    df1lb <- df1
    df1lb$RESULT <- ifelse(df1lb$RESULT %in% c("XB", "SB"), 
                           "BL", df1lb$RESULT)
    ldBuglb <- merge(df1lb, subset(subsInfo, class %in% wadeableAllOneBoulderClass, 
                                   select = c(class, diam, lDiam)), by.x = "RESULT", 
                     by.y = "class", all.x = TRUE)
    ldBug1lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                          quantile, 0.16, na.rm = TRUE, names = FALSE, type = 2)
    ldBug2lb <- rename(ldBug1lb, "x", "RESULT")
    ldBug2lb$METRIC <- "lsub_d16"
    ldBug3lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                          quantile, 0.25, na.rm = TRUE, names = FALSE, type = 2)
    ldBug4lb <- rename(ldBug3lb, "x", "RESULT")
    ldBug4lb$METRIC <- "lsub_d25"
    ldBug5lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                          quantile, 0.5, na.rm = TRUE, names = FALSE, type = 2)
    ldBug6lb <- rename(ldBug5lb, "x", "RESULT")
    ldBug6lb$METRIC <- "lsub_d50"
    ldBug7lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                          quantile, 0.75, na.rm = TRUE, names = FALSE, type = 2)
    ldBug8lb <- rename(ldBug7lb, "x", "RESULT")
    ldBug8lb$METRIC <- "lsub_d75"
    ldBug9lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                          quantile, 0.84, na.rm = TRUE, names = FALSE, type = 2)
    ldBug10lb <- rename(ldBug9lb, "x", "RESULT")
    ldBug10lb$METRIC <- "lsub_d84"
    intermediateMessage(".6")
    ldBug11lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                           mean, na.rm = TRUE)
    ldBug12lb <- rename(ldBug11lb, "x", "RESULT")
    ldBug12lb$METRIC <- "lsub_dmm"#####################################################################
    ldBug13lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                           sd, na.rm = TRUE)
    ldBug14lb <- rename(ldBug13lb, "x", "RESULT")
    ldBug14lb$METRIC <- "lsubd_sd"
    ldBug15lb <- aggregate(ldBuglb$lDiam, list(UID = ldBuglb$UID), 
                           iqr)
    ldBug16lb <- rename(ldBug15lb, "x", "RESULT")
    ldBug16lb$METRIC <- "lsub_iqr"
    intermediateMessage(".6")
    df1ttlb <- df1
    df1ttlb$RESULT <- ifelse(df1ttlb$RESULT %in% c("XB", 
                                                   "SB"), "BL", df1ttlb$RESULT)
    ldBugttbl <- merge(df1ttlb, subset(subsInfo, class %in% 
                                         wadeableMobileOneBoulderClass, select = c(class, 
                                                                                   diam, lDiam)), by.x = "RESULT", by.y = "class", all.x = TRUE)
    ldBug11ttbl <- aggregate(ldBugttbl$lDiam, list(UID = ldBugttbl$UID), 
                             mean, na.rm = TRUE)
    ldBug12ttbl <- rename(ldBug11ttbl, "x", "RESULT")
    ldBug12ttbl$METRIC <- "lsub_dmm_nor"
    ldBug13ttbl <- aggregate(ldBugttbl$lDiam, list(UID = ldBugttbl$UID), 
                             sd, na.rm = TRUE)
    ldBug14ttbl <- rename(ldBug13ttbl, "x", "RESULT")
    ldBug14ttbl$METRIC <- "lsubd_sd_nor"
    dBug11ttbl <- aggregate(ldBugttbl$diam, list(UID = ldBugttbl$UID), 
                            mean, na.rm = TRUE)
    dBug12ttbl <- rename(dBug11ttbl, "x", "RESULT")
    dBug12ttbl$METRIC <- "sub_dmm_nor"
    dBug13ttbl <- aggregate(ldBugttbl$diam, list(UID = ldBugttbl$UID), 
                            sd, na.rm = TRUE)
    dBug14ttbl <- rename(dBug13ttbl, "x", "RESULT")
    dBug14ttbl$METRIC <- "subd_sd_nor"
    intermediateMessage(".7")
    streamlb <- rbind(ldBug2lb, ldBug4lb, ldBug6lb, ldBug8lb, 
                      ldBug10lb, ldBug12lb, ldBug14lb, ldBug16lb, ldBug12ttbl, 
                      ldBug14ttbl, dBug12ttbl, dBug14ttbl)
    realallsize <- df1
    realallsize$PARAMETER <- NULL
    allsize <- subset(realallsize, RESULT %in% wadeableNumericTwoBoulderClasses)
    norsize <- subset(realallsize, RESULT %in% wadeableMeasurableTwoBoulderClasses)
    allSZ <- aggregate(allsize$RESULT, list(UID = allsize$UID), 
                       count)
    allSZ <- rename(allSZ, "x", "RESULT")
    allSZ$METRIC <- "n"
    allSZ2 <- aggregate(realallsize$RESULT, list(UID = realallsize$UID), 
                        count)
    allSZ2 <- rename(allSZ2, "x", "RESULT")
    allSZ2$METRIC <- "n2"
    allNOR <- aggregate(norsize$RESULT, list(UID = norsize$UID), 
                        count)
    allNOR <- rename(allNOR, "x", "RESULT")
    allNOR$METRIC <- "n_nor"
    allSZBL <- subset(realallsize, realallsize$RESULT %in% 
                        c("XB", "SB"))
    allSZBL <- aggregate(allSZBL$RESULT, list(UID = allSZBL$UID), 
                         count)
    allSZBL <- rename(allSZBL, "x", "nBL")
    allSZCB <- subset(realallsize, realallsize$RESULT == 
                        "CB")
    allSZCB <- aggregate(allSZCB$RESULT, list(UID = allSZCB$UID), 
                         count)
    allSZCB <- rename(allSZCB, "x", "nCB")
    allSZFN <- subset(realallsize, realallsize$RESULT == 
                        "FN")
    allSZFN <- aggregate(allSZFN$RESULT, list(UID = allSZFN$UID), 
                         count)
    allSZFN <- rename(allSZFN, "x", "nFN")
    allSZGC <- subset(realallsize, realallsize$RESULT == 
                        "GC")
    allSZGC <- aggregate(allSZGC$RESULT, list(UID = allSZGC$UID), 
                         count)
    allSZGC <- rename(allSZGC, "x", "nGC")
    allSZGF <- subset(realallsize, realallsize$RESULT == 
                        "GF")
    allSZGF <- aggregate(allSZGF$RESULT, list(UID = allSZGF$UID), 
                         count)
    allSZGF <- rename(allSZGF, "x", "nGF")
    allSZHP <- subset(realallsize, realallsize$RESULT == 
                        "HP")
    if (nrow(allSZHP) == 0) {
      allSZHP <- data.frame(UID = unique(realallsize$UID), 
                            nHP = 0, stringsAsFactors = FALSE)
    }
    else {
      allSZHP <- aggregate(allSZHP$RESULT, list(UID = allSZHP$UID), 
                           count)
      allSZHP <- rename(allSZHP, "x", "nHP")
    }
    allSZOT <- subset(realallsize, realallsize$RESULT == 
                        "OT")
    allSZOT <- aggregate(allSZOT$RESULT, list(UID = allSZOT$UID), 
                         count)
    allSZOT <- rename(allSZOT, "x", "nOT")
    allSZOM <- subset(realallsize, realallsize$RESULT == 
                        "OM")
    if (nrow(allSZOM) == 0) {
      allSZOM <- data.frame(UID = unique(realallsize$UID), 
                            nOM = 0, stringsAsFactors = FALSE)
    }
    else {
      allSZOM <- aggregate(allSZOM$RESULT, list(UID = allSZOM$UID), 
                           count)
      allSZOM <- rename(allSZOM, "x", "nOM")
    }
    allSZRC <- subset(realallsize, realallsize$RESULT == 
                        "RC")
    if (nrow(allSZRC) == 0) {
      allSZRC <- data.frame(UID = unique(realallsize$UID), 
                            nRC = 0, stringsAsFactors = FALSE)
    }
    else {
      allSZRC <- aggregate(allSZRC$RESULT, list(UID = allSZRC$UID), 
                           count)
      allSZRC <- rename(allSZRC, "x", "nRC")
    }
    allSZRR <- subset(realallsize, realallsize$RESULT == 
                        "RR")
    allSZRR <- aggregate(allSZRR$RESULT, list(UID = allSZRR$UID), 
                         count)
    allSZRR <- rename(allSZRR, "x", "nRR")
    allSZRS <- subset(realallsize, realallsize$RESULT == 
                        "RS")
    allSZRS <- aggregate(allSZRS$RESULT, list(UID = allSZRS$UID), 
                         count)
    allSZRS <- rename(allSZRS, "x", "nRS")
    allSZSA <- subset(realallsize, realallsize$RESULT == 
                        "SA")
    allSZSA <- aggregate(allSZSA$RESULT, list(UID = allSZSA$UID), 
                         count)
    allSZSA <- rename(allSZSA, "x", "nSA")
    allSZSB <- subset(realallsize, realallsize$RESULT == 
                        "SB")
    allSZSB <- aggregate(allSZSB$RESULT, list(UID = allSZSB$UID), 
                         count)
    allSZSB <- rename(allSZSB, "x", "nSB")
    allSZWD <- subset(realallsize, realallsize$RESULT == 
                        "WD")
    allSZWD <- aggregate(allSZWD$RESULT, list(UID = allSZWD$UID), 
                         count)
    allSZWD <- rename(allSZWD, "x", "nWD")
    allSZXB <- subset(realallsize, realallsize$RESULT == 
                        "XB")
    allSZXB <- aggregate(allSZXB$RESULT, list(UID = allSZXB$UID), 
                         count)
    allSZXB <- rename(allSZXB, "x", "nXB")
    one <- rename(allNOR, "RESULT", "n_nor")
    one$METRIC <- NULL
    two <- rename(allSZ, "RESULT", "n")
    two$METRIC <- NULL
    three <- rename(allSZ2, "RESULT", "n2")
    three$METRIC <- NULL
    intermediateMessage(".8")
    pct0 <- within(merge(merge(one, two, by = "UID", all.x = TRUE, 
                               all.y = TRUE), three, by = "UID", all.x = TRUE, all.y = TRUE), 
{
  n <- ifelse(is.na(n), 0, n)
  n_nor <- ifelse(is.na(n_nor), 0, n_nor)
  n2 <- ifelse(is.na(n2), 0, n2)
})
    pct1 <- merge(pct0, allSZBL, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct2 <- merge(pct1, allSZCB, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct3 <- merge(pct2, allSZFN, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct4 <- merge(pct3, allSZGC, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct5 <- merge(pct4, allSZGF, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct6 <- merge(pct5, allSZHP, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct7 <- merge(pct6, allSZOT, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct7b <- merge(pct7, allSZOM, by = "UID", all.x = TRUE, 
                   all.y = FALSE)
    pct8 <- merge(pct7b, allSZRC, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct9 <- merge(pct8, allSZRR, by = "UID", all.x = TRUE, 
                  all.y = FALSE)
    pct10 <- merge(pct9, allSZRS, by = "UID", all.x = TRUE, 
                   all.y = FALSE)
    pct11 <- merge(pct10, allSZSA, by = "UID", all.x = TRUE, 
                   all.y = FALSE)
    pct12 <- merge(pct11, allSZSB, by = "UID", all.x = TRUE, 
                   all.y = FALSE)
    pct13 <- merge(pct12, allSZWD, by = "UID", all.x = TRUE, 
                   all.y = FALSE)
    pct14 <- merge(pct13, allSZXB, by = "UID", all.x = TRUE, 
                   all.y = FALSE)
    pct14$pct_bl <- ifelse(is.na(pct14$nBL), 0, (pct14$nBL/pct14$n2) * 
                             100)
    pct14$pct_cb <- ifelse(is.na(pct14$nCB), 0, (pct14$nCB/pct14$n2) * 
                             100)
    pct14$pct_fn <- ifelse(is.na(pct14$nFN), 0, (pct14$nFN/pct14$n2) * 
                             100)
    pct14$pct_gc <- ifelse(is.na(pct14$nGC), 0, (pct14$nGC/pct14$n2) * 
                             100)
    pct14$pct_gf <- ifelse(is.na(pct14$nGF), 0, (pct14$nGF/pct14$n2) * 
                             100)
    pct14$pct_hp <- ifelse(is.na(pct14$nHP), 0, (pct14$nHP/pct14$n2) * 
                             100)
    pct14$pct_om <- ifelse(is.na(pct14$nOM), 0, (pct14$nOM/pct14$n2) * 
                             100)
    pct14$pct_ot <- ifelse(is.na(pct14$nOT), 0, (pct14$nOT/pct14$n2) * 
                             100)
    pct14$pct_rc <- ifelse(is.na(pct14$nRC), 0, (pct14$nRC/pct14$n2) * 
                             100)
    pct14$pct_rr <- ifelse(is.na(pct14$nRR), 0, (pct14$nRR/pct14$n2) * 
                             100)
    pct14$pct_rs <- ifelse(is.na(pct14$nRS), 0, (pct14$nRS/pct14$n2) * 
                             100)
    pct14$pct_sa <- ifelse(is.na(pct14$nSA), 0, (pct14$nSA/pct14$n2) * 
                             100)
    pct14$pct_sb <- ifelse(is.na(pct14$nSB), 0, (pct14$nSB/pct14$n2) * 
                             100)
    pct14$pct_wd <- ifelse(is.na(pct14$nWD), 0, (pct14$nWD/pct14$n2) * 
                             100)
    pct14$pct_xb <- ifelse(is.na(pct14$nXB), 0, (pct14$nXB/pct14$n2) * 
                             100)
    pct14$pct_bigr <- (pct14$pct_rr + pct14$pct_rs + pct14$pct_rc + 
                         pct14$pct_bl + pct14$pct_cb + pct14$pct_gc)
    pct14$pct_bdrk <- (pct14$pct_rr + pct14$pct_rs)
    pct14$pct_safn <- (pct14$pct_sa + pct14$pct_fn)
    pct14$pct_sfgf <- (pct14$pct_sa + pct14$pct_fn + pct14$pct_gf)
    pct14$pct_org <- pct14$pct_om + pct14$pct_wd
    pct14$n2 <- NULL
    pct14$nBL <- NULL
    pct14$nCB <- NULL
    pct14$nFN <- NULL
    pct14$nGC <- NULL
    pct14$nGF <- NULL
    pct14$nHP <- NULL
    pct14$nOM <- NULL
    pct14$nOT <- NULL
    pct14$nRS <- NULL
    pct14$nRC <- NULL
    pct14$nRR <- NULL
    pct14$nSA <- NULL
    pct14$nSB <- NULL
    pct14$nWD <- NULL
    pct14$nXB <- NULL
    pct15 <- reshape(pct14, idvar = c("UID"), direction = "long", 
                     varying = names(pct14)[names(pct14) != "UID"], times = names(pct14)[names(pct14) != 
                                                                                           "UID"], v.names = "RESULT", timevar = "METRIC")
    row.names(pct15) <- NULL
    intermediateMessage(".9")
    mets <- rbind(streamld, streamlb, calcs, pct15)
    intermediateMessage(".10")
  }
  if (nrow(as.data.frame(df2)) > 0) {
    ldRivmm <- merge(df2, subset(subsInfo, class %in% boatableNumericThalwegClasses, 
                                 select = c(class, diam, lDiam)), by.x = "RESULT", 
                     by.y = "class", all.x = TRUE)
    ldRivCt <- aggregate(df2$RESULT, list(UID = df2$UID), 
                         count)
    ldRivCt <- rename(ldRivCt, "x", "RESULT")
    ldRivCt$METRIC <- "n"
    ldRiv1mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                          quantile, 0.16, na.rm = TRUE, names = FALSE, type = 2)
    ldRiv2mm <- rename(ldRiv1mm, "x", "RESULT")
    ldRiv2mm$METRIC <- "lsub_d16"
    ldRiv3mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                          quantile, 0.25, na.rm = TRUE, names = FALSE, type = 2)
    ldRiv4mm <- rename(ldRiv3mm, "x", "RESULT")
    ldRiv4mm$METRIC <- "lsub_d25"
    ldRiv5mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                          quantile, 0.5, na.rm = TRUE, names = FALSE, type = 2)
    ldRiv6mm <- rename(ldRiv5mm, "x", "RESULT")
    ldRiv6mm$METRIC <- "lsub_d50"
    ldRiv7mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                          quantile, 0.75, na.rm = TRUE, names = FALSE, type = 2)
    ldRiv8mm <- rename(ldRiv7mm, "x", "RESULT")
    ldRiv8mm$METRIC <- "lsub_d75"
    ldRiv9mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                          quantile, 0.84, na.rm = TRUE, names = FALSE, type = 2)
    ldRiv10mm <- rename(ldRiv9mm, "x", "RESULT")
    ldRiv10mm$METRIC <- "lsub_d84"
    intermediateMessage(".11")
    ldRiv11mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                           mean, na.rm = TRUE)
    ldRiv12mm <- rename(ldRiv11mm, "x", "RESULT")
    ldRiv12mm$METRIC <- "lsub_dmm"#####################################################################
    ldRiv13mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                           sd, na.rm = TRUE)
    ldRiv14mm <- rename(ldRiv13mm, "x", "RESULT")
    ldRiv14mm$METRIC <- "lsubd_sd"
    ldRiv15mm <- aggregate(ldRivmm$lDiam, list(UID = ldRivmm$UID), 
                           iqr)
    ldRiv16mm <- rename(ldRiv15mm, "x", "RESULT")
    ldRiv16mm$METRIC <- "lsub_iqr"
    intermediateMessage(".12")
    riv1 <- rbind(ldRivCt, ldRiv2mm, ldRiv4mm, ldRiv6mm, 
                  ldRiv8mm, ldRiv10mm, ldRiv12mm, ldRiv14mm, ldRiv16mm)
    indivcl <- aggregate(list(n = df2$RESULT), list(UID = df2$UID, 
                                                    PARAMETER = df2$PARAMETER, RESULT = df2$RESULT), 
                         count)
    allct <- aggregate(list(nAll = df2$RESULT), list(UID = df2$UID, 
                                                     PARAMETER = df2$PARAMETER), count)
    scCTS <- merge(indivcl, allct, by = c("UID", "PARAMETER"))
    sc <- expand.grid(UID = unique(scCTS$UID), RESULT = boatableAllThalwegClasses)
    sc$RESULT <- as.character(sc$RESULT)
    ss3m <- merge(scCTS, sc, by = c("RESULT", "UID"), all.y = TRUE)
    ss3m$pct <- (ss3m$n/ss3m$nAll) * 100
    ss3m$METRIC <- paste("pct_", tolower(ss3m$RESULT), sep = "")
    ss3m$pct <- ifelse(is.na(ss3m$pct), 0, ss3m$pct)
    ss3m$RESULT <- NULL
    ss3m$PARAMETER <- NULL
    ss3m$n <- NULL
    ss3m$nAll <- NULL
    ss3m <- rename(ss3m, "pct", "RESULT")
    intermediateMessage(".13")
    safn <- within(merge(subset(ss3m, METRIC == "pct_sa", 
                                select = c(UID, RESULT)), subset(ss3m, METRIC == 
                                                                   "pct_fn", select = c(UID, RESULT)), by = "UID", all = TRUE, 
                         suffix = c(".sa", ".fn")), RESULT <- RESULT.sa + 
                     RESULT.fn)
    safn$METRIC <- "pct_safn"
    safn <- safn[c("UID", "RESULT", "METRIC")]
    mets <- rbind(mets, riv1, ss3m, safn)
    intermediateMessage(".14")
  }
  if (nrow(as.data.frame(df3)) > 0) {
    indiv <- aggregate(list(n = df3$RESULT), list(UID = df3$UID, 
                                                  PARAMETER = df3$PARAMETER, RESULT = df3$RESULT), 
                       count)
    big4 <- aggregate(list(n4 = df3$RESULT), list(UID = df3$UID, 
                                                  PARAMETER = df3$PARAMETER), count)
    ss4m <- merge(indiv, big4, by = c("UID", "PARAMETER"))
    ss <- expand.grid(UID = unique(ss4m$UID), PARAMETER = c("BOTTOMDOM", 
                                                            "BOTTOMSEC", "SHOREDOM", "SHORESEC"), RESULT = boatableLittoralClasses)
    ss$PARAMETER <- as.character(ss$PARAMETER)
    ss$RESULT <- as.character(ss$RESULT)
    ss4m <- merge(ss4m, ss, by = c("RESULT", "UID", "PARAMETER"), 
                  all.y = TRUE)
    ss4m$pct <- (ss4m$n/ss4m$n4) * 100
    ss4m$pct <- ifelse(is.na(ss4m$pct), 0, ss4m$pct)
    ss4m$METRIC <- paste("pct_", ifelse(ss4m$PARAMETER == 
                                          "BOTTOMDOM", "db", ifelse(ss4m$PARAMETER == "BOTTOMSEC", 
                                                                    "sb", ifelse(ss4m$PARAMETER == "SHOREDOM", "ds", 
                                                                                 ifelse(ss4m$PARAMETER == "SHORESEC", "ss", "DAMMIT")))), 
                         tolower(ss4m$RESULT), sep = "")
    intermediateMessage(".15")
    ss4m$RESULT <- NULL
    ss4m$PARAMETER <- NULL
    ss4m$n <- NULL
    ss4m$n4 <- NULL
    ss4m <- rename(ss4m, "pct", "RESULT")
    intermediateMessage(".16")
    mets <- rbind(mets, ss4m)
  }
  if (is.null(mets)) {
    intermediateMessage(". No metrics calculated")
  }
  intermediateMessage(" Done.", loc = "end")
  return(mets)
}
<bytecode: 0x10716c6c>
  <environment: namespace:aquamet>
  