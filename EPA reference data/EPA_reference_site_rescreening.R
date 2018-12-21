####Rescreening the EPA reference data#####
library(plyr)
setwd('Z:\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\RevisingThresholds\\discriminatory boxplots')

combined=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\Comb_21Oct2014_Rinput_DoNotAlter.csv")

#add back in a few columns from Mass_Combination_NRSA_EMAP
#key in combined file is SITE_ID
#as of March 31 2017 the above code to merge NRSA and EMAP data is throwing a row.names duplicate error because REALM is duplicated in the NRSA dataset. 
#I don't know why this wasn't an issue previously but I assume that the dataset in Mass_combination_NRSA_EMAP.csv is correct and would join needed fields back in from this csv rather than the code below.
Final2=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\Mass_Combination_NRSA_EMAP.csv")
Final2sub=subset(Final2,select=c(SITE_ID,XBKF_H,W1_HAG, W1H_CROP,W1H_WALL,PROJECT,XC,XM,XG,XSLOPE,XSLOPE_FIELD,XSLOPE_FRDATA,XSLOPE_FRREF,XSLOPE_MAP,XWD_RAT,XWIDTH,XWXD,XBKF_W,XDEPTH,V1W_MSQ,V1WM100,V2W_MSQ,V2WM100,V4W_MSQ,V4WM100,C1TM100,C1WM100,C2TM100,C2WM100,C4TM100,C4WM100,FLOWSITE))
combined2=join(combined,Final2sub,by='SITE_ID', type="left",match="all")

##############################################################
#Pick one of the options below to get the ecoregion stream site categories in the format that you need

##############################################################
# 1 Use to get typical THRESH3 values
# combined2$BNK_THRESH=ifelse(as.numeric(combined2$XBKF_W)>10,"LargeWadeable","SmallWadeable")
# combined2$BNK_THRESH=ifelse(combined2$REALM=="WADEABLE",combined2$BNK_THRESH,'BOATABLE')
# combined2$THRESH=paste(combined2$ECO10,combined2$BNK_THRESH, sep="_")
# 
# combined2$THRESH3=as.factor(combined2$THRESH)
# levels(combined2$THRESH3) <- list( XE_SOUTH_SmallWadeable="XE_SOUTH_SmallWadeable",XE_SOUTH_LargeWadeable="XE_SOUTH_LargeWadeable",
#                                    MT_SWEST_SmallWadeable="MT_SWEST_SmallWadeable",MT_SWEST_LargeWadeable="MT_SWEST_LargeWadeable",
#                                    XE_EPLAT_SmallWadeable="XE_EPLAT_SmallWadeable",XE_EPLAT_LargeWadeable="XE_EPLAT_LargeWadeable",
#                                    MT_PNW_SmallWadeable="MT_PNW_SmallWadeable", MT_PNW_LargeWadeable="MT_PNW_LargeWadeable",MT_PNW_BOATABLE="MT_PNW_BOATABLE",
#                                    PL_NCULT_SmallWadeable="PL_NCULT_SmallWadeable", PL_NCULT_LargeWadeable="PL_NCULT_LargeWadeable",PL_NCULT_BOATABLE="PL_NCULT_BOATABLE",
#                                    PL_RANGE_SmallWadeable="PL_RANGE_SmallWadeable",PL_RANGE_LargeWadeable="PL_RANGE_LargeWadeable", PL_RANGE_BOATABLE="PL_RANGE_BOATABLE",
#                                    MT_SROCK_SmallWadeable="MT_SROCK_SmallWadeable",MT_SROCK_LargeWadeable="MT_SROCK_LargeWadeable",
#                                    MT_NROCK_SmallWadeable="MT_NROCK_SmallWadeable", MT_NROCK_LargeWadeable="MT_NROCK_LargeWadeable",
#                                    XE_NORTH_SmallWadeable="XE_NORTH_SmallWadeable",XE_NORTH_LargeWadeable="XE_NORTH_LargeWadeable",
#                                    Other=c("XE_CALIF_LargeWadeable","XE_CALIF_SmallWadeable","0_BOATABLE","XE_SOUTH_NA","MT_PNW_0","MT_NROCK_NA", "_SmallWadeable","_LargeWadeable","_BOATABLE"  ,"_NA", "MT_SROCK_NA" , "0_LargeWadeable" , "MT_SWEST_NA", "0_SmallWadeable", "XE_CALIF_BOATABLE","MT_SWEST_BOATABLE"),
#                                    MT_ROCK_BOATABLE=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),
#                                    XE_SEPLAT_BOATABLE=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
# )
# combined2$THRESH2=combined2$THRESH
# combined2$THRESH2=ifelse(combined2$THRESH2=="PL_RANGE_BOATABLE"|combined2$THRESH2=="PL_NCULT_BOATABLE"|combined2$THRESH2=="MT_PNW_BOATABLE"|combined2$THRESH2=="MT_NROCK_BOATABLE"|combined2$THRESH2=="MT_SROCK_BOATABLE"|combined2$THRESH2=="XE_NORTH_BOATABLE"|combined2$THRESH2=="XE_EPLAT_BOATABLE"|combined2$THRESH2=="XE_SOUTH_BOATABLE","ALL_BOATING",combined2$THRESH2)

######################################
# # 2 use if making boxplots so that names don't get too long
# combined2$BNK_THRESH=ifelse(as.numeric(combined2$XBKF_W)>10,"L","S")
# combined2$BNK_THRESH=ifelse(combined2$REALM=="WADEABLE",combined2$BNK_THRESH,'B')
# combined2$THRESH=paste(combined2$ECO10,combined2$BNK_THRESH, sep="_")
# 
# combined2$THRESH3=as.factor(combined2$THRESH)
# levels(combined2$THRESH3) <- list( XE_SOUTH_S="XE_SOUTH_S",XE_SOUTH_L="XE_SOUTH_L",
#                                    MT_SWEST_S="MT_SWEST_S",MT_SWEST_L="MT_SWEST_L",
#                                    XE_EPLAT_S="XE_EPLAT_S",XE_EPLAT_L="XE_EPLAT_L",
#                                    MT_PNW_S="MT_PNW_S", MT_PNW_L="MT_PNW_L",MT_PNW_B="MT_PNW_B",
#                                    PL_NCULT_S="PL_NCULT_S", PL_NCULT_L="PL_NCULT_L",PL_NCULT_B="PL_NCULT_B",
#                                    PL_RANGE_S="PL_RANGE_S",PL_RANGE_L="PL_RANGE_L", PL_RANGE_B="PL_RANGE_B",
#                                    MT_SROCK_S="MT_SROCK_S",MT_SROCK_L="MT_SROCK_L",
#                                    MT_NROCK_S="MT_NROCK_S", MT_NROCK_L="MT_NROCK_L",
#                                    XE_NORTH_S="XE_NORTH_S",XE_NORTH_L="XE_NORTH_L",
#                                    #Other=c("XE_CALIF_L","XE_CALIF_S","0_B","XE_SOUTH_NA","MT_PNW_0","MT_NROCK_NA", "_S","_L","_B"  ,"_NA", "MT_SROCK_NA" , "0_L" , "MT_SWEST_NA", "0_S", "XE_CALIF_B","MT_SWEST_B"),
#                                    MT_ROCK_B=c("MT_NROCK_B", "MT_SROCK_B","XE_NORTH_B"),
#                                    XE_SEPLAT_B=c( "XE_EPLAT_B" ,"XE_SOUTH_B")
# )
# combined2$THRESH2=combined2$THRESH
# combined2$THRESH2=ifelse(combined2$THRESH2=="PL_RANGE_B"|combined2$THRESH2=="PL_NCULT_B"|combined2$THRESH2=="MT_PNW_B"|combined2$THRESH2=="MT_NROCK_B"|combined2$THRESH2=="MT_SROCK_B"|combined2$THRESH2=="XE_NORTH_B"|combined2$THRESH2=="XE_EPLAT_B"|combined2$THRESH2=="XE_SOUTH_B","ALL_BOATING",combined2$THRESH2)

#######################################
#3 use to match benchmark tool or aquadat 
combined2$BNK_THRESH=ifelse(as.numeric(combined2$XBKF_W)>10,"LargeWadeable","SmallWadeable")
combined2$BNK_THRESH=ifelse(combined2$REALM=="WADEABLE",combined2$BNK_THRESH,'Boatable')
combined2$THRESH=paste(combined2$ECO10,combined2$BNK_THRESH, sep="_")
combined2$THRESH3=as.factor(combined2$THRESH)

levels(combined2$THRESH3) <- list( SouthernXericBasin_SmallWadeable="XE_SOUTH_SmallWadeable",SouthernXericBasin_LargeWadeable="XE_SOUTH_LargeWadeable", 
                                    SouthwestMountains_SmallWadeable="MT_SWEST_SmallWadeable",SouthwestMountains_LargeWadeable="MT_SWEST_LargeWadeable", 
                                    EasternXericBasin_SmallWadeable="XE_EPLAT_SmallWadeable",EasternXericBasin_LargeWadeable="XE_EPLAT_LargeWadeable", 
                                    PacificNorthwest_SmallWadeable="MT_PNW_SmallWadeable", PacificNorthwest_LargeWadeable="MT_PNW_LargeWadeable",PacificNorthwest_Boatable="MT_PNW_Boatable",  
                                    NorthernCultivatedPlains_SmallWadeable="PL_NCULT_SmallWadeable", NorthernCultivatedPlains_LargeWadeable="PL_NCULT_LargeWadeable",NorthernCultivatedPlains_Boatable="PL_NCULT_Boatable", 
                                    RangelandPlains_SmallWadeable="PL_RANGE_SmallWadeable",RangelandPlains_LargeWadeable="PL_RANGE_LargeWadeable", RangelandPlains_Boatable="PL_RANGE_Boatable",
                                    SouthernRockies_SmallWadeable="MT_SROCK_SmallWadeable",SouthernRockies_LargeWadeable="MT_SROCK_LargeWadeable", 
                                    NorthernRockies_SmallWadeable="MT_NROCK_SmallWadeable", NorthernRockies_LargeWadeable="MT_NROCK_LargeWadeable",
                                    NorthernXericBasin_SmallWadeable="XE_NORTH_SmallWadeable",NorthernXericBasin_LargeWadeable="XE_NORTH_LargeWadeable",
                                    NorthernRockiesSouthernRockiesNorthernXericBasin_Boatable=c("MT_NROCK_Boatable", "MT_SROCK_Boatable","XE_NORTH_Boatable"),  
                                    EasternXericBasinSouthernXericBasin_Boatable=c( "XE_EPLAT_Boatable" ,"XE_SOUTH_Boatable")
                                   
)
combined2=subset(combined2,is.na(THRESH3)==FALSE)

combined2$Ecoregion_spelledout=combined2$ECO10
combined2$Ecoregion_spelledout=as.factor(combined2$Ecoregion_spelledout)
levels(combined2$Ecoregion_spelledout)<- list(SouthernXericBasin="XE_SOUTH",SouthwestMountains="MT_SWEST",EasternXericBasin="XE_EPLAT",PacificNorthwest="MT_PNW",NorthernCultivatedPlains="PL_NCULT",RangelandPlains="PL_RANGE",SouthernRockies="MT_SROCK",NorthernRockies="MT_NROCK",NorthernXericBasin="XE_NORTH")


##################################################################################################
#remove revisit sites

RIP_RS_combined=combined2
#Now I need to remove duplicate sites, but choose to remove the one with the most recent year. 
#Remove the one with the most recent year because it may not still be in reference condition when it was sampled the second time.
###So I order by if it was revisited, the site code, and then the year.  Check this with the view
RIP_RS_reorder=RIP_RS_combined[order(RIP_RS_combined$REVISITED_OVERLAP,RIP_RS_combined$DUPLICATE_ID, RIP_RS_combined$YEAR, decreasing=FALSE),]
#View(RIP_RS_reorder[1250:2041,])
###Now I remove the duplicate that is listed second.
RIP_RS_minusDup= RIP_RS_reorder[!duplicated(RIP_RS_reorder$DUPLICATE_ID),]
#View(RIP_RS_minusDup[1000:1999,])
RIP_RS_final=RIP_RS_minusDup[order(RIP_RS_minusDup$REVISITED_OVERLAP, decreasing=TRUE),]

#join in StreamCat data
StreamCat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\EPA_Data_StreamCat.csv")
combined3=join(RIP_RS_final,StreamCat, by='SITE_ID', type="left",match="all")

####################################################################################################
#make additional streamcat screening criteria
combined3$UrbCombWs=combined3$PctUrbHi2006Ws+combined3$PctUrbLo2006Ws+combined3$PctUrbMd2006Ws
combined3$AgCombWs=combined3$PctAg2006Slp10Ws+combined3$PctAg2006Slp20Ws
combined3$AgUrbCombWs=combined3$UrbCombWs+combined3$AgCombWs
combined3$UrbCombCat=combined3$PctUrbHi2006Cat+combined3$PctUrbLo2006Cat+combined3$PctUrbMd2006Cat
combined3$AgCombCat=combined3$PctAg2006Slp10Cat+combined3$PctAg2006Slp20Cat
combined3$AgUrbCombCat=combined3$UrbCombCat+combined3$AgCombCat
combined3$NRdCrCat=combined3$RdCrsCat*combined3$CatAreaSqKm
combined3$NRdCrWs=combined3$RdCrsWs*combined3$WsAreaSqKm
reference=combined3

#Variables in EPA reference data set that may have been used in initial screening
#SO4	TURB	PTL	PHLAB	PHSTVL	NTL	CL	ANC	DOC	PCT_URB	W1_HALL	W1_HAG	W1H_CROP	W1H_WALL	NHD100_DAMS_CNT

##########################
#review boxplots showing reference vs. degraded sites depending on screening criteria applied

attach(reference)
#starting criteria
#reference$screen=ifelse(NRdCrCat<10 & NRdCrWs<50 & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensWs==0 & SuperfundDensWs==0 & RdDensWs<1.5 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0,'Good','Poor')
#relaxed road density
#reference$screen=ifelse(NRdCrCat<10 & NRdCrWs<50 & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensWs==0 & SuperfundDensWs==0 & RdDensWs<2 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0,'Good','Poor')
#relaxed W1Hall
#reference$screen=ifelse(NRdCrCat<10 & NRdCrWs<50 & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensWs==0 & SuperfundDensWs==0 & RdDensWs<1.5 & W1_HALL<2 & DamDensCat==0 & CanalDensCat==0,'Good','Poor')
#relaxed NRdCrWs 100
#reference$screen=ifelse(NRdCrCat<10 & NRdCrWs<100 & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensWs==0 & SuperfundDensWs==0 & RdDensWs<1.5 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0,'Good','Poor')
###removed NdRdCrsWs 
#reference$screen=ifelse(NRdCrCat<10  & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensWs==0 & SuperfundDensWs==0 & RdDensWs<1.5 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0,'Good','Poor')
###removed NdRdCrsWS added canalsws
reference$screen=ifelse(NRdCrCat<10 & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensWs==0 & SuperfundDensWs==0 & RdDensWs<1.5 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0 & CanalDensWs<0.075,'Good','Poor')
#boating
#reference$screen=ifelse(NRdCrCat<10 & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensCat==0 & SuperfundDensCat==0 & RdDensWs<1.5 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0 & CanalDensWs<0.075,'Good','Poor')
detach(reference)


#reference=subset(reference,BNK_THRESH=='B')
reference=subset(reference,is.na(THRESH3)==FALSE)
reference=droplevels(reference)

ecoregions=sort(unique(reference$THRESH3))

png(file=paste("XCDENBK.png"), width=1000,height=700,pointsize=15)
par(mfrow=c(5,5))  
for (s in 1:length(ecoregions)){
  EPAsubset=subset(reference,THRESH3==ecoregions[s])
  EPAsubset=droplevels(EPAsubset)
   b=boxplot(XCDENBK~screen,ylab='XCDENBK',xlab=paste(ecoregions[1]),data=EPAsubset,plot=FALSE)
   boxplot(XCDENBK~screen,ylab='XCDENBK',xlab=paste(ecoregions[s],names=paste0(b$names,"(n=",b$n,")")),data=EPAsubset) 
}

dev.off()

png(file=paste("XCMG.png"), width=1000,height=700,pointsize=15)
par(mfrow=c(5,5))  
for (s in 1:length(ecoregions)){
  EPAsubset=subset(reference,THRESH3==ecoregions[s])
  EPAsubset=droplevels(EPAsubset)
  b=boxplot(XCMG~screen,ylab='XCMG',xlab=paste(ecoregions[1]),data=EPAsubset,plot=FALSE)
  boxplot(XCMG~screen,ylab='XCMG',xlab=paste(ecoregions[s],names=paste0(b$names,"(n=",b$n,")")),data=EPAsubset) 
}
dev.off()

png(file=paste("PCT_SAFN.png"), width=1000,height=700,pointsize=15)
par(mfrow=c(5,5))  
for (s in 1:length(ecoregions)){
  EPAsubset=subset(reference,THRESH3==ecoregions[s])
  EPAsubset=droplevels(EPAsubset)
  b=boxplot(PCT_SAFN~screen,ylab='PCT_SAFN',xlab=paste(ecoregions[1]),data=EPAsubset,plot=FALSE)
  boxplot(PCT_SAFN~screen,ylab='PCT_SAFN',xlab=paste(ecoregions[s],names=paste0(b$names,"(n=",b$n,")")),data=EPAsubset) 
}
dev.off()

png(file=paste("LINCIS_H.png"), width=1000,height=700,pointsize=15)
par(mfrow=c(5,5))  
for (s in 1:length(ecoregions)){
  EPAsubset=subset(reference,THRESH3==ecoregions[s])
  EPAsubset=droplevels(EPAsubset)
  b=boxplot(LINCIS_H~screen,ylab='LINCIS_H',xlab=paste(ecoregions[1]),data=EPAsubset,plot=FALSE)
  boxplot(LINCIS_H~screen,ylab='LINCIS_H',xlab=paste(ecoregions[s],names=paste0(b$names,"(n=",b$n,")")),data=EPAsubset) 
}
dev.off()

png(file=paste("XFC_NAT.png"), width=1000,height=700,pointsize=15)
par(mfrow=c(5,5))  
for (s in 1:length(ecoregions)){
  EPAsubset=subset(reference,THRESH3==ecoregions[s])
  EPAsubset=droplevels(EPAsubset)
  b=boxplot(XFC_NAT~screen,ylab='XFC_NAT',xlab=paste(ecoregions[1]),data=EPAsubset,plot=FALSE)
  boxplot(XFC_NAT~screen,ylab='XFC_NAT',xlab=paste(ecoregions[s],names=paste0(b$names,"(n=",b$n,")")),data=EPAsubset) 
}
dev.off()


##write out all data so that you can toggle with different screening criteria and see which ones effect the most sites 
columns=c('PROJECT','SITE_ID',	'DUPLICATE_ID',	'REVISITED_OVERLAP',	'WSA_SITEID',	'YEAR',	'JULDAY',	'DATE_COL',	'LAT_DD',	'LON_DD',	'STATE',	'ECO10',	'EPA_hybird',	'FW_ECO9',	'ECO3',	'FW_ECO3',	'ECOLVL3',	'ECO_LVL_3NAME',	'SITECLS',	'LOC_NAME',	'RST_FSED_AND_RMD_PHAB',	'RST_FRIP_AND_RMD_PHAB','screen','REALM','THRESH3','XCMGW','XCMG','LINCIS_H',	'XCDENBK','XFC_NAT',	'PCT_SAFN',	'W1_HALL' ,'CanalDensCat',	'CanalDensWs','DamDensCat',	'DamDensWs','MineDensCat',	'MineDensWs','NRdCrCat','NRdCrWs','RdCrsWs','RdCrsCat','UrbCombWs','AgCombWs','AgUrbCombWs','UrbCombCat','AgCombCat','AgUrbCombCat','PctUrbOp2006Cat','PctUrbOp2006Ws','SuperfundDensWs','RdDensWs')
referencesubset=reference[,columns]
#write.csv(referencesubset,'reference.csv')

########################################################################################
#subset data by wadeable and boatable to apply two different sets of criteria
reference_wadeable=subset(reference,REALM=='WADEABLE')
reference_boatable=subset(reference,REALM=='BOATABLE')
#wadeable
reference_wadeable=subset(reference_wadeable,NRdCrCat<10  & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensWs==0 & SuperfundDensWs==0 & RdDensWs<1.5 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0 & CanalDensWs<0.075)
#boatable
reference_boatable=subset(reference_boatable,NRdCrCat<10  & UrbCombWs<3 & AgCombWs<3 & AgUrbCombWs<5& UrbCombCat<3 & AgCombCat<3 & AgUrbCombCat<5& PctUrbOp2006Cat<7 &PctUrbOp2006Ws<10 & MineDensCat==0 & SuperfundDensCat==0 & RdDensWs<1.5 & W1_HALL<1.5 & DamDensCat==0 & CanalDensCat==0 & CanalDensWs<0.075)
reference=rbind(reference_wadeable,reference_boatable)
#exclude T sites that were screened out using Google Earth
siteid=c('FW08UT023','WWYP99-0613','FW08CO021','FW08CO087','FW08RUT9122','WCOP04-R010','WCOP99-0506','WCOP99-0650','WUTP99-0542','WWYP99-0515','WWYP99-0525','WWYP99-0653')
reference=subset(reference, !(SITE_ID %in% siteid))



###############################################################
                      #Get thresholds and sample sizes#
###############################################################

# Use riparian reference sites for:  XCDENMID, XCDENBK, XCMG, XCMGW, and PH!!
#Use riparian reference for PH because 
## 1) P-hab reference were selected using a variety of filters, since we do not have specific chemical reference sites we used p-hab reference
## 2) There are a few more reference sites for riparian than for sediment so Riparian was used over Sediment.  
#XCDENMID
T1=setNames(aggregate(reference$XCDENMID, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCDENMID_0.10"))
T2=setNames(aggregate(reference$XCDENMID, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCDENMID_0.30"))
T5=join_all(list(T1,T2), by="THRESH3")
#XCDENBK
T1=setNames(aggregate(reference$XCDENBK, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCDENBK_0.10"))
T2=setNames(aggregate(reference$XCDENBK, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCDENBK_0.30"))
T6=join_all(list(T1,T2), by="THRESH3")
#XCMG
T1=setNames(aggregate(reference$XCMG, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCMG_0.10"))#changing alpha
T2=setNames(aggregate(reference$XCMG, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCMG_0.30"))#changing alpha
T7=join_all(list(T1,T2), by="THRESH3")
#XCMGW
T1=setNames(aggregate(reference$XCMGW, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCMGW_0.10"))
T2=setNames(aggregate(reference$XCMGW, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCMGW_0.30"))
T8=join_all(list(T1,T2), by="THRESH3")

#PH
#PH thresholds were determine to be too strict so we are using the national standards of <6.5 and >9.0 as Poor, >7 and <8.5 as Good.
#if EPA reference thresholds want to be used you must uncomment the below code and add "T18" to the liste of code to join it back into the RIP_THRESHOLDS_lvlIII object
#A few pH issues
#PH_EPA=reference[,1:25]
#Replace NAs with if statement to combine columns
#PH_EPA$PH=ifelse(is.na(PH_EPA$PHLAB),PH_EPA$PHSTVL,PH_EPA$PHLAB)
#thresholds
#T11=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","PH_0.10"))
#T12=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","PH_0.30"))
#T13=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO10","PH_0.90"))
#T14=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO10","PH_0.70"))
#T18=join_all(list(T11,T12,T14,T13), by="ECO10")

##Combine all
RIP_THRESHOLDS_ECO10=join_all(list(T5,T6,T7,T8),by="THRESH3")

###################################################################################
###################################################################################
# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#PCT_SAFN
T3=setNames(aggregate(reference$PCT_SAFN, by = list(reference$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","PCT_SAFN_0.70"))
T4=setNames(aggregate(reference$PCT_SAFN, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","PCT_SAFN_0.90"))
T5=join_all(list(T3,T4), by="THRESH3")
#DPCT_SF
###Modeled indicator, does not use Ecoregion percentil THRESH3olds as these others do.. 
#XFC_NAT
T1=setNames(aggregate(reference$XFC_NAT, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XFC_NAT_0.10"))
T2=setNames(aggregate(reference$XFC_NAT, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XFC_NAT_0.30"))
T7=join_all(list(T1,T2), by="THRESH3")
#LINCIS_H
T3=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","LINCIS_H_0.70"))
T4=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","LINCIS_H_0.90"))
T8=join_all(list(T3,T4), by="THRESH3")
#XEMBED
T3=setNames(aggregate(reference$XEMBED, by = list(reference$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","XEMBED_0.70"))
T4=setNames(aggregate(reference$XEMBED, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","XEMBED_0.90"))
T9=join_all(list(T3,T4), by="THRESH3")
#RP100
T1=setNames(aggregate(reference$RP100, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","RP100_0.10"))
T2=setNames(aggregate(reference$RP100, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","RP100_0.30"))
T10=join_all(list(T1,T2), by="THRESH3")

#C1WM100
T1=setNames(aggregate(as.numeric(reference$C1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","C1WM100_0.10"))
T2=setNames(aggregate(as.numeric(reference$C1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","C1WM100_0.30"))
#V1WM100
T3=setNames(aggregate(as.numeric(reference$V1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","V1WM100_0.10"))
T4=setNames(aggregate(as.numeric(reference$V1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","V1WM100_0.30"))

T10=join_all(list(T1,T2,T3,T4), by="THRESH3")

##Combine all
SED_THRESHOLDS_ECO10=join_all(list(T5,T7,T8,T9,T10),by="THRESH3")

###################################################################################
# Use set thresholds for:  L_XCMGW, W1_HALL, QR1
# Defined when thresholds are implemented and applied to field measured indicators.
###################################################################################

#Make one file for ECO 10 thresholds and remove unwanted working files
Thresholds_ECO10=merge(SED_THRESHOLDS_ECO10,RIP_THRESHOLDS_ECO10, all=TRUE)

####boating incision with all boating sites combined
#LINCIS_H
T3=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH2), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","LINCIS_H_0.70"))
T4=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH2), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","LINCIS_H_0.90"))
T8=join_all(list(T3,T4), by="THRESH3")
SubT8=subset(T8,THRESH3=="ALL_BOATING")

Thresholds_Final=merge(Thresholds_ECO10,SubT8, all=TRUE)
write.csv(Thresholds_Final,'Thresholds_Final.csv')

#get sample sizes
pvt2=aggregate(XCDENBK~THRESH3,data=reference,FUN=length)
pvt3=aggregate(XCMG~THRESH3,data=reference,FUN=length)
pvt4=aggregate(XCMGW~THRESH3,data=reference,FUN=length)
pvt5=aggregate(PCT_SAFN~THRESH3,data=reference,FUN=length)
pvt7=aggregate(XFC_NAT~THRESH3,data=reference,FUN=length)
pvt8=aggregate(LINCIS_H~THRESH3,data=reference,FUN=length)
ECO10_SampSizes=join_all(list(pvt2,pvt3,pvt4,pvt5, pvt7,pvt8),by="THRESH3")
write.csv(ECO10_SampSizes,'Eco10_SampSizes.csv')

write.csv(reference,"Z:\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\RevisingThresholds\\revised_EPAreference.csv")



######################################################################

    #Exploratory#

# ####################################################################
# #subset only R and S sites
# combined3$UrbCombWs=combined3$PctUrbHi2006Ws+combined3$PctUrbLo2006Ws+combined3$PctUrbMd2006Ws
# combined3$AgCombWs=combined3$PctAg2006Slp10Ws+combined3$PctAg2006Slp20Ws
# reference=subset(combined3, RST_FRIP_AND_RMD_PHAB == "R"|RST_FRIP_AND_RMD_PHAB == "S")
# 
# 
# #reference=combined3
# 
UrbCombWs=aggregate(UrbCombWs~THRESH3, data=reference, FUN=max)
AgCombWs=aggregate(AgCombWs~THRESH3, data=reference, FUN=max)
DamDensWs=aggregate(DamDensWs~THRESH3, data=reference, FUN=max)
MineDensWs=aggregate(MineDensWs~THRESH3, data=reference, FUN=max)
NPDESDensWs=aggregate(NPDESDensWs~THRESH3, data=reference, FUN=max)
RdDensWs=aggregate(RdDensWs~THRESH3, data=reference, FUN=max)
W1_HALL=aggregate(W1_HALL~THRESH3, data=reference, FUN=max)
maxes=join_all(list(UrbCombWs,AgCombWs,DamDensWs,MineDensWs,NPDESDensWs,RdDensWs,W1_HALL), by="THRESH3")
write.csv(maxes,'current_benchmark_anthro_influence.csv')



T1=setNames(aggregate(reference$UrbCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","UrbCombWs_0.25"))
T2=setNames(aggregate(reference$UrbCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","UrbCombWs_0.90"))


T3=setNames(aggregate(reference$AgCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","AgCombWs_0.25"))
T4=setNames(aggregate(reference$AgCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","AgCombWs_0.90"))


T5=setNames(aggregate(reference$DamDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","DamDensWs_0.25"))#changing alpha
T6=setNames(aggregate(reference$DamDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","DamDensWs_0.90"))#changing alpha
T7=join_all(list(T1,T2), by="THRESH3")

T8=setNames(aggregate(reference$MineDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","MineDensWs_0.25"))
T9=setNames(aggregate(reference$MineDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","MineDensWs_0.90"))

T10=setNames(aggregate(reference$NPDESDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","NPDESDensWs_0.25"))
T11=setNames(aggregate(reference$NPDESDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","NPDESDensWs_0.90"))

T10=setNames(aggregate(reference$RdDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","RdDensWs_0.25"))
T11=setNames(aggregate(reference$RdDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","RdDensWs_0.90"))

T12=setNames(aggregate(reference$W1_HALL, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","W1_HALL_0.25"))
T13=setNames(aggregate(reference$W1_HALL, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","W1_HALL_0.90"))


T14=join_all(list(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), by="THRESH3")

write.csv(T14,'current_anthro_quantiles.csv')





