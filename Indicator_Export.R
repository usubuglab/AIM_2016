##Indicator Export for AquADat or Benchmark Tool##
#Benchmark tool made previous NC_ConditionDetermination obsolete. This code exports the data so benchmarks can be set outside of R.
#Resulting Conditions from the benchmark tool are then read into the SpSurvey_ExtentEstimates

##########Get all indicators and join ecoregion and size class info to them

###Get indicators
#either use saved csv or run the JC_IndicatorCalc.R
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\IndicatorCheckAll14march2018_allsites.csv')

###Add spatial attributes or Misc other categorical fields needed for setting benchmarks
#size class info
IndicatorCheck$BNK_THRESH=ifelse(as.numeric(IndicatorCheck$XBKF_W_CHECK)>10,"LargeWadeable","SmallWadeable")


#get Ecoregions

#Kent replace code below with reading in D_Point feature class
#2018 design database
GISInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\GIS_table_for_Design_Database.csv')
DesignInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\site_table_for_Design_Database.csv')
SiteInfo=join(GISInfo,DesignInfo,by="MS_ID")
#2019 file
#SiteInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\2019DesignSites_for_QC_input.csv')


#Kent join Indicators to D_Point table by PointID but only want most PointID from most recent design (may need to get design year from D_Design table)
Indicators=join(IndicatorCheck, SiteInfo, by=c("SITE_ID_CHECK"),type="left",match="first")

#join ecoregions, size class, and protocol info
Indicators$BNK_THRESH=ifelse(Indicators$PROTOCOL2_CHECK=='BOATABLE',"Boatable",Indicators$BNK_THRESH)
Indicators$NAMC_Benchmark=paste(Indicators$Ecoregion_spelledout,Indicators$BNK_THRESH, sep="_")
Indicators$NAMC_Benchmark=ifelse(Indicators$NAMC_Benchmark=="NorthernRockies_Boatable"|Indicators$NAMC_Benchmark=="SouthernRockies_Boatable"|Indicators$NAMC_Benchmark=="NorthernXericBasin_Boatable","NorthernRockiesSouthernRockiesNorthernXericBasin_Boatable",
                                 ifelse(Indicators$NAMC_Benchmark=="EasternXericBasin_Boatable"|Indicators$NAMC_Benchmark=="SouthernXericBasin_Boatable", "EasternXericBasinSouthernXericBasin_Boatable", Indicators$NAMC_Benchmark))

# alternative formats for ecoregions
# Indicators$THRESH=paste(Indicators$ECO10,Indicators$BNK_THRESH, sep="_")#2011-2015#works with new design database as well
# Indicators$THRESH3=as.factor(Indicators$THRESH)
# levels(Indicators$THRESH3) <- list( XE_SOUTH_SmallWadeable="XE_SOUTH_SmallWadeable",XE_SOUTH_LargeWadeable="XE_SOUTH_LargeWadeable", 
#                                     MT_SWEST_SmallWadeable="MT_SWEST_SmallWadeable",MT_SWEST_LargeWadeable="MT_SWEST_LargeWadeable", 
#                                     XE_EPLAT_SmallWadeable="XE_EPLAT_SmallWadeable",XE_EPLAT_LargeWadeable="XE_EPLAT_LargeWadeable", 
#                                     MT_PNW_SmallWadeable="MT_PNW_SmallWadeable", MT_PNW_LargeWadeable="MT_PNW_LargeWadeable",MT_PNW_BOATABLE="MT_PNW_BOATABLE",  
#                                     PL_NCULT_SmallWadeable="PLN_CULT_SmallWadeable", PL_NCULT_LargeWadeable="PLN_CULT_LargeWadeable",PL_NCULT_BOATABLE="PLN_CULT_BOATABLE", 
#                                     PL_RANGE_SmallWadeable="PL_RANGE_SmallWadeable",PL_RANGE_LargeWadeable="PL_RANGE_LargeWadeable", PL_RANGE_BOATABLE="PL_RANGE_BOATABLE",
#                                     MT_SROCK_SmallWadeable="MT_SROCK_SmallWadeable",MT_SROCK_LargeWadeable="MT_SROCK_LargeWadeable", 
#                                     MT_NROCK_SmallWadeable="MT_NROCK_SmallWadeable", MT_NROCK_LargeWadeable="MT_NROCK_LargeWadeable",
#                                     XE_NORTH_SmallWadeable="XE_NORTH_SmallWadeable",XE_NORTH_LargeWadeable="XE_NORTH_LargeWadeable",
#                                     Other=c( "AK_SmallWadeable","AK_LargeWadeable"),   
#                                     MT_ROCK_BOATABLE=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),  
#                                     XE_SEPLAT_BOATABLE=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
# )
# levels(Indicators$THRESH3) <- list( SouthernXericBasin_SmallWadeable="XE_SOUTH_SmallWadeable",SouthernXericBasin_LargeWadeable="XE_SOUTH_LargeWadeable",
#                                    SouthwestMountains_SmallWadeable="MT_SWEST_SmallWadeable",SouthwestMountains_LargeWadeable="MT_SWEST_LargeWadeable",
#                                    EasternXericBasin_SmallWadeable="XE_EPLAT_SmallWadeable",EasternXericBasin_LargeWadeable="XE_EPLAT_LargeWadeable",
#                                    PacificNorthwest_SmallWadeable="MT_PNW_SmallWadeable", PacificNorthwest_LargeWadeable="MT_PNW_LargeWadeable",PacificNorthwest_Boatable="MT_PNW_BOATABLE",
#                                    NorthernCultivatedPlains_SmallWadeable="PL_NCULT_SmallWadeable", NorthernCultivatedPlains_LargeWadeable="PL_NCULT_LargeWadeable",NorthernCultivatedPlains_Boatable="PL_NCULT_BOATABLE",
#                                    RangelandPlains_SmallWadeable="PL_RANGE_SmallWadeable",RangelandPlains_LargeWadeable="PL_RANGE_LargeWadeable", RangelandPlains_Boatable="PL_RANGE_BOATABLE",
#                                    SouthernRockies_SmallWadeable="MT_SROCK_SmallWadeable",SouthernRockies_LargeWadeable="MT_SROCK_LargeWadeable",
#                                    NorthernRockies_SmallWadeable="MT_NROCK_SmallWadeable", NorthernRockies_LargeWadeable="MT_NROCK_LargeWadeable",
#                                    NorthernXericBasin_SmallWadeable="XE_NORTH_SmallWadeable",NorthernXericBasin_LargeWadeable="XE_NORTH_LargeWadeable",
#                                    NorthernRockiesSouthernRockiesNorthernXericBasin_Boatable=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),
#                                    EasternXericBasinSouthernXericBasin_Boatable=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
# )
# Indicators$THRESH2=Indicators$THRESH
# Indicators$THRESH2=ifelse(Indicators$THRESH2=="PL_RANGE_BOATABLE"|Indicators$THRESH2=="PLN_CULT_BOATABLE"|Indicators$THRESH2=="MT_PNW_BOATABLE"|Indicators$THRESH2=="MT_NROCK_BOATABLE"|Indicators$THRESH2=="MT_SROCK_BOATABLE"|Indicators$THRESH2=="XE_NORTH_BOATABLE"|Indicators$THRESH2=="XE_EPLAT_BOATABLE"|Indicators$THRESH2=="XE_SOUTH_BOATABLE","ALL_BOATING",Indicators$THRESH2)
# Indicators$THRESH4=ifelse(Indicators$ECO10=="XE_EPLAT"|Indicators$ECO10=="XE_SOUTH"|Indicators$ECO10=="PLN_CULT"|Indicators$ECO10=="PL_RANGE","lowstab","highstab")

# ############Join bug data to all other indicator data
#Kent you can leave this section as is for now. We will likely continue reading this info in from a csv 
#If joining bug data to data already in benchmark tool or AquADat just run this portion
Bugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\OE_Modeling\\NAMC_Supported_OEmodels\\final_bug_scores_R_input_join_to_all_other_metrics_statebasedscores.csv")
#subset so that only the state model or the model of interest is in the data...this is necesssary because this file has westwide as well as state based scores for a single sample
Bugs=subset(Bugs,MODEL!="Westwide")#Westwide, ColumbiaRiverBasin_PIBO, OR_WesternCordillera_ColumbiaPlateau, OR_MarineWesternCoastalForest, UT_DEQ_2015,CO_EDAS-Biotype1, CO_EDAS-Biotype2,CO_EDAS-Biotype3,CA_CSCI,NV_MMI
#exclude sites that were outside the experience of the model and counts less than 200 if condition is poor but include everything in AquDat and Benchmark tool export
Bugs=Bugs[,c(1,12,5,6,7,8,10,11)]
#write.csv(Bugs,'Bugs.csv') #uncomment this line to export bug data that needs to be added to already existing records in AquADat
Indicators=join(Indicators,Bugs, by="UID",type="left")


#########Join in Temperature data
#Kent you can leave this section as is for now. We will likely continue reading this info in from a csv
Temperature=read.csv("\\\\share1.bluezone.usu.edu\\miller\\GIS\\Projects\\ModeledStreamTemperature\\2019finaltemperature.csv")
Indicators=join(Indicators,Temperature, by="UID",type="left")

###########Join in Total Nitogen and Total Phosphorus data from USU along with predicted Specific Conductance and predicted TN and TP
#All WQ data from Baker lab or modeling
#Kent you can leave this section as is for now. We will likely read this in from a csv
#Read in csv with Total Nitori
WQpvt$PTL_CHECK=round(WQpvt$PTL,digits=1)
WQpvt$NTL_CHECK=round(WQpvt$NTL,digits=1)
WQpvt$TN_PRED_CHECK=round(WQpvt$TN_PRED,digits=1)
WQpvt$TP_PRED_CHECK=round(WQpvt$TP_PRED,digits=1)
WQpvt$EC_PRED_CHECK=round(WQpvt$EC_PRED,digits=1)
WQfinal=WQpvt
Indicators=join(Indicators,WQfinal, by="UID",type="left")

##########Finally format file for storage in I_Indicator feature class
#To allow for easy indicator name and order changes we have created a custom crosswalk function
#The crosswalk file below as a column for old names and a column for new names and also inserts columns if they arent previously in the dataset
#read in crosswalk file
data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat_computed_metrics\\indicatorXwalkLocal2020_benchmarktool.csv')

data.input=Indicators

#indicatorXwalk function is created in a seperate R script
IndicatorsFinal=indicatorXwalk(data.input,data.xwalk)

#sort by Project and PointID and write out indicators 
IndicatorsFinal=IndicatorsFinal[order(IndicatorsFinal$Project,IndicatorsFinal$SiteCode),]
write.csv(IndicatorsFinal,paste0('IndicatorsFinalExport',Sys.Date(),'.csv'),na="",row.names=FALSE)     


########Format file for prelimnary indicator computation during field season
#Read in Previously calculated Indicators from the I_Indicator feature class
historicIndicators=read.csv("Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\Aquadat9.csv")
#subset to only include indicators pertinent during field season
historicIndicators2=historicIndicators[,c(2:3,4,5,6,8,9:11,24:101)]
#change site information to be in list format
historicIndicators3=melt(historicIndicators2,id.vars=c('UID','Project','SiteCode','MasterCode','VisitNumber','StreamName','FieldStatus'))
historicInidicators4=cast(historicIndicators3,'MasterCode+variable~VisitNumber',value='value')
uniquesites=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\site_table_for_Design_Database_unique_sites.csv')
DesignInfosub=setNames(uniquesites[,c('MS_ID','SITE_ID_CHECK')],c('MasterCode','SiteCode'))
historicIndicators5=join(historicInidicators4,DesignInfosub,by=c('MasterCode'), type='left')


#add crew to recently computed indicators by joining IndicatorsFinal to the F_SampledReaches feature class
IndicatorsFinal=addKEYS(IndicatorsFinal,c('CREW_LEADER'))
#subset to only include indicators pertinent to field season
IndicatorsFinal2=IndicatorsFinal[,c(1,3:6,9:11,24:105)]

#change site information to be in list format
ComputedIndicators=melt(IndicatorsFinal2,id.vars=c('UID','Project','CREW_LEADER','SiteCode','MasterCode','StreamName','FieldStatus'))
ComputedIndicators2=setNames(join(ComputedIndicators, historicIndicators5,type='left',by=c('SiteCode','variable')),c('UID','Project','CREW_LEADER','SiteCode','MasterCode','StreamName','FieldStatus','variable','value','MasterCode','Visit1','Visit2','Visit3','Visit4','Visit5','Visit6'))
#add indicator type information by pulling it from this file...will need to figure out where this info will come from now
filter=read.csv("Z:\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat_computed_metrics\\indicatorXwalkLocal2018_benchmarktool_IndicatorType.csv")
ComputedIndicators2=join(ComputedIndicators2,filter,by='variable',type='left')
ComputedIndicators2=ComputedIndicators2[,c(1:7,17,8:9,11:16)]
Date=subset(ComputedIndicators2, variable=='Date')
Date1=setNames(Date[,c(1,10:15)],c('UID','SampleDate','RevisitData1SampleDate','RevisitData2SampleDate','RevisitData3SampleDate','RevisitData4SampleDate','RevisitData5SampleDate'))
ComputedIndicators3=subset(ComputedIndicators2,variable!='Date')
ComputedIndicators4=join(ComputedIndicators3,Date1, type='left',by='UID')
ComputedIndicators4=ComputedIndicators4[,c(1:5,17,6:16,18:22)]
write.csv(ComputedIndicators4,paste0('QC_Indicators',Sys.Date(),'.csv'),na="",row.names=FALSE)

