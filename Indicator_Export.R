##Indicator Export for AquADat or Benchmark Tool##
#Benchmark tool made previous NC_ConditionDetermination obsolete. This code exports the data so benchmarks can be set outside of R.
#Resulting Conditions from the benchmark tool are then read into the SpSurvey_ExtentEstimates

##########Get all indicators and join ecoregion and size class info to them

###Get indicators
#either use saved csv or run the JC_IndicatorCalc.R
#Indicators=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\final_updated_crosschecked_metrics.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Aquatics\\IndicatorCheck_21Dec2016_GrandStaircase.csv')
#IndicatorCheck=read.csv('IndicatorCheck2021-01-29.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\IndicatorCheck_29April2016.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\priority projects\\IndicatorCheck7Nov2017.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Idaho\\Statewide\\Analysis\\Weights_ExtentEstimates\\IndicatorCheckIDstatewidedata_13April2017.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Analysis\\Weights_ExtentEstimates\\IndicatorCheckGrandStaircase_18April2017.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\IndicatorCheckAll14march2018_allsites.csv')

###Add spatial attributes or Misc other categorical fields needed for setting benchmarks
#size class info
IndicatorCheck$BNK_THRESH=ifelse(as.numeric(IndicatorCheck$XBKF_W_CHECK)>10,"LargeWadeable","SmallWadeable")


#get Ecoregions
#new design database
GISInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\GIS_table_for_Design_Database.csv')
DesignInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\site_table_for_Design_Database.csv')
SiteInfo=join(GISInfo,DesignInfo,by="MS_ID")
#temp 2019 file
#SiteInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\2019DesignSites_for_QC_input.csv')


Indicators=join(IndicatorCheck, SiteInfo, by=c("SITE_ID_CHECK"),type="left",match="first")

#join ecoregions, size class, and protocol info
Indicators$BNK_THRESH=ifelse(Indicators$PROTOCOL2_CHECK=='BOATABLE',"Boatable",Indicators$BNK_THRESH)
Indicators$NAMC_Benchmark=paste(Indicators$Ecoregion,Indicators$BNK_THRESH, sep="_")
Indicators$NAMC_Benchmark=ifelse(Indicators$NAMC_Benchmark=="NorthernRockies_Boatable"|Indicators$NAMC_Benchmark=="SouthernRockies_Boatable"|Indicators$NAMC_Benchmark=="NorthernXericBasin_Boatable","NorthernRockiesSouthernRockiesNorthernXericBasin_Boatable",
                                 ifelse(Indicators$NAMC_Benchmark=="EasternXericBasin_Boatable"|Indicators$NAMC_Benchmark=="SouthernXericBasin_Boatable", "EasternXericBasinSouthernXericBasin_Boatable", Indicators$NAMC_Benchmark))


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
#If joining bug data to data already in benchmark tool or AquADat just run this portion
Bugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\OE_Modeling\\NAMC_Supported_OEmodels\\final_bug_scores_R_input_join_to_all_other_metrics_statebasedscores.csv")
#subset so that only the state model or the model of interest is in the data...this is necesssary because this file has westwide as well as state based scores for a single sample
Bugs=subset(Bugs,MODEL!="Westwide")#Westwide, ColumbiaRiverBasin_PIBO, OR_WesternCordillera_ColumbiaPlateau, OR_MarineWesternCoastalForest, UT_DEQ_2015,CO_EDAS-Biotype1, CO_EDAS-Biotype2,CO_EDAS-Biotype3,CA_CSCI,NV_MMI
#exclude sites that were outside the experience of the model and counts less than 200 if condition is poor but include everything in AquDat and Benchmark tool export
Bugs=Bugs[,c(1,12,5,6,7,8,10,11)]
#write.csv(Bugs,'Bugs.csv') #uncomment this line to export bug data that needs to be added to already existing records in AquADat
Indicators=join(Indicators,Bugs, by="UID",type="left")


#########Join in Temperature data
Temperature=read.csv("\\\\share1.bluezone.usu.edu\\miller\\GIS\\Projects\\ModeledStreamTemperature\\2020finaltemperature.csv")
Indicators=join(Indicators,Temperature, by="UID",type="left")

###########May need to join in WQ data as well if WQ not complete at the time of indicator calc, if so uncomment all lines
# #All WQ data from Baker lab or modeling
# WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','NTL','PTL','EC_PRED','TN_PRED','TP_PRED'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes)
# WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')
# WQpvt$CONDUCTIVITY_CHECK=round(WQpvt$CONDUCTIVITY,digits=2)
# WQpvt$PTL_CHECK=round(WQpvt$PTL,digits=1)
# WQpvt$NTL_CHECK=round(WQpvt$NTL,digits=1)
# WQpvt$TN_PRED_CHECK=round(WQpvt$TN_PRED,digits=1)
# WQpvt$TP_PRED_CHECK=round(WQpvt$TP_PRED,digits=1)
# WQpvt$EC_PRED_CHECK=round(WQpvt$EC_PRED,digits=1)
# WQfinal=WQpvt
# Indicators=join(Indicators,WQfinal, by="UID",type="left")

##########Finally format file for AquADat or benchmark tool
# #Format for AquADat OR
# data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat_computed_metrics\\indicatorXwalkLocal2018_aquadat.csv')

#format for benchmark tool
data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat_computed_metrics\\indicatorXwalkLocal2018_benchmarktool.csv')

#To Format data for editing AquADat, download latest data from ArcGISOnline and paste it into the edit worksheet of the following excel file "Z:\buglab\Research Projects\AIM\Database_Development\AquaDat_computed_metrics\2017 updates\2017 updates.xlsx"
#this worksheet had the correct column headings.
#Then delete all columns except the UID and the column (s) you want to change.
#If making edits to different columns depending on the UID then a seperate file will need to be provided for each set of UIDs.
#final result should be a csv and should look like "Z:\buglab\Research Projects\AIM\Database_Development\AquaDat\InterfaceToServeOutComputedMetrics\2017 updates\myExampleEditData.csv"


#Run for all formats
data.input=Indicators
IndicatorsFinal=indicatorXwalk(data.input,data.xwalk)

IndicatorsFinal=IndicatorsFinal[order(IndicatorsFinal$Project,IndicatorsFinal$SiteCode),]
write.csv(IndicatorsFinal,paste0('IndicatorsFinalExport',Sys.Date(),'.csv'),na="")     

historicIndicators=read.csv("Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\Aquadat9.csv")
historicIndicators2=historicIndicators[,c(2:3,4,5,6,8,9:11,24:29,34:38,51,53:99)]
historicIndicators3=melt(historicIndicators2,id.vars=c('UID','Project','SiteCode','MasterCode','VisitNumber','StreamName','FieldStatus'))
historicInidicators4=cast(historicIndicators3,'MasterCode+variable~VisitNumber',value='value')
uniquesites=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\site_table_for_Design_Database_unique_sites.csv')
DesignInfosub=setNames(uniquesites[,c('MS_ID','SITE_ID_CHECK')],c('MasterCode','SiteCode'))
historicIndicators5=join(historicInidicators4,DesignInfosub,by=c('MasterCode'), type='left')


IndicatorsFinal=read.csv('IndicatorsFinalExport2020-09-03.csv')
IndicatorsFinal=addKEYS(IndicatorsFinal,c('CREW_LEADER'))
IndicatorsFinal2=IndicatorsFinal[,c(1,3:6,9:11,24:31,53,55:56,58:86,88:102,105)]

ComputedIndicators=melt(IndicatorsFinal2,id.vars=c('UID','Project','CREW_LEADER','SiteCode','MasterCode','StreamName','FieldStatus'))
ComputedIndicators2=setNames(join(ComputedIndicators, historicIndicators5,type='left',by=c('SiteCode','variable')),c('UID','Project','CREW_LEADER','SiteCode','MasterCode','StreamName','FieldStatus','variable','value','MasterCode','Visit1','Visit2','Visit3','Visit4','Visit5','Visit6'))
#ComputedIndicators2$PercentDifference=abs(as.numeric(ComputedIndicators2$value)-ComputedIndicators2$Visit1)/mean(ComputedIndicators2,ComputerIndicators2$Visit1)*100
filter=read.csv("Z:\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat_computed_metrics\\indicatorXwalkLocal2018_benchmarktool_IndicatorType.csv")
ComputedIndicators2=join(ComputedIndicators2,filter,by='variable',type='left')
ComputedIndicators2=ComputedIndicators2[,c(1:7,17,8:9,11:16)]
Date=subset(ComputedIndicators2, variable=='Date')
Date1=setNames(Date[,c(1,10:15)],c('UID','SampleDate','RevisitData1SampleDate','RevisitData2SampleDate','RevisitData3SampleDate','RevisitData4SampleDate','RevisitData5SampleDate'))
ComputedIndicators3=subset(ComputedIndicators2,variable!='Date')
ComputedIndicators4=join(ComputedIndicators3,Date1, type='left',by='UID')
ComputedIndicators4=ComputedIndicators4[,c(1:5,17,6:16,18:22)]
write.csv(ComputedIndicators4,'ComputedIndicators4.csv',row.names=FALSE)

#################################### Add additional indicators in seperate table and add a seperate worksheet for metadata
Metadata=read.csv("Z:\\buglab\\Research Projects\\AIM\\Analysis\\Indicator_Scoping\\TR 1735-3\\IndicatorMetadataBenchmarkTool.csv")


project=unique(IndicatorsFinal$Project)

for (p in 1:length(project)){
  p1=IndicatorsFinal[which(IndicatorsFinal$Project==project[p]),]
  wb = createWorkbook()
  sheet1 = addWorksheet(wb, "IndicatorValues")
  sheet2 = addWorksheet(wb, "IndicatorMetadata")
  writeData(wb=wb,sheet=sheet1,p1, startCol=1, rowNames=FALSE)
  writeData(wb=wb,sheet=sheet2,Metadata, startCol=1, rowNames=FALSE)
  saveWorkbook(wb, paste0('IndicatorsFinalExport_',project[p],'_',Sys.Date(),'.xlsx'),overwrite=TRUE)
  }
