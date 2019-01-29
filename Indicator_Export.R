##Indicator Export for AquADat or Benchmark Tool##
#Benchmark tool made previous NC_ConditionDetermination obsolete. This code exports the data so benchmarks can be set outside of R.
#Resulting Conditions from the benchmark tool are then read into the SpSurvey_ExtentEstimates

##########Get all indicators and join ecoregion and size class info to them

###Get indicators
#either use saved csv or run the JC_IndicatorCalc.R
#Indicators=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\final_updated_crosschecked_metrics.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Aquatics\\IndicatorCheck_21Dec2016_GrandStaircase.csv')
#IndicatorCheck=read.csv('IndicatorCheck2016data_21October2016.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\IndicatorCheck_29April2016.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\priority projects\\IndicatorCheck7Nov2017.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Idaho\\Statewide\\Analysis\\Weights_ExtentEstimates\\IndicatorCheckIDstatewidedata_13April2017.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Analysis\\Weights_ExtentEstimates\\IndicatorCheckGrandStaircase_18April2017.csv')
IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\IndicatorCheckAll14march2018_allsites.csv')

###Add spatial attributes or Misc other categorical fields needed for setting benchmarks
#size class info
IndicatorCheck$BNK_THRESH=ifelse(as.numeric(IndicatorCheck$XBKF_W_CHECK)>10,"LargeWadeable","SmallWadeable")


#get Ecoregions
#new design database
GISInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\GIS_table_for_Design_Database.csv')
DesignInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\site_table_for_Design_Database.csv')
SiteInfo=join(GISInfo,DesignInfo,by="MS_ID")
Indicators=join(IndicatorCheck, SiteInfo, by="SITE_ID_CHECK",type="left",match="first")

#join ecoregions, size class, and protocol info
Indicators$BNK_THRESH=ifelse(Indicators$PROTOCOL2_CHECK=='BOATABLE',"BOATABLE",Indicators$BNK_THRESH)
Indicators$NAMC_Benchmark=paste(Indicators$Ecoregion_spelledout,Indicators$BNK_THRESH, sep="_")
#need to add some code here to get boating sites lumped for benchmark tool

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
levels(combined2$THRESH3) <- list( SouthernXericBasin_SmallWadeable="XE_SOUTH_SmallWadeable",SouthernXericBasin_LargeWadeable="XE_SOUTH_LargeWadeable",
                                   SouthwestMountains_SmallWadeable="MT_SWEST_SmallWadeable",SouthwestMountains_LargeWadeable="MT_SWEST_LargeWadeable",
                                   EasternXericBasin_SmallWadeable="XE_EPLAT_SmallWadeable",EasternXericBasin_LargeWadeable="XE_EPLAT_LargeWadeable",
                                   PacificNorthwest_SmallWadeable="MT_PNW_SmallWadeable", PacificNorthwest_LargeWadeable="MT_PNW_LargeWadeable",PacificNorthwest_Boatable="MT_PNW_BOATABLE",
                                   NorthernCultivatedPlains_SmallWadeable="PL_NCULT_SmallWadeable", NorthernCultivatedPlains_LargeWadeable="PL_NCULT_LargeWadeable",NorthernCultivatedPlains_Boatable="PL_NCULT_BOATABLE",
                                   RangelandPlains_SmallWadeable="PL_RANGE_SmallWadeable",RangelandPlains_LargeWadeable="PL_RANGE_LargeWadeable", RangelandPlains_Boatable="PL_RANGE_BOATABLE",
                                   SouthernRockies_SmallWadeable="MT_SROCK_SmallWadeable",SouthernRockies_LargeWadeable="MT_SROCK_LargeWadeable",
                                   NorthernRockies_SmallWadeable="MT_NROCK_SmallWadeable", NorthernRockies_LargeWadeable="MT_NROCK_LargeWadeable",
                                   NorthernXericBasin_SmallWadeable="XE_NORTH_SmallWadeable",NorthernXericBasin_LargeWadeable="XE_NORTH_LargeWadeable",
                                   NorthernRockiesSouthernRockiesNorthernXericBasin_Boatable=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),
                                   EasternXericBasinSouthernXericBasin_Boatable=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
)
# Indicators$THRESH2=Indicators$THRESH
# Indicators$THRESH2=ifelse(Indicators$THRESH2=="PL_RANGE_BOATABLE"|Indicators$THRESH2=="PLN_CULT_BOATABLE"|Indicators$THRESH2=="MT_PNW_BOATABLE"|Indicators$THRESH2=="MT_NROCK_BOATABLE"|Indicators$THRESH2=="MT_SROCK_BOATABLE"|Indicators$THRESH2=="XE_NORTH_BOATABLE"|Indicators$THRESH2=="XE_EPLAT_BOATABLE"|Indicators$THRESH2=="XE_SOUTH_BOATABLE","ALL_BOATING",Indicators$THRESH2)
# Indicators$THRESH4=ifelse(Indicators$ECO10=="XE_EPLAT"|Indicators$ECO10=="XE_SOUTH"|Indicators$ECO10=="PLN_CULT"|Indicators$ECO10=="PL_RANGE","lowstab","highstab")

############Join bug data to all other indicator data
#If joining bug data to data already in benchmark tool or AquADat just run this portion
Bugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\OE_Modeling\\NAMC_Supported_OEmodels\\final_bug_scores_R_input_join_to_all_other_metrics_statebasedscores.csv")
#subset so that only the state model or the model of interest is in the data...this is necesssary because this file has westwide as well as state based scores for a single sample
Bugs=subset(Bugs,MODEL!="Westwide")#Westwide, ColumbiaRiverBasin_PIBO, OR_WesternCordillera_ColumbiaPlateau, OR_MarineWesternCoastalForest, UT_DEQ_2015,CO_EDAS-Biotype1, CO_EDAS-Biotype2,CO_EDAS-Biotype3,CA_CSCI,NV_MMI
#exclude sites that were outside the experience of the model and counts less than 200 if condition is poor but include everything in AquDat and Benchmark tool export
Bugs=Bugs[,c(1,12,5,6,7,8,10,11)]
#write.csv(Bugs,'Bugs.csv') #uncomment this line to export bug data that needs to be added to already existing records in AquADat
Indicators=join(Indicators,Bugs, by="UID",type="left")


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
#Format for AquADat OR
data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat_computed_metrics\\indicatorXwalkLocal2018_aquadat.csv')

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

write.csv(IndicatorsFinal,'IndicatorsFinalExport14August2018.csv',na="")     


