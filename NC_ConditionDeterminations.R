################################################################

#WRSA ECO10: Applying Thresholds

################################################################
##########Get all indicators and join ecoregion and size class info to them
#either use saved csv or run the JC_IndicatorCalc.R
#Indicators=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\final_updated_crosschecked_metrics.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Aquatics\\IndicatorCheck_21Dec2016_GrandStaircase.csv')
#IndicatorCheck=read.csv('IndicatorCheck2016data_21October2016.csv')
#IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\IndicatorCheck_29April2016.csv')
IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\priority projects\\IndicatorCheck7Nov2017.csv')
IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Idaho\\Statewide\\Analysis\\Weights_ExtentEstimates\\IndicatorCheckIDstatewidedata_13April2017.csv')
IndicatorCheck=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Utah\\GSENM\\Analysis\\Weights_ExtentEstimates\\IndicatorCheckGrandStaircase_18April2017.csv')
#size class info
IndicatorCheck$BNK_THRESH=ifelse(as.numeric(IndicatorCheck$XBKF_W_CHECK)>10,"LargeWadeable","SmallWadeable")
# #2011-2015 data
# #SiteInfo=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\AquaticProjectSummaries\\ProjectsPtSummary\\AIM_Aquatic_Sampled_2011_2015_Rinput_into_conditions_final_report_do_not_alter.csv')
# 
# #2016 data
# listsites=tblRetrieve(Parameters=c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','PROJECT','PROTOCOL','VALXSITE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR','Z_DISTANCEFROMX','TRCHLEN','REPEAT_VISIT'),Projects=projects,Years=years,Protocols=protocols)
# listsites=cast(listsites,'UID~PARAMETER',value='RESULT')
# designs=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\AIM_DataManagement\\ProjectMngtSystem\\design_table2.csv')
# postseason=join(listsites,designs, by="SITE_ID", type="left")
# #get ecoregional and stream size info for context for values
# designmetadata=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\GRTS_CodeGuidance\\MasterSample\\MasterSampleDraws\\Aquatic\\LoticMasterSample\\Attributed\\LoticMasterSampleAttributedPtsWithHybridEcoregions.csv')
# SiteInfoSub=join(postseason,designmetadata, by="MS_ID", type="left")
# SiteInfoSub$SITE_ID_CHECK=SiteInfoSub$SITE_ID
# 
# #2011-2015 edits
# #SiteInfoSub=subset(SiteInfo, select=c(SITE_ID_CHECK,Project,Protocol,ECO10,State,FieldOffice,District,StreamOrder,Stratum,StreamSize,Code,Climate))
# #SiteInfoSub$ECO10=ifelse(SiteInfoSub$ECO10=="XE_CALIF","MT_PNW",SiteInfoSub$ECO10)
# #SiteInfoSub$ECO10=ifelse(SiteInfoSub$SITE_ID_CHECK=="OT-LS-7009","XE_SOUTH",SiteInfoSub$ECO10)
# 
# #Indicators=join(IndicatorCheck, SiteInfoSub, by="SITE_ID_CHECK",type="left",match="first")

#new design database
GISInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\GIS_table_for_Design_Database.csv')
DesignInfo=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\site_table_for_Design_Database.csv')
SiteInfo=join(GISInfo,DesignInfo,by="MS_ID")
Indicators=join(IndicatorCheck, SiteInfo, by="SITE_ID_CHECK",type="left",match="first")
Indicators$NAMC_Benchmark=paste(Indicators$Ecoregion_spelledout,Indicators$BNK_THRESH, sep="_")

Indicators$BNK_THRESH=ifelse(Indicators$PROTOCOL2_CHECK=='BOATABLE',"BOATABLE",Indicators$BNK_THRESH)
#Indicators$BNK_THRESH=ifelse(Indicators$PROTOCOL_CHECK=='BOAT2016',"BOATABLE",Indicators$BNK_THRESH)
#Indicators$BNK_THRESH=ifelse(Indicators$Protocol=="BOATABLE","BOATABLE",Indicators$BNK_THRESH)
Indicators$THRESH=paste(Indicators$ECO10,Indicators$BNK_THRESH, sep="_")#2011-2015#works with new design database as well
#Indicators$THRESH=paste(Indicators$ECO_10,Indicators$BNK_THRESH, sep="_")#2016
Indicators$THRESH3=as.factor(Indicators$THRESH)
levels(Indicators$THRESH3) <- list( XE_SOUTH_SmallWadeable="XE_SOUTH_SmallWadeable",XE_SOUTH_LargeWadeable="XE_SOUTH_LargeWadeable", 
                                    MT_SWEST_SmallWadeable="MT_SWEST_SmallWadeable",MT_SWEST_LargeWadeable="MT_SWEST_LargeWadeable", 
                                    XE_EPLAT_SmallWadeable="XE_EPLAT_SmallWadeable",XE_EPLAT_LargeWadeable="XE_EPLAT_LargeWadeable", 
                                    MT_PNW_SmallWadeable="MT_PNW_SmallWadeable", MT_PNW_LargeWadeable="MT_PNW_LargeWadeable",MT_PNW_BOATABLE="MT_PNW_BOATABLE",  
                                    PL_NCULT_SmallWadeable="PLN_CULT_SmallWadeable", PL_NCULT_LargeWadeable="PLN_CULT_LargeWadeable",PL_NCULT_BOATABLE="PLN_CULT_BOATABLE", 
                                    PL_RANGE_SmallWadeable="PL_RANGE_SmallWadeable",PL_RANGE_LargeWadeable="PL_RANGE_LargeWadeable", PL_RANGE_BOATABLE="PL_RANGE_BOATABLE",
                                    MT_SROCK_SmallWadeable="MT_SROCK_SmallWadeable",MT_SROCK_LargeWadeable="MT_SROCK_LargeWadeable", 
                                    MT_NROCK_SmallWadeable="MT_NROCK_SmallWadeable", MT_NROCK_LargeWadeable="MT_NROCK_LargeWadeable",
                                    XE_NORTH_SmallWadeable="XE_NORTH_SmallWadeable",XE_NORTH_LargeWadeable="XE_NORTH_LargeWadeable",
                                    Other=c( "AK_SmallWadeable","AK_LargeWadeable"),   
                                    MT_ROCK_BOATABLE=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),  
                                    XE_SEPLAT_BOATABLE=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
)

Indicators$THRESH2=Indicators$THRESH
Indicators$THRESH2=ifelse(Indicators$THRESH2=="PL_RANGE_BOATABLE"|Indicators$THRESH2=="PLN_CULT_BOATABLE"|Indicators$THRESH2=="MT_PNW_BOATABLE"|Indicators$THRESH2=="MT_NROCK_BOATABLE"|Indicators$THRESH2=="MT_SROCK_BOATABLE"|Indicators$THRESH2=="XE_NORTH_BOATABLE"|Indicators$THRESH2=="XE_EPLAT_BOATABLE"|Indicators$THRESH2=="XE_SOUTH_BOATABLE","ALL_BOATING",Indicators$THRESH2)
Indicators$THRESH4=ifelse(Indicators$ECO10=="XE_EPLAT"|Indicators$ECO10=="XE_SOUTH"|Indicators$ECO10=="PLN_CULT"|Indicators$ECO10=="PL_RANGE","lowstab","highstab")
#Indicators$THRESH4=ifelse(Indicators$ECO_10=="XE_EPLAT"|Indicators$ECO_10=="XE_SOUTH"|Indicators$ECO_10=="PLN_CULT"|Indicators$ECO_10=="PL_RANGE","lowstab","highstab")


############Join bug data to all other indicator data
#bug threholds are determined depending on the model and the thresholds can be found in Z:\buglab\OE_Modeling\OE_Model_Documentation\OE_Data_Prep_Instructions_SWJ
#thresholds are applied in excel after running the model
# WRSABugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\final_bug_scores_R_input_join_to_all_other_metrics.csv")
# Indicators=join(Indicators,WRSABugs, by="UID",type="left")
# Indicators$OErtg=ifelse(Indicators$MODELTEST=="F",NA,Indicators$OErtg)#exclude sites that were outside the experience of the model, 9 sites and most were major rivers
# Indicators$OE_less100rtg=ifelse(Indicators$COUNT<100,NA,Indicators$OErtg)#75 sites and after consulting Chuck decided to include them for the population estimates but for sites specific flag as having low counts and interpret with caution
# Indicators$OE_50_100rtg=ifelse(Indicators$COUNT>50 & Indicators$COUNT<100,NA,Indicators$OErtg)#75 sites and after consulting Chuck decided to include them for the population estimates but for sites specific flag as having low counts and interpret with caution
Bugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\WQ_Bug_Model_GIS_stats_and_results_DO_NOT_MOVE\\final_bug_scores_R_input_join_to_all_other_metrics_statebasedscores.csv")
#subset so that only the state model or the model of interest is in the data...this is necesssary because this file has westwide as well as state based scores for a single sample
Bugs=subset(Bugs,MODEL=="ColumbiaRiverBasin_PIBO")#Westwide, ColumbiaRiverBasin_PIBO, OR_WesternCordillera_ColumbiaPlateau, OR_MarineWesternCoastalForest, UT_DEQ_2015,CO_EDAS-Biotype1, CO_EDAS-Biotype2,CO_EDAS-Biotype3,CA_CSCI,NV_MMI
#Indicators=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Idaho\\Statewide\\Analysis\\Weights_ExtentEstimates\\IndicatorsCond_IDstatewidedata_13April2017_revisedsed.csv')
Indicators=join(Indicators,Bugs, by="UID",type="left")
#exclude sites that were outside the experience of the model...however we are keeping sites will low bug counts
Indicators$OErtg=ifelse(Indicators$MODELTEST=="F",NA,Indicators$OErtg)

############ WQ modeled thresholds
Indicators$OE_EC_CHECK=round(Indicators$CONDUCTIVITY-Indicators$EC_PRED,digits=2)
Indicators$OE_TN_CHECK=round(Indicators$NTL-Indicators$TN_PRED,digits=1)
Indicators$OE_TP_CHECK=round(Indicators$PTL-Indicators$TP_PRED,digits=1)


# # 70th and 90th percentiles
# Indicators$OE_ECrtg=ifelse(Indicators$OE_EC_CHECK <22.4,'Good',ifelse(Indicators$OE_EC_CHECK >53.7, 'Poor','Fair'))
# Indicators$OE_TNrtg=ifelse(Indicators$OE_TN_CHECK <43.9,'Good',ifelse(Indicators$OE_TN_CHECK >87.7, 'Poor','Fair'))
# Indicators$OE_TPrtg=ifelse(Indicators$OE_TP_CHECK <8.6,'Good',ifelse(Indicators$OE_TP_CHECK >16.0, 'Poor','Fair'))
#75th and 95th percentiles
Indicators$OE_ECrtg=ifelse(Indicators$OE_EC_CHECK <27.1,'Good',ifelse(Indicators$OE_EC_CHECK >74.5, 'Poor','Fair'))
Indicators$OE_TNrtg=ifelse(Indicators$OE_TN_CHECK <52.1,'Good',ifelse(Indicators$OE_TN_CHECK >114.7, 'Poor','Fair'))
Indicators$OE_TPrtg=ifelse(Indicators$OE_TP_CHECK <9.9,'Good',ifelse(Indicators$OE_TP_CHECK >21.3, 'Poor','Fair'))


#EPA WQ Regional Reference Thresholds
# Indicators$OE_ECrtg=ifelse(Indicators$CONDUCTIVITY_CHECK <500,'Good',ifelse(Indicators$CONDUCTIVITY_CHECK >=1000, 'Poor','Fair'))
# 
# Indicators$OE_TNrtg=ifelse(
#   ((Indicators$Climate=="Mountains"& Indicators$NTL_CHECK>229)|
#     (Indicators$Climate=="Plains"& Indicators$NTL_CHECK>1570)|
#     (Indicators$Climate=="Xeric"& Indicators$NTL_CHECK>462)),"Poor",
# ifelse(((Indicators$Climate=="Mountains"& Indicators$NTL_CHECK<229 & Indicators$NTL_CHECK>131 )|
#          (Indicators$Climate=="Plains"& Indicators$NTL_CHECK<1570 &Indicators$NTL_CHECK>948)|
#          (Indicators$Climate=="Xeric"& Indicators$NTL_CHECK<462 & Indicators$NTL_CHECK>346)),"Fair","Good"
#  ))
# Indicators$OE_TPrtg=ifelse(
#   ((Indicators$Climate=="Mountains"& Indicators$PTL_CHECK>36)|
#      (Indicators$Climate=="Plains"& Indicators$PTL_CHECK>183)|
#      (Indicators$Climate=="Xeric"& Indicators$PTL_CHECK>70)),"Poor",
#   ifelse(((Indicators$Climate=="Mountains"& Indicators$PTL_CHECK<36 & Indicators$PTL_CHECK>14 )|
#             (Indicators$Climate=="Plains"& Indicators$PTL_CHECK<183 &Indicators$PTL_CHECK>91.8)|
#             (Indicators$Climate=="Xeric"& Indicators$PTL_CHECK<70 & Indicators$PTL_CHECK>35.5)),"Fair","Good"
#   ))

# Ecoregion  Salinity Good-Fair	Salinity Fair-Poor	Total N Good-Fair	Total N Fair-Poor	Total P Good-Fair	Total P Fair-Poor
# Mountains	500	1000	131	229	14	36
# Plains	500	1000	948	1570	91.8	183
# Xeric	500	1000	346	462	35.5	70 

########### Best Professional Judgement Thresholds
#PH
Indicators$PH_CHECKrtg=ifelse((Indicators$PH_CHECK<6.5|Indicators$PH_CHECK >9),"Poor",ifelse((Indicators$PH_CHECK >7 & Indicators$PH_CHECK <8.5),"Good","Fair"))

# Bank stability and cover
#Indicators$BnkStability_Erosionalrtg=ifelse(Indicators$BnkStability_Erosional_CHECK>80,'Good',ifelse(Indicators$BnkStability_Erosional_CHECK<60,'Poor','Fair'))
#Indicators$BnkCover_Erosional_CHECKrtg=ifelse(Indicators$BnkCover_Erosional_CHECK>60,'Good',ifelse(Indicators$BnkCover_Erosional_CHECK<40,'Poor','Fair'))
Indicators$BnkCover_StabErosionalrtg=ifelse(Indicators$BnkCover_StabErosional_CHECK>80,'Good',ifelse(Indicators$BnkCover_StabErosional_CHECK<60,'Poor','Fair'))

#Indicators$BnkStability_Allrtg=ifelse(Indicators$BnkStability_All_CHECK>80,'Good',ifelse(Indicators$BnkStability_All_CHECK<60,'Poor','Fair'))
#Indicators$BnkCover_All_CHECKrtg=ifelse(Indicators$BnkCover_All_CHECK>60,'Good',ifelse(Indicators$BnkCover_All_CHECK<40,'Poor','Fair'))
#Indicators$BnkCover_StabAllrtg=ifelse(Indicators$BnkCover_StabAll_CHECK>80,'Good',ifelse(Indicators$BnkCover_StabAll_CHECK<60,'Poor','Fair'))

Indicators$BnkCover_StabErosionalrtg=ifelse(Indicators$THRESH4=="lowstab" & Indicators$BnkCover_StabErosional_CHECK>70,'Good',
                                        ifelse(Indicators$THRESH4=="lowstab"& Indicators$BnkCover_StabErosional_CHECK<50,'Poor',
                                               ifelse(Indicators$THRESH4=="lowstab" & Indicators$BnkCover_StabErosional_CHECK<=70 & Indicators$BnkCover_StabErosional_CHECK>=50,'Fair',Indicators$BnkCover_StabErosionalrtg)))


############Join indicators to PHAB thresholds determined using EPA reference data in NC_EPA_Data_and_SettingThresholds.R
Thresholds_Final=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\AIM_2011_2015_results\\Thresholds_Final27April2016.csv')
IndicatorsJoin=join(Indicators,Thresholds_Final, by="THRESH3",type="left")

#Apply thresholds
IndicatorsJoin$XFC_NATrtg=ifelse(IndicatorsJoin$XFC_NAT_CHECK <IndicatorsJoin$XFC_NAT_0.10,"Poor",ifelse(IndicatorsJoin$XFC_NAT_CHECK >IndicatorsJoin$XFC_NAT_0.30,"Good","Fair"))
IndicatorsJoin$XCMGrtg=ifelse(IndicatorsJoin$XCMG_CHECK <IndicatorsJoin$XCMG_0.10,"Poor",ifelse(IndicatorsJoin$XCMG_CHECK >IndicatorsJoin$XCMG_0.30,"Good","Fair"))
#IndicatorsJoin$XCMGWrtg=ifelse(IndicatorsJoin$XCMGW_CHECK <IndicatorsJoin$XCMWG_0.10,"Poor",ifelse(IndicatorsJoin$XCMGW_CHECK >IndicatorsJoin$XCMGW_0.30,"Good","Fair"))
IndicatorsJoin$XCDENMIDrtg=ifelse(IndicatorsJoin$XCDENMID_CHECK <IndicatorsJoin$XCDENMID_0.10,"Poor",ifelse(IndicatorsJoin$XCDENMID_CHECK >IndicatorsJoin$XCDENMID_0.30,"Good","Fair"))
IndicatorsJoin$XCDENBKrtg=ifelse(IndicatorsJoin$XCDENBK_CHECK <IndicatorsJoin$XCDENBK_0.10,"Poor",ifelse(IndicatorsJoin$XCDENBK_CHECK >IndicatorsJoin$XCDENBK_0.30,"Good","Fair"))

#because of low reference sample sizes for boating incision, incision was combined into all boating to get the thresholds, the threshold uses is in the Thresholds_Final table but was not able to be joined into the indicator file because other boating indicators were seperated by ecoregion. Therefore the thresholds were manually input below.
IndicatorsJoin$LINCIS_Hrtg=ifelse(IndicatorsJoin$LINCIS_H_CHECK >IndicatorsJoin$LINCIS_H_0.90,"Poor",ifelse(IndicatorsJoin$LINCIS_H_CHECK <IndicatorsJoin$LINCIS_H_0.70,"Good","Fair"))
IndicatorsJoin$LINCIS_Hrtg=ifelse(IndicatorsJoin$THRESH2=="ALL_BOATING"& IndicatorsJoin$LINCIS_H_CHECK >0.3986,"Poor",
                                         ifelse(IndicatorsJoin$THRESH2=="ALL_BOATING"& IndicatorsJoin$LINCIS_H_CHECK< 0.2222,"Good",
                                                ifelse(IndicatorsJoin$THRESH2=="ALL_BOATING"& IndicatorsJoin$LINCIS_H_CHECK <=0.3986 & IndicatorsJoin$LINCIS_H_CHECK>= 0.2222,"Fair",IndicatorsJoin$LINCIS_Hrtg)))
IndicatorsJoin$allPCT_SAFN2rtg=ifelse(IndicatorsJoin$allPCT_SAFN2_CHECK >IndicatorsJoin$PCT_SAFN_0.90,"Poor",ifelse(IndicatorsJoin$allPCT_SAFN2_CHECK <IndicatorsJoin$PCT_SAFN_0.70,"Good","Fair"))
#IndicatorsJoin$XEMBEDrtg=ifelse(IndicatorsJoin$XEMBED_CHECK >IndicatorsJoin$XEMBED_0.90,"Poor",ifelse(IndicatorsJoin$XEMBED_CHECK <IndicatorsJoin$XEMBED_0.70,"Good","Fair"))


########Do additional formating to get it in the right format for AquADat############

IndicatorsCond=IndicatorsJoin
IndicatorsCond$PROTOCOL2_CHECK=ifelse(IndicatorsCond$PROTOCOL_CHECK=="BOAT14"|IndicatorsCond$PROTOCOL_CHECK=="BOAT2016","BOATABLE","WADEABLE")
IndicatorsCond$FieldStatus=ifelse(IndicatorsCond$VALXSITE_CHECK=="WADEABLE"|IndicatorsCond$VALXSITE_CHECK=="BOATABLE","Sampled - Full Reach",
                              ifelse(IndicatorsCond$VALXSITE_CHECK=="PARBYWADE"|IndicatorsCond$VALXSITE_CHECK=="PARBYBOAT","Sampled - Partial Reach",
                                 ifelse(IndicatorsCond$VALXSITE_CHECK=="INTWADE","Sampled Interrupted Flow",IndicatorsCond$VALXSITE_CHECK)))
#master
data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat\\InterfaceToServeOutComputedMetrics\\indicatorXwalkMaster.csv')
#local
#data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat\\InterfaceToServeOutComputedMetrics\\indicatorXwalkLocal.csv')
data.input=IndicatorsCond
IndicatorsCond=indicatorXwalk(data.input,data.xwalk)

write.csv(IndicatorsCond,'priority_projectQC.csv',na="")     

############### continue on to the SpSurvey_DesignWeights and SPSurvey_ExtentEstimates R scripts ######################                                 
####################################################################################################################
##################################################################################################################
##################################################################################################################
###################################################################################################################
# ###trial loop
# DesiredIndicators=c('XEMBED','XFC_NAT','XCMG','XCDENBK','LINCIS_H','PCT_SAFN')
# for (i in 1:length(DesiredIndicators)){
# name=sprintf('%s_rtg',DesiredIndicators[[i]])
# nameCheck=sprintf('%s_CHECK',DesiredIndicators[[i]])
# namelow=sprintf('%s_0.10',DesiredIndicators[[i]])
# namehigh=sprintf('%s_0.30',DesiredIndicators[[i]])
# IndicatorsJoin$name=ifelse(IndicatorsJoin$nameCheck <=IndicatorsJoin$namelow,"Poor",ifelse(IndicatorsJoin$nameCheck >IndicatorsJoin$namehigh,"Good","Fair"))}
# #####

################################################################################################################################

#############################       Archive NorCal condition determinations       ############################################                                  
                                  
###############################################################################################################################                                  
#How to order data
New table name=data[order(data$column),]

#R studio only allows the first 1000 records and 100 fields to be viewed. With the below code I can choose which records I want to view. This is an example of viewing rows 3500-4500
#View(data[records or rows, fields or columns])
View(data[3500:4500,])

# Quick notes:
#### ! means negate
#### | means OR
#### & means AND

#Read indicator data in...
IndicatorCheck=read.csv('C:\\Users\\Nicole\\Desktop\\AIM_2016\\Rinput_IndicatorCheck_3October2016.csv')


#############################################################################

##############    Water quality condition determinations      ###############

#############################################################################

# # First get field measured WQ data (conductivity, tn, tp) using code in NC_DataConsumption
# # Read in predicted WQ results from the WQ models
# PrdWQresults=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\Pred_WQresults_2014All.csv")
# 
# #Rename the siteCode column in predicted file to match the Site ID column in the data file so that the files can be merged
# PrdWQresults$SITE_ID=PrdWQresults$SiteCode
# 
# #Merge the files and check that it worked correctly
# #to use the merge function you must have a column in each file that match exactly. 
# AllWQ=merge(PrdWQresults,WQfinal)
# 
# #Reorganize and remove unwanted columns by only choosing those you want, you can use column numbers or column names. I think column names are more intuitive but column numbers left as an example.
# AllWQ2=AllWQ[,c('UID','SITE_ID','LOC_NAME','DATE_COL','PrdCond','Pred_TN','Pred_TP','CONDUCTIVITY',	'NTL',	'PTL')]
# #Trial2=AllWQ[,c(7,1,13,11,6,5,4,8,9,10)]
# 
# #SRM input
# #AllWQ2=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\SRM_2015\\final_updated_crosschecked_metrics.csv')
# 
# #Now subtract Observe - expected to get the OE score to be classified as G F P. Then set thresholds for each indicator
# #If o-e is > lower limit it is fair, if it is > upper limit its poor, if it is < lower limit it is good(all else is good)

# ###Conductivity #Modelled thresholds
# AllWQ2$OE_Conduct = AllWQ2$CONDUCTIVITY - AllWQ2$PrdCond 
# AllWQ2$OE_Conductrtg=ifelse(AllWQ2$OE_Conduct <=27.1,'Good',ifelse(AllWQ2$OE_Conduct >53.7, 'Poor','Fair'))
# 
# ###Total N
# AllWQ2$OE_TN = AllWQ2$NTL - AllWQ2$Pred_TN 
# AllWQ2$OE_TNrtg=ifelse(AllWQ2$OE_TN <=52.1,'Good',ifelse(AllWQ2$OE_TN >114.7, 'Poor','Fair'))
# 
# ###Total P
# AllWQ2$OE_TP = AllWQ2$PTL - AllWQ2$Pred_TP 
# AllWQ2$OE_TPrtg=ifelse(AllWQ2$OE_TP <=9.9,'Good',ifelse(AllWQ2$OE_TP >21.3, 'Poor','Fair'))
# 
# #View(AllWQ2)
# rm(PrdWQresults,AllWQ)

###Conductivity #Modelled thresholds
IndicatorCheck$OE_ECrtg=ifelse(WQfinal$OE_EC <=27.1,'Good',ifelse(WQfinal$OE_EC >53.7, 'Poor','Fair'))

#IndicatorCheck$OE_ECrtg=ifelse(IndicatorCheck$OE_EC_CHECK <=27.1,'Good',ifelse(IndicatorCheck$OE_EC_CHECK >53.7, 'Poor','Fair'))

###Total N
IndicatorCheck$OE_TNrtg=ifelse(WQfinal$OE_TN <=52.1,'Good',ifelse(WQfinal$OE_TN >114.7, 'Poor','Fair'))
#IndicatorCheck$OE_TNrtg=ifelse(IndicatorCheck$OE_TN_CHECK <=52.1,'Good',ifelse(IndicatorCheck$OE_TN_CHECK >114.7, 'Poor','Fair'))
###Total P
IndicatorCheck$OE_TPrtg=ifelse(WQfinal$OE_TP <=9.9,'Good',ifelse(WQfinal$OE_TP >21.3, 'Poor','Fair'))
#IndicatorCheck$OE_TPrtg=ifelse(IndicatorCheck$OE_TP_CHECK <=9.9,'Good',ifelse(IndicatorCheck$OE_TP_CHECK >21.3, 'Poor','Fair'))


##Trying to Check how the EPA thresholds would cause a difference in the number of G F P 
##Need to run line ~345 "NorCalSites_Ecoregions"
#WQ3=AllWQ2
#t1=NorCalSites_Ecoregions[order(NorCalSites_Ecoregions$UID, decreasing=FALSE),]
#t2=WQ3[order(WQ3$UID, decreasing=FALSE),]
#t3=cbind(t1,t2)
#WQ3=t3[,-1] 
##Nitrogen, Phosphorus, and conductivity
#WQ3$EPA_TN=ifelse(WQ3$ECO10=='MT-PNW'& WQ3$NTL>200|WQ3$ECO10=='XE-NORTH'& WQ3$NTL>600, "Poor",ifelse(WQ3$ECO10=='MT-PNW'& WQ3$NTL<125|WQ3$ECO10=='XE-NORTH'& WQ3$NTL<200, 'Good','Fair'))
#WQ3$EPA_TP=ifelse(WQ3$ECO10=='MT-PNW'& WQ3$PTL>40|WQ3$ECO10=='XE-NORTH'& WQ3$PTL>175, "Poor",ifelse(WQ3$ECO10=='MT-PNW'& WQ3$PTL<10|WQ3$ECO10=='XE-NORTH'& WQ3$PTL<40, 'Good','Fair'))
#WQ3$EPA_Conduct=ifelse(WQ3$ECO10=='MT-PNW'& WQ3$CONDUCTIVITY>1000|WQ3$ECO10=='XE-NORTH'& WQ3$CONDUCTIVITY>1000, "Poor",ifelse(WQ3$ECO10=='MT-PNW'& WQ3$CONDUCTIVITY<500|WQ3$ECO10=='XE-NORTH'& WQ3$CONDUCTIVITY<500, 'Good','Fair'))
##Conductivity thresholds are VERY high, everything but one value is considered good. 



#Write to a csv, but I would prefer not to do this... UID issue may cause problems in the future so it would be easier to just keep the data active and in R. 
#However I did write to a csv so there was a hardcopy of the results for a backup.
#write.csv(AllWQ2, "\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\WQconditions_2014All_8Oct2014.csv")



#############################################################################

##############     Invertebrate condition determinations      ###############

#############################################################################

#All raw model results
#2014
NorCalBugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\ALL_BugModel_Results.csv")
#2016 Smoke Creek WS assessment
NorCalBugs=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\2013to2015NVMMI_Results.csv")


#Invasive bugs 
NorCalInvasives=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\InvertebrateData\\FinalInvasive_R_Input.csv")

#############################################################################
#######################     NV MODELS     ###################################
#############################################################################
#Scores below the impaired threshold imply degradation. 
#Scores greater than or equal to the equivalence threshold imply reference condition. 
#A value between the two thresholds indicates that the score cannot be definitively determined to fall within or below reference condition. (Techinically this does not mean "fair")
#Pc>0
#Impaired = 0.6020531
#Equivalence = 0.6864668

#Pc>=0.5
#Impaired = 0.6035492
#Equivalence = 0.701018

#Excel formula: =IF(N3> OR EQUAL TO 0.6864668,"Good",IF(N3<0.6020531,"Poor","Fair"))

NorCalBugs$NV_OE5_Cond=ifelse(NorCalBugs$NV_OE5 >=0.701018,'Good',ifelse(NorCalBugs$NV_OE5 <0.6035492, 'Poor','Fair'))

NorCalBugs$NV_OE0_Cond=ifelse(NorCalBugs$NV_OE0 >=0.6864668,'Good',ifelse(NorCalBugs$NV_OE0 <0.6020531, 'Poor','Fair'))


#############################################################################
#######################     CSCI MODELS     #################################
#############################################################################
#Uses 0.5 detection! Poor=(Mean-(1.65*SD)), good is .825*SD
#### Mean:  CSCI: 1.010739; O/E: 1.021478
#### StDv:  CSCI: 0.123775; O/E: 0.190266
### CSCI
# Good >=0.9086246
# Fair
# Poor <0.8067303

###O/E
# Good >=0.8645086
# Fair
# Poor <0.7075391

#Uses 0.5 detection!Poor=(Mean-(2*SD)), good is 1*SD
### CSCI
# Good >=0.886964
# Fair
# Poor < 0.763189

###O/E
# Good >=0.831212
# Fair
# Poor <0.640946

######Current recommendation: classify impairment (poor) as scores falling 1.65 SD  
######below the Mean (Mean-(1.65*SD));  This is still undergoing discussion in the 
######SWAMP group, so the traditional 2 SD approach can still be implemented. 

##########################

#Index   Likely intact        Possibly intact        Likely altered      Very likely altered
#          (≥0.30)            (0.10 to 0.30)         (0.01 to 0.10)           (<0.01)
#CSCI       ≥0.92              0.79 to 0.92	          0.63 to 0.79	       0.00 to 0.63
#pMMI	      ≥0.91	             0.77 to 0.91	          0.58 to 0.77	       0.00 to 0.58
#O/E        ≥0.90	             0.76 to 0.90	          0.56 to 0.76	       0.00 to 0.56

#            Good                              Fair                            Poor
#CSCI      >= 0.855                       <0.855 and >0.71                    < 0.71        	      
#pMMI	     >= 0.84	                      <0.84 and >0.675                    < 0.675	              
#O/E       >= 0.83	                      <0.83 and >0.66                     < 0.66	                


# My first attempt is to set thresholds based on the Mean and SD for OE and CSCI and then compare to the tabled approach. 
# However, I will using a three category approach rather than the 4 categories. To do this I will take the midpoint of 
# each middle category to create one middle category and expanding the thresholds of the "good" and "poor" categories.
# Column names: CSCI_OE  CSCI_MMI	CSCI_Hybrid

# Using the midpoint criteria 
NorCalBugs$CSCI_Hybrid_MidCond=ifelse(NorCalBugs$CSCI_Hybrid >=0.855,'Good',ifelse(NorCalBugs$CSCI_Hybrid <0.71, 'Poor','Fair'))
NorCalBugs$CSCI_MMI_MidCond=ifelse(NorCalBugs$CSCI_MMI >=0.84,'Good',ifelse(NorCalBugs$CSCI_MMI <0.675, 'Poor','Fair'))
NorCalBugs$CSCI_OE_MidCond=ifelse(NorCalBugs$CSCI_OE >=0.83,'Good',ifelse(NorCalBugs$CSCI_OE <0.66, 'Poor','Fair'))

#Using the two upper categories merged to create just 3 categories rather than 4
NorCalBugs$CSCI_Hybrid_UpperCond=ifelse(NorCalBugs$CSCI_Hybrid >=0.79,'Good',ifelse(NorCalBugs$CSCI_Hybrid <0.63, 'Poor','Fair'))
NorCalBugs$CSCI_MMI_UpperCond=ifelse(NorCalBugs$CSCI_MMI >=0.7,'Good',ifelse(NorCalBugs$CSCI_MMI <0.58, 'Poor','Fair'))
NorCalBugs$CSCI_OE_UpperCond=ifelse(NorCalBugs$CSCI_OE >=0.76,'Good',ifelse(NorCalBugs$CSCI_OE <0.56, 'Poor','Fair'))




#### Checking to make sure results make sense 
Freq1=cbind(count(NorCalBugs,var='CSCI_Hybrid_MidCond'),count(NorCalBugs,var='CSCI_MMI_MidCond'),count(NorCalBugs,var='CSCI_OE_MidCond'),count(NorCalBugs,var='NV_OE0_Cond'),count(NorCalBugs,var='NV_OE5_Cond'),count(NorCalBugs,var='NV_MMI_Cond'),count(NorCalBugs,var='CSCI_OE_UpperCond'),count(NorCalBugs,var='CSCI_Hybrid_UpperCond'),count(NorCalBugs,var='CSCI_MMI_UpperCond'))

NorCalCHECK_Bugs=subset(NorCalBugs, SiteCode=='AR-SS-8066'|SiteCode=='SU-SS-8311'|SiteCode=='AR-LS-8018'|SiteCode=='AR-SS-8017'|SiteCode=='HC-SS-8456'|SiteCode=='HC-SS-8475')
NorCalCHECK_Bugs$NicoleJudge=c('G','P','P','P','G','G')
  



####Plotting OE and MMI to compare
#Must get all scores on same scale so I've divided by the reference score Average
#CSCI MMI and NV OE Pc=0 both have an average of 1.00 so no adjustment is needed. 
NorCalBugs$NV_MMI_2=NorCalBugs$NV_MMI/56.04737
NorCalBugs$NV_OE5_2=NorCalBugs$NV_OE5/1.06
NorCalBugs$CSCI_OE_2=NorCalBugs$CSCI_OE/1.021478
NorCalBugs$CSCI_Hybrid_2=NorCalBugs$CSCI_Hybrid/1.010739

par(mfrow=c(1,3))

############  NV Models   ############
#Plot NV O/E (Pc=0) to NV MMI
plot(NorCalBugs$NV_OE0,NorCalBugs$NV_MMI_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$NV_MMI_2~NorCalBugs$NV_OE0)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$NV_MMI_2,NorCalBugs$NV_OE0))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

abline(0.4857937, 0.5143079, col='purple')
abline(v=.6020531,col="red")
abline(v=.6864668,col="orange")
abline(h=.79543437,col="red")
abline(h=.838849,col="orange")


#plot NV O/E (0.5) and NV MMI 
plot(NorCalBugs$NV_OE5_2,NorCalBugs$NV_MMI_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$NV_MMI_2~NorCalBugs$NV_OE5_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$NV_MMI_2,NorCalBugs$NV_OE5_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

abline(0.526601,0.4454208,col="purple")
abline(v=.6035492,col="red")
abline(v=.701018,col="orange")
abline(h=.79543437,col="red")
abline(h=.838849,col="orange")


load("C:\\Users\\Nicole\\Desktop\\2014_Work_NV_OE_MMI\\NV_MMI\\2014_NV_MMI\\OE_MMI_models.rdata")
MMI=data.frame(ref.MMI.score)
OE=data.frame(refOE.scores)
MMI$score=MMI$ref.MMI.score/56.047
plot(OE$OoverE,MMI$score,abline((lm(MMI$score~OE$OoverE))), ylim=c(0,1.4),xlim=c(0,1.45))
abline(0,1)




#Plot NV O/E (Pc=0) and NV O/E (0.5)
plot(NorCalBugs$NV_OE5_2,NorCalBugs$NV_OE0, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$NV_OE0~NorCalBugs$NV_OE5_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$NV_OE0,NorCalBugs$NV_OE5_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)




############  CSCI Models   ############
# Plot CSCI OE and CSCI MMI
plot(NorCalBugs$CSCI_OE_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.1), xlim=c(0,1.1),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$CSCI_OE_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$CSCI_OE_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)
# Plot CSCI OE and CSCI MMI
plot(NorCalBugs$CSCI_OE_2,NorCalBugs$CSCI_Hybrid_2, ylim=c(0,1.1), xlim=c(0,1.1),abline((lm(NorCalBugs$CSCI_Hybrid_2~NorCalBugs$CSCI_OE_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_Hybrid_2,NorCalBugs$CSCI_OE_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)
# Plot CSCI Hybrid and CSCI MMI
plot(NorCalBugs$CSCI_Hybrid_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.1), xlim=c(0,1.1),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$CSCI_Hybrid_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$CSCI_Hybrid_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)



############  NV and CSCI Models   ############
#plot NV O/E (0.5) and CSCI O/E (also a 0.5 prob of capture)
plot(NorCalBugs$NV_OE5_2,NorCalBugs$CSCI_OE_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_OE_2~NorCalBugs$NV_OE5_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_OE_2,NorCalBugs$NV_OE5_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

#plot NV O/E (0) and CSCI O/E (0.5 prob of capture)
plot(NorCalBugs$NV_OE0,NorCalBugs$CSCI_OE_2, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_OE_2~NorCalBugs$NV_OE0)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_OE_2,NorCalBugs$NV_OE0))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)

#plot NV and CSCI MMI
plot(NorCalBugs$NV_MMI_2,NorCalBugs$CSCI_MMI, ylim=c(0,1.4), xlim=c(0,1.4),abline((lm(NorCalBugs$CSCI_MMI~NorCalBugs$NV_MMI_2)), col="blue", lty="longdash"))
abline(0,1)
r2=(cor(NorCalBugs$CSCI_MMI,NorCalBugs$NV_MMI_2))^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = .1, y = 1, labels = mylabel)





################# Confusions Matrix of results ##############################
# Could also use table() but this will not label rows vs columns as xtab does. 
#NV only
NorCalBugs$NV_MMI_Cond_2=ifelse(NorCalBugs$NV_MMI_Cond=="Reference","Good",ifelse(NorCalBugs$NV_MMI_Cond=="Impaired","Poor","Fair"))
xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$NV_MMI_Cond)

xtabs(~NorCalBugs$NV_OE0_Cond+NorCalBugs$NV_MMI_Cond_2)
xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$NV_OE0_Cond)

#CSCI only
xtabs(~NorCalBugs$CSCI_OE_MidCond+NorCalBugs$CSCI_MMI_MidCond)

xtabs(~NorCalBugs$CSCI_OE_MidCond+NorCalBugs$CSCI_Hybrid_MidCond)
xtabs(~NorCalBugs$CSCI_Hybrid_MidCond+NorCalBugs$CSCI_MMI_MidCond)

#CSCI to NV
xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$CSCI_OE_MidCond)
xtabs(~NorCalBugs$NV_MMI_Cond_2+NorCalBugs$CSCI_MMI_MidCond)

xtabs(~NorCalBugs$NV_OE5_Cond+NorCalBugs$CSCI_MMI_MidCond)
xtabs(~NorCalBugs$NV_MMI_Cond_2+NorCalBugs$CSCI_OE_MidCond)

xtabs(~NorCalBugs$NV_OE0_Cond+NorCalBugs$CSCI_OE_MidCond)



# If NV=CA in concordance, if in dicordance with Good and Fair them is gets Bad_Discord, but if it is just one step of discordance it is Med_Discord
# Checked with the count function to make sure the values were the same as the xtab tables above. 
NorCalBugs$MMI_ModAgreement=ifelse(NorCalBugs$NV_MMI_Cond_2==NorCalBugs$CSCI_MMI_MidCond,"Concord",
       ifelse(NorCalBugs$NV_MMI_Cond_2=="Good" & NorCalBugs$CSCI_MMI_MidCond=="Poor"|NorCalBugs$NV_MMI_Cond_2=="Poor" & NorCalBugs$CSCI_MMI_MidCond=="Good",'Bad_Discord',
              ifelse(NorCalBugs$NV_MMI_Cond_2=="Fair" & NorCalBugs$CSCI_MMI_MidCond=="Poor"|NorCalBugs$NV_MMI_Cond_2=="Fair" & NorCalBugs$CSCI_MMI_MidCond=="Good"|NorCalBugs$NV_MMI_Cond_2=="Poor" & NorCalBugs$CSCI_MMI_MidCond=="Fair"|NorCalBugs$NV_MMI_Cond_2=="Good" & NorCalBugs$CSCI_MMI_MidCond=="Fair",
                     "Med_Discord",0)))

NorCalBugs$OE5_ModAgreement=ifelse(NorCalBugs$NV_OE5_Cond==NorCalBugs$CSCI_OE_MidCond,"Concord",
                                   ifelse(NorCalBugs$NV_OE5_Cond=="Good" & NorCalBugs$CSCI_OE_MidCond=="Poor"|NorCalBugs$NV_OE5_Cond=="Poor" & NorCalBugs$CSCI_OE_MidCond=="Good",'Bad_Discord',
                                          ifelse(NorCalBugs$NV_OE5_Cond=="Fair" & NorCalBugs$CSCI_OE_MidCond=="Poor"|NorCalBugs$NV_OE5_Cond=="Fair" & NorCalBugs$CSCI_OE_MidCond=="Good"|NorCalBugs$NV_OE5_Cond=="Poor" & NorCalBugs$CSCI_OE_MidCond=="Fair"|NorCalBugs$NV_OE5_Cond=="Good" & NorCalBugs$CSCI_OE_MidCond=="Fair",
                                                 "Med_Discord",0)))





#Second EXAMPLE TEXT IN GRAPH
#TEXT IN GRAPH
#plot(Weight~Length)
#lines(m1,col="red")
#text(locator(1), " Y=9e-07x^3.4198   R Squared = .92")



#############################################################################

##############   Physical Habitat condition determinations    ###############

#############################################################################

###### BANKS stability and cover
IndicatorCheck$BnkStability_Erosionalrtg=ifelse(IndicatorCheck$BnkStability_Erosional_CHECK>0.80,'Good',ifelse(IndicatorCheck$BnkStability_Erosional_CHECK<0.60,'Poor','Fair'))
IndicatorCheck$BnkCover_Erosional_CHECKrtg=ifelse(IndicatorCheck$BnkCover_Erosional_CHECK>0.60,'Good',ifelse(IndicatorCheck$BnkCover_Erosional_CHECK<0.40,'Poor','Fair'))
IndicatorCheck$BnkCover_StabErosionalrtg=ifelse(IndicatorCheck$BnkCover_StabErosional_CHECK>0.80,'Good',ifelse(IndicatorCheck$BnkCover_StabErosional_CHECK<0.60,'Poor','Fair'))

IndicatorCheck$BnkStability_Allrtg=ifelse(IndicatorCheck$BnkStability_All_CHECK>0.80,'Good',ifelse(IndicatorCheck$BnkStability_All_CHECK<0.60,'Poor','Fair'))
IndicatorCheck$BnkCover_All_CHECKrtg=ifelse(IndicatorCheck$BnkCover_All_CHECK>0.60,'Good',ifelse(IndicatorCheck$BnkCover_All_CHECK<0.40,'Poor','Fair'))
IndicatorCheck$BnkCover_StabAllrtg=ifelse(IndicatorCheck$BnkCover_StabAll_CHECK>0.80,'Good',ifelse(IndicatorCheck$BnkCover_StabAll_CHECK<0.60,'Poor','Fair'))


#First I need to combine Ecoregions to the sampled sites. 
#Read in ecoregion to sample sitecode
NorCalSites_Ecoregions=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\UID_SiteID_Ecoregions.csv")
#SmokeCreek WS
NorCalSites_Ecoregions=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\Rinput_2016run_NorCal_SiteID_EcoregionJoin_FromAccess.csv")

#Because UIDs get turned into different values when exported as a csv I was unable to use the merge function because UIDs did not match exactly. Be careful with using this method in the future... 
t1=NorCalSites_Ecoregions[order(NorCalSites_Ecoregions$UID, decreasing=FALSE),]
t2=IndicatorCheck[order(IndicatorCheck$UID, decreasing=FALSE),]
t3=cbind(t1,t2)
t3=t3[,-1] 
Indicators=t3[,c(4,1,2,3,5:24)]
rm(t1,t2,t3,NorCalSites_Ecoregions)


#Subset all by ecoregions... got to be a better way...
################################################################

#Level III ecoregions: Applying Thresholds

################################################################
unique(Indicators$ECO_LVL_3NAME)

Ind_PL_NCULT=subset(Indicators, ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills')
Ind_NorthBasin=subset(Indicators, ECO_LVL_3NAME=='Northern Basin and Range')
Ind_SierraNV=subset(Indicators, ECO_LVL_3NAME=='Sierra Nevada')

#Apply pH thresholds
Ind_PL_NCULT$PH_CHECKrtg=ifelse(Ind_PL_NCULT$PH_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'PH_0.10'])
                        |Ind_PL_NCULT$PH_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'PH_0.90']),"Poor",
                        ifelse(Ind_PL_NCULT$PH_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','PH_0.30']) &
                                 Ind_PL_NCULT$PH_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','PH_0.70']),"Good","Fair"))

Ind_NorthBasin$PH_CHECKrtg=ifelse(Ind_NorthBasin$PH_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'PH_0.10'])
                          |Ind_NorthBasin$PH_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'PH_0.90']),"Poor",
                          ifelse(Ind_NorthBasin$PH_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','PH_0.30']) &
                                   Ind_NorthBasin$PH_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','PH_0.70']),"Good","Fair"))

Ind_SierraNV$PH_CHECKrtg=ifelse(Ind_SierraNV$PH_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'PH_0.10'])
                        |Ind_SierraNV$PH_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'PH_0.90']),"Poor",
                        ifelse(Ind_SierraNV$PH_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','PH_0.30']) &
                                 Ind_SierraNV$PH_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','PH_0.70']),"Good","Fair"))

###### Checking how other pH thresholds would alter G, F, P classifications
### Checking CA/NV state and national standards... 
# THis was created and run out of order so it cannot just be run again.... 
#IndicatorCond_ECO_LVL_3NAME$PH_StateThresh=ifelse(IndicatorCond_ECO_LVL_3NAME$PH_CHECK<6.5|IndicatorCond_ECO_LVL_3NAME$PH_CHECK>9,"Poor","Good")
#IndicatorCond_ECO_LVL_3NAME$PH_CHECKrtg_75_25=ifelse(IndicatorCond_ECO_LVL_3NAME$PH_CHECK<7.3325|IndicatorCond_ECO_LVL_3NAME$PH_CHECK>8.365,"Poor","Good")
#IndicatorCond_ECO_LVL_3NAME$PH_CHECKrtg_5_95=ifelse(IndicatorCond_ECO_LVL_3NAME$PH_CHECK<6.769|IndicatorCond_ECO_LVL_3NAME$PH_CHECK>8.626,"Poor","Good")
#count(IndicatorCond_ECO_LVL_3NAME$PH_StateThresh)
#count(IndicatorCond_ECO_LVL_3NAME$PH_CHECKrtg)
#count(IndicatorCond_ECO_LVL_3NAME$PH_CHECKrtg_75_25)
#count(IndicatorCond_ECO_LVL_3NAME$PH_CHECKrtg_5_95)



# Eastern Cascades Slopes and Foothills
Ind_PL_NCULT$XFC_NAT_CHECKrtg=ifelse(Ind_PL_NCULT$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XFC_NAT_0.10']),"Poor",ifelse(Ind_PL_NCULT$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XFC_NAT_0.30']),"Good","Fair"))
Ind_PL_NCULT$XCMG_CHECKrtg=ifelse(Ind_PL_NCULT$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCMG_0.10']),"Poor",ifelse(Ind_PL_NCULT$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCMG_0.30']),"Good","Fair"))
Ind_PL_NCULT$XGB_CHECKrtg=ifelse(Ind_PL_NCULT$XGB_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XGB_0.90']),"Poor",ifelse(Ind_PL_NCULT$XGB_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XGB_0.70']),"Good","Fair"))
Ind_PL_NCULT$XCMGW_CHECKrtg=ifelse(Ind_PL_NCULT$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCMGW_0.10']),"Poor",ifelse(Ind_PL_NCULT$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCMGW_0.30']),"Good","Fair"))
Ind_PL_NCULT$xcdenmid_CHECKrtg=ifelse(Ind_PL_NCULT$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XCDENMID_0.10']),"Poor",ifelse(Ind_PL_NCULT$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XCDENMID_0.30']),"Good","Fair"))
Ind_PL_NCULT$LINCIS_H_CHECKrtg=ifelse(Ind_PL_NCULT$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'LINCIS_H_0.90']),"Poor",ifelse(Ind_PL_NCULT$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','LINCIS_H_0.70']),"Good","Fair"))
Ind_PL_NCULT$PCT_SAFN_CHECKrtg=ifelse(Ind_PL_NCULT$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'PCT_SAFN_0.90']),"Poor",ifelse(Ind_PL_NCULT$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','PCT_SAFN_0.70']),"Good","Fair"))
Ind_PL_NCULT$XEMBED_CHECKrtg=ifelse(Ind_PL_NCULT$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 'XEMBED_0.90']),"Poor",ifelse(Ind_PL_NCULT$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills','XEMBED_0.70']),"Good","Fair"))

# Ind_NorthBasin
Ind_NorthBasin$XFC_NAT_CHECKrtg=ifelse(Ind_NorthBasin$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XFC_NAT_0.10']),"Poor",ifelse(Ind_NorthBasin$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XFC_NAT_0.30']),"Good","Fair"))
Ind_NorthBasin$XCMG_CHECKrtg=ifelse(Ind_NorthBasin$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCMG_0.10']),"Poor",ifelse(Ind_NorthBasin$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCMG_0.30']),"Good","Fair"))
Ind_NorthBasin$XGB_CHECKrtg=ifelse(Ind_NorthBasin$XGB_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XGB_0.90']),"Poor",ifelse(Ind_NorthBasin$XGB_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XGB_0.70']),"Good","Fair"))
Ind_NorthBasin$XCMGW_CHECKrtg=ifelse(Ind_NorthBasin$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCMGW_0.10']),"Poor",ifelse(Ind_NorthBasin$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCMGW_0.30']),"Good","Fair"))
Ind_NorthBasin$xcdenmid_CHECKrtg=ifelse(Ind_NorthBasin$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XCDENMID_0.10']),"Poor",ifelse(Ind_NorthBasin$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XCDENMID_0.30']),"Good","Fair"))
Ind_NorthBasin$LINCIS_H_CHECKrtg=ifelse(Ind_NorthBasin$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'LINCIS_H_0.90']),"Poor",ifelse(Ind_NorthBasin$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','LINCIS_H_0.70']),"Good","Fair"))
Ind_NorthBasin$PCT_SAFN_CHECKrtg=ifelse(Ind_NorthBasin$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'PCT_SAFN_0.90']),"Poor",ifelse(Ind_NorthBasin$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','PCT_SAFN_0.70']),"Good","Fair"))
Ind_NorthBasin$XEMBED_CHECKrtg=ifelse(Ind_NorthBasin$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range', 'XEMBED_0.90']),"Poor",ifelse(Ind_NorthBasin$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Northern Basin and Range','XEMBED_0.70']),"Good","Fair"))


# Ind_SierraNV
Ind_SierraNV$XFC_NAT_CHECKrtg=ifelse(Ind_SierraNV$XFC_NAT_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XFC_NAT_0.10']),"Poor",ifelse(Ind_SierraNV$XFC_NAT_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XFC_NAT_0.30']),"Good","Fair"))
Ind_SierraNV$XCMG_CHECKrtg=ifelse(Ind_SierraNV$XCMG_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCMG_0.10']),"Poor",ifelse(Ind_SierraNV$XCMG_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCMG_0.30']),"Good","Fair"))
Ind_SierraNV$XGB_CHECKrtg=ifelse(Ind_SierraNV$XGB_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XGB_0.90']),"Poor",ifelse(Ind_SierraNV$XGB_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XGB_0.70']),"Good","Fair"))
Ind_SierraNV$XCMGW_CHECKrtg=ifelse(Ind_SierraNV$XCMGW_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCMGW_0.10']),"Poor",ifelse(Ind_SierraNV$XCMGW_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCMGW_0.30']),"Good","Fair"))
Ind_SierraNV$xcdenmid_CHECKrtg=ifelse(Ind_SierraNV$xcdenmid_CHECK <= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XCDENMID_0.10']),"Poor",ifelse(Ind_SierraNV$xcdenmid_CHECK>(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XCDENMID_0.30']),"Good","Fair"))
Ind_SierraNV$LINCIS_H_CHECKrtg=ifelse(Ind_SierraNV$LINCIS_H_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'LINCIS_H_0.90']),"Poor",ifelse(Ind_SierraNV$LINCIS_H_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','LINCIS_H_0.70']),"Good","Fair"))
Ind_SierraNV$PCT_SAFN_CHECKrtg=ifelse(Ind_SierraNV$PCT_SAFN_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'PCT_SAFN_0.90']),"Poor",ifelse(Ind_SierraNV$PCT_SAFN_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','PCT_SAFN_0.70']),"Good","Fair"))
Ind_SierraNV$XEMBED_CHECKrtg=ifelse(Ind_SierraNV$XEMBED_CHECK >= (Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada', 'XEMBED_0.90']),"Poor",ifelse(Ind_SierraNV$XEMBED_CHECK<(Thresholds_lvlIII [Thresholds_lvlIII$ECO_LVL_3NAME=='Sierra Nevada','XEMBED_0.70']),"Good","Fair"))


IndicatorCond_ECO_LVL_3NAME=rbind(Ind_SierraNV,Ind_NorthBasin,Ind_PL_NCULT)

rm(Ind_PL_NCULT,Ind_NorthBasin,Ind_SierraNV)


#L_XCMGW thresholds
IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECKrtg=ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Sierra Nevada'|IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK<(-0.492),"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK>=-0.261, "Good","Fair")), 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Northern Basin and Range',
                                                       ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK<(-0.444),"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$L_XCMGW_CHECK>=-0.240, "Good","Fair")),"NA"))

#W1_HALL
#When using EMAP-West 
IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECKrtg=ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Sierra Nevada'|IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Eastern Cascades Slopes and Foothills', 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK>0.90,"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK<=0.35, "Good","Fair")), 
                                                ifelse(IndicatorCond_ECO_LVL_3NAME$ECO_LVL_3NAME=='Northern Basin and Range',
                                                       ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK>0.9,"Poor",ifelse(IndicatorCond_ECO_LVL_3NAME$EMAP_W1_HALL_CHECK<=0.7, "Good","Fair")),"NA"))
#When using NRSA 
#If I am going to use this I MUST change the the way it is calculated
IndicatorCond_ECO_LVL_3NAME$NRSA_W1_HALL_CHECKrtg=ifelse(IndicatorCond_ECO_LVL_3NAME$NRSA_W1_HALL_CHECK<0.33,"Good",ifelse(IndicatorCond_ECO_LVL_3NAME$NRSA_W1_HALL_CHECK>=1.5,"Poor","Fair"))


#QR1: Not sure yet......#WI_Hall used for QR1
#QR1 doesn't have a threshold

#Add bug, INVASIVES, conductivity, and other WQ columns to the Indicator Conditions file. 
#AllWQ2
#For 2016 run# NorCalBugs$NV_MMI_Cond=NorCalBugs$MMI_Condition
NorCalBugs$NV_MMIrtg=ifelse(NorCalBugs$NV_MMI_Cond=="Reference","Good",ifelse(NorCalBugs$NV_MMI_Cond=="Impaired","Poor","Fair"))
NVMMIfinal=NorCalBugs[,c(1,2,5,6,10)]

IndicatorCond_ECO3=merge(IndicatorCond_ECO_LVL_3NAME, AllWQ2, all=TRUE)

t1=NVMMIfinal[order(NVMMIfinal$UID, decreasing=FALSE),]
t2=IndicatorCond_ECO3[order(IndicatorCond_ECO3$UID, decreasing=FALSE),]
t3=NorCalInvasives[order(NorCalInvasives$UID, decreasing=FALSE),]
IndicatorConditions_ECO3_FINAL=cbind(t1,t2,t3)


#2016 merging bug and indicators manually in excel due to having UID issues. 
#write.csv(IndicatorCond_ECO_LVL_3NAME,"IndicatorConditions2016_MinusBugs.csv")
#IndicatorConditions_ECO3_FINAL=read.csv("REAL_IndicatorConditions2016_MinusBugs.csv")
#IndicatorConditions_ECO3_FINAL$NV_MMI_Cond_2=ifelse(IndicatorConditions_ECO3_FINAL$NV_MMI_Cond=="Reference","Good",ifelse(IndicatorConditions_ECO3_FINAL$NV_MMI_Cond=="Impaired","Poor","Fair"))
g


#IndicatorConditions_ECO3_FINAL=merge(IndicatorCond_ECO3, NVMMIfinal, all=TRUE)

#IndicatorCond_subset1=IndicatorConditions_ECO3_FINAL[,c(2:5,10:40,43:54,56:57)]

#colnames(IndicatorCond_subset1)

#IndicatorCond_ExtEstSsubset=IndicatorCond_subset1[,c("UID", "NV_MMI","NV_MMIrtg","NV_Invasives","NV_Invasivesrtg",
#                                                     "OE_Conduct","OE_Conductrtg","OE_TN","OE_TNrtg","OE_TP","OE_TPrtg","PH_CHECK","PH_CHECKrtg",
#                                                     "BnkStability_BLM_CHECK","BnkStability_BLM_CHECKrtg","PCT_SAFN_CHECK","PCT_SAFN_CHECKrtg",
#                                                     "XCMG_CHECK","XCMG_CHECKrtg","XGB_CHECK","XGB_CHECKrtg","XFC_NAT_CHECK","XFC_NAT_CHECKrtg",
#                                                     "LINCIS_H_CHECK","LINCIS_H_CHECKrtg","xcdenmid_CHECK","xcdenmid_CHECKrtg")]

#Should be able to just put all column names that you want in the below code
IndicatorCond_ExtEstSsubset=IndicatorConditions_ECO3_FINAL[,c("UID", 
                                                              "NV_MMI","NV_MMI_Cond_2","NV_Invasives","NV_Invasivesrtg",
                                                    # "OE_Conduct","OE_Conductrtg",
                                                     "OE_EC_CHECK", "OE_ECrtg",
                                                     "OE_TN_CHECK","OE_TNrtg","OE_TP_CHECK","OE_TPrtg","PH_CHECK","PH_CHECKrtg",
                                                     "BnkStability_Erosional_CHECK","BnkStability_Erosionalrtg","bedPCT_SAFN_CHECK","PCT_SAFN_CHECKrtg",
                                                     "XCMG_CHECK","XCMG_CHECKrtg",
                                                    #"XGB_CHECK","XGB_CHECKrtg",
                                                     "XFC_NAT_CHECK","XFC_NAT_CHECKrtg",
                                                     "LINCIS_H_CHECK","LINCIS_H_CHECKrtg","XCDENMID_CHECK","xcdenmid_CHECKrtg")]


write.csv(IndicatorCond_ExtEstSsubset,'\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\ExtentEstimates\\NorCal_ExtEst_Input.csv')
#2016# write.csv(IndicatorCond_ExtEstSsubset,'C:\\Users\\Nicole\\Desktop\\NorCal_SmokeCreekWSassessment\\SmokeCreek_ExtEst_Input.csv')

                                             


#How many G, F, P values for each indicator... Not all indicators listed 
Freq_ECOIII=cbind(count(IndicatorCond_ECO_LVL_3NAME,var='XFC_NAT_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCMG_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XCMGW_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='L_XCMGW_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XEMBED_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='EMAP_W1_HALL_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='NRSA_W1_HALL_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='xcdenmid_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='LINCIS_H_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='PCT_SAFN_CHECKrtg'),
             count(IndicatorCond_ECO_LVL_3NAME,var='XGB_CHECKrtg'))
            
         
          
          
          
               


