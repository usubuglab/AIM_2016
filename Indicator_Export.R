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

Indicators$THRESH=paste(Indicators$ECO10,Indicators$BNK_THRESH, sep="_")#2011-2015#works with new design database as well
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
#Format for benchmark tool
data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat\\InterfaceToServeOutComputedMetrics\\indicatorXwalkLocal.csv')

#Format for AquADat inital import
data.xwalk=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Database_Development\\AquaDat\\InterfaceToServeOutComputedMetrics\\indicatorXwalkMaster2.csv')

#To Format data for editing AquADat, download latest data from ArcGISOnline and paste it into the edit worksheet of the following excel file "Z:\buglab\Research Projects\AIM\Database_Development\AquaDat\InterfaceToServeOutComputedMetrics\2017 updates\2017 updates.xlsx"
#this worksheet had the correct column headings.
#Then delete all columns except the UID and the column (s) you want to change.
#If making edits to different columns depending on the UID then a seperate file will need to be provided for each set of UIDs.
#final result should be a csv and should look like "Z:\buglab\Research Projects\AIM\Database_Development\AquaDat\InterfaceToServeOutComputedMetrics\2017 updates\myExampleEditData.csv"


#Run for all formats
data.input=Indicators
IndicatorsFinal=indicatorXwalk(data.input,data.xwalk)

write.csv(IndicatorsFinal,'IndicatorsFinalExport15March2018.csv',na="")     


#QC computed indicators by looking at boxplots by indicator and ecoregion

IndicatorsFinal=read.csv("Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\AK\\IndicatorsAK8feb2018_Alldata_boxplots_for_workshop.csv")
IndicatorsFinal=read.csv("Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\AK\\IndicatorsAK8feb2018_Alldata_boxplots_for_workshop.csv")


#IndicatorsFinal=read.csv("Z:\\buglab\\Research Projects\\AIM\\Analysis\\QC\\2017\\AK\\AK_points_ecoregions.csv")
#IndicatorsFinal$NAMC_Benchmark=IndicatorsFinal$US_L3NAME
str(IndicatorsFinal)

library (ggplot2)

IndicatorsFinalnum=IndicatorsFinalnum[,c(56:57,1:55)]

#for indicator by ecoregion & targeted
ecoregions=unique(unlist(IndicatorsFinal$EcoregionHybrid10))
for (s in 2:length(ecoregions)) {
  Ecoregionsubset=subset(IndicatorsFinal,EcoregionHybrid10==ecoregions[s])
nums=sapply(Ecoregionsubset,is.numeric)
IndicatorsFinalnum=Ecoregionsubset[,nums]
IndicatorsFinalnum$EcoregionHybrid10=Ecoregionsubset$EcoregionHybrid10
IndicatorsFinalnum$Targeted=Ecoregionsubset$Targeted
  for (f in 10:length(colnames(IndicatorsFinalnum))-2){
  png(file=paste(ecoregions[s], colnames(IndicatorsFinalnum[f]),".png"), width=2000,height=700,pointsize=15)
  boxplot(IndicatorsFinalnum[,f]~Targeted, data=IndicatorsFinalnum,xlab=paste(colnames(IndicatorsFinalnum[f]),names=paste0(Indicato$names, " (n=",b$n,")"))),lwd=3,cex.lab=2)
  dev.off()
}
}

nums=sapply(IndicatorsFinal,is.numeric)
IndicatorsFinalnum=Ecoregionsubset[,nums]
IndicatorsFinalnum$EcoregionHybrid10=Ecoregionsubset$EcoregionHybrid10
IndicatorsFinalnum$Targeted=Ecoregionsubset$Targeted

#for indicator by ecoregion/stream size
for (f in 10:length(colnames(IndicatorsFinalnum))-2){
  png(file=paste("boxplot",colnames(IndicatorsFinalnum[f]),".png"), width=2000,height=700,pointsize=10)
  boxplot(IndicatorsFinalnum[,f]~Targeted, data=IndicatorsFinalnum,xlab=paste(colnames(IndicatorsFinalnum[f])),lwd=3,cex.lab=2)
  dev.off()
}


#for indicator by ecoregion/stream size
for (f in 10:length(colnames(IndicatorsFinalnum))){
  png(file=paste("boxplot",colnames(IndicatorsFinalnum[f]),".png"), width=2000,height=700,pointsize=10)
  boxplot(IndicatorsFinalnum[,f]~NAMC_Benchmark, data=IndicatorsFinalnum,xlab=paste(colnames(IndicatorsFinalnum[f])),lwd=3,cex.lab=2)
  dev.off()
}

#for all indicators

for (f in 10:length(colnames(IndicatorsFinalnum))){
  png(file=paste("boxplot",colnames(IndicatorsFinalnum[f]),".png"), width=1000,height=700,pointsize=24)
  boxplot(IndicatorsFinalnum[,f],xlab=paste(colnames(IndicatorsFinalnum[f])),lwd=3,cex.lab=2)
  dev.off()
}

f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# define outlier as you want    
o <- function(x) {
  subset(x, x < quantile(x)[2] | quantile(x)[4] < x)
}

library(ggplot2)


#####works!!!!

IndicatorsFinal=read.csv("Z:\\buglab\\Research Projects\\AIM\\Workshops\\NAMF Data Analysis\\AK_2018\\BenchmarkScreeningDevelopment\\ALL_AK_data_for_workshop_16Feb2018_benchmark_boxplots_JC_exploration.csv")
IndicatorsFinal=subset(IndicatorsFinal, IndicatorsFinal$Targeted=='Random')
Indicators=c("NAMC_Benchmark","EcoregionHybrid10","OtherEcoregion","Targeted","BankCover","BankStability","BnkCover_Stab","BankfullWidth","BankOverheadCover","InstreamHabitatComplexity","FloodplainConnectivity","LWD_Freq","PctOverheadCover","pH","Slope","SpecificConductance","VegComplexity","PctFines","ThalwegDepthCV","Entrench")
IndicatorsFinalsub=IndicatorsFinal[,Indicators]  
IndicatorsFinalsub=subset(IndicatorsFinalsub, EcoregionHybrid10  %in% c('Alaska Range','Interior Highlands','Arctic Coastal Plain',"Interior Forested Lowlands and Uplands"))
IndicatorsFinalsub=subset(IndicatorsFinalsub, EcoregionHybrid10  %in% c('Interior Highlands','Arctic Coastal Plain'))

#IndicatorsFinalsub$EcoregionHybrid10=factor(IndicatorsFinalsub$EcoregionHybrid10, levels=levels(IndicatorsFinalsub$EcoregionHybrid10)[c(1,2,6,7)])
IndicatorsFinalsub$NAMC_Benchmark=droplevels(IndicatorsFinalsub$NAMC_Benchmark)
levels(IndicatorsFinalsub$NAMC_Benchmark)
('Beaufort Coastal Plain','Alaska Range','Yukon-Tanana Uplands','Ray Mountains')
for (s in 5:length(colnames(IndicatorsFinalsub))){
  png(file=paste("boxplot",colnames(IndicatorsFinalsub[s]),".png"), width=2000,height=700,pointsize=20)
  b=boxplot(IndicatorsFinalsub[,s]~NAMC_Benchmark*Targeted, data=IndicatorsFinalsub,xlab=paste(colnames(IndicatorsFinalsub[s])),lwd=3,cex.lab=2)
  boxplot(IndicatorsFinalsub[,s]~NAMC_Benchmark*Targeted, data=IndicatorsFinalsub,xlab=paste(colnames(IndicatorsFinalsub[s])),names=paste0(b$names,"\n(n=",b$n,")"),lwd=3,cex.lab=2)
   dev.off()
}

###########

for (s in 4:length(Indicators)) {  
ggplot(IndicatorsFinal, aes(EcoregionHybrid10,IndicatorsFinalsub[,5]))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
  stat_summary(fun.y = o, geom="point")+
  labs(x="", y = colnames(IndicatorsFinalsub[,5]))+
  ylim(0.0,2.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold") )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
}


#example new boxplot for vegComplexity
#create paired reference and targeted boxplot
png('VegComplex.png',width=1500, height=700, size=15)
IndicatorsFinal=subset(IndicatorsFinal,OtherEcoregion %in% c('Beaufort Coastal Plain','Alaska Range','Yukon-Tanana Uplands','Ray Mountains'))
  ggplot(IndicatorsFinal, aes(OtherEcoregion,VegComplexity))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
  stat_summary(fun.y = o, geom="point")+
  labs(x="", y = "VegComplexity")+
 ylim(0.0,2.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold") )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
dev.off()

png('FloodplainConnect.png',width=1500, height=700, size=15)
  ggplot(IndicatorsFinal, aes(OtherEcoregion,FloodplainConnectivity))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
    stat_summary(fun.y = o, geom="point")+
    labs(x="", y = "Floodplain Connectivity")+
    #ylim(0.0,0.5)+
    theme_classic()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank() )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
dev.off()
    
png('Entrench.png',width=1500, height=700, size=15)
  ggplot(IndicatorsFinal, aes(OtherEcoregion,Entrench))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
    stat_summary(fun.y = o, geom="point")+
    labs(x="", y = "Entrench")+
    #ylim(0.0,0.5)+
    theme_classic()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold") )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
  dev.off()

  
  png('LWD_freq.png',width=1500, height=700, size=15)      
  ggplot(IndicatorsFinal, aes(OtherEcoregion,LWD_Freq))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
    stat_summary(fun.y = o, geom="point")+
    labs(x="", y = "LWD_Freq")+
    #ylim(0.0,0.5)+
    theme_classic()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold") )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
  dev.off()
  
  png('PctFines.png',width=1500, height=700, size=15) 
    ggplot(IndicatorsFinal, aes(OtherEcoregion,PctFines))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
    stat_summary(fun.y = o, geom="point")+
    labs(x="", y = "PctFines")+
    ylim(0.0,2.5)+
    theme_classic()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold") )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
    dev.off()
    
    png('PctOverheadCover.png',width=1500, height=700, size=15)    
  ggplot(IndicatorsFinal, aes(OtherEcoregion,PctOverheadCover))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
    stat_summary(fun.y = o, geom="point")+
    labs(x="", y = "PctOverheadCover")+
    #ylim(0.0,0.5)+
    theme_classic()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank() )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
  dev.off()
  
  png(file='boxplotpH.png', width=1500,height=700,pointsize=15)  
  ggplot(IndicatorsFinal, aes(OtherEcoregion,pH))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
    stat_summary(fun.y = o, geom="point")+
    labs(x="", y = "pH")+
    #ylim(0.0,0.5)+
    theme_classic()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold") )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
  dev.off()
  
  png('Slope.png',width=1500, height=700, size=15)   
  ggplot(IndicatorsFinal, aes(OtherEcoregion,Slope))+ stat_summary(fun.data=f,geom="boxplot",fill='lightgray',colour="darkslategray") +
    stat_summary(fun.y = o, geom="point")+
    labs(x="", y = "Slope")+
    #ylim(0.0,0.5)+
    theme_classic()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold") )#see http://ggplot2.tidyverse.org/reference/theme.html for more info on changing spacing and text size
  dev.off()  
  
  
dev.off()






boxplot(FloodplainConnectivity~NAMC_Benchmark,data=IndicatorsFinal)



# # boxplot of Cal vs. Val OE scores:
# b = boxplot(OE.assess.cal$OE.scores$OoverE, OE.assess.vld$OE.scores$OoverE,names=c("calibration","validation"),border=c("red", "blue"),ylab="O/E")
# 
# boxplot(OE.assess.cal$OE.scores$OoverE, OE.assess.vld$OE.scores$OoverE,border=c("red", "blue"),ylab="O/E",names=paste0(b$names, " (n=",b$n,")"))
# abline(h=1.0,col=8,lty=3)
# 
#   png(file=paste("boxplot",".png"), width=1000,height=700,pointsize=10)
#   par(mfrow=c(10,10))
#   for (f in 10:length(colnames(IndicatorsFinalnum))){
#   boxplot(IndicatorsFinalnum[,f],xlab=paste(colnames(IndicatorsFinalnum[f])),lwd=3,cex.lab=2)
#   }
# dev.off()  