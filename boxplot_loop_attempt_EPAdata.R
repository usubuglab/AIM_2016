library(ggplot2)
EPA_referencedata=read.csv('Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\withoutdup_first_visit_for_boxplots.csv')
str(EPA_referencedata)
unique(EPA_referencedata$Instream)

RiparianVariables=c("THRESH3_boxplot","THRESH2_boxplot","EcoregionHybrid10","InstreamHabitatComplexity","Pctfines","FloodplainConnectivity","LWD_Freq")
#SedimentVariables=c("PctOverheadCover","BankOverheadCover","RiparianVegComplexity")
RiparianVariables=c("NAMC_Benchmark","EcoregionHybrid10","InstreamHabitatComplexity","Pctfines","FloodplainConnectivity","LWD_Freq")



EPA_referencedata1=EPA_referencedata[,RiparianVariables] 
ecoregions=unique(unlist(EPA_referencedata1$EcoregionHybrid10))

for (s in 1:length(ecoregions)) {
  EPAsubset=subset(EPA_referencedata,EcoregionHybrid10==ecoregions[s])
   EPA_referencedata1=EPAsubset[,RiparianVariables] 
 for (f in 3:length(EPA_referencedata1)) { 
   png(file=paste("boxplot",ecoregions[s],colnames(EPA_referencedata1[f]),".png"), width=1000,height=700,pointsize=24)
  boxplot(EPA_referencedata1[,f]~NAMC_Benchmark,data=EPAsubset)
   dev.off()
   }
}


for (s in 1:length(ecoregions)) {
  EPAsubset=subset(EPA_referencedata,EcoregionHybrid10==ecoregions[s])
  EPA_referencedata1=EPAsubset[,RiparianVariables] 
  for (f in 4:length(EPA_referencedata1)) { 
    png(file=paste("boxplot",ecoregions[s],colnames(EPA_referencedata1[f]),".png"), width=1000,height=700,pointsize=24)
    boxplot(EPA_referencedata1[,f]~NAMC_Benchmark,data=EPAsubset)
    dev.off()
  }
}




  
png(file=paste("boxplot.png"), width=1000,height=700,pointsize=4)
par(mfrow=c(5,5)) 
 for (f in 3:length(EPA_referencedata1)) { 
    boxplot(EPA_referencedata1[,f]~THRESH3_boxplot,data=EPA_referencedata1)
  }
  dev.off()
  
boxplot.formula(EPA_referencedata1$InstreamHabitatComplexity)

png(file=paste("boxplot",colnames(EPA_referencedata1[f]),".png"), width=1000,height=700,pointsize=4)

stressorsVAR2=c("NTL_CHECK","PTL_CHECK","CONDUCTIVITY_CHECK","PH_CHECK","allPCT_SAFN2_CHECK","LINCIS_H_CHECK","XCDENBK_CHECK","XFC_NAT_CHECK","BnkCover_StabErosional_CHECK","XCMG_CHECK")
axislabels=c('Total Nitrogen (ug/L)','Total Phosphorus (ug/L)','Specific Conductance (uS/cm)','pH','% Fine Sediment','Floodplain Connectivity (unitless)','% Bank Overhead Cover','Instream Habitat Complexity (unitless)','% Banks Stable and Covered','Vegetative Complexity (unitless)')

ResponseInfo2=ResponseInfo[,c(stressorsVAR2)]
str(ResponseInfo2)
for (f in 1:length(ResponseInfo2)){
  png(file=paste("boxplot",stressorsVAR2[f],".png"), width=1000,height=700,pointsize=24)
  boxplot(ResponseInfo2[,f],xlab=paste(axislabels[f]),lwd=3,cex.lab=2,horizontal=TRUE)
  dev.off()
}

epadata=read.csv("Z:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\EPA_Data\\withoutdup_first_visit_for_boxplots.csv")
str(epadata)
boxplot(PctOverheadCover~NAMC_Benchmark, data=epadata)boxplot(PctOverheadCover~NAMC_Benchmark, data=epadata)boxplot(PctOverheadCover~NAMC_Benchmark, data=epadata)boxplot(PctOverheadCover~NAMC_Benchmark, data=epadata)boxplot(PctOverheadCover~NAMC_Benchmark, data=epadata)boxplot(RiparianVegComplexity~NAMC_Benchmark, data=epadata)boxplot(BankOverheadCover~NAMC_Benchmark, data=epadata)