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