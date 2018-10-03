####################### Report Boxplot code ###################
#The purpose of this code is to create boxplots to display raw 
#indicator values for an appendix of reports to go along with 
#extent estimates.

##Programer: Jennifer Courtwright
##Created: 10-1-18

###############################################################

##### Read in and format sampled data ######

#set working directory to be desired analysis folder
setwd('Z:\\buglab\\Research Projects\\AIM\\Projects\\Colorado\\StatewideAssessment\\Anaysis\\Weights_ExtentEstimates\\Benchmark Boxplots')

#run the extent estimate code to merge the benchmark tool output with the reporting unit info from designs, then write out the SiteInfo file
sampled=read.csv('Z:\\buglab\\Research Projects\\AIM\\Projects\\Colorado\\StatewideAssessment\\Anaysis\\Weights_ExtentEstimates\\SiteInfo.csv')
str(sampled)

#get column names from benchmark tool to match EPA reference data
names=gsub("_CHECK$","",colnames(sampled))
sampled=setNames(sampled,names)
sampled$PCT_SAFN=sampled$allPCT_SAFN2
sampled$REALM=sampled$Protocol

#subset and clean data
sampled=subset(sampled,TNT=="TS")# only sampled sites
sampled$TP_PRED=ifelse(is.na(sampled$PTL)==FALSE, sampled$TP_PRED,NA)#exclude predicted TP values when no observed value is present
sampled$TN_PRED=ifelse(is.na(sampled$NTL)==FALSE, sampled$TN_PRED,NA)#exclude predicted TN values when no observed value is present
sampled$EC_PRED=ifelse(is.na(sampled$CONDUCTIVITY)==FALSE, sampled$EC_PRED,NA)#exclude predicted EC values when no observed value is present

#set up desired reporting unit comparisons
sampled$POPULATION=sampled$ReportingUnit2
sampled2=sampled
sampled2$POPULATION="CO"
sampled=rbind(sampled,sampled2)

unique(sampled$POPULATION)
###input desired population names and order####
#this seems repeative of "names" argument in boxplots below...need this here to get order of categories correct but could also change names here?
sampled$POPULATION=ordered(sampled$POPULATION,levels=c("CO", "ROCKY MOUNTAIN DISTRICT OFFICE", "SOUTHWEST DISTRICT OFFICE","NORTHWEST DISTRICT OFFICE"))     


######## WQ #######
population=unlist(unique(sampled$POPULATION))
for (p in 1:length(population)){
  population2=population[p]
  sampledWQ=subset(sampled,POPULATION==population2)
  
png(file=paste("TP",population[p],".png"), width=900,height=750,pointsize=24)
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
a = boxplot(sampledWQ$PTL+0.1,sampledWQ$TP_PRED+0.1,log="y",names=c("",""),main="Total Phosphorous",col=c("white","lightgrey"),ylab=expression(paste("TP (",mu, "g/L)")), plot=FALSE)
boxplot(sampledWQ$PTL+0.1,sampledWQ$TP_PRED+0.1,log="y",names=c("",""),main="Total Phosphorous",col=c("white","lightgrey"),ylab=expression(paste("TP (",mu, "g/L)")))
names=c("Observed","Predicted")
axis(1,at=1:2,c(paste0(names, "\n(n=",a$n,")")),line=0.5,lwd=0)
data.diff = sampledWQ$PTL - sampledWQ$TP_PRED
summary(data.diff)
b = boxplot(data.diff,names=c(""),main="Observed - Predicted TP",ylab=expression(paste("TP (",mu, "g/L)")),show.names=TRUE,plot=FALSE)
boxplot(data.diff,names=c(""),main="Observed - Predicted TP",ylab=expression(paste("Observed - Predicted TP (",mu, "g/L)")),show.names=TRUE)
names=c("Observed - Predicted TP")
axis(1,at=1:2,c(paste0(names, "\n(n=",a$n,")")),line=0.5,lwd=0)
mtext("Total Phosphorus", outer = TRUE, cex = 1.5)
dev.off()


png(file=paste("TN",population[p],".png"), width=900,height=750,pointsize=24)
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
a = boxplot(sampledWQ$NTL,sampledWQ$TN_PRED,log="y",names=c("",""),main="Total Nitrogen",col=c("white","lightgrey"),ylab=expression(paste("TN (",mu, "g/L)")),plot=FALSE)
boxplot(sampledWQ$NTL,sampledWQ$TN_PRED,log="y", names=c("",""),main="Total Nitrogen",col=c("white","lightgrey"),ylab=expression(paste("TN (",mu, "g/L)")))
names=c("Observed","Predicted")
axis(1,at=1:2,c(paste0(names, "\n(n=",a$n,")")),line=0.5,lwd=0)
data.diff = sampledWQ$NTL - sampledWQ$TN_PRED
summary(data.diff)
b = boxplot(data.diff,names=c(""),main="Observed - Predicted TN",ylab=expression(paste("TN (",mu, "g/L)")),show.names=TRUE,plot=FALSE)
boxplot(data.diff,names=c(""),main="Observed - Predicted TN",ylab=expression(paste("Observed - Predicted TN (",mu, "g/L)")),show.names=TRUE)
names=c("Observed - Predicted TN")
axis(1,at=1:2,c(paste0(names, "\n(n=",a$n,")")),line=0.5,lwd=0)
mtext("Total Nitrogen", outer = TRUE, cex = 1.5)
dev.off()


png(file=paste("SC",population[p],".png"), width=900,height=750,pointsize=24)
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
a = boxplot(sampledWQ$CONDUCTIVITY,sampledWQ$EC_PRED,names=c("",""),main="Specific Conductance",col=c("white","lightgrey"),ylab=expression(paste("SC (",mu, "S/cm)")),plot=FALSE)
boxplot(sampledWQ$CONDUCTIVITY,sampledWQ$EC_PRED,names=c("",""),main="Specific Conductance",col=c("white","lightgrey"),ylab=expression(paste("SC (",mu, "S/cm)")))
names=c("Observed","Predicted")
axis(1,at=1:2,c(paste0(names, "\n(n=",a$n,")")),line=0.5,lwd=0)
data.diff = sampledWQ$CONDUCTIVITY - sampledWQ$EC_PRED
summary(data.diff)
b = boxplot(data.diff,names=c(""),main="Observed - Predicted SC",ylab=expression(paste("SC (",mu, "S/cm)")),show.names=TRUE,plot=FALSE)
boxplot(data.diff,names=c(""),main="Observed - Predicted SC",ylab=expression(paste("Observed - Predicted SC (",mu, "S/cm)")),show.names=TRUE)
names=c("Observed - Predicted SC")
axis(1,at=1:2,c(paste0(names, "\n(n=",a$n,")")),line=0.5,lwd=0)
mtext("Specific Conductance", outer = TRUE, cex = 1.5)
dev.off()

}


########   Bugs  ########

#OE
png(file="OEboxplot.png", width=900,height=700,pointsize=24)
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
a = boxplot(sampled$O,sampled$E,names=c("O","E"),main="Macroinvertebrates",col=c("white","lightgrey"),ylab="Macroinvertebrates",plot=FALSE)
boxplot(sampled$O,sample$E,names=paste0(a$names, " (n=",a$n,")"),main="Macroinvertebrates",col=c("white","lightgrey"),ylab="Macroinvertebrates")
data.diff = sampled$OE
summary(data.diff)
b = boxplot(data.diff,names=c("O/E Macroinvertebrates"),main="O/E Macroinvertebrates",ylab="O/E Macroinvertebrates",show.names=TRUE,plot=FALSE)
boxplot(data.diff,names=paste0(b$names, " \n(n=",b$n,")"),main="O/E Macroinvertebrates",ylab="O/E Macroinvertebrates",show.names=TRUE)
mtext("Macroinvertebrates", outer = TRUE, cex = 1.5)
dev.off()

#MMI
#option1
biotype=c("Biotype1","Biotype2","Biotype3")
png(file="MMI_by_reporting_unit.png", width=1200,height=700,pointsize=24)
par(mfrow=c(1,3))
for (b in 1:length(biotype)){
sampledBug=subset(sampled,MODEL==paste("CO_EDAS-",biotype[b],sep=""))
boxplot(OE~POPULATION,names=c('CO','RMD','SWD','NWD'),main=biotype[b],data=sampledBug,ylab="CO MMI Score",ylim=c(10,85))
}
dev.off()

##option2
png(file="MMI_statewide.png", width=900,height=700,pointsize=24)
boxplot(OE~MODEL,names=c("Biotype1","Biotype2","Biotype3"),data=sampled,ylab="CO MMI Score",ylim=c(10,85))
dev.off()


####  Best professional judgment indicators  #####

#input desired indicators and axis labels but they must be put in the same order
#population should always stay in both
indicators=c("POPULATION","PH","BnkCover_StabErosional","BnkCover_Erosional","BnkStability_Erosional","BNK_HT_RATIO","INVASW")
axislabels=c("POPULATION","pH (SU)","Bank Cover/Stability (%)","Bank Cover (%)","Bank Stability (%)","Bank Height Ratio (unitless)","% NonNative Woody Vegetation")
#specify y axis min and max, must be in same order as indicators and axislabels!!
min=c(1,5,0,0,0,1,0)
max=c(200,10,100,100,100,13,100)

sampledBestProf=sampled[,indicators]
for (i in 2:length(indicators)){
  png(file=paste(indicators[i],".png"), width=900,height=700,pointsize=24)
  par(mfrow=c(1,1), oma = c(0, 0, 2, 0),mgp=c(3,1.5,0))#middle argument of mgp controls where the x and y axis labels are
  a=boxplot(sampledBestProf[,i]~POPULATION,names=c('CO','RMD','SWD','NWD'), ylim=c(min[i],max[i]),data=sampledBestProf,plot=FALSE) ###change names to desired names for each analysis       
  boxplot(sampledBestProf[,i]~POPULATION,ylab=axislabels[i],names=paste0(a$names, " \n(n=",a$n,")"), ylim=c(min[i],max[i]),data=sampledBestProf)        
    dev.off()
  }


######    PHAB indicators with EPA data benchmarks   ######

##read in EPA reference data, this should be the "revised_EPAreference.csv" export from the end of the EPA_reference_site_rescreening.R file
reference=read.csv("Z:\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\RevisingThresholds\\revised_EPAreference.csv")

##get reference data and sampled data in similar formats
#make data more manageable and subset only relevant columns
reference=reference[,c("SITE_ID","ECO10","EPA_hybird","Ecoregion_spelledout","REALM","BNK_THRESH","THRESH3","PCT_SAFN","LINCIS_H","XCDENBK","XFC_NAT","XCMG","XCMGW")]
#identify data as reference
reference$POPULATION="R"
#subset sampled data to only columns needed and make sure these match reference data
sampled=sampled[,c("POPULATION","SITE_ID","Ecoregion_spelledout","REALM","BNK_THRESH","THRESH3","PCT_SAFN","LINCIS_H","XCDENBK","XFC_NAT","XCMG","XCMGW")]
#merge reference and sampled data and for any columns that don't match append and fill with NA
data=rbind.fill(reference,sampled)

##input desired population names and order##
unique(data$POPULATION)
#should just be same as above but with "R" added
#needed to get categories in desired order but labels are specified in "names" argument below...seems like we could combine these two steps here
data$POPULATION=ordered(data$POPULATION,levels=c("R","CO", "ROCKY MOUNTAIN DISTRICT OFFICE", "SOUTHWEST DISTRICT OFFICE","NORTHWEST DISTRICT OFFICE"))     

##input desired indicators and axis labels but must be in same order
#population should always stay in list
indicators=c("POPULATION","THRESH3","BNK_THRESH","PCT_SAFN","LINCIS_H","XCDENBK","XFC_NAT","XCMG","XCMGW")
axislabels=c('POPULATION','THRESH3','BNK_THRESH','% Fine Sediment','Floodplain Connectivity (unitless)','% Bank Overhead Cover','Instream Habitat Complexity (unitless)','Vegetative Complexity (unitless)','Woody Vegetative Complexity (unitless)')
#specify y axis min and max, must be in same order as indicators and axislabels!!
min=c(1,1,1,0,-1,0,0,0,0)
max=c(200,200,200,100,1,100,2.3,3.5,2.6)


##create boxplots after changing "names" argument below for a given analysis
data=data[,indicators]
ecoregions=unlist(unique(sampled$Ecoregion_spelledout))
ecoregions=subset(ecoregions,ecoregions!='RangelandPlains')#only for this analysis

for (i in 4:length(indicators)){
      for (e in 1:length(ecoregions)){
      
      data1=data[grep(ecoregions[e],data$THRESH3),]#subset data by ecoregion but this is written so that it includes other ecoregions for boatable benchmark categories
      streamsize=c("SmallWadeable","LargeWadeable","Boatable")#must match BNK_THRESH categories
      
      png(file=paste(indicators[i],"_",ecoregions[e],".png"),width=2000,height=800,pointsize =34)
      par(mfrow=c(1,3), oma = c(0, 0, 2, 0),mgp=c(3,1.5,0))
            for (s in 1:length(streamsize)){
              data2=data1[data1$BNK_THRESH==streamsize[s],]
            a=boxplot(data2[,i]~POPULATION,col=c("lightgrey","white","white","white","white"),names=c('R','CO','RMD','SWD','NWD'), ylim=c(min[i],max[i]), data=data2,plot=FALSE)#specify desired labels for reporting units for this analysis        
            boxplot(data2[,i]~POPULATION,main=streamsize[s],ylab=axislabels[i],col=c("lightgrey","white","white","white","white"),ylim=c(min[i],max[i]),names=paste0(a$names, " \n(n=",a$n,")"), data=data2)        
            mtext(ecoregions[e], outer = TRUE, cex = 1.5)        
            }
      dev.off()
      }

}