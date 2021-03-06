#run SpSurvey_DesignWeights.R and SpSurvey_ExtentEstimates.R first!
#source('SpSurvey_DesignWeights.R')
#source('SpSurvey_ExtentEstimates.R')

##EXTENT FIGURES##
#Global Figure parameters#
axissize=2#cex
GFPcol=c('firebrick','gold','lightgreen')#c('red','yellow','green')#fix to be global (category can be fed into to only generate relevant colors), needs to be in this order for current code to color good, fair, poor correctly

#Export='PNL'#options: 'PNG' (exported png); 'PNL' (saved to workspace for later panelling) ## not working as anticipated
ScaleTYPE='Percent'#options: Percent, Absolute (meaning Percentage (Segments or StreamKM same) or StreamKM ) # set to absolute by default if an extent variable
SubpopTypes=unique(results.cat$Type)#SubpopTypes=c('Districts','Utah')
SubpopSort='N'#TO DO! if SubpopSort='Y', will sort each subpopulation based on its own order (may need additional improvement for matching RelExtentPoor to RelRisk...probably saving variableORDER with a district speficic name and then calling via eval at relrisk)
ResponseInclude='Y'# set to "Y" for UTBLM 2014 report in which OE is treated as an equal metric, not the main response variable
#keep an eye on LBFXWRat, was previously sorting incorrectly in the figure generation
for (s in 1:length(SubpopTypes)){#Temporary! only all and district - length(SubpopTypes)
  SubpopStrata=as.character(unclass(unique(subset(results.cat,select=Subpopulation, subset=Type==SubpopTypes[[s]])))[[1]])
  for (t in 1:length(SubpopStrata)){
    ExtentSuffix=sprintf('%s-%s_%s',SubpopTypes[[s]],SubpopStrata[[t]],ScaleTYPE)
    if(ResponseInclude=='Y') {BarDataPoor=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]]  & Category=='Poor')
    } else if (ResponseInclude=='N'){BarDataPoor=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]]  & Category=='Poor'& Indicator %in% sprintf('%srtg',responseVAR) ==FALSE)}
    if (ScaleTYPE=='Percent'){ BarDataPoor$X= BarDataPoor$Estimate.P; 
                               BarDataPoor$UConf= BarDataPoor$UCB95Pct.P; BarDataPoor$LConf= BarDataPoor$LCB95Pct.P;
                               XmaxPoor=100#setting at 100 sometimes causes problems with large error bars
    } else{ BarDataPoor$X= BarDataPoor$Estimate.U; 
            BarDataPoor$UConf= BarDataPoor$UCB95Pct.U; BarDataPoor$LConf= BarDataPoor$LCB95Pct.U
            XmaxPoor=sum(BarDataPoor$X)
    }
    #code repeated from below, consolidate
    if(s==1 & length(SubpopStrata)==1){#set variable order for remainder
      BarDataPoor=BarDataPoor[with(BarDataPoor,order(X)),]
      variableORDER=data.frame(cbind(Indicator=as.character(BarDataPoor$Indicator), NumericOrder=seq(1,nrow(BarDataPoor))),stringsAsFactors = F)
      variableORDER$StressorV=as.character(sub('rtg','',BarDataPoor$Indicator)); variableORDER$NumericOrder=as.numeric(variableORDER$NumericOrder)
      varFIG=varConvert(variableORDER$Indicator)
      variableORDER=data.frame(cbind(variableORDER,varFIG)); variableORDER$color=as.character(variableORDER$color)
    }
    BarDataPoor=merge(variableORDER,BarDataPoor, by="Indicator",all.x=T)
    BarDataPoor=BarDataPoor[with(BarDataPoor,order(NumericOrder)),]
    if (s!=1 & SubpopSort=='Y'){BarDataPoor=BarDataPoor[with(BarDataPoor,order(X)),]}
    png(sprintf('ExtentPOOR_%s-%s.png',ExtentSuffix,selectVARchoice),width=800,height=700, bg='transparent')
    par(mar = c(2, 1,2 ,0)  ,oma = c(2,7, 1, 2),cex=axissize)   
    #modeled after Fig 23 - http://www.epa.gov/owow/streamsurvey/pdf/WSA_Assessment_May2007.pdf
    #Category barchart: http://www.epa.gov/nheerl/arm/orpages/streamorimpair.htm (cleaner examples in EMAP report)    
    BarEXTp=barplot( BarDataPoor$X,xlim=c(0,XmaxPoor),#FIX! Note 1157 total stream km seems low  (adjwgt adds to 3305, original was **)
                     xlab=ScaleTYPE,
                     names.arg= BarDataPoor$names,horiz=T,
                     col=BarDataPoor$color,las=1) #Temporary! make color global and assign specfically to variables
    title(sprintf('Extent Poor\n%s: %s',SubpopTypes[[s]],SubpopStrata[[t]])
          ,cex=.1) 
    mtext(ifelse(ScaleTYPE=='Percent','% of Stream KM','Stream KM'),side=1,line=2,cex=axissize)#not sure why xlab is not working in barplot
    BarDataPoor$UConf=ifelse(is.na(BarDataPoor$UConf),0,BarDataPoor$UConf) ; BarDataPoor$X=ifelse(is.na(BarDataPoor$X),0,round(BarDataPoor$X,1))     
    arrows(x0=BarDataPoor$LConf,x1=BarDataPoor$UConf,y0=BarEXTp,length=.1,angle=90,code=3)#use Conf or StErr? why are upper limits so much higher?
    text(y=BarEXTp,x=BarDataPoor$UConf-1, cex=.5,labels=sprintf('%s%s',BarDataPoor$X,ifelse(ScaleTYPE=='Percent','%','')),pos=4,srt=360)#Replace labels with % stream  (from Cell Proportion) 
    graphics.off()
    variablesUSE=c(variablesrtg,extentVAR)#variablesUSE=c(extentVAR,variablesrtg[[1]])#
    for (i in 1:length(variablesUSE)){
      varNAME=sub('rtg','',variablesUSE[[i]])
      if(varNAME %in% extentVAR){ScaleTYPE2='Absolute'} else{ScaleTYPE2=ScaleTYPE}
      BarData=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]] & Indicator==variablesUSE[[i]]  & Category!='Total')
      #ToDO! add in non assessed from Total before removing total
      if(variablesUSE[[i]]!=extentVAR){
        CATcheck=nrow(BarData)
        if(CATcheck<3){#if category missing
          for(f in 1:(3-CATcheck)) {
            BarData=rbind(BarData,0)
            BarData$Category[[(CATcheck+f)]]=ifelse('Fair' %in% BarData$Category==F,'Fair', ifelse('Good' %in% BarData$Category==F,"Good",'Poor'))
          }}
        BarData=BarData[with(BarData,order(Category)),]
        BarData=BarData[with(BarData,order(Category <- c('Good','Fair','Poor'),decreasing=T)),] #sort (very fickle, hence below warning)
        WARNsort=ifelse(BarData$Indicator %in% extentVAR | (BarData$Category[[1]]=='Poor' & BarData$Category[[3]]=='Good'),'',print(sprintf('SORT incorrect-%s-%s-%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]])))#I think this is corrected with the double sort, but left in just to be safe
             }
      if (ScaleTYPE2=='Percent'){BarData$X=BarData$Estimate.P;
                                BarData$UConf=BarData$UCB95Pct.P;BarData$LConf=BarData$LCB95Pct.P;
                                Xmax=100
      } else{BarData$X=BarData$Estimate.U;
             BarData$UConf=BarData$UCB95Pct.U;BarData$LConf=BarData$LCB95Pct.U;
             if(varNAME %in% extentVAR){XmaxKM=max(results.cat$UCB95Pct.U)} else{XmaxKM=max(results.cat$UCB95Pct.U[(results.cat$Indicator %in% extentVAR)==FALSE])}
             Xmax=round(XmaxKM,0)  #the highest upper confidence rounded to the nearest 1000 (actual adjusted pop size is 2500 down from the original 3300)
      }
      #custom sort order for access #BarData=BarData[with(BarData,order(Estimate.U)),] 
      #manual Xmax for access# Xmax=3000
      png(sprintf('ExtentBAR_%s_%s.png', varNAME,ExtentSuffix),width=1000,height=700, bg='transparent',pointsize = 24)#if(Export=='PNG') {}#temporarily wrapped in if, but recordplot alternative did not work
      par(mar = c(1.5, 1,3 ,1 )  ,oma = c(2, 3, 2, 2),cex=axissize)   
      #modeled after Fig 23 - http://www.epa.gov/owow/streamsurvey/pdf/WSA_Assessment_May2007.pdf
      #Category barchart: http://www.epa.gov/nheerl/arm/orpages/streamorimpair.htm (cleaner examples in EMAP report)    
      BarEXT=barplot( BarData$X,xlim=c(0,Xmax),
                      xlab=ScaleTYPE2,
                      names.arg= BarData$Category,horiz=T,col=GFPcol,las=1)
      # title(sprintf('Extent: %s\n%s: %s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]])    ,cex=.5, line=1) 
      mtext(ifelse(ScaleTYPE2=='Percent','% of Stream KM','Stream KM'),side=1,line=2,cex=axissize)#not sure why xlab is not working in barplot
      arrows(x0=BarData$LConf,x1=BarData$UConf,y0=BarEXT,length=.1,angle=90,code=3)#use Conf or StErr? why are upper limits so much higher?
      text(y=BarEXT,x=BarData$UConf, cex=.5,labels=sprintf('%s%s',round(BarData$X,1),ifelse(ScaleTYPE2=='Percent','%','')),pos=4,srt=360)#Replace labels with % stream  (from Cell Proportion) 
      text(y=0,x=0,WARNsort,cex=5,col='purple')
      # if(Export=='PNL') { assign(sprintf('Extent%s%s%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]]),recordPlot())  }#temporarily wrapped in if, but recordplot alternative did not work
      graphics.off()
      #         png(sprintf('ExtentPIE_%s_%s.png', varNAME,ExtentSuffix),width=800,height=600)
      #            par(mar = c(1.5, 1,3 ,1 ),cex=axissize)
      #           #modelled after: RelRisk Pie: http://www.epa.gov/nheerl/arm/orpages/strmorchembiol.htm
      #           pie(BarData$X,
      #                 labels=sprintf('%s%s', round(BarData$X,1),ifelse(ScaleTYPE=='Percent','%',''))
      #                 , main=sprintf('Extent: %s\n%s: %s', varNAME,SubpopTypes[[s]],SubpopStrata[[t]])
      #                 ,clockwise=T
      #                 ,col=GFPcol
      #                 )
      #           legend(x="bottomright",cex=.75,legend=rev(c(BarData$Category)),fill=rev(GFPcol))
      #           text(y=0,x=0,WARNsort,cex=5,col='purple')
      #             #legend placement is fickle! x=-.5 also works for bottom right and is unrelated to png width
      #         graphics.off()
      #could do more complex bars that stack by SubpopStrata (i.e. district)
    }}}



###-------------------Final Panelled figures (maps + extent + groups)-----------------------###
#implemented for UTBLM, not adapted for WRSA yet

#Panelling
#specify custom groupings
#for the number of groups, 
#create X number of columns for the length of the group
#create Y number of row for the number of strata (intended use = all of UT on top, districts below - but make flexible for potential future consumption for WRSA by ecoregion)
#maps yes/no for 1st column
#in original extent figures, have option for toggling titles on or off (want them off for panelling, but keep them on until confident panelling working)
#ExtentBAR_PCT_SAFN_Districts-Y_Percent.png #strata
#ExtentBAR_PCT_SAFN_Utah-Utah_Percent #all
#ExtentPIE_C1WM100_Districts-Y_Percent.png #additional var
#"M:\buglab\Research Projects\UT_BLM_Prob_Baseline\Analyses\Maps\Shaded_West.jpg" #strata map
#"M:\buglab\Research Projects\UT_BLM_Prob_Baseline\Analyses\Maps\Shaded_AllUT.jpg" #all map


library(jpeg)
#specify maps
SubPop_names=list('Utah'='UTAH','Y'='Canyon','C'='Color','G'='Green','W'='West')#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
SubPop_names2=list('UTAH'='UTAH','Canyon'='Canyon','Color'='Color','Green'='Green','West'='West')#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
mapWD="M:\\buglab\\Research Projects\\UT_BLM_Prob_Baseline\\Analyses\\Maps\\"
mapFILES=c("Shaded_AllUT",'Shaded_canYon','Shaded_Color','Shaded_Green','Shaded_West')#specify in the desired order; if large number or standard name or iterating over different strata, could extract automatically using read.files()
mapNAMES=as.character(unlist(SubPop_names))
mapLEN=length(mapFILES)
#compile map panel and save
jpeg('MapStack_Districts.jpeg',width=1000,height=(1000*mapLEN)+(100*mapLEN))
par(mar = c(0, 0, 0, 0),oma=c(20,0,20,0),#spacing of components
    mfcol=c(mapLEN,1))#vertical panelling
for (m in 1:mapLEN){
  tmp <- readJPEG(sprintf("%s%s.jpg",mapWD,mapFILES[m]))
  plot(c(0, 10), c(0,10), type="n",
       bty="n",  xlab='',ylab='',yaxt ="n", xaxt = "n")
  mtext(text=mapNAMES[m],cex=8, font=2,line=-8)
  rasterImage(tmp, 1, 1, 9, 9)
}
#save jpeg
graphics.off()
#bring map panel back in and panel with the extent figures live (readJPEG is very slow) 
mapSTACK <- readJPEG('MapStack_Districts.jpeg')



varGRPS=c('WQ','Bio','Rip','Sub')
#need to consolidate with "presentation names" and "varConvert" above
WQ=list('ECrtg'='Conductivity','TPrtg'='Phosphorus','TNrtg'='Nitrogen','AugSTrtg'='Temperature')##WQ=c('ECrtg','TPrtg','TNrtg','AugSTrtg')#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
Bio=list('OErtg'='OE')#could add additional metrics that are relevant in the random forest modelling
Rip=list('Stab2rtg'='Bank Stability','XCDENMIDrtg'='Canopy Cover','LBFXWRatrtg'='Floodplain Connectivity')
Sub=list('PCT_SAFNrtg'='% Fine Sediment','C1WM100rtg'='LWD')
#labeller function for ggplot2
facet_names=c(WQ,SubPop_names,SubPop_names2,Bio,Sub,Rip,Phys)#consider writing a conversion function for these (might already exist for this dataset, otherwise example in FGD)
labelALL <- function(variable, value){
  #labelNAME=ifelse(is.null(unlist(facet1_names[as.character(value)])),list(value),facet1_names[as.character(value)])#this isn't quite solving the subpopulation two name issue, so I've cheated and concatenated two subpop lists into facet1 - - the function works by itself but not in labeller for an unknown reason, would be nice to fix it to have a universal labeller function that reads from lists or uses the value if no match is found
  labelNAME=facet_names[as.character(value)]  
  return(paste(labelNAME,"\n",sep=''))
  
}


library(ggplot2)
library(grid)
library(gridExtra)

TextSize=100
LineSize=5
LegendLocation='bottom'


###SFS settings (UT only, no maps)
#SubPop_names=list('Utah'='UTAH'); SubPop_names2=list('UTAH'='UTAH');SubpopTypes='Utah'
#varGRPS=c('WQ','Bio','Phys')
#Phys=list('Stab2rtg'='Bank Stability','XCDENMIDrtg'='Canopy','LBFXWRatrtg'='Floodplain','PCT_SAFNrtg'='% Fines','C1WM100rtg'='LWD')
#facet_names=c(WQ,SubPop_names,SubPop_names2,Bio,Phys)
#LegendLocation='none'

varLEN=length(varGRPS)
for (g in 1:varLEN){
  varLIST=eval(parse(text=varGRPS[g]))
  variablesUSE=names(varLIST)
  BarDataGG=subset(results.cat,subset= Indicator %in% variablesUSE & Category!='Total' & Type %in% SubpopTypes)
  BarDataGG$Color=as.factor(ifelse(BarDataGG$Category=='Good','lightgreen',ifelse(BarDataGG$Category=='Fair','gold','firebrick')))
  BarDataGG$Category=factor(BarDataGG$Category,               levels = c('Poor','Fair','Good'))#BarDataGG$Order=ifelse(BarDataGG$Category=='Good',3,ifelse(BarDataGG$Category=='Fair',2,1))
  BarDataGG$Subpop=factor(SubPop_names[as.character(BarDataGG$Subpopulation)],levels=as.character(unlist(SubPop_names)))
  outputwidth=length(varLIST)+1
  rc=ggplot(BarDataGG, aes(x=Category,y=Estimate.P,fill=Color))
  rc= rc + geom_bar(stat = "identity")   + scale_fill_identity(labels=c('Most',"Moderate","Least"),guide=guide_legend(size=50,title='Condition',reverse=T,direction='horizontal')) + 
    coord_flip() + facet_grid(. +  Subpop ~ Indicator ,labeller=labelALL) + #using labeller now, but still need to replace the variable to support ordering by factor; previous: would prefer to use ggplot labeller rather than new columns, but works very inconsistently and doesn't sort - see label_WQ
    labs(y='% of Stream Length',x='') +
    geom_errorbar(aes(ymax = UCB95Pct.P, ymin =LCB95Pct.P), width = .3,colour='black',size=LineSize) + #run to here to see fully labelled panels before stripping off
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),#get rid of horizontal grid and axis
          panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),# element_line(size=LineSize/2),
          axis.line=element_line(size = LineSize/2,colour='black'),axis.ticks.x = element_line(size = LineSize/2,colour='black'),axis.ticks.length = unit( LineSize/2, "lines"),axis.text.x=element_text(colour='black'),#xaxis settings (visible)
          text=element_text(size =TextSize),
          plot.background = element_blank(), panel.background = element_blank(),#along with bg='transparent' in png device call via ggsave, this is needed to get a blank background for overlays
          #axis.text.x = element_text(size=TextSize/2),axis.title.=element_text(size =TextSize),
          strip.text.y= element_blank(),strip.background= element_blank(),#strip.text.x= element_text(size =TextSize),#discard default facet (panel) labels
          panel.margin = unit(LineSize, "lines"),plot.margin = unit(c(7,10,5,0), "lines"),#spacing between panels (not controllable differentially by x and y) and around plot
          legend.position=LegendLocation #legend location - can specify precise coordinates or 'none' to remove
          #base_size = 50#text size - ggplots have trouble scaling proportionally when exported#base_size seems to be deprecated, try rel
    ) 
  ggsave(plot=rc,filename=sprintf('ExtentPanel_%s.png',varGRPS[g]), bg="transparent", units='in', width=8*outputwidth,height=15*length(unique(BarDataGG$Subpopulation)))
  #    jpeg(sprintf('MapBarCombine%s.jpeg',varGRPS[g]),width=(1000* outputwidth),height=(1000*mapLEN)+(150*mapLEN))#width=(5*(varLEN+1)),height=(5*mapLEN),units='in',res=100)#width=(1000*(varLEN))+(100),height=(1000*mapLEN)+(100*mapLEN)
  #    grid.arrange(rasterGrob(mapSTACK) #from package gridExtra
  #                 , rc, widths=c(1/ outputwidth,length(varLIST)/outputwidth), nrow=1)
  #    graphics.off()
}
##SWJ TO DO:
###standardize ggplot 2 width feeding into grid.arrange so not so variable by # of columns #DONE
###resize ggplot text: base_size or rel or size (think I dealt with this in fgd too)#DONE
###Custom variable labels#DONE
###strip good/fair/poor axis#DONE
###only vertical lines in background#DONE
###squish grid arrange and/or try with ggplot custom annotation #http://stackoverflow.com/questions/13299496/reduce-space-between-grid-arrange-plots#DONE
###move ecoregion labels to maps? #DONE
### add percent stream length at the bottom#DONE


##RELATIVE RISK + Figures## see UTblmGRTS_WGTs_FIGs.R

#?will viewports or grid.arrange work with base graphics? --> answer for WRSA boxplots

##grid attempt
# someText <- paste("A panel of text", "produced using", "raw grid code", "that could be used", "to describe","he plot", "to the right.", sep = "\n")
# grid.rect(gp = gpar(lty = "dashed"))
# pushViewport(viewport(layout = grid.layout(1, 2, widths = unit.c(unit(1, "strwidth", someText) , unit(2, "cm"), unit(1, "null")))))
# pushViewport(viewport(layout.pos.col = 1))
# grid.rect(gp = gpar(fill = "light grey"))
# grid.text(someText, x = unit(1, "cm"), y = unit(1, "npc") - unit(1, "inches"), just = c("left", "top"))
# popViewport()
# pushViewport(viewport(layout.pos.col = 2))
# #replayPlot(ExtentTest1)
# plot(rc,vp=2)
# popViewport(2)


##split screen attempt
# split.screen(rbind(c(0.1,0.292,0.1, 0.98), c(0.312, 0.95, 0.1, 0.98)))
# screen(1)
# par(mar = c(0, 0, 0, 0))
# plot(1:30, rnorm(30), xaxs = "i", ylim = c(-3, 3), xaxt = "n")
# axis(1, at = seq(0, 30, 20))
# 
# screen(2)
# rc




## attempt to use lattice
# library(lattice)
# attach(results.cat)
# barchart(X~Category|Subpopulation*Indicator, 
#          main="Scatterplots by Cylinders and Gears", 
#          ylab="Miles per Gallon", xlab="Car Weight")

##attempt to use base graphics
# par(mfrow=c(mapLEN,varLEN),mar = c(0, 2,0 ,0),oma = c(2, 3, 2, 2))#par(mar = c(1.5, 1,3 ,1 )  ,oma = c(2, 3, 2, 2),cex=axissize)  
# #copied from EXTENT figures (would have preferred to store the figures as objects and then just call them here, but couldn't easily find a solution)
# #consider wrapping BarData and BarEXT into a function since repeated (would need toggling options for axis names and titles)
# 
# for (g in 1:varLEN){
#   variablesUSE=eval(parse(text=varGRPS[g]))
#   track=0
# 
#   for (s in 1:length(SubpopTypes)){
#   SubpopStrata=as.character(unclass(unique(subset(results.cat,select=Subpopulation, subset=Type==SubpopTypes[[s]])))[[1]])
#   for (t in 1:length(SubpopStrata)){
#     for (i in 1:length(variablesUSE)){
#       #replayPlot(eval(parse(text=sprintf('Extent%s%s%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]]))))#unfortunately, doesn't adopt new par settings for panelling
#       varNAME=sub('rtg','',variablesUSE[[i]])
#       BarData=subset(results.cat,subset=Subpopulation==SubpopStrata[[t]] & Indicator==variablesUSE[[i]]  & Category!='Total')
#       #ToDO! add in non assessed from Total before removing total
#       if(variablesUSE[[i]]!=extentVAR){
#         CATcheck=nrow(BarData)
#         if(CATcheck<3){#if category missing
#           for(f in 1:(3-CATcheck)) {
#             BarData=rbind(BarData,0)
#             BarData$Category[[(CATcheck+f)]]=ifelse('Fair' %in% BarData$Category==F,'Fair', ifelse('Good' %in% BarData$Category==F,"Good",'Poor'))
#           }}
#         BarData=BarData[with(BarData,order(Category)),]
#         BarData=BarData[with(BarData,order(Category <- c('Good','Fair','Poor'),decreasing=T)),] #sort (very fickle, hence below warning)
#         WARNsort=ifelse(BarData$Category[[1]]=='Poor' & BarData$Category[[3]]=='Good','',print(sprintf('SORT incorrect-%s-%s-%s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]])))#I think this is corrected with the double sort, but left in just to be safe
#       }
#       if (ScaleTYPE=='Percent'){BarData$X=BarData$Estimate.P;
#                                 BarData$UConf=BarData$UCB95Pct.P;BarData$LConf=BarData$LCB95Pct.P;
#                                 Xmax=100
#       } else{BarData$X=BarData$Estimate.U;
#              BarData$UConf=BarData$UCB95Pct.U;BarData$LConf=BarData$LCB95Pct.U;
#              Xmax=round(max(results.cat$UCB95Pct.U[results.cat$Indicator!='EvalStatus_Access']),-3)  #the highest upper confidence rounded to the nearest 1000 (actual adjusted pop size is 2500 down from the original 3300)
#       }
#       #modeled after Fig 23 - http://www.epa.gov/owow/streamsurvey/pdf/WSA_Assessment_May2007.pdf
#       #Category barchart: http://www.epa.gov/nheerl/arm/orpages/streamorimpair.htm (cleaner examples in EMAP report)    
#       track=track+1
#       BarEXT=barplot( BarData$X,xlim=c(0,Xmax),
#                      xaxt='n',yaxt='n',#replaced xlab with no axes
#                      names.arg= BarData$Category,horiz=T,col=GFPcol,las=1)
#       if(track<=varLEN) axis(3, label=FALSE,outer=TRUE)
#       #box()
#       axis(2,lwd.ticks=0,labels=FALSE); axis(4,lwd.ticks=0,labels=FALSE)
#       if(track>(varLEN * (mapLEN-1))) {axis(1)}
#       #title(sprintf('Extent: %s\n%s: %s',varNAME,SubpopTypes[[s]],SubpopStrata[[t]]),cex=.5, line=1) #remove once formatting complete
#       arrows(x0=BarData$LConf,x1=BarData$UConf,y0=BarEXT,length=.1,angle=90,code=3)#use Conf or StErr? why are upper limits so much higher?
#       #attempt to compensate for high percents getting knocked off page (but EPA figures don't even have the annotations)
#       # if(BarData$UConf>80){labARROWx=ifelse(BarData$UConf>80,90,BarData$UConf); #labARROWy=ifelse(BarData$UConf>80,BarEXT+.25,BarEXT)
#       #  } else {}   labARROWx=BarData$UConf; #labARROWy=BarEXT
#       #          }#Replace labels with % stream  (from Cell Proportion) 
#       #labARROWy=BarEXT
#       #text(y=labARROWy,x=labARROWx, cex=.5,labels=sprintf('%s%s',round(BarData$X,1),ifelse(ScaleTYPE=='Percent','%','')),pos=4,srt=360)
#    }
#   }
# }
# }
#    