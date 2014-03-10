#Failed site analysis of 2013 sites to inform prescout for future years
#watersheds delineated and stats computed by Ryan Lokteff in January 2014. Metadata for specific variables at GIS\Projects\PrescoutData


GISfiles=c('SegmentOutput2.csv',#custom requested stats at the segment (_S) and watershed/catchment (_C) levels
           'AtmosFinal.csv','DamsetcFinal.csv','EcoregionsFinal.csv','GeologyFinal.csv','LandCovFinal.csv', 'MetricsFinal.csv', 'SlopeFinal.csv', #standard "kitchen sink" stats at the watershed level (missing Climate)
          'SegmentDataAll2_26_14.csv',#standard "kitchen sink" stats at the segment scale
           'ClimateFinal.csv') #missing WS climate added here to not alterate number notes for running subsets
removeCOL=c('StreamName', 'MidpointLa', 'MidpointLo', 'XsiteLatDD', 'XsiteLongD', 'SampleCrew', 'Reason', 'Comments','rolled')
sameCOL=c('SiteCode','SampledRes','Sampleable','Reason1','SiteID2')
for (f in 1:length(GISfiles)){
  GISread=read.csv(GISfiles[f])
  GISread=GISread[,!(names(GISread) %in% removeCOL)]
  if (f==1){GISattrbIN=GISread} else{GISattrbIN=merge(GISattrbIN,GISread, by=sameCOL)}# f7 made it go to zero, f8 made it go to 33 obs 
}

attrbNAMES=colnames(GISattrbIN)
attrbNAMES=attrbNAMES[attrbNAMES!=sameCOL]
#attrbNAMES=attrbNAMES[[1]]# attrbNAMES="QExcedProb" #attrbNAMES=attrbNAMES[1:5]#ATTRIBUTE subsets
##this was cleanup for SegmentDataAll1.csv only; need to cleanup for mashed dataset
#remove shared but irrelevant columns (midpoint, etc) earlier in the loop import
#attrbNAMES=colnames(GISattrbIN)[14:ncol(GISattrbIN)]
#attrbNAMES=attrbNAMES[attrbNAMES != "State"]


#Failure determined by crews (Sampleable) and reevaluated by Jennifer (Reason,Reason1) - additional review and categorization is planned
ReasonTypes=c('Sampleable','Reason1','ReasonLUMP','Climate','Size')#'State.y','Basin','Ecoregion3', #State and Ecoreg to fine and uninterpretable to have much use, but could be good for targetting problem areas - most problems are very site specific
GISattrbIN$Climate=toupper(substr(GISattrbIN$SiteCode,1,2))
GISattrbIN$Size=toupper(substr(GISattrbIN$SiteCode,4,5))
##specific ones to look into: Partially wadeable - did they actually sample this site?
##why do some "Dry-Not Visited" sites have a crew and xsite? Dry-Visited is more confident
#EPA groupings of sampleability are narrowed down to 3 in final GRTS analysis: Non-Target (dry, wetland), Target (Successfully sampled), and Inaccessible (Unknown)
GISattrbIN$ReasonLUMP=ifelse(GISattrbIN$Reason1 %in% c('WA) Wadeable', 'BO) Boatable' ,'PW) Partial - Wadeable'), 'TARGET',
                             ifelse(GISattrbIN$Reason1 %in% c('WT) Wetland', 'IP) Impounded','DV) Dry - Visited','DN) Dry - Not Visited'), 'NONTARGET',
                                    'INACCESSIBLE'))
                                    

for (a in 1:length(attrbNAMES) ){#length(attrbNAMES) ){#9){#8:13 = StreamDensity #1:2 = Area; #3:4 = slope #5:7 = Roads #14:20 = interesting landcover (seg) #21:25 uninteresting landcover (seg) #26:33 = interesting landcover (ws) #34:39 uninteresting landcover (ws); #40:41 = QEexced; #42:45 = landownership #51:56 = dam spring mine WS; #165:170 = dam spring mine seg;#68:69 and 179:180 = Ag/Urb #112 and 224 = alfisols #100 and 212 = flowmx #3:4 117:118 and 185:186 = slope #126:164 = climate (seg)#229:267 - WS climate #MeanP_WS = 258; SumAveP_WS = 260; Pmax_WS = 242; Wdmax_WS=234/131 #bfi=115/227
  #boxplot
  GISattrbIN$VAR=GISattrbIN[[attrbNAMES[a]]]
  if (class(GISattrbIN$VAR) %in% c('numeric','integer')){#could have the corresponding else to do something with categorical variables
  for (r in 1:length(ReasonTypes)){
    GISattrbIN$REAS=GISattrbIN[[ReasonTypes[r]]]
    DataSet=subset(GISattrbIN,select=c('VAR','REAS'))
    boxplot(DataSet$VAR~substr(DataSet$REAS,1,3),main=paste(attrbNAMES[a],ReasonTypes[r]))
  #data tests
  ##normality or bimodality
  ##signal to noise ratio or CV
}}}

graphics.off()


#manual investigations of specific groups
#slope--> MP-LS-2004, MS-LS-3004, MS-SS-3108, XE-SS-5125, XN-SS-4098  (?MP-SS-2092, XE-LS-5018)#subset(GISattrbIN,select=c(SiteCode,SlpMeanS,SlpNHDS,SlpMeanWS,SlpNHD,SlopeMeanS,SlopeMeanC),subset=SiteCode %in% c('MP-LS-2004', 'MS-LS-3004', 'MS-SS-3108', 'XE-SS-5125', 'XN-SS-4098',  'MP-SS-2092','XE-LS-5018' )) #subset(GISattrbIN,select=c(SiteCode,SlpMeanS,SlpNHDS,SlpMeanWS,SlpNHD,SlopeMeanS,SlopeMeanC),subset=SlopeMeanS>60) #subset(GISattrbIN,select=c(SiteCode,SlpMeanS,SlpNHDS,SlpMeanWS,SlpNHD,SlopeMeanS,SlopeMeanC),subset=substr(Reason1,1,2)=="PI") 
# #subset(GISattrbIN,select=c(SiteCode,MeanP_WS,SumAveP_WS,Pmax_WS,Wdmax_WS),subset=substr(Reason1,1,1)=="D") 
#cor(subset(GISattbIN,select=c(Wdmax_WS,Pmax_WS, SumAveP_WS,MeanP_WS)))
#cor(subset(GISattrbIN,select=c(Hydr_WS,BFI_WS,FlowMx,QExProbC)))

