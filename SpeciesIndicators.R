##### Non-Natives ######
speciesdata=addKEYS(tblRetrieve(Parameters=c('INVAS_COMMON_NAME'),Years=years, Projects=projects,SiteCodes=sitecodes,Insertion=insertion),c('SITE_ID'))
speciesdata$CommonName=speciesdata$RESULT
countspeciesdata=plyr::count(speciesdata,c("UID","CommonName"))


#add full list of species from each state along with scientific name #need full list of sites query unique UIDs for nonnative presence absence and join....
RipBLM=tblRetrieve(Parameters=c('CANRIPW','UNRIPW','GCRIP','INVASW', 'NATIVW','INVASH','NATIVH','SEGRUSH','INVASAQ'),Projects=projects,Years=years,Protocols=protocols,SiteCodes=sitecodes,Insertion=insertion)
FQCY_VEG=subset(RipBLM, PARAMETER == 'INVASW'|PARAMETER == 'NATIVW'|PARAMETER == 'INVASH'|PARAMETER == 'NATIVH'|
                  PARAMETER == 'SEGRUSH'|PARAMETER=='INVASAQ')
nFQCY_VEG=setNames(plyr::count(FQCY_VEG,c("UID",'PARAMETER')),c("UID",'PARAMETER',"nFQCY_VEG_CHECK"))#total=22,but collected at 5 transects=10, so min N=10
nFQCY_VEG=cast(nFQCY_VEG,"UID~PARAMETER",value="nFQCY_VEG_CHECK",fun="sum")
FQCY_VEG$RESULT_A=as.numeric(ifelse(FQCY_VEG$RESULT == 'N', 0,ifelse(FQCY_VEG$RESULT == 'Y', 100,"NA")))
FQCY_VEG=cast(FQCY_VEG,'UID~PARAMETER',value='RESULT_A',fun=mean)
FQCY_VEG=setNames(merge(nFQCY_VEG,FQCY_VEG,by="UID"),c("UID","nINVASAQ_CHECK","nINVASH_CHECK","nINVASW_CHECK","nNATIVH_CHECK","nNATIVW_CHECK","nSEGRUSH_CHECK","INVASAQ_CHECK","INVASH_CHECK","INVASW_CHECK","NATIVH_CHECK","NATIVW_CHECK","SEGRUSH_CHECK"))                  
FQCY_VEG$INVASAQ_CHECK=round(FQCY_VEG$INVASAQ_CHECK,digits=0)
FQCY_VEG$INVASW_CHECK=round(FQCY_VEG$INVASW_CHECK,digits=0)
FQCY_VEG$INVASH_CHECK=round(FQCY_VEG$INVASH_CHECK,digits=0)
FQCY_VEG$NATIVH_CHECK=round(FQCY_VEG$NATIVH_CHECK,digits=0)
FQCY_VEG$NATIVW_CHECK=round(FQCY_VEG$NATIVW_CHECK,digits=0)
FQCY_VEG$SEGRUSH_CHECK=round(FQCY_VEG$SEGRUSH_CHECK,digits=0)
FQCY_VEG$INVASAQ_CHECK=ifelse(FQCY_VEG$nINVASAQ_CHECK<10,NA,FQCY_VEG$INVASAQ_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$INVASW_CHECK=ifelse(FQCY_VEG$nINVASW_CHECK<10,NA,FQCY_VEG$INVASW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$INVASH_CHECK=ifelse(FQCY_VEG$nINVASH_CHECK<10,NA,FQCY_VEG$INVASH_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$NATIVH_CHECK=ifelse(FQCY_VEG$nNATIVH_CHECK<10,NA,FQCY_VEG$NATIVH_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$NATIVW_CHECK=ifelse(FQCY_VEG$nNATIVW_CHECK<10,NA,FQCY_VEG$NATIVW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
FQCY_VEG$SEGRUSH_CHECK=ifelse(FQCY_VEG$nSEGRUSH_CHECK<10,NA,FQCY_VEG$SEGRUSH_CHECK)#total=22,but collected at 5 transects=10, so min N=10

speciesDataFreq=addKEYS(join(FQCY_VEG,countspeciesdata, by="UID",type="left"),c('SITE_ID'))


sites=unique(speciesDataFreq$SITE_ID)

#add state info
designs=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Design\\DesignDatabase\\lkp_GRTS_SiteInfo.csv')
state=designs[,c('SITE_ID','STATE')]

specieslists=read.csv("Z:\\buglab\\Research Projects\\AIM\\Protocols\\NonNativeVeg\\Comprehensive Aquatic AIM Nonnative Riparian Plant Species List_JC2020.csv")

statespecies=join(state,specieslists,by="STATE", type="left")
statespecies=subset(statespecies, SITE_ID %in% sites)

#Allspecies=join(speciesDataFreq,statespecies,by=c("CommonName","SITE_ID"),type="left")# check for sites or species not in app
Allspecies=join(statespecies,speciesDataFreq,by=c("CommonName","SITE_ID"),type="inner")# change this to a left join if we want all species considered for a given site and then populate with 0s

AllSpeciesFinal=addKEYS(Allspecies,c('CREW_LEADER','PROJECT'))
#AllSpeciesFinal[is.na(AllSpeciesFinal$PercentPlotsPresent)]<-0

#convert to percent
AllSpeciesFinal$PercentPlotsPresent=ifelse(AllSpeciesFinal$Type2=="NativeWoody",round(AllSpeciesFinal$freq/AllSpeciesFinal$nNATIVW_CHECK*100,digits=0),
                                           ifelse(AllSpeciesFinal$Type2=="NonNativeAquatic",round(AllSpeciesFinal$freq/AllSpeciesFinal$nINVASAQ_CHECK*100,digits=0),
                                           ifelse(AllSpeciesFinal$Type2=="NonNativeHerb",round(AllSpeciesFinal$freq/AllSpeciesFinal$nINVASH_CHECK*100,digits=0),
                                                round(AllSpeciesFinal$freq/AllSpeciesFinal$nINVASW_CHECK*100,digits=0))))
AllSpeciesFinal$PercentPlotsPresent=ifelse(AllSpeciesFinal$Type2=="NativeWoody"&AllSpeciesFinal$nNATIVW_CHECK<10,NA,AllSpeciesFinal$PercentPlotsPresent)
AllSpeciesFinal$PercentPlotsPresent=ifelse(AllSpeciesFinal$Type2=="NonNativeAquatic"&AllSpeciesFinal$nINVASAQ_CHECK<10,NA,AllSpeciesFinal$PercentPlotsPresent)
AllSpeciesFinal$PercentPlotsPresent=ifelse(AllSpeciesFinal$Type2=="NonNativeHerb"&AllSpeciesFinal$nINVASH_CHECK<10,NA,AllSpeciesFinal$PercentPlotsPresent)
AllSpeciesFinal$PercentPlotsPresent=ifelse(AllSpeciesFinal$Type2=="NonNativeWoody"&AllSpeciesFinal$nINVASW_CHECK<10,NA,AllSpeciesFinal$PercentPlotsPresent)

AllSpeciesFinal=AllSpeciesFinal[,c('PROJECT','SITE_ID','UID','CommonName','PercentPlotsPresent','ScientificName','USDA_Code','NonNative','WoodyHerb','WetlandStatus')]

write.csv(AllSpeciesFinal,'SpeciesFrequencyall.csv')
