############################################################################################
################                  NV and NorCal Data                        ################            
############################################################################################
NVNorCalEnv=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\EnvirOutliers\\NVandNorCal_EnvData_12Nov2014.csv")
par(mfrow=c(2,2))

#NVEnv=subset(NVNorCalEnv, Project=="NVmodel")
#NorCalEnv=subset(NVNorCalEnv, Project=="NorCal")

boxplot(SQ_KM~Project, data=NVNorCalEnv,ylab="SQ_KM",main='NV',col=c("steelblue","grey"))
boxplot(log(SQ_KM)~Project, data=NVNorCalEnv,ylab="log(SQ_KM)",main='NV',col=c("steelblue","grey"))
boxplot(ELVmax_WS~Project, data=NVNorCalEnv,ylab="ELVmax_WS",main='NV',col=c("steelblue","grey"))
boxplot(ELVmin_WS~Project, data=NVNorCalEnv,ylab="ELVmin_WS",main='NV',col=c("steelblue","grey"))
boxplot(ELVmean_WS~Project, data=NVNorCalEnv,ylab="ELVmean_WS",main='NV',col=c("steelblue","grey"))
boxplot(HYDR_WS~Project, data=NVNorCalEnv,ylab="HYDR_WS",main='NV',col=c("steelblue","grey"))
boxplot(WDmax_WS~Project, data=NVNorCalEnv,ylab="WDmax_WS",main='NV',col=c("steelblue","grey"))
boxplot(Pmax_WS~Project, data=NVNorCalEnv,ylab="Pmax_WS",main='NV',col=c("steelblue","grey"))
boxplot(Pmin_WS~Project, data=NVNorCalEnv,ylab="Pmin_WS",main='NV',col=c("steelblue","grey"))
boxplot(Tmax_WS~Project, data=NVNorCalEnv,ylab="Tmax_WS",main='NV',col=c("steelblue","grey"))
boxplot(BFI_WS~Project, data=NVNorCalEnv,ylab="BFI_WS",main='NV',col=c("steelblue","grey"))
boxplot(ELVcv_PT~Project, data=NVNorCalEnv,ylab="ELVcv_PT",main='NV',col=c("steelblue","grey"))
boxplot(Pmax_PT~Project, data=NVNorCalEnv,ylab="Pmax_PT",main='NV',col=c("steelblue","grey"))
boxplot(Tmax_PT~Project, data=NVNorCalEnv,ylab="Tmax_PT",main='NV',col=c("steelblue","grey"))
boxplot(PrdCond~Project, data=NVNorCalEnv,ylab="PrdCond",main='NV',col=c("steelblue","grey"))
boxplot(Slope_WS~Project, data=NVNorCalEnv,ylab="Slope_WS",main='NV',col=c("steelblue","grey"))


############################################################################################
###############                  CSCI and NorCal Data                        ###############           
############################################################################################
CSCINorCalEnv=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\EnvirOutliers\\CSCIandNorCalEnvData_12Nov2014.csv")
CSCI_NorCal=subset(CSCINorCalEnv, SiteSet=="RefCal"|SiteSet=="")

boxplot(AREA_SQKM~Project, data=CSCI_NorCal,ylab="AREA_SQKM",main='CSCI',col=c("grey","steelblue"))
boxplot(log(AREA_SQKM)~Project, data=CSCI_NorCal,ylab="log(AREA_SQKM)",main='CSCI',col=c("grey","steelblue"))
boxplot(New_Long~Project, data=CSCI_NorCal,ylab="New_Long",main='CSCI',col=c("grey","steelblue"))
boxplot(New_Lat~Project, data=CSCI_NorCal,ylab="New_Lat",main='CSCI',col=c("grey","steelblue"))
boxplot(SITE_ELEV~Project, data=CSCI_NorCal,ylab="SITE_ELEV",main='CSCI',col=c("grey","steelblue"))
boxplot(ELEV_RANGE~Project, data=CSCI_NorCal,ylab="ELEV_RANGE",main='CSCI',col=c("grey","steelblue"))
boxplot(TEMP_00_09~Project, data=CSCI_NorCal,ylab="TEMP_00_09",main='CSCI',col=c("grey","steelblue"))
boxplot(PPT_00_09~Project, data=CSCI_NorCal,ylab="PPT_00_09",main='CSCI',col=c("grey","steelblue"))
boxplot(SumAve_P~Project, data=CSCI_NorCal,ylab="SumAve_P",main='CSCI',col=c("grey","steelblue"))
boxplot(KFCT_AVE~Project, data=CSCI_NorCal,ylab="KFCT_AVE",main='CSCI',col=c("grey","steelblue"))
boxplot(BDH_AVE~Project, data=CSCI_NorCal,ylab="BDH_AVE",main='CSCI',col=c("grey","steelblue"))
boxplot(MgO_Mean~Project, data=CSCI_NorCal,ylab="MgO_Mean",main='CSCI',col=c("grey","steelblue"))
boxplot(CaO_Mean~Project, data=CSCI_NorCal,ylab="CaO_Mean",main='CSCI',col=c("grey","steelblue"))
boxplot(PRMH_AVE~Project, data=CSCI_NorCal,ylab="PRMH_AVE",main='CSCI',col=c("grey","steelblue"))
boxplot(S_Mean~Project, data=CSCI_NorCal,ylab="S_Mean",main='CSCI',col=c("grey","steelblue"))
boxplot(LPREM_mean~Project, data=CSCI_NorCal,ylab="LPREM_mean",main='CSCI',col=c("grey","steelblue"))
boxplot(P_MEAN~Project, data=CSCI_NorCal,ylab="P_MEAN",main='CSCI',col=c("grey","steelblue"))
boxplot(N_MEAN~Project, data=CSCI_NorCal,ylab="N_MEAN",main='CSCI',col=c("grey","steelblue"))
boxplot(PCT_SEDIM~Project, data=CSCI_NorCal,ylab="PCT_SEDIM",main='CSCI',col=c("grey","steelblue"))

summary(subset(CSCI_NorCal, Project=='CSCImodel', select=PCT_SEDIM))
summary(subset(CSCI_NorCal, Project=='NorCal', select=PCT_SEDIM))
