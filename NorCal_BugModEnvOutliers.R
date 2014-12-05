# To make graphs 4 on a page(2 by 2)
par(mfrow=c(2,2))



############################################################################################
################                  NV and NorCal Data                        ################            
############################################################################################
NVNorCalEnv=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\EnvirOutliers\\NVandNorCal_EnvData_12Nov2014.csv")

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




############################################################################################
############################################################################################
############################################################################################

#Ordinations Try

NV_NC_Env=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\EnvirOutliers\\NVandNorCal_EnvData_12Nov2014.csv')


library("vegan", lib.loc="~/R/win-library/2.15")

#Add a column using JUST numbers
NV_NC_Env$UID_1=1:238
#Put that column first
NV_NC_Env=NV_NC_Env[,c(25,1:24)]
#Make a reference for what numbers go to which sites
UID_1_Ref=NV_NC_Env[,1:5]

# Remove the unneeded columns
NV_NC_EnvInput=NV_NC_Env[,c(1,6:20)]


#Standardize the data. Divide each observation by the column mean
#Arg, off to excel
#Also deleted the UID column in excel otherwise it gets used as an environmental variable
#nv1= standardized data by mean. 
#nv2= SQ_KM was log+1, Sope_WS log+3, then standardaized data by mean
nv1=read.csv('C:\\Users\\Nicole\\Desktop\\NV1.csv')
nv2=read.csv('C:\\Users\\Nicole\\Desktop\\NV2.csv')
nv22=nv2[,2:16]

########################
######    NMDS    ######

#### Try 1 Data has only been standardized by mean
ord_m=metaMDS(nv1)
plot(ord_m)
#Species=columns Sites=rows
#plot(ord_m, type="t", display='species')
plot(ord_m, type="n")
UID_1_Ref$color=ifelse(UID_1_Ref$Project=='NVmodel','blue','red')
points(ord_m, display = 'sites', col= UID_1_Ref[,6])
ordihull(ord_m, UID_1_Ref$Project, col = "green", lty = 1, lwd=1)

#### Try 2 with transormed sqkm and slope
ord_m2=metaMDS(nv22)
plot(ord_m2)
#Species=columns Sites=rows
#plot(ord_m2, type="t", display='species')
plot(ord_m2, type="n")
UID_1_Ref$color=ifelse(UID_1_Ref$Project=='NVmodel','blue','red')
points(ord_m2, display = 'sites', col= UID_1_Ref[,6])
ordihull(ord_m2, UID_1_Ref$Project, col = "green", lty = 1, lwd=1)

#Remove tmax_pt
nv23=nv22[,c(1:12,14:15)]
ord_m3=metaMDS(nv23)
plot(ord_m3)
#Species=columns Sites=rows
#plot(ord_m3, type="t", display='species')
plot(ord_m3, type="n")
UID_1_Ref$color=ifelse(UID_1_Ref$Project=='NVmodel','blue','red')
points(ord_m3, display = 'sites', col= UID_1_Ref[,6])
ordihull(ord_m3, UID_1_Ref$Project, col = "green", lty = 1, lwd=1)

#Remove Pmax_pt and tmax
nv24=nv22[,c(1:11,14:15)]
ord_m4=metaMDS(nv24)
plot(ord_m4)
#Species=columns Sites=rows
#plot(ord_m4, type="t", display='species')
plot(ord_m4, type="n")
UID_1_Ref$color=ifelse(UID_1_Ref$Project=='NVmodel','blue','red')
points(ord_m4, display = 'sites', col= UID_1_Ref[,6])
ordihull(ord_m4, UID_1_Ref$Project, col = "green", lty = 1, lwd=1)






###### stop NMDS  ######
########################
      






names(ord_m)
[1] "nobj"       "nfix"       "ndim"       "ndis"       "ngrp"       "diss"      
[7] "iidx"       "jidx"       "xinit"      "istart"     "isform"     "ities"     
[13] "iregn"      "iscal"      "maxits"     "sratmx"     "strmin"     "sfgrmn"    
[19] "dist"       "dhat"       "points"     "stress"     "grstress"   "iters"     
[25] "icause"     "call"       "model"      "distmethod" "distcall"   "data"      
[31] "distance"   "converged"  "tries"      "engine"     "species"   
> ord_m$distance
[1] "bray"
> ord_m$distmethod
[1] "bray"
> ord_m$species
MDS1         MDS2
UID_1      -0.086701181  0.166138283
ELVmin_WS   0.034379442 -0.026875454
ELVmax_WS   0.010673494 -0.042107350
ELVmean_WS  0.028990189 -0.034315138
SQ_KM      -0.596909587 -0.147056050
HYDR_WS     0.039593044  0.004767528
WDmax_WS    0.004283417  0.029267891
Pmax_WS     0.090295027  0.073430158
Pmin_WS     0.040281430 -0.062163804
Tmax_WS    -0.049701403 -0.006321269
BFI_WS     -0.002457994 -0.019678490
ELVcv_PT   -0.010719821  0.028368492
Pmax_PT     0.096415008  0.100305670
Tmax_PT    -0.046605331 -0.015508897
PrdCond    -0.126553090 -0.053996567
Slope_WS    0.166008267 -0.077430217
attr(,"shrinkage")
MDS1        MDS2 
0.020673860 0.007810754 
attr(,"centre")
MDS1                         MDS2 
0.0000000000000000012468325 -0.0000000000000000003489776 


#To identify just a couple points on the graph.. 
# Run Fig and identify species or sites one at a time
# Click on points you want to ID, then ESCAPE and the points will be labeled on the graph
Fig=ordiplot(ord_m)
identify(fig,'spec')
identify(fig,'sites')
