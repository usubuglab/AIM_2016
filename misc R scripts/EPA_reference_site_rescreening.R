####Rescreening the EPA reference data#####


combined=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\Comb_21Oct2014_Rinput_DoNotAlter.csv")

#add back in a few columns from Mass_Combination_NRSA_EMAP
#key in combined file is SITE_ID
#as of March 31 2017 the above code to merge NRSA and EMAP data is throwing a row.names duplicate error because REALM is duplicated in the NRSA dataset. 
#I don't know why this wasn't an issue previously but I assume that the dataset in Mass_combination_NRSA_EMAP.csv is correct and would join needed fields back in from this csv rather than the code below.
Final2=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\Mass_Combination_NRSA_EMAP.csv")
Final2sub=subset(Final2,select=c(SITE_ID,XBKF_H,W1_HAG, W1H_CROP,W1H_WALL,PROJECT,XC,XM,XG,XSLOPE,XSLOPE_FIELD,XSLOPE_FRDATA,XSLOPE_FRREF,XSLOPE_MAP,XWD_RAT,XWIDTH,XWXD,XBKF_W,XDEPTH,V1W_MSQ,V1WM100,V2W_MSQ,V2WM100,V4W_MSQ,V4WM100,C1TM100,C1WM100,C2TM100,C2WM100,C4TM100,C4WM100,FLOWSITE))
combined2=join(combined,Final2sub,by='SITE_ID', type="left",match="all")


#Use to get wadeable or boatable only, if this line isn't run both boatable and wadeable will be used. Change REALM== "" to specify 
#combined2=subset(combined2, REALM == "WADEABLE")
combined2$BNK_THRESH=ifelse(as.numeric(combined2$XBKF_W)>10,"LargeWadeable","SmallWadeable")
combined2$BNK_THRESH=ifelse(combined2$REALM=="WADEABLE",combined2$BNK_THRESH,combined2$REALM)
combined2$THRESH=paste(combined2$ECO10,combined2$BNK_THRESH, sep="_")

combined2$THRESH3=as.factor(combined2$THRESH)
levels(combined2$THRESH3) <- list( XE_SOUTH_SmallWadeable="XE_SOUTH_SmallWadeable",XE_SOUTH_LargeWadeable="XE_SOUTH_LargeWadeable", 
                                   MT_SWEST_SmallWadeable="MT_SWEST_SmallWadeable",MT_SWEST_LargeWadeable="MT_SWEST_LargeWadeable", 
                                   XE_EPLAT_SmallWadeable="XE_EPLAT_SmallWadeable",XE_EPLAT_LargeWadeable="XE_EPLAT_LargeWadeable", 
                                   MT_PNW_SmallWadeable="MT_PNW_SmallWadeable", MT_PNW_LargeWadeable="MT_PNW_LargeWadeable",MT_PNW_BOATABLE="MT_PNW_BOATABLE",  
                                   PL_NCULT_SmallWadeable="PL_NCULT_SmallWadeable", PL_NCULT_LargeWadeable="PL_NCULT_LargeWadeable",PL_NCULT_BOATABLE="PL_NCULT_BOATABLE", 
                                   PL_RANGE_SmallWadeable="PL_RANGE_SmallWadeable",PL_RANGE_LargeWadeable="PL_RANGE_LargeWadeable", PL_RANGE_BOATABLE="PL_RANGE_BOATABLE",
                                   MT_SROCK_SmallWadeable="MT_SROCK_SmallWadeable",MT_SROCK_LargeWadeable="MT_SROCK_LargeWadeable", 
                                   MT_NROCK_SmallWadeable="MT_NROCK_SmallWadeable", MT_NROCK_LargeWadeable="MT_NROCK_LargeWadeable",
                                   XE_NORTH_SmallWadeable="XE_NORTH_SmallWadeable",XE_NORTH_LargeWadeable="XE_NORTH_LargeWadeable",
                                   #Other=c("XE_CALIF_LargeWadeable","XE_CALIF_SmallWadeable","0_BOATABLE","XE_SOUTH_NA","MT_PNW_0","MT_NROCK_NA", "_SmallWadeable","_LargeWadeable","_BOATABLE"  ,"_NA", "MT_SROCK_NA" , "0_LargeWadeable" , "MT_SWEST_NA", "0_SmallWadeable", "XE_CALIF_BOATABLE","MT_SWEST_BOATABLE"),   
                                   MT_ROCK_BOATABLE=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),  
                                   XE_SEPLAT_BOATABLE=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
)
combined2$THRESH2=combined2$THRESH
combined2$THRESH2=ifelse(combined2$THRESH2=="PL_RANGE_BOATABLE"|combined2$THRESH2=="PL_NCULT_BOATABLE"|combined2$THRESH2=="MT_PNW_BOATABLE"|combined2$THRESH2=="MT_NROCK_BOATABLE"|combined2$THRESH2=="MT_SROCK_BOATABLE"|combined2$THRESH2=="XE_NORTH_BOATABLE"|combined2$THRESH2=="XE_EPLAT_BOATABLE"|combined2$THRESH2=="XE_SOUTH_BOATABLE","ALL_BOATING",combined2$THRESH2)


RIP_RS_combined=combined2
#Now I need to remove duplicate sites, but choose to remove the one with the most recent year. 
#Remove the one with the most recent year because it may not still be in reference condition when it was sampled the second time.
###So I order by if it was revisited, the site code, and then the year.  Check this with the view
RIP_RS_reorder=RIP_RS_combined[order(RIP_RS_combined$REVISITED_OVERLAP,RIP_RS_combined$DUPLICATE_ID, RIP_RS_combined$YEAR, decreasing=FALSE),]
#View(RIP_RS_reorder[1250:2041,])
###Now I remove the duplicate that is listed second.
RIP_RS_minusDup= RIP_RS_reorder[!duplicated(RIP_RS_reorder$DUPLICATE_ID),]
#View(RIP_RS_minusDup[1000:1999,])
RIP_RS_final=RIP_RS_minusDup[order(RIP_RS_minusDup$REVISITED_OVERLAP, decreasing=TRUE),]

#join in StreamCat data
StreamCat=read.csv("\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\AIM\\Analysis\\Benchmarks\\EPA_Data\\EPA_Data_StreamCat.csv")
combined3=join(RIP_RS_final,StreamCat, by='SITE_ID', type="left",match="all")




####################################################################
#subset only R and S sites
combined3$UrbCombWs=combined3$PctUrbHi2006Ws+combined3$PctUrbLo2006Ws+combined3$PctUrbMd2006Ws
combined3$AgCombWs=combined3$PctAg2006Slp10Ws+combined3$PctAg2006Slp20Ws
reference=subset(combined3, RST_FRIP_AND_RMD_PHAB == "R"|RST_FRIP_AND_RMD_PHAB == "S")


UrbCombWs=aggregate(UrbCombWs~THRESH3, data=reference, FUN=max)
AgCombWs=aggregate(AgCombWs~THRESH3, data=reference, FUN=max)
DamDensWs=aggregate(DamDensWs~THRESH3, data=reference, FUN=max)
MineDensWs=aggregate(MineDensWs~THRESH3, data=reference, FUN=max)
NPDESDensWs=aggregate(NPDESDensWs~THRESH3, data=reference, FUN=max)
RdDensWs=aggregate(RdDensWs~THRESH3, data=reference, FUN=max)
W1_HALL=aggregate(W1_HALL~THRESH3, data=reference, FUN=max)
maxes=join_all(list(UrbCombWs,AgCombWs,DamDensWs,MineDensWs,NPDESDensWs,RdDensWs,W1_HALL), by="THRESH3")
write.csv(maxes,'current_benchmark_anthro_influence.csv')



T1=setNames(aggregate(reference$UrbCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","UrbCombWs_0.25"))
T2=setNames(aggregate(reference$UrbCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","UrbCombWs_0.90"))


T3=setNames(aggregate(reference$AgCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","AgCombWs_0.25"))
T4=setNames(aggregate(reference$AgCombWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","AgCombWs_0.90"))


T5=setNames(aggregate(reference$DamDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","DamDensWs_0.25"))#changing alpha
T6=setNames(aggregate(reference$DamDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","DamDensWs_0.90"))#changing alpha
T7=join_all(list(T1,T2), by="THRESH3")

T8=setNames(aggregate(reference$MineDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","MineDensWs_0.25"))
T9=setNames(aggregate(reference$MineDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","MineDensWs_0.90"))

T10=setNames(aggregate(reference$NPDESDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","NPDESDensWs_0.25"))
T11=setNames(aggregate(reference$NPDESDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","NPDESDensWs_0.90"))

T10=setNames(aggregate(reference$RdDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","RdDensWs_0.25"))
T11=setNames(aggregate(reference$RdDensWs, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","RdDensWs_0.90"))

T12=setNames(aggregate(reference$W1_HALL, by = list(reference$THRESH3), FUN = quantile,probs=0.25,na.rm=TRUE), c("THRESH3","W1_HALL_0.25"))
T13=setNames(aggregate(reference$W1_HALL, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","W1_HALL_0.90"))


T14=join_all(list(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), by="THRESH3")

write.csv(T14,'current_anthro_quantiles.csv')

####################################################################################################



#subset data to only be data that meets filters in Ode et al. but made up some of these too
#UrbCombWs<5, AgCombWs<3,DamDensWs<0.005,MineDensWs=0,NPDESDensWS=0,RdDensWs<1,W1_HALL<1.5,
combined3$UrbCombWs=combined3$PctUrbHi2006Ws+combined3$PctUrbLo2006Ws+combined3$PctUrbMd2006Ws
combined3$AgCombWs=combined3$PctAg2006Slp10Ws+combined3$PctAg2006Slp20Ws
reference=subset(combined3,UrbCombWs<5 & AgCombWs<3 & DamDensWs<0.005 & MineDensWs==0 & NPDESDensWs==0 & RdDensWs<1 & W1_HALL<1.5)
#UrbCombWs<5 & AgCombWs<3 not in stream cat...need to calc by adding different columns together ...likely used 2006 versions of this data
#PctCrop2006Ws<15 an additional filter the EPA used instead of pct ag

#Variables in EPA reference data set that may have been used in initial screening
#SO4	TURB	PTL	PHLAB	PHSTVL	NTL	CL	ANC	DOC	PCT_URB	W1_HALL	W1_HAG	W1H_CROP	W1H_WALL	NHD100_DAMS_CNT

#colnames(reference)

pvt1=aggregate(XCDENMID~THRESH3,data=reference,FUN=length)
pvt2=aggregate(XCDENBK~THRESH3,data=reference,FUN=length)
pvt3=aggregate(XCMG~THRESH3,data=reference,FUN=length)
pvt4=aggregate(XCMGW~THRESH3,data=reference,FUN=length)
pvt5=aggregate(PCT_SAFN~THRESH3,data=reference,FUN=length)
pvt7=aggregate(XFC_NAT~THRESH3,data=reference,FUN=length)
pvt8=aggregate(LINCIS_H~THRESH3,data=reference,FUN=length)
pvt9=aggregate(XEMBED~THRESH3,data=reference,FUN=length)
ECO10_SampSizes=join_all(list(pvt1,pvt2,pvt3,pvt4,pvt5, pvt7,pvt8,pvt9),by="THRESH3")

# Use riparian reference sites for:  XCDENMID, XCDENBK, XCMG, XCMGW, and PH!!
#Use riparian reference for PH because 
## 1) P-hab reference were selected using a variety of filters, since we do not have specific chemical reference sites we used p-hab reference
## 2) There are a few more reference sites for riparian than for sediment so Riparian was used over Sediment.  
#XCDENMID
T1=setNames(aggregate(reference$XCDENMID, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCDENMID_0.10"))
T2=setNames(aggregate(reference$XCDENMID, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCDENMID_0.30"))
T5=join_all(list(T1,T2), by="THRESH3")
#XCDENBK
T1=setNames(aggregate(reference$XCDENBK, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCDENBK_0.10"))
T2=setNames(aggregate(reference$XCDENBK, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCDENBK_0.30"))
T6=join_all(list(T1,T2), by="THRESH3")
#XCMG
T1=setNames(aggregate(reference$XCMG, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCMG_0.10"))#changing alpha
T2=setNames(aggregate(reference$XCMG, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCMG_0.30"))#changing alpha
T7=join_all(list(T1,T2), by="THRESH3")
#XCMGW
T1=setNames(aggregate(reference$XCMGW, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XCMGW_0.10"))
T2=setNames(aggregate(reference$XCMGW, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XCMGW_0.30"))
T8=join_all(list(T1,T2), by="THRESH3")

#PH
#PH thresholds were determine to be too strict so we are using the national standards of <6.5 and >9.0 as Poor, >7 and <8.5 as Good.
#if EPA reference thresholds want to be used you must uncomment the below code and add "T18" to the liste of code to join it back into the RIP_THRESHOLDS_lvlIII object
#A few pH issues
#PH_EPA=reference[,1:25]
#Replace NAs with if statement to combine columns
#PH_EPA$PH=ifelse(is.na(PH_EPA$PHLAB),PH_EPA$PHSTVL,PH_EPA$PHLAB)
#thresholds
#T11=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.10,na.rm=TRUE), c("ECO10","PH_0.10"))
#T12=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.30,na.rm=TRUE), c("ECO10","PH_0.30"))
#T13=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.90,na.rm=TRUE), c("ECO10","PH_0.90"))
#T14=setNames(aggregate(PH_EPA$PH, by = list(PH_EPA$ECO10), FUN = quantile,probs=0.70,na.rm=TRUE), c("ECO10","PH_0.70"))
#T18=join_all(list(T11,T12,T14,T13), by="ECO10")

##Combine all
RIP_THRESHOLDS_ECO10=join_all(list(T5,T6,T7,T8),by="THRESH3")

###################################################################################
###################################################################################
# Use sediment reference sites for:  PCT_SAFN, DPCT_SF, XEMBED, XFC_NAT,LINCIS_H,
#PCT_SAFN
T3=setNames(aggregate(reference$PCT_SAFN, by = list(reference$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","PCT_SAFN_0.70"))
T4=setNames(aggregate(reference$PCT_SAFN, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","PCT_SAFN_0.90"))
T5=join_all(list(T3,T4), by="THRESH3")
#DPCT_SF
###Modeled indicator, does not use Ecoregion percentil THRESH3olds as these others do.. 
#XFC_NAT
T1=setNames(aggregate(reference$XFC_NAT, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","XFC_NAT_0.10"))
T2=setNames(aggregate(reference$XFC_NAT, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","XFC_NAT_0.30"))
T7=join_all(list(T1,T2), by="THRESH3")
#LINCIS_H
T3=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","LINCIS_H_0.70"))
T4=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","LINCIS_H_0.90"))
T8=join_all(list(T3,T4), by="THRESH3")
#XEMBED
T3=setNames(aggregate(reference$XEMBED, by = list(reference$THRESH3), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","XEMBED_0.70"))
T4=setNames(aggregate(reference$XEMBED, by = list(reference$THRESH3), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","XEMBED_0.90"))
T9=join_all(list(T3,T4), by="THRESH3")
#RP100
T1=setNames(aggregate(reference$RP100, by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","RP100_0.10"))
T2=setNames(aggregate(reference$RP100, by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","RP100_0.30"))
T10=join_all(list(T1,T2), by="THRESH3")

#C1WM100
T1=setNames(aggregate(as.numeric(reference$C1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","C1WM100_0.10"))
T2=setNames(aggregate(as.numeric(reference$C1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","C1WM100_0.30"))
#V1WM100
T3=setNames(aggregate(as.numeric(reference$V1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.10,na.rm=TRUE), c("THRESH3","V1WM100_0.10"))
T4=setNames(aggregate(as.numeric(reference$V1WM100), by = list(reference$THRESH3), FUN = quantile,probs=0.30,na.rm=TRUE), c("THRESH3","V1WM100_0.30"))

T10=join_all(list(T1,T2,T3,T4), by="THRESH3")

##Combine all
SED_THRESHOLDS_ECO10=join_all(list(T5,T7,T8,T9,T10),by="THRESH3")

###################################################################################
# Use set thresholds for:  L_XCMGW, W1_HALL, QR1
# Defined when thresholds are implemented and applied to field measured indicators.
###################################################################################

#Make one file for ECO 10 thresholds and remove unwanted working files
Thresholds_ECO10=merge(SED_THRESHOLDS_ECO10,RIP_THRESHOLDS_ECO10, all=TRUE)

####boating incision with all boating sites combined
#LINCIS_H
T3=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH2), FUN = quantile,probs=0.70,na.rm=TRUE), c("THRESH3","LINCIS_H_0.70"))
T4=setNames(aggregate(reference$LINCIS_H, by = list(reference$THRESH2), FUN = quantile,probs=0.90,na.rm=TRUE), c("THRESH3","LINCIS_H_0.90"))
T8=join_all(list(T3,T4), by="THRESH3")
SubT8=subset(T8,THRESH3=="ALL_BOATING")

Thresholds_Final=merge(Thresholds_ECO10,SubT8, all=TRUE)

write.csv(ECO10_SampSizes,'Eco10_SampSizes.csv')
write.csv(Thresholds_Final,'Thresholds_Final.csv')