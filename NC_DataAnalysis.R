#How to order data
New table name=data[order(data$column),]


##############    Water quality condition determinations      ###############
#############################################################################

# First get field measured WQ data (conductivity, tn, tp) using code in NC_DataConsumption
# Read in predicted WQ results from the WQ models
PrdWQresults=read.csv("N:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\Pred_WQresults_2014All.csv")

#Rename the siteCode column in predicted file to match the Site ID column in the data file so that the files can be merged
PrdWQresults$SITE_ID=PrdWQresults$SiteCode

#Merge the files and check that it worked correctly
#to use the merge function you must have a column in each file that match exactly. 
AllWQ=merge(PrdWQresults,WQfinal)

#Reorganize and remove unwanted columns by only choosing those you want, you can use column numbers or column names. I think column names are more intuitive but column numbers left as an example.
AllWQ2=AllWQ[,c('UID','SITE_ID','LOC_NAME','DATE_COL','PrdCond','Pred_TN','Pred_TP','CONDUCTIVITY',	'NTL',	'PTL')]
#Trial2=AllWQ[,c(7,1,13,11,6,5,4,8,9,10)]

#Now subtract Observe - expected to get the OE score to be classified as G F P. Then set thresholds for each indicator
#If o-e is > lower limit it is fair, if it is > upper limit its poor, if it is < lower limit it is good(all else is good)

###Conductivity
AllWQ2$OE_Cond = AllWQ2$CONDUCTIVITY - AllWQ2$PrdCond 
AllWQ2$Cond_condition=ifelse(AllWQ2$OE_Cond <=27.1,'Good',ifelse(AllWQ2$OE_Cond >53.7, 'Poor','Fair'))

###Total N
AllWQ2$OE_TN = AllWQ2$NTL - AllWQ2$Pred_TN 
AllWQ2$TN_condition=ifelse(AllWQ2$OE_TN <=52.1,'Good',ifelse(AllWQ2$OE_TN >114.7, 'Poor','Fair'))

###Total P
AllWQ2$OE_TP = AllWQ2$PTL - AllWQ2$Pred_TP 
AllWQ2$TP_condition=ifelse(AllWQ2$OE_TP <=9.9,'Good',ifelse(AllWQ2$OE_TP >21.3, 'Poor','Fair'))

View(AllWQ2)


#Write to a csv, but I would prefer not to do this... UID issue may cause problems in the future so it would be easier to just keep the data active and in R. 
#However I did write to a csv so there was a hardcopy of the results for a backup.
write.csv(AllWQ2, "N:\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\WaterQualityModels\\WQconditions_2014All_8Oct2014.csv")



##############      Aquamet Indicator calculations check      ###############
#############################################################################

#XFC_NAT
###Get the approrpaite fish metrics from NC_DataConsumption
######'BOULDR','BRUSH','LVTREE','OVRHNG','UNDCUT','WOODY'
###### Change numeric categories into appropriate percentages, pivot and take the mean or each fish cover category at a site. 
######Then sum to categories of fish cover for each site to have the final results to compared to aquamet's xfc_nat
###### The way this is calculated causes NA's to be treated as blanks that do not count for or against the average. For example if only 1 NA for BOULDR then you would divide boulders by 10 transects instead of 11. See UID 11625 for an example.
fish$ResultsPer=ifelse(fish$RESULT == 1, 0.05,ifelse(fish$RESULT == 2, 0.25,ifelse(fish$RESULT == 3, 0.575,ifelse(fish$RESULT == 4, 0.875,ifelse(fish$RESULT ==0, 0, NA)))))
#fishpvt=cast(fish,'UID+TRANSECT~PARAMETER', value='RESULT')
fishpvt2=cast(fish,'UID~PARAMETER', value='ResultsPer',fun='mean')
fishpvt2$XFC_NAT_CHECK=rowSums(fishpvt2[,c(2,3,4,5,6,7)])

#xcdenmid
MidDensiom = subset(densiom, POINT == "CU"|POINT =="CD"|POINT == "CL"|POINT == "CR")
DensPvt=cast(MidDensiom,'UID~PARAMETER',value='RESULT',fun=mean)
DensPvt$xcdenmid_CHECK=(DensPvt$DENSIOM/17)*100
#Trying to figure out what is going on with UID 11802.
#Dens_Pvt3=cast(MidDens3,'UID+TRANSECT~PARAMETER',value='RESULT',fun=mean)

#LINCIS_H

###############################################
##########   TROUBLESHOOTING START  ###########
##IncBnk2=cast(Incision,'UID~PARAMETER', value='RESULT', fun=mean)
##INCISED=Incision[,c("UID","TRANSECT","PARAMETER","RESULT")]
#IB=aggregate(Incision)
#IB=cast(Incision,'UID~PARAMETER',  value='RESULT', fun=aggregate)
#IBagg=aggregate(Incision,by='PARAMETER',value='RESULT', FUN=mean)

###LINCIS_h REMOVAL OF SIDE CHANNELS TO FIX PLROBLEM.. DIDN'T FIX THE ISSUE... 
#NoSide= subset(Incision, TRANSECT == "A"|TRANSECT == "B"|TRANSECT == "C"|TRANSECT == "D"|TRANSECT == "E"|TRANSECT == "F"|TRANSECT == "G"|TRANSECT == "H"|TRANSECT == "I"|TRANSECT == "J"|TRANSECT == "K")
#NS_IncBnk=cast(NoSide,'UID~PARAMETER', value='RESULT', fun=mean)
#NS_IncBnk$LINCIS_H_Check=log10(NS_InkBnk$INCISED-NS_IncBnk$BANKHT+0.1)
#########    TROUBLESHOOTING END   ############
###############################################

###First the max value of either the side channel or main channel needs to be chosen. To do this I changed all side channels (X-letter) to just the main letter (Sidechannel at A (XA) would be changed to just A).
###Then I subset the data so that missing values would not cause errors. 
###Then I pivoted by the max to chose the max transect value (If XA=5 and A=2 then the XA value would be chosen and the A value removed, note that it is no longer called XA so there would just be 2 A transects for a site with an A sidechannel)
### Then take the average at each site for bank height and incised height. Merge the data back together and then calculate LINCIS_H
Incision$TRANSECT=mapvalues(Incision$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
INCISED=subset(Incision, PARAMETER=="INCISED")
BANKHT=subset(Incision, PARAMETER=="BANKHT")
Inc=cast(INCISED,'UID+TRANSECT~PARAMETER', value='RESULT', fun=max)
Bnk=cast(BANKHT,'UID+TRANSECT~PARAMETER', value='RESULT', fun=max)
xIncht=setNames(aggregate(Inc$INCISED,list(UID=Inc$UID),mean),c("UID","xinc_h_CHECK"))
xBnkht=setNames(aggregate(Bnk$BANKHT,list(UID=Bnk$UID),mean),c("UID","xbnk_h_CHECK"))
IncBnk=merge(xBnkht,xIncht,all=TRUE)
IncBnk$LINCIS_H_CHECK=log10(IncBnk$xinc_h_CHECK-IncBnk$xbnk_h_CHECK+0.1)










