
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







