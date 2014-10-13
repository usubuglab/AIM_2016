
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
DensPvt$xcdenmid_CHECK=(DensPvt$DENSIOM/17)*100

