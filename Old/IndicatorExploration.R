#thalCheck=tblRetrieve(Parameters='TRCHLEN',UIDS=UIDs,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
#When in retrieve code use UIDS!!
#UIDs='004C9175-A7EE-4AE4-8EE4-A61332D538E2'#custom filter (need working knowledge of primary keys)
#PR-RV-10239
BnkDensPvt=merge(nBnkDensPvt,BnkDensPvt,by="UID")
BnkDensPvt$XCDENBK_CHECK=ifelse(BnkDensPvt$nXCDENBK_CHECK<11,NA,BnkDensPvt$XCDENBK_CHECK)#8 have n=8-10

##################
#### New RIP STart
#Has been merged into JC indicator calc
#### New RIP STOP
#################
#colnames(FQCY_VEG)[2:6]=paste(colnames(FQCY_VEG[,2:6]), "CHECK",sep = "_")
#RipBLM=addKEYS(tblRetrieve(Parameters=c('CANRIPW','UNRIPW','GCRIP','INVASW', 'NATIVW','INVASH','NATIVH','SEGRUSH'),Projects=projects,Years=years,Protocols=protocols, SiteCodes='', UIDS=UIDs),c('SITE_ID'))
#pvtRipBLM=cast(RipBLM,'UID+TRANSECT+POINT~PARAMETER',value='RESULT')
#### Wrong: Divides by 22 transects rather than the number of observations
#### TO use this methods you would just have to run the code for individial indicators and not try to do all three at once
#RIP_VEG=subset(RipBLM, PARAMETER == 'CANRIPW'|PARAMETER == 'UNRIPW'|PARAMETER == 'GCRIP')
#RIP_VEG$ResultsPer=ifelse(RIP_VEG$RESULT == 1, 0.05,ifelse(RIP_VEG$RESULT == 2, 0.25,ifelse(RIP_VEG$RESULT == 3, 0.575,ifelse(RIP_VEG$RESULT == 4, 0.875,ifelse(RIP_VEG$RESULT ==0, 0, NA)))))
#RIP_VEG=cast(RIP_VEG,'UID+TRANSECT+POINT~PARAMETER', value='ResultsPer',fun='sum')
#RIP_VEG1=aggregate(cbind(CANRIPW,GCRIP,UNRIPW)~UID,data=RIP_VEG,FUN=mean)
####
#NonNwdy=subset(RipBLM, PARAMETER == "INVASW")
#NonNwdy$RESULT_A=ifelse(NonNwdy$RESULT == 'N', 0,ifelse(NonNwdy$RESULT == 'Y', 1,'NA'))
#NonNwdy=cast(NonNwdy,'UID~PARAMETER',value='RESULT_A',fun=mean)




###################################
#LWD START
###################################################################################################################################################
#average # of pieces of wood?
LwdCatWet=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'W%'"))$PARAMETER
#LwdCatDry=unclass(sqlQuery(wrsa1314,"select SAMPLE_TYPE,PARAMETER from tblMetadata where Sample_TYPE like 'LWDW%' and PARAMETER like 'D%'"))$PARAMETER
LwdWet=addKEYS(tblRetrieve(Parameters=LwdCatWet,Projects=projects,Years=years,Protocols=protocols),c('SITE_ID','DATE_COL'))

#LwdDry=tblRetrieve(Parameters=LwdCatDry,Projects=projects,Years=years,Protocols=protocols)
TRCHLEN=tblRetrieve(Parameters=c('TRCHLEN','INCREMENT'),Projects=projects,Years=years,Protocols=protocols)#not using TRCHLEN
TRCHLEN=cast(TRCHLEN,'UID~PARAMETER',value='RESULT')
#TRCHLEN is not the same as the reachlen used in Aquamet
#The reachlen is calc from mulitplying INCREMENT by the thalweg stations
#We need to estimate the intended number of wadeable thalweg stations at each transect
#which are considered sampled (even if there is no data) for the purposes of
#calculating residual pools and channel lengths.  The number of stations at
#a transect is calculated as the greater of either the number of stations
#occuring in the dataframe for that transect, or the most common count of
#stations (i.e. station mode) occuring at that site. 

### Getting Data to calculate Indicators Stops here

#LWD
#C1WM100- (Cummulative count of LWD in bankfull channel across all size classes)/(Reach Length) units are pieces/100m
LwdWet$TRANSECT=mapvalues(LwdWet$TRANSECT, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
LWD_test=setNames(aggregate(RESULT~UID+TRANSECT,data=LwdWet,FUN=sum),c("UID","TRANSECT","C1W"))# count of all LWD pieces per site
LWD_test2=setNames(count(LWD_test,"UID"),c("UID","NUMTRAN"))# count of the number of transects that wood was collected for #may require package plyr which requires R version > 3????? but I have also gotten count to work in other situations with other version of R required for aquamet
LWD=setNames(aggregate(RESULT~UID,data=LwdWet,FUN=sum),c("UID","C1W"))# count of all LWD pieces per site
LWD=merge(LWD_test2,LWD,by=c('UID'),all=T)
LWD=merge(LWD,TRCHLEN,by=c('UID'), all=T)
#To get the reach length for which LWD was accesssed divide the total reach length by 10 to get the transect spacing and then multiply times the number of LWD transects sampled
#This is different than the EPA's reach length. The EPA determines reach length by approximating the number of intended thalweg stations
#They take the greater of either the max number of stations occurring at each transect or the station "mode" occurring at a site
#This seems overly complex....particularly the mode part why not get an accurate number of stations per transect? 
#Our method could over estimate wood though...crews may have evaluated sections of the thalweg but not recorded a wood value because they forgot or something else
#However not all crews collecting thalweg in future and may not be able to get thalweg depths where you could get wood...we probably could use another parameter that is collected at all thalweg stations (side channel presence- but did not collect this in 2016) 
LWD$LWD_RCHLEN=(LWD$TRCHLEN/10)*LWD$NUMTRAN 
LWD$C1WM100_CHECK=round((LWD$C1W/LWD$LWD_RCHLEN)*100,digits=1)# get the pieces/100m


#V1WM100
#MUST run C1WM100 code prior to this code so that the RCHLEN (reach length) code works. 
#Duplications of size classes below are so that the table works with sites sampled with the 2013 protocol as well as the 2014 and later protocol 
#Because the Boatable and Wadeable sites have the same PARAMETER name but different size classes, To distinquish I've added "B_" in front of the boatable site PARAMETER. 
#All categories match our metadata table. 
#Based on Jawson Law's Code and communication with Phil Kauffman I have assigned a max or upper end value to those categories which were >some number

LWDtt <- textConnection(
  "PARAMETER	Min_DIAMETER	Max_DIAMETER	Min_LENGTH	Max_LENGTH
B_WLDLL	0.8	1	30	75
B_WLDML	0.8	1	15	30
B_WLDSL	0.8	1	5	15
B_WMDLL	0.6	0.8	30	75
B_WMDML	0.6	0.8	15	30
B_WMDSL	0.6	0.8	5	15
B_WSDLL	0.3	0.6	30	75
B_WSDML	0.3	0.6	15	30
B_WSDSL	0.3	0.6	5	15
B_WXDLL	1	2	30	75
B_WXDML	1	2	15	30
B_WXDSL	1	2	5	15
WLDLL	0.6	0.8	15	30
WLDML	0.6	0.8	5	15
WLDSL	0.6	0.8	1.5	5
WMDLL	0.3	0.6	15	30
WMDML	0.3	0.6	5	15
WMDSL	0.3	0.6	1.5	5
WSDLL	0.1	0.3	15	30
WSDML	0.1	0.3	5	15
WSDSL	0.1	0.3	1.5	5
WXDLL	0.8	2	15	30
WXDML	0.8	2	5	15
WXDSL	0.8	2	1.5	5
WLDSML	0.6	0.8	3	5
WMDSML	0.3	0.6	3	5
WSDSML	0.1	0.3	3	5
WXDSML	0.8	2	3	5
WSDSSL	0.1	0.3	1.5	3
WXDSSL	0.8	2	1.5	3
WMDSSL	0.3	0.6	1.5	3
WLDSSL	0.6	0.8	1.5	3"
  )


#Creates a table of the volume value that corresponds to the size category.
LWD_sizes <- read.table(LWDtt, header = TRUE, stringsAsFactors = FALSE)
close(LWDtt)
#Equation found in Kauffman 99 (pg 31 of Kauffman 1999 and he cites Robison 1998) is wrong, first noticed because it does not produce values equal to those found in aquamet. Jason Law has the correct code, confirmed by Phil Kauffman and also produces the same values found in Aquamet. Aquamet code only uses calculated values from the equation but does not contain the equation itself
LWD_sizes$VOLUME=pi*((0.5*(LWD_sizes$Min_DIAMETER+((LWD_sizes$Max_DIAMETER-LWD_sizes$Min_DIAMETER)/3)))^2)*(LWD_sizes$Min_LENGTH+((LWD_sizes$Max_LENGTH-LWD_sizes$Min_LENGTH)/3))

LwdWet$PARAMETER=ifelse(LwdWet$SAMPLE_TYPE=="LWDB",sprintf('%s%s',"B_",LwdWet$PARAMETER),LwdWet$PARAMETER)#Adds a "B_" to all boatable parameters so that we can run the same code for everything. 
LWDvol1=join(LwdWet,LWD_sizes, by='PARAMETER') #Appends the correct volume value to size class in the table of data
LWDvol1$VOLcalc=LWDvol1$VOLUME*LWDvol1$RESULT #Get the overall volume of wood for each size class
LWDvolume=setNames(cast(LWDvol1,"UID+DATE_COL+SAMPLE_TYPE~ACTIVE", value="VOLcalc", fun="sum"),c('UID','DATE_COL','SAMPLE_TYPE','LWDvol')) #Pivots table to get the overall volume of wood for the entire reach. Keep Date so we can set different sample size requirements for different years
LWDvolume=merge(LWDvolume,subset(LWD, select=c(UID,LWD_RCHLEN),all=TRUE), by="UID",all=TRUE)#Need to run code within lwd count code before this will work
LWDvolume$V1WM100=(LWDvolume$LWDvol/LWDvolume$LWD_RCHLEN)*100
nLWD=setNames(count(LwdWet,"UID"),c("UID","nLWD"))#Calculates sample size for each site
LWDvolume$YEAR=format(as.Date(LWDvolume$DATE_COL,'%m/%d/%Y'),'%Y') #Separates year from the date as an easy way to apply apply the sample size needed code below since we had different samplke sizes in different years.
LWDvolume=merge(nLWD,LWDvolume,by="UID",all=TRUE) #Merge sample sizes to data file by UID
#Excludes all site values if they do not meet the minumum sample size required:
LWDvolume$V1WM100_CHECK=ifelse(LWDvolume$nLWD>=64&LWDvolume$YEAR>='2014'&LWDvolume$SAMPLE_TYPE!='LWDB',LWDvolume$V1WM100,# For wadeable sites collected in 2014 or later, total data points for entire reach=160 (16 per transect, 10 transects), 5 transects of data would be 80, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 64
                               ifelse(LWDvolume$nLWD>=48&LWDvolume$YEAR<'2014'&LWDvolume$SAMPLE_TYPE!='LWDB',LWDvolume$V1WM100,# For wadeable sites collected in 2013, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
                                      ifelse(LWDvolume$nLWD>=48&LWDvolume$SAMPLE_TYPE=='LWDB',LWDvolume$V1WM100,NA))) # For boatable sites collected in any year, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48


#The if statement above was not working, so I started developing this code, in the process I fixed the if statement above....
#LWDvolumeBoat=subset(LWDvolume,SAMPLE_TYPE=='LWDB')
#LWDvolume2014beyond=subset((LWDvolume,SAMPLE_TYPE=='LWDW'&YEAR>=2014))
#LWDvolume2013before=subset((LWDvolume,SAMPLE_TYPE=='LWDW'&YEAR<2014))

#LWDvolumeBoat$V1WM100_CHECK= ifelse(LWDvolumeBoat$nLWD>=48,LWDvolumeBoat$V1WM100,'NA')
#LWDvolume2014beyond$V1WM100_CHECK=ifelse(LWDvolume2014beyond$nLWD>=64,LWDvolumeBoat$V1WM100,'NA')
#LWDvolume2013before$V1WM100_CHECK=ifelse(LWDvolume2013before$nLWD>=48,LWDvolumeBoat$V1WM100,'NA')

#LWDvolume=rbind(LWDvolumeBoat,LWDvolume2014beyond,LWDvolume2013before)


#EPA CODE START
#
####EPA CODE FROM AQUAMET WHERE VOLUME VALUES ARE CONTAINED. AQUAMET HAS MUCH MORE CODE, BUT THIS SHOWS THE BASIC VOLUME VALUES AND PROCESS. rch% IS THE "RESULT" COLUMN (TALLY OF WOOD AT THE WHOLE REACH) FOR EACH WOOD SIZE CATEGORY
####WADEABLE:
#mmw$v1w <- ((mmw$rchwsdsl * 0.058) + (mmw$rchwsdml * 0.182) + 
#              (mmw$rchwsdll * 0.438) + (mmw$rchwmdsl * 0.333) + (mmw$rchwmdml * 
#                                                                   1.042) + (mmw$rchwmdll * 2.501) + (mmw$rchwldsl * 0.932) + 
#              (mmw$rchwldml * 2.911) + (mmw$rchwldll * 6.988) + (mmw$rchwxdsl * 
#                                                                   3.016) + (mmw$rchwxdml * 9.421) + (mmw$rchwxdll * 22.62))
#mmw$v1wm100 <- (mmw$v1w/mmw$reachlen) * 100
#
####BOATABLE:
#mmb$v1w <- ((mmb$rchwsdsl * 1.047) + (mmb$rchwsdml * 2.513) + 
#              (mmb$rchwsdll * 5.655) + (mmb$rchwmdsl * 2.909) + (mmb$rchwmdml * 
#                                                                   6.981) + (mmb$rchwmdll * 15.708) + (mmb$rchwldsl * 4.916) + 
#              (mmb$rchwldml * 11.798) + (mmb$rchwldll * 26.546) + (mmb$rchwxdsl * 
#                                                                     11.636) + (mmb$rchwxdml * 27.925) + (mmb$rchwxdll * 62.832))
#mmb$v1wm100 <- (mmb$v1w/(mmb$numtran * 20)) * 100
#
####EPA CODE STOP

