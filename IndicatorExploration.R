#thalCheck=tblRetrieve(Parameters='TRCHLEN',UIDS=UIDs,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
#When in retrieve code use UIDS!!
#UIDs='004C9175-A7EE-4AE4-8EE4-A61332D538E2'#custom filter (need working knowledge of primary keys)
#PR-RV-10239
BnkDensPvt=merge(nBnkDensPvt,BnkDensPvt,by="UID")
BnkDensPvt$XCDENBK_CHECK=ifelse(BnkDensPvt$nXCDENBK_CHECK<11,NA,BnkDensPvt$XCDENBK_CHECK)#8 have n=8-10


##################
#### New RIP STart

####
# get data for new Riparian
RipBLM=addKEYS(tblRetrieve(Parameters=c('CANRIPW','UNRIPW','GCRIP','INVASW', 'NATIVW','INVASH','NATIVH','SEGRUSH'),Projects=projects,Years=years,Protocols=protocols),c('SITE_ID'))

#### 
#Riparian vegetation cover
RIP_VEG=subset(RipBLM, PARAMETER == 'CANRIPW'|PARAMETER == 'UNRIPW'|PARAMETER == 'GCRIP')
RIP_VEG$ResultsPer=ifelse(RIP_VEG$RESULT == 1, 0.05,ifelse(RIP_VEG$RESULT == 2, 0.25,ifelse(RIP_VEG$RESULT == 3, 0.575,ifelse(RIP_VEG$RESULT == 4, 0.875,ifelse(RIP_VEG$RESULT ==0, 0, NA)))))
nRIP_VEG=setNames(count(RIP_VEG,c("UID",'PARAMETER')),c("UID",'PARAMETER',"nRIP_VEG_CHECK"))#
nRIP_VEG=setNames(cast(nRIP_VEG,"UID~PARAMETER",value="nRIP_VEG_CHECK",fun="sum"),c('UID','nCANRIPW_CHECK','nUNRIPW_CHECK', 'nGCRIP_CHECK'))
RIP_VEG=setNames(cast(RIP_VEG,'UID~PARAMETER', value='ResultsPer',fun='mean'),c('UID','CANRIPW_CHECK','UNRIPW_CHECK', 'GCRIP_CHECK'))
RIP_VEG=merge(nRIP_VEG,RIP_VEG,by="UID")
RIP_VEG$CANRIPW_CHECK=ifelse(RIP_VEG$nCANRIPW_CHECK<10,NA,RIP_VEG$CANRIPW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
RIP_VEG$UNRIPW_CHECK=ifelse(RIP_VEG$nUNRIPW_CHECK<10,NA,RIP_VEG$UNRIPW_CHECK)#total=22,but collected at 5 transects=10, so min N=10
RIP_VEG$GCRIP_CHECK=ifelse(RIP_VEG$nGCRIP_CHECK<10,NA,RIP_VEG$GCRIP_CHECK)#total=22,but collected at 5 transects=10, so min N=10

####
#Riparian vegetation frequency
#Works!!!Try to take NA out of quotes and you may not need as.numeric
FQCY_VEG=subset(RipBLM, PARAMETER == 'INVASW'|PARAMETER == 'NATIVW'|PARAMETER == 'INVASH'|PARAMETER == 'NATIVH'|
                  PARAMETER == 'SEGRUSH')
nFQCY_VEG=setNames(count(FQCY_VEG,c("UID",'PARAMETER')),c("UID",'PARAMETER',"nFQCY_VEG_CHECK"))#6 strata *2 banks*11 transects=132 so half data=66
nFQCY_VEG=cast(nFQCY_VEG,"UID~PARAMETER",value="nFQCY_VEG_CHECK",fun="sum")
FQCY_VEG$RESULT_A=as.numeric(ifelse(FQCY_VEG$RESULT == 'N', 0,ifelse(FQCY_VEG$RESULT == 'Y', 1,"NA")))
FQCY_VEG=cast(FQCY_VEG,'UID~PARAMETER',value='RESULT_A',fun=mean)
FQCY_VEG=merge(nFQCY_VEG,FQCY_VEG,by="UID")
FQCY_VEG$INVASW_CHECK=ifelse(FQCY_VEG$nCANRIPW_CHECK<10,NA,FQCY_VEG$CANRIPW_CHECK)#total=22,but collected at 5 transects=10, so min N=10

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
#####

####
#NonNwdy=subset(RipBLM, PARAMETER == "INVASW")
#NonNwdy$RESULT_A=ifelse(NonNwdy$RESULT == 'N', 0,ifelse(NonNwdy$RESULT == 'Y', 1,'NA'))
#NonNwdy=cast(NonNwdy,'UID~PARAMETER',value='RESULT_A',fun=mean)


###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################

###################################################################################################################################################

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
####EPA CODE FROM AQUAMET WHERE VOLUME VALUES ARE CONTAINED. AQUAMET HAS MUCH MORE CODE, BUT THIS SHOWS THE BASIC VOLUME VALUES AND PROCESS. rch% IS THE "RESULT" COLUMN (TALLY OF WOOD AT THE WHOLE REACH) FOR EACH WOOD SIZE CATEGORY
####THESE VALUES TO NOT MATCH THOSE VALUES CREATED IN OUR TABLE BELOW USING THE EQUATION FOUND IN THE EPA'S DOCUMENTATION. 
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

#Duplications of size classes below are so that the table works with 2013 and 2014+ 
#Because the Boatable and Wadeable sites have the same PARAMETER name but different size classes, To distinquish I've added "B_" in front of the boatable site PARAMETER. 
#All categories match our metadata, these values are the lower end of the size categories. 
LWDtt <- textConnection(
  "LWD_Cat  DIAMETER	LENGTH
  WLDLL	0.6	15
  WLDML	0.6	5
  WLDSL	0.6	1.5
  WMDLL	0.3	15
  WMDML	0.3	5
  WMDSL	0.3	1.5
  WSDLL	0.1	15
  WSDML	0.1	5
  WSDSL	0.1	1.5
  WXDLL	0.8	15
  WXDML	0.8	5
  WXDSL	0.8	1.5
  WLDSML  0.6	3
  WMDSML	0.3	3
  WSDSML	0.1	3
  WXDSML	0.8	3
  WSDSSL	0.1	1.5
  WXDSSL	0.8	1.5
  WMDSSL	0.3	1.5
  WLDSSL	0.6	1.5
  B_WLDLL	0.8	30
  B_WLDML	0.8	15
  B_WLDSL	0.8	5
  B_WMDLL	0.6	30
  B_WMDML	0.6	15
  B_WMDSL	0.6	5
  B_WSDLL	0.3	30
  B_WSDML	0.3	15
  B_WSDSL	0.3	5
  B_WXDLL	1.0	30
  B_WXDML	1.0	15
  B_WXDSL	1.0	5"
)

#Creates a table of the volume value that corresponds to the size category.
LWD_sizes <- read.table(LWDtt, header = TRUE, stringsAsFactors = FALSE)
close(LWDtt)
#This equation may be wrong, it does not produce values equal to those found in aquamet... Jason Law has a different equation, but his equation also does not produce values that match the values found in aquamet. 
LWD_sizes$VOLUME=pi*((1.33*(LWD_sizes$DIAMETER/2)^2)*(1.33*LWD_sizes$LENGTH))#pg 31 of Kauffman 1999 and he cites Robison 1998
LWD_sizes$PARAMETER=LWD_sizes$LWD_Cat

#Adds a "B_" to all boatable parameters so that we can run the same code for everything. 
LwdWet$PARAMETER=ifelse(LwdWet$SAMPLE_TYPE=="LWDB",sprintf('%s%s',"B_",LwdWet$PARAMETER),LwdWet$PARAMETER)

LWDvol1=join(LwdWet,LWD_sizes, by='PARAMETER') #Appends the correct volume value to size class in the table of data
LWDvol1$VOLcalc=LWDvol1$VOLUME*LWDvol1$RESULT #Get the overall volume of wood for each size class
LWDvolume=cast(LWDvol1,"UID+DATE_COL~SAMPLE_TYPE", value="VOLcalc", fun="sum") #Pivots table to get the overall volume of wood for the entire reach. Keep Date so we can set different sample size requirements for different years
nLWD=setNames(count(LwdWet,"UID"),c("UID","nLWD"))#Calculates sample size for each site
LWDvolume$YEAR=format(as.Date(LWDvolume$DATE_COL,'%m/%d/%Y'),'%Y') #Separates year from the date as an easy way to apply apply the sample size needed code below since we had different samplke sizes in different years.
LWDvolume=merge(nLWD,LWDvolume,by="UID") #Merge sample sizes to data file by UID
#Need to encorporate boating into this, maybe Cast by something other than sample type so we can keep that and add this to the if statement, if = boat, if =2016 and does not = boat, 
LWDvolume$V=ifelse(LWDvolume$nLWD>=80&LWDvolume$YEAR>='2016',LWDvolume$LWDW,ifelse(LWDvolume$nLWD>=60&LWDvolume$YEAR<'2016',LWDvolume$LWDW,NA)) #Excludes all site values if they do not meet the minumum sample size required: total=160 for 2014 and beyond data (16 categories of wood size, 10 transect possible), but collected at 5 transects=5*16=80, so min n=80

##Need to divide by reach length and multiply by 100. I should probably do this before excluding values so....












#NOPE only uses the first row in LWD_Sizes, otherwise would have worked# LwdWet$VolClass=ifelse(LwdWet$PARAMETER==LWD_sizes$LWD_Cat,LWD_sizes$VOLUME,"1")
#NOPE#VolClass=merge(LwdWet, LWD_sizes, by.x="PARAMETER", by.y="LWD_Cat")
#NOPE# LwdWet$VolClass=mapvalues(LwdWet$PARAMETER, from=list(LWD_sizes$LWD_Cat),to=LWD_sizes$VOLUME)
#NOPE# merge(dataA, dataB, by = "name", all = TRUE)
07C9FAA5-09E8-4495-B0AF-89CEFB79DAAE
08713340-4E0B-4FE5-B5B3-0D6FBC94F0F0
5932BE8D-DF9A-4145-A924-E51E39895BE1
ghj=subset(LWDvol1, UID == '5932BE8D-DF9A-4145-A924-E51E39895BE1')
unique(ghj$TRANSECT)
try=cast(ghj,"UID+TRANSECT~PARAMETER", value="RESULT", fun="sum")
View(try)


count(LwdWet,c("UID",'DATE_COL')
00B03EF8-EC0E-4FF0-9CAF-DA64FAC3FFE3




mmw$v1w <- ((mmw$rchwsdsl * 0.058) + (mmw$rchwsdml * 0.182) + 
              (mmw$rchwsdll * 0.438) + (mmw$rchwmdsl * 0.333) + (mmw$rchwmdml * 
                                                                   1.042) + (mmw$rchwmdll * 2.501) + (mmw$rchwldsl * 0.932) + 
              (mmw$rchwldml * 2.911) + (mmw$rchwldll * 6.988) + (mmw$rchwxdsl * 
                                                                   3.016) + (mmw$rchwxdml * 9.421) + (mmw$rchwxdll * 22.62))
