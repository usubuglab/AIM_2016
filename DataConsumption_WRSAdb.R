#------------------------------------------------------DOCUMENTATION------------------------------------------#
#for detailed explanation of R and SQL structure, see buglab\Research Projects\BLM_WRSA_Stream_Surveys\Technology\WRSA data management.docx
#test

#-------------------------------------------------------INPUTS--------------------------------------------------------#
# #In the ideal world, users should only need to put inputs here and be able to get results out of the 'black box' below using existing functions.
# DBpassword=''#Always leave blank when saving for security and because changes annually. Contact Sarah Judson for current password.
# DBuser=''#ditto as with DBpassword
# DBserver=''#ditto as with DBpassword
# DBname=''#ditto as with DBpassword
# #this is a change

#--------------------------------------------------------SETUP--------------------------------------------------------#
secretName = 'aimReader'
vaultPath = '//share1.bluezone.usu.edu/miller/buglab/Vault/aimReader.vault'
keyFile = file.path( path.expand("~"), "Vault", "aimReader.key" )
#keyFile = file.path( path.expand("~"), "Vault", "aimWriter.key" ) # uncomment line to get write access

#LOAD required packages#
requiredPACKAGES=c('reshape','R6','odbc','DBI','openssl','grid','gridExtra','openxlsx','zipR', 'sqldf','jpeg','spsurvey','tcltk','plyr','mapview','tidyverse')
for (r in 1:length(requiredPACKAGES)){
  if ((requiredPACKAGES[r] %in% installed.packages()[,1])==FALSE & requiredPACKAGES[r] %in% c('R6','odbc','DBI','openssl')){install.packages(requiredPACKAGES[r])}#auto-install if not present
  library(requiredPACKAGES[r],character.only = TRUE)
}

source('setupEnv.R');source('.BaseClass_R6.R'); source('Vault.R');
##needed to convert data to spatial layers and map coordinates
##to get this package to work properly you might also need R 3.4.4
library(sf)#if this gives an error run the line below
#devtools::install_github("edzer/sfr")#this is the most up to date version of the package but might not be stable. The stable version can be gotten from CRAN

#default working directory is the location of the Rproject which is custom to each collaborator and should automatically be set when project is loaded
#setwd('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Technology\\Output\\WRSA')#SWJ to do: map more dynamically but securely
#setwd('C:\\Users\\Sarah\\Desktop\\NAMCdevelopmentLocal\\WRSA')##Sarah desktop
dbConfig = Vault$new(vaultPath = vaultPath, keyFile = keyFile)$open()

wrsa1314 = DBI::dbConnect(odbc::odbc(),
	Driver = dbConfig$driver,
	Server = dbConfig$server,
	UID    = dbConfig$user,
	PWD    = dbConfig$password,
	Port   = 1433,
	Database = dbConfig$database
)
sqlQuery = odbc::dbGetQuery
sqlSave = function(conn, dat, tablename, rownames,  append){
	odbc::dbWriteTable(conn = conn, name = tablename, value = dat, row.names = rownames, overwrite = FALSE, append = append)
}
#test that connection is open # sqlQuery(wrsa1314,"select top 10 * from tblVerification")
#SWJ to do: throw this into a function that also prompts for server and password if missing (='')
#SWJ to do: throw the function into a separate referenced script because multiple files are using this

options(stringsAsFactors=F,"scipen"=50)#general option, otherwise default read is as factors which assigns arbitrary number behind the scenes to most columns


#SQL assistance functions
#loaded from a separate R script
source('FNC_tblRetrievePVT_new.R')
#common sQL strings that need to be incorporated:
##(select * from tblVERIFICATION where PARAMETER='site_id') v on v.UID=tblPOINT.uid
source('indicatorXwalk.R')


#------------------------------------------------------FILTERS-----------------------------------------------


###SITECODES####
####all site codes
sitecodes=c('')
sitecodes=c('SB-LS-21007','NW-TR-1166','MT-RV-1243')
sitecodes=c('AA-STR-0008','AA-013-2016','AA-TR-1137')
sitecodes=c('AA-STR-0015','AA-TR-1085','SW-TR-1300',	'SW-TR-1195',	'SW-TR-1218',	'FR-TR-1325',	'FR-TR-1097',	'FR-TR-1267',	'SV-RV-10722',	'FR-TR-1413',	'WE-LS-55160',	'NO-TR-1140',	'VB-TR-1225',	'VB-TR-1361',	'VB-TR-1493',	'VB-TR-1219',	'VB-TR-1020',	'BSG-SS-1084',	'VB-TR-1177',	'BT-TR-1414',	'BT-TR-1329',	'BT-TR-1329',	'BT-TR-1381',	'BT-TR-1110',	'BA-TR-1419',	'BA-TR-1479','BA-TR-1172',	'BA-TR-1083',	'AO-LS-1109',	'MB-LS-1219',	'MB-LS-15378',	'MB-LS-17613',	'MB-LS-17869',	'MB-LS-18450',	'MB-LS-18957',	'MB-SS-1212',	'MB-SS-1213',	'MB-SS-1216',	'MB-SS-12813',	'MB-SS-16909',	'MB-SS-17677',	'MT-LS-23525',	'MT-LS-24370',	'MT-LS-25394',	'MT-LS-28466',	'MT-SS-14386',	'MT-SS-29618',	'MT-TR-1064',	'XE-SS-5105',	'MT-RV-1243',	'MT-RV-1244',	'MT-TR-1062',	'WD-TR-020',	'WD-TR-021',	'WD-TR-022',	'WD-TR-023',	'WD-TR-1027',	'WD-TR-1034',	'WD-TR-1082',	'WD-TR-1167',	'WD-TR-1214',	'WD-TR-1232',	'WD-TR-1236',	'WD-TR-1259',	'WD-TR-1379',	'WD-TR-1435',	'WD-TR-1440',	'WD-TR-1473',	'WD-TR-1481','RI-RV-1158', 'CL-TR-1008','CL-TR-1263','BF-LS-1120',	'BF-RV-1105',	'BO-LS-1022',	'BO-RV-1026',	'BO-SS-1028',	'BO-SS-1040',	'BRF-SS-1164',	'BSG-LS-1077',	'BSG-SS-1073',	'BSG-SS-1084',	'VB-TR-1020',	'VB-TR-1072',	'VB-TR-1177',	'VB-TR-1219',	'VB-TR-1225',	'VB-TR-1356',	'VB-TR-1361',	'VB-TR-1427',	'VB-TR-1493')
####SITECODES FOR SPECIFIC ANALYSES
##QAduplicateSite
#sitecodes=c('EL-LS-8134','EL-SS-8127','MN-LS-1004','MN-SS-1104','MS-SS-3103','XE-RO-5086','XN-LS-4016','XN-SS-4128','XS-LS-6029' )
#escalante watershed sites
#sitecodes=c('CO-LS-9400','CO-LS-9432','CO-RO-9416','GS-LS-9010','GS-LS-9026','GS-LS-9027','GS-LS-9036','GS-RO-9003','GS-RO-9007','GS-RO-9008','GS-RO-9014','GS-SS-9004','GS-SS-9006','GS-SS-9012','GS-SS-9020','GS-SS-9022', 'GS-SS-9024','XE-LS-5005','XE-LS-5021','XE-LS-5025', 'XE-SS-5143')
##ID statewide design sites
#sitecodes=c('MN-LS-1005','MN-LS-1017','MN-SS-1121','MN-SS-1142','MN-LS-1003','MN-LS-1004','MN-LS-1014','MN-LS-1019','MN-RO-1086','IF-RV-10266','MN-SS-1104','MN-SS-1123','MN-SS-1129','MN-SS-1147','IF-SS-10310','IF-SS-10314','IF-SS-10438','IF-SS-10458','IF-SS-10570','MN-LS-1020','SA-LS-11386','SA-LS-12318','SA-LS-13342','MN-SS-1099','MN-SS-1143','SA-SS-10014','SA-SS-10250','SA-SS-10362','SA-SS-10442','SA-SS-10506','SA-SS-10554','SA-SS-10590','SA-SS-10810','SA-SS-11038','SA-SS-11078','SA-SS-11130','TF-SS-10638','XN-LS-4003','XN-LS-4010','XN-LS-4014','XN-LS-4016','XN-LS-4019','XN-LS-4026','XN-LS-4028','XN-RO-4082','XN-RO-4085','XN-RO-4086','BO-RV-10024','XN-SS-4097','XN-SS-4101','XN-SS-4113','XN-SS-4124','XN-SS-4135','XN-SS-4145','BO-SS-10140','BO-SS-10238','BO-SS-10248','XN-LS-4029','XN-LS-4006','XN-LS-4012','XN-LS-4018','TF-LS-10152','XN-SS-4143','TF-SS-10126','TF-SS-10385','TF-SS-10536')
##GrandStaircase site codes
#sitecodes=c('CO-LS-9432','CO-RO-9416','CO-SS-9415','GS-LS-9009','GS-LS-9010','GS-LS-9016','GS-LS-9025','GS-LS-9026','GS-LS-9027','GS-LS-9032','GS-LS-9036','GS-RO-9001','GS-RO-9003','GS-RO-9007','GS-RO-9008','GS-RO-9013','GS-RO-9014','GS-RO-9017','GS-SS-9004','GS-SS-9005','GS-SS-9006','GS-SS-9012','GS-SS-9020','GS-SS-9022','GS-SS-9024','XE-LS-5005','XE-LS-5025','XE-SS-5143')
#sitecodes=c('W3012','W308O2')
##cassie data request
#sitecodes=c('WD-TR-004', 'BO-SS-10238', 'GN-SS-10434', 'CR-SS-10621')
#sitecodes=c('FI-SS-11761', 'FI-SS-10737', 'FI-SS-12529', 'W202O2', 'WD-TR-011', 'XS-SS-6111')
#sitecodes=c('FI-RV-14028','FI-RV-17420','FI-SS-10737','FI-SS-11761','FI-SS-12529','SL-LS-10465','SL-LS-12065','SL-LS-14033','SL-RV-15697','SL-SS-10001','SL-SS-10577','SL-SS-11041','SL-SS-11068','SL-SS-11729','SL-SS-12513')
#sitecodes=c('CN-SS-12314','SA-LS-13770','US-SS-11678','PR-LS-13628','PR-SS-13036','VE-SS-19433','W3012','RA-LS-16633','RA-RV-16901','RA-SS-13906')
#sitecodes=c('MP-SS-2090','XE-SS-5126','NY-LS-9232','MP-SS-2080','XN-LS-4014','OT-LS-7017','XN-SS-4097','MS-LS-3016','NW-SS-9262','OT-SS-7128','XE-LS-5018','AR-LS-8032','CO-LS-9452','CO-LS-9423','GS-LS-9009','XN-SS-4165','GS-LS-9026','SU-SS-8315','GS-LS-9025','AR-SS-8066','MS-SS-3127','MS-LS-3003','XN-LS-4021','MN-RO-1081','XE-LS-5030','XS-SS-6111','XE-LS-5027','XE-LS-5023','GS-SS-9006','MS-SS-3097','CO-LS-9429','TP-SS-8290','EL-LS-8136','XN-LS-4027','MP-SS-2105','MS-SS-3114','MS-LS-3026','MN-SS-1142','MP-SS-2111','XE-SS-5155','NO-RO-9200','XN-LS-4007','GS-LS-9027','CO-SS-9425','NY-SS-9222','XS-LS-6007','MS-RO-3089','GS-RO-9001','MP-SS-2097','CO-RO-9412','OT-SS-7154','NO-RO-9205','CO-SS-9441','CO-LS-9400','XN-LS-4028','XE-SS-5143','MP-SS-2091','GS-LS-9032','CO-LS-9410','XE-SS-5154','OT-SS-7150','XS-SS-6146','AR-RO-8001','MP-LS-2002','GS-SS-9005','SU-LS-8348','MN-SS-1147','GS-LS-9010','MN-RO-1085','XE-RO-5081','GS-SS-9012','MS-LS-3028','MN-SS-1137','CO-LS-9424','GS-RO-9003','XN-LS-4018','XS-RM-6076','XN-SS-4146','CO-SS-9440','XE-SS-5150','MP-SS-2116','XE-LS-5019','NO-SS-9202','CO-SS-9430','XE-LS-5025','NY-LS-9230','GS-SS-9004','MN-SS-1121','NY-RO-9221','XE-SS-5106','XE-RO-5085','MP-SS-2108','XE-SS-5146','XS-RO-6087','OT-SS-7145','GS-LS-9036','EL-SS-8160','XE-LS-5031','MS-SS-3110','MP-LS-2017','XS-LS-6040','CO-SS-9419','XE-SS-5099','MS-LS-3027','CO-RO-9411','MP-LS-2003','XN-SS-4163','XS-SS-6095','XE-SS-5153','CO-LS-9445','CO-LS-9432','MP-SS-2103','OT-SS-7109','NY-LS-9220','CO-LS-9442','MP-SS-2079','XN-SS-4136','MS-SS-3126','MP-SS-2095','MN-LS-1022','CO-SS-9426','MS-LS-3009','XE-LS-5011','MN-SS-1129','XN-RO-4086','MN-LS-1017','MS-SS-3133','XS-LS-6030','XS-LS-6041','MN-RO-1086','NW-LS-9276','MS-SS-3100','XE-LS-5032','GS-RO-9008','NB-RO-9127','NO-LS-9206','CO-LS-9417','GS-RO-9007','XS-SS-6105','MS-SS-3101','TP-SS-8273','CO-RO-9401','XE-LS-5034','CO-LS-9448','MN-LS-1019','NC-LS-9156','GS-SS-9020','CO-RO-9402','MP-SS-2098','GS-SS-9024','MS-LS-3007','MS-LS-3024','EL-SS-8172','SU-RO-8330','MP-SS-2088','RG-LS-10658','VE-LS-10589','FMD-TR-005','PR-RV-10172','RG-SS-10121','IF-RV-10266','SA-SS-11130','G502O1','FI-SS-10737','WD-TR-004','WR-RV-10461','TA-LS-15090','WR-LS-10285','W21O2','VE-SS-10556','TR-LS-15538','PR-LS-10495','PR-LS-11167','TA-LS-12258','PR-SS-10463','PR-LS-10447','G406O1','SL-SS-12513','FA-RV-15794','NW-TR-002','FA-RV-11698','TR-SS-10882','FR-TR-015','SV-RV-10978','FMD-TR-012','WD-TR-014','G2062','WR-SS-11053','PR-RV-11004','TA-LS-15330','IF-TR-001','VE-RV-11165','FR-TR-004','WR-LS-10653','WD-TR-023','WD-TR-021','PR-LS-11596','PR-LS-11260','PR-LS-10719','IF-SS-10314','SL-SS-10577','TR-RV-10034','WD-TR-001','VE-RV-10397','WR-LS-10717','W210O2','WD-TR-020','SL-LS-14033','PR-SS-10687','GJ-LS-10237','GN-SS-10518','FR-TR-008','GN-LS-10114','KR-SS-10233','LS-RV-11065','PR-RV-10572','WR-LS-10973','SL-LS-12065','PR-RV-10607','SA-SS-11038','TR-LS-12146','PR-LS-11836','SL-SS-10001','GJ-LS-10381','TA-RV-10018','SL-SS-13393','VE-SS-13005','FR-TR-016','FMD-TR-006','SV-SS-11586','TF-SS-10385','GN-SS-10498','PR-LS-10319','SV-SS-10146','FMD-TR-008','FR-TR-009','TA-SS-38882','FR-TR-001','W2082','G221O2','TR-LS-10674','SA-TR-001','FMD-TR-013','SA-SS-10810','W202O2','FR-TR-006','FR-TR-013','BO-RV-10024','WR-SS-10505','IN-LS-009','KR-SS-10313','SV-LS-12610','TA-RV-14114','TA-SS-23522','VE-LS-11577','VE-LS-15517','GN-SS-10262','PR-SS-10015','LS-SS-13577','PR-RV-11132','WD-TR-022','UC-LS-10253','FMD-TR-003','LS-LS-10077','PR-RV-10684','GN-LS-10242','PR-RV-10063','GJ-LS-10493','FR-TR-010','UC-SS-10061','VE-LS-15673','W2061','G4012','PR-SS-11743','UC-RV-10317','PR-RV-11087','PR-SS-10044','SA-TR-002','TA-LS-13986','FMD-TR-009','SW-TR-002','IF-TR-002','FR-TR-012','VE-RV-11357','W206O2','PR-SS-10143','TF-SS-10126','PR-LS-12319','FI-SS-12529','FMD-TR-001','GJ-LS-10749','TA-SS-35810','UC-SS-10413','SL-SS-11041','WR-LS-11565','SA-TR-003','G406O2','SA-LS-13342','FMD-TR-004','NW-TR-003')
#sitecodes=c('CO-SS-9419',	'EL-LS-8134',	'EL-SS-8127',	'MN-LS-1004',	'MN-LS-1009',	'MN-LS-1017',	'MN-SS-1104',	'MN-SS-1129',	'MP-LS-2003',	'MP-SS-2078',	'MP-SS-2091',	'MS-LS-3024',	'MS-SS-3103',	'MS-SS-3126',	'NB-LS-9111',	'NW-LS-9276',	'OT-SS-7154',	'XE-LS-5019',	'XE-RO-5086',	'XE-SS-5105',	'XN-LS-4016',	'XN-LS-4018',	'XN-SS-4128',	'XS-LS-6029',	'XS-SS-6095')

###YEARS####
#all years
years=c('2020')
#current year
years=c('2019','2020')
#WRSA years
years=c('2013','2014','2015')#as character, not number
#years prior to 2019
years=c('2013','2014','2015','2016','2017','2018','2019')#as character, not number


###PROTOCOLS###
#2020
protocols=c('WADE2020','BOAT2020','WADE2016','BOAT2016')
#2016+ protocols (current)
protocols=c('WADE2016','BOAT2016')
#pre 2016 protocols
protocols=c('NRSA13','WRSA14','BOAT14','AK14')#for separating differences in overall protocol, may not be relevant for some parameters
#all protocols
protocols=c('NRSA13','WRSA14','BOAT14','AK14','WADE2016','BOAT2016')
#all wadeable
protocols=c('NRSA13','WRSA14','AK14','WADE2016')
#all boating
protocols=c('BOAT14','BOAT2016')
protocols=c('')#includes failed sites


###PROJECTS###
# #ALL 2019 projects
# projects=c('AKEFO','AK_AN_BSWI_2017','AK_CY_PLANUNIT_2017','AK_CY_UTILITYCORRIDOR_2016','AK_GL_STANDARD_2016','AZ_TU','CA_NC','CO_FR_STANDARD_2016','CO_NW_STANDARD_2016','CO_SW_STANDARD_2016','ID_PO','ID_JA','ID_US_STANDARD_2017','ID_CT','ID_CH_STANDARD_2017','ID_CH_FISH_2017','ID_SA_STANDARD_2016','NM_SONM_STANDARD_2018','NV_NO','NV_NC','NV_NW','OR_BU','OR_LL','OR_LK','OR_VB','OR_VM','UT_CL','UT_CY_STANDARD_2017','UT_WD_SHEEPROCK_2017','WY_HD_STANDARD_2017','WY_RA_STANDARD_2016','WRSA_AZ','WRSA_CA','WRSA_CO','WRSA_ID','WRSA_MT','WRSA_NM','WRSA_NV','WRSA_OR_WA','WRSA_UT','WRSA_WY')
# 
# #ALL 2018 projects
# projects=c('AKEFO','NPRA15','AK_AN_BSWI_2017','AK_CY_PLANUNIT_2017','AK_CY_UTILITYCORRIDOR_2016','AK_GL_STANDARD_2016','AK_DNP_KANTISHNAHILLS_2018','CO_FR_STANDARD_2016','CO_SW_STANDARD_2016','CO_NW_STANDARD_2016','NM_SONM_STANDARD_2018','OR_PR_WSR_2018','OR_PD_OREGONSPOTTEDFROG_2018','UT_GR_WSP_2018','WY_RA_STANDARD_2016','UT_CY_STANDARD_2017','UT_WD_SHEEPROCK_2017','WY_HD_STANDARD_2017')
# 
# #ALL 2017 projects
# projects=c('AKEFO','NPRA15','AK_AN_BSWI_2017','AK_CY_PLANUNIT_2017','AK_CY_UTILITYCORRIDOR_2016','AK_GL_STANDARD_2016','WA_SP_STANDARD_2016','CO_FR_STANDARD_2016','CO_SW_STANDARD_2016','CO_NW_STANDARD_2016','ID_SA_STANDARD_2016','ID_CH_STANDARD_2017','ID_US_STANDARD_2017','NM_FMD_STANDARD_2016','OR_PR_PERENNIAL_2016','OR_PR_INTERMITTENT_2016','UT_GR_STANDARD_2016','UT_WD_STANDARD_2016','WY_RA_STANDARD_2016','UT_CY_STANDARD_2017','UT_WD_SHEEPROCK_2017','WY_HD_STANDARD_2017')
# #2017 priority projects
# projects=c('UT_GR_STANDARD_2016','UT_WD_STANDARD_2016','WY_RA_STANDARD_2016','UT_WD_SHEEPROCK_2017','ID_SA_STANDARD_2016','AKEFO')
# #2017 non priority projects
# projects=c('NPRA15','AK_AN_BSWI_2017','AK_CY_PLANUNIT_2017','AK_CY_UTILITYCORRIDOR_2016','AK_GL_STANDARD_2016','WA_SP_STANDARD_2016','CO_FR_STANDARD_2016','CO_SW_STANDARD_2016','CO_NW_STANDARD_2016','ID_CH_STANDARD_2017','ID_US_STANDARD_2017','NM_FMD_STANDARD_2016','OR_PR_PERENNIAL_2016','OR_PR_INTERMITTENT_2016','UT_CY_STANDARD_2017','WY_HD_STANDARD_2017')
# 
# #ALL 2016 projects
# projects=c('AKEFO','NPRA15','AK_GL_STANDARD_2016','AK_CY_UTILITYCORRIDOR_2016','WA_SP_STANDARD_2016','CO_FR_STANDARD_2016','CO_SW_STANDARD_2016','CO_NW_STANDARD_2016','ID_SA_STANDARD_2016','ID_STATE_STANDARD_2016','NM_FMD_STANDARD_2016','OR_PR_PERENNIAL_2016','OR_PR_INTERMITTENT_2016','UT_GR_STANDARD_2016','UT_WD_STANDARD_2016','WY_RA_STANDARD_2016')
# 
# #PRE 2016 projects
# projects=c('WRSA','NV','GSENM','COPLT','2015ProtocolOverlap','AKEFO','NORCAL')# most useful for separating NorCal and WRSA, note that abbreviations differ between Access and SQL/FM
# projects=c('OR_PR_PERENNIAL_2016','OR_PR_INTERMITTENT_2016')

#ALL projects
# projects=c('UT_CL_STANDARD_2019','AKEFO','NPRA15','AK_AN_BSWI_2017','AK_CY_PLANUNIT_2017','AK_GL_STANDARD_2016','AK_CY_UTILITYCORRIDOR_2016','AK_DNP_KANTISHNAHILLS_2018','AZ_TU','CA_NC','WA_SP_STANDARD_2016','CO_FR_STANDARD_2016','CO_SW_STANDARD_2016','CO_NW_STANDARD_2016','ID_PO','ID_CT','ID_SA_STANDARD_2016','ID_STATE_STANDARD_2016','ID_CH_STANDARD_2017','ID_CH_FISH_2017','ID_JA','ID_US_STANDARD_2017','NM_FMD_STANDARD_2016','NM_SONM_STANDARD_2018','NV_NC','NV_NO','NV_NW','OR_BU','OR_LL','OR_LK','OR_VB','OR_VM','UT_CL','OR_PR_WSR_2018','OR_PD_OREGONSPOTTEDFROG_2018','UT_GR_WSP_2018','OR_PR_PERENNIAL_2016','OR_PR_INTERMITTENT_2016','UT_GR_STANDARD_2016','UT_CY_STANDARD_2017','WY_HD_STANDARD_2017','UT_WD_STANDARD_2016','UT_WD_SHEEPROCK_2017','WY_RA_STANDARD_2016','WRSA','WRSA_AZ','WRSA_CA','WRSA_CO','WRSA_ID','WRSA_MT','WRSA_NM','WRSA_NV','WRSA_OR_WA','WRSA_UT','WRSA_WY','NV','GSENM','COPLT','2015ProtocolOverlap','AKEFO','NORCAL')

projects=c('WRSA_AZ',	'WRSA_CA',	'WRSA_CO',	'WRSA_ID',	'WRSA_MT',	'WRSA_NM',	'WRSA_NV',	'WRSA_OR_WA',	'WRSA_UT',	'WRSA_WY','2015ProtocolOverlap',	'AZ_TusconFO','AK_AnchorageFO',	'AK_ArcticDO',	'AK_CentralYukonFO',	'AK_DenaliNP',	'AK_EasternInteriorFO',	'AK_GlennallenFO',	'CA_NorthernCaliforniaDO',	'CO_NorthwestDO',	'CO_RockyMountainDO',	'CO_SouthwestDO',	'ColoradoPlateau',	'ID_ChallisFO',	'ID_CottonwoodFO',	'ID_JarbidgeFO',	'ID_PocatelloFO',	'ID_SalmonFO',	'ID_Statewide',	'ID_UpperSnakeFO',	'NM_FarmingtonDO',	'NM_SouthernNewMexico',	'NV_CarsonCityDO',	'NV_ElkoDO',	'NV_Statewide',	'NV_WinnemuccaDO',	'OR_BakerFO',	'OR_BurnsDO',	'OR_KlamathFallsFO',	'OR_LakeviewFO',	'OR_MalheurFO',	'OR_PrinevilleDO',	'UT_CanyonCountryDO',	'UT_ColorCountryDO',	'UT_GrandStaircaseEscalanteNM',	'UT_PariaRiverDO','UT_GreenRiverDO',	'UT_WestDesertDO',	'WA_SpokaneDO',	'WRSA',	'WY_HighDesertDO',	'WY_RawlinsFO')
projects=c('WRSA_UT',	'AK_AnchorageFO',	'CA_NorthernCaliforniaDO','OR_MalheurFO')

#TRAINING
projects=c('Training','Training- Grand Junction','Training- Logan')

# #CO projects
# projects=c('WRSA','COPLT','CO_FR_STANDARD_2016','CO_SW_STANDARD_2016','CO_NW_STANDARD_2016')
#AK projects
projects=c('AK_AnchorageFO',	'AK_ArcticDO',	'AK_CentralYukonFO',	'AK_DenaliNP',	'AK_EasternInteriorFO',	'AK_GlennallenFO')


###INSERTION###
#all data 
insertion=c('')

#number of the week of the year starting on Wednesday(because Wednesday was the 1st) for 2020
#batches of 2020 QC
insertion=c('25') # June 10th
insertion=c()


# ####OTHER POSSIBLE FILTERS######
# 
# AllData='N'#set to 'Y' (meaning 'yes') if you want to query all sites (note this is quite time consuming and large, use provided filters wherever possible)
# AllParam='Y'#set to 'Y' (meaning 'yes') if you want to query all parameters
# #If you put this in instead of any parameters it will get all from a specific table, tblcomment, tblverification, tbltransect etc.
# Table=''
# Parameters=c('')#input desired 
# 
# filter="PARAMETER='SITE_ID'and month(INSERTION) in('3')" # the problem with this filter is it does a union of this with all other filters used (project ect.) so you can't use this to get everything in a given project AND apply this filter. Can't figure out how to edit that in the table retreive function...that could be really useful
# dates=''##example:c('05/05/2005')
# hitchs=c('')#NOT WORKING YET, hitch and crew level generally maintained by Access not SQL
# crews=c('R1')#NOT WORKING YET, hitch and crew level generally maintained by Access not SQL#see crewKC in customrequests for possible method
# filter=''#custom filter (need working knowledge of Parameter:Result pairs and SQL structure; example: "(Parameter='ANGLE' and Result>50) OR (Parameter='WETWID' and Result<=0.75))"
# UIDs='BLANK'#custom filter (need working knowledge of primary keys)
# QAdup='N'#set QAdup='N' to eliminate site QA duplicates
# #NorCal settings: #years=c('2013','2014');projects='NorCal';protocols=c('WRSA14','NRSA13')
# #WRSA QC settings: #years=c('2014'); projects='WRSA';protocols=c('WRSA14')
# 
# 
# #------------------------------------------------------EXAMPLES------------------------------------------------------------------#
# 
# #Most data requests use the following basic workflow and structure. Save any custom requests created to CustomRequest_WRSAdb.R for documentation.
# #CALL data in using tblRetrieve() #at least ONE filter required, Parameters NOT required, Comments optional (default is no). For possible filters, see "WRSA data managment.docx" OR use getAnywhere(tblRetrieve) and examine available varaiables in the function() inputs section.
# EXAMPLEcond=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED'), Comments='N',Projects='NorCal',Years=c('2013','2014'))
# #remove QA duplicates
# EXAMPLEcond=removeDUP(EXAMPLEcond,QA='N')
# #PIVOT data using cast() function for easier viewing. IND will be lost if need for tracking. Alternative: aggregate() function OR PVTconstruct() assists in building SQL string for custom PIVOTS in SQL Server.
# EXAMPLEcondPVT=cast(EXAMPLEcond,'UID~PARAMETER',value='RESULT') 
# #KEYS added for data interpretability. Any parameters stored in tblVERIFICATION are available to add. Suggested minimum additions are Site_ID + Date_COL. In this example, coordinates for mapping.
# EXAMPLEcondPVT=addKEYS(EXAMPLEcondPVT ,c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))
# #EXPORT results via csv
# write.csv(EXAMPLEcondPVT,'ExampleConductivityCorrected_TodaysDate.csv')#pivoted does not contain IND
# 
# #Example of retrieving all raw data for an entire project
# NorCal1314=tblRetrieve(ALLp='Y',Years=c('2013','2014'),Projects='NorCal')
# NorCal1314subCOND=subset(NorCal1314,PARAMETER %in% c('CONDUCTIVITY','CORRECTED'))#and again subsetting it just for a few parameters like EXAMPLEcond
# 
# ##---------METADATA for reference---------##
# #use RODBC package sqlQuery() function, not tblRetrieve
# #parameter descriptions
# METADATA=sqlQuery (wrsa1314,"select * from tblMETADATA where ACTIVE='TRUE'")#see "Label" for interpretable names #be careful with SQL strings, enclose in double quote and use single quotes for text
# #query a particular protocol
# METADATAprotocol=sqlQuery (wrsa1314,"select * from tblMETADATAprotocol where ACTIVE='Y' and Protocol='WRSA14'")#expected counts
# #compare protocols
# METADATAprotocolS=sqlQuery(wrsa1314, "select distinct Result from tblverification where parameter='Protocol' union select distinct Protocol from tblmetadataprotocol")
#     #METADATAprotocolS=protocols
#     METADATAprotocolS=data.frame(METADATAprotocolS)
#     protocolSTR1="select m.SAMPLE_TYPE,m.PARAMETER,m.UNITS,m.LABEL,m.VAR_TYPE,m.ACTIVE,m.INSERTION,m.REASON"
#     protocolSTR2a="left join (select PROTOCOL,SAMPLE_TYPE as ST, PARAMETER as PM, POINTS,REPS,Insertion,NOTE  from tblMetadataProtocol where Protocol='%s' and ACTIVE='Y') %s on %s.ST=substring(m.SAMPLE_TYPE,1,len(m.SAMPLE_TYPE)-1) and %s.PM=m.parameter"
#     METADATAprotocolSpairs=subset(expand.grid(P1=unclass(METADATAprotocolS)[[1]],P2=unclass(METADATAprotocolS)[[1]]),P1!=P2)
#     protocolSTR3a="(isnull(%s.Points*%s.Reps,0) <> isnull(%s.Points*%s.Reps,0))"
#     for (p in 1:nrow(METADATAprotocolS)){
#       currP=METADATAprotocolS[p,1]
#       protocolSTR1=sprintf("%s, %s.*",protocolSTR1,currP)
#       protocolSTR2=sprintf("%s %s",ifelse(p==1,'',protocolSTR2),sprintf(protocolSTR2a,currP,currP,currP,currP))
#       pair=subset(METADATAprotocolSpairs,P1==currP)
#       for (r in 1:nrow(pair)){
#         p1=pair$P1[r];p2=pair$P2[r]
#         protocolSTR3=sprintf("%s %s",ifelse(p==1,'',sprintf("%s or", protocolSTR3)),sprintf(protocolSTR3a,p1,p1,p2,p2))
#       }
#     }
#     protocolSTRdiff=sprintf("%s from tblmetadata m %s where %s order by m.SAMPLE_TYPE, m.parameter",protocolSTR1,protocolSTR2,protocolSTR3)
#     protocolSTRcomp=sprintf("%s from tblmetadata m %s order by m.SAMPLE_TYPE, m.parameter",protocolSTR1,protocolSTR2)
#     METADATAprotocolCOMPARE=sqlQuery(wrsa1314,protocolSTRcomp)  
#     METADATAprotocolDIFF=sqlQuery(wrsa1314,protocolSTRdiff)  
#     View(METADATAprotocolDIFF)
# #legal values for parameters
# METADATArange=sqlQuery (wrsa1314,"select * from tblMETADATArange where ACTIVE='TRUE' and Protocol='WRSA14'")#legal values
# #matchup of parameters to indicators
# METADATAindicators=sqlQuery (wrsa1314,"select * from tblXwalk where NAME_xwalk='MissingBackend' and type_xwalk='Indicator'")
#     indicators=NULL
#     for (i in 1:nrow(METADATAindicators)){indicators=paste(indicators,METADATAindicators$Parameter_Xwalk[i],sep="|")}
#     indicators=unique(unlist(strsplit(indicators,"\\|")));indicators=indicators[2:length(indicators)];indicators=gsub(" ","",indicators)
#     for (p in 1:length(indicators)){
#       METADATAparameters=sqlQuery (wrsa1314,sprintf("select * from tblXwalk where NAME_xwalk='MissingBackend' and PARAMETER_Xwalk like '%%%s%%'",indicators[p]))
#       METADATAparameters$INDICATOR=indicators[p]
#       if(p==1){parameters=METADATAparameters} else{parameters=rbind(parameters,METADATAparameters)}
#     }  
#     View(indicators);View(parameters)
# 
# 
# #--------------------------------------------------------SQL RETRIEVE (old examples)--------------------------------------------------------#
# 
# #select samples
# UIDs=UIDselect(ALL=AllData,Filter=filter,UIDS='',SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
# #SWJ to do: add additional filters
# #SWJ to do: prompt for data entry (mini-GUI)
# 
# 
# #retrieve all data as a single list table
# UnionTBL=tblRetrieve(Table='',Parameters='',ALLp=AllParam,UIDS=UIDs,ALL=AllData,Filter=filter,SiteCodes=sitecodes,Dates=dates,Years=years,Projects=projects,Protocols=protocols)
# 
# 
# Sites=subset(UnionTBL,select=c(UID,RESULT),subset=PARAMETER=='SITE_ID'); colnames(Sites)=c('UID','SITE_ID')#!append sitecode instead of UID to make the table more readable --> migrate this into tblRetrieve or some kind of "convert" function
# UnionTBL=merge(UnionTBL,Sites)
# UnionTBL$SITE_ID=as.character(UnionTBL$SITE_ID)
# UnionTBL1=merge(UnionTBL,UIDs)#limit by UIDs ("select samples)
# 
# #retrieve desired tables
# #EXAMPLES of tblRetrieve function# (note: parameter lists were specified in the "Inputs" section at the beginning of this script)
# tblREACH=tblRetrieve('tblREACH')#not specifying parameters will retrieve the entire table
# tblREACHtest=tblRetrieve('tblREACH',testP)
# tblPOINTbank=tblRetrieve('tblPOINT',bankP)
# #SWJ to do - could add GIS tables (pull from PilotDB if possible)
# #SWJ to do - could add logistics tables (pull from UTBLM.accdb)
# 
# 
# 
# #Close ODBC connection when done talking to SQL Server
# odbcClose(wrsa1314); rm(DBpassword); rm(DBserver); rm(DBuser)
# 
# #--------------------------------------------------------CUSTOM PIVOT VIEWS--------------------------------------------------------#
# ##RESHAPE to PIVOT## 
# #EXAMPLES of both methods#
# #SQL option ('View' creation to copy/paste)
# bankPVTstr=PVTconstruct(parameters=bankP,tblTYPE='tblPOINT', filter="POINT in ('LF','RT')");print(bankPVTstr)#- need permission from Sarah Judson and to reopen ODBC before saving Views for permanent use in SQL Server
#   #retrieve said query from SQL
#     wrsa1314_2=odbcDriverConnect(connection = wrsaConnectSTR)
#     tblPOINTbankPVTs=sqlQuery(wrsa1314_2,bankPVTstr)
#     odbcClose(wrsa1314_2)
# #R option (cast)
# tblPOINTbankPVTr=cast(subset(tblPOINTbank,select=c(UID, TRANSECT,POINT,PARAMETER,RESULT)), UID + TRANSECT + POINT ~ PARAMETER)#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function
# 
# 
# #--------------------------------------------------------ANALYSIS--------------------------------------------------------#
# ##AGGREGATION##
# #EXAMPLES#
# #count number of records per parameter to check for missing data
# qastatsBANK_CNTcast=cast(tblPOINTbank, UID ~ PARAMETER, value='RESULT', fun.aggregate=length)#should this filter out NULLs or flags? does EPA write a line for each record even if no value recorded?
# qastatsBANK_CNTagg=aggregate(tblPOINTbank,FUN='length', by=list(tblPOINTbank$UID,tblPOINTbank$TRANSECT,tblPOINTbank$POINT))
# #cast seems like the more elegant solution
# #convert numerics before performing stats
# tblPOINTbankNUM=subset(tblPOINTbank,subset= is.na(as.numeric(as.character(tblPOINTbank$RESULT)))==FALSE);tblPOINTbankNUM$RESULT=as.numeric(as.character(tblPOINTbankNUM$RESULT))
# qastatsBANK_MEANcast=cast(tblPOINTbankNUM, UID ~ PARAMETER, value='RESULT', fun.aggregate=mean)
# 
# #iteration example
# list=c(1,2,4,6,7)
# for (i in 1:length(list)){
#   if(list[i]<5){
#     print(list[i] + 2)
#   } else {print(list[i] *5 )}
# }
# 
# 
# ##QA checks##
# ##!QA checks moved to DataQA_WRSA
# 
# 
# ##GRTS adjusted weights##
# #TBD# Pull from UTBLM
# 
# ##EPA aquamet##
# #TBD# Pull from aquamet 1.0 provided by Tom Kincaid and Curt Seegler via Marlys Cappaert
# #go to NRSAmetrics_SWJ.R
# 
# ##OE computation##
# #TBD# Pull from VanSickle
# 
# #Predicted WQ##
# #TBD#  Pull from UTBLM, John Olson/Ryan Hill
# 
# ##NMDS##
# #TBD#
# 
# ##GIS connections##
# #TBD#
# 
# 
# #--------------------------------------------------------REPORTING--------------------------------------------------------#
# ##Figures and Tables##
# #TBD# Pull from UTBLM
# 
# ##SWEAVE##
# #TBD#
# 
# ##BibTex##
# #TBD#
# 
# 
# #--------------------------------------------------------sarah's Gibberish-------------------------------------------------------#
# # #pseudocode - consume data from flat db
# # #ODBC connection to SQL server WRSAdb
# # #import via SQL string call - include filters on data (i.e. hitch, project, crew)
# # #mash (merge) tables (?) OR pvt for viewing (?) -- SQL: view, Access: Query
# # ##ex (old): merge(EVENT3,subset(original,select=selectCOLparse),by="SampleID")
# # ##demonstrate complexity of calling by column name vs. parameter text in both SQL and R
# # ###SQL: filter query --> possibly PIVOT to view --> aggregate query
# # ###R: filter strings, apply across multiple --> PVT to view --> aggregate OR run predefined(EPA,R)/custom functions
# # ###common: convert numerics
# # ###differences: null handling, reproducability and documentation
# # ###leaning (SWJ): R for dynamic queries/code, reproducability and 'instant' documentation; in either mode, PIVOTS should be treated as temporary views for scanning data, not basis for subsequent queries because they will then be tied to column names 
# # ##ex: library('reshape'); cast(data, x~y)
# # #separate numbers and characters (will R autodetect?) -- SQL: Cast/Convert
# # #filter by parameter and run metric  -- SQL: sub-queries
# # ##ex: subset(tblPOINT, subset=Parameter=='Angle')
# # ##could set it up so that user doesn't even need to which table
# # ##set up to easily call the parameters table and other metadata (crew, hitch) tables --> will we store crew and hitch info in sampletracking access or SQL server?
# # #aggregate by site and crew  -- SQL: group by (aggregate) query
# # ##ex (old): aggregate(x=as.numeric(sampDATAin$SampleID),FUN=agg3,by=list(sampDATAin$SamplingEvent,sampDATAin$Station,sampDATAin$WaterYear))
# # #report -- R SWEAVE vs. Access report vs. Crystal Reports
# # 
# # #check for existing packages
# # #install.packages('reshape')
# # library('reshape')
# # 
# # #establish an ODBC connection#
# # #the db was created in SQL Server Manager on 11/19/2013
# # #manually set up the database (WRSAdb) and the odcb connection (WRSAconnect)
# # library("RODBC")
# # user='feng'
# # #ENTER DB PASSWORD
# # print ("Please enter Password")
# # password='Something~Clever!@'#("Enter Password")#raw_input() in python, not sure of R equivalent #http://rosettacode.org/wiki/Dynamic_variable_names#R
# # nrsa1314<-odbcConnect("WRSAconnect",uid=user,pwd=password)
# # #SQL assistance functions
# # #inLOOP: concatenate list objects into an "IN" string for insertion into queries
# # inLOOP=function(inSTR) {
# #   inSTR=unlist(inSTR)
# #   for (i in 1:length(inSTR)){
# #     comma=ifelse(i==length(inSTR),'',',')
# #     STRl=sprintf("'%s'%s",inSTR[i],comma)
# #     if(i==1){loopSTR=STRl} else{loopSTR=paste(loopSTR,STRl)}
# #   }   
# #   return(loopSTR) 
# # }
# # #tblRetrieve: standard retrieval query
# # tblRetrieve=function(table, parameters=''){
# #   if(parameters==''){parameters=sqlQuery(nrsa1314,sprintf("select distinct parameter from %s", table))}
# #   sqlTABLE=sqlQuery(nrsa1314, sprintf('select * from %s where UID in (%s) and parameter in (%s)',table, inLOOP(UIDs),inLOOP(parameters)))
# #   return(sqlTABLE)#could auto return the pivoted view, but currently assuming that is for on the fly viewing and is not the easiest way to perform metrics
# # }
# # 
# # 
# # #FILTERS
# # ##from most to least specific
# # sitecodes=c('AR-LS-8003','AR-LS-8007', 'TP-LS-8240')
# # dates=c('05/05/2005')
# # hitchs=c('')
# # crews=c('R1')
# # projects=c('NRSA')
# # 
# # 
# # 
# # #select samples
# # UIDs=sqlQuery(nrsa1314, sprintf("select distinct UID from tblVERIFICATION 
# #                                 where (active='TRUE') 
# #                                 AND ((Parameter='SITE_ID' and Result in (%s)) OR (Parameter='DATE_COL' and Result in (%s)))"
# #                                 ,inLOOP(sitecodes),inLOOP(dates)))
# # #SWJ to do: add additional filters
# # #SWJ to do: prompt for data entry (mini-GUI)
# # 
# # #PARAMETERS
# # #specify if desired (will make queries less intensive):
# # testP=c('ANGLE','APPEALING','ALGAE')#test, one from each level of table
# # bankP=c('ANGLE','UNDERCUT','EROSION','COVER','STABLE')
# # 
# # #retrieve desired tables
# # tblREACH=tblRetrieve('tblREACH')#not specifying parameters will retrieve the entire table
# # tblREACHtest=tblRetrieve('tblREACH',testP)
# # tblPOINTbank=tblRetrieve('tblPOINT',bankP)
# # 
# # #pivot tables for viewing
# # tblPOINTbankPVT=cast(subset(tblPOINTbank,select=c(UID, TRANSECT,POINT,PARAMETER,RESULT)), UID + TRANSECT + POINT ~ PARAMETER)#very predictable structure except for the input table and whether transect and point need to be included in the columns = possibly plug into function
# # 
# # #further subset data in custom ways
# # 
# # #compute aggregate statistics
# # #count number of records per parameter to check for missing data
# # qastatsBANK_CNTcast=cast(tblPOINTbank, UID ~ PARAMETER, value='RESULT', fun.aggregate=length)#should this filter out NULLs or flags? does EPA write a line for each record even if no value recorded?
# # qastatsBANK_CNTagg=aggregate(tblPOINTbank,FUN='length', by=list(tblPOINTbank$UID,tblPOINTbank$TRANSECT,tblPOINTbank$POINT))
# # #cast seems like the more elegant solution
# # #convert numerics before performing stats
# # tblPOINTbankNUM=subset(tblPOINTbank,subset= is.na(as.numeric(as.character(tblPOINTbank$RESULT)))==FALSE);tblPOINTbankNUM$RESULT=as.numeric(as.character(tblPOINTbankNUM$RESULT))
# # qastatsBANK_MEANcast=cast(tblPOINTbankNUM, UID ~ PARAMETER, value='RESULT', fun.aggregate=mean)
# # 
# # #plugging into aquamet
# # 
# # #end ODBC connection#
# # odbcClose(nrsa1314)
