##Alaska Site Inventory
AKdesignation=addKEYS(tblRetrieve(Comments='Y',Parameters=c('SITE_ID'),Projects='AKEFO'),c('DATE_COL','LOC_NAME','LAT_DD','LON_DD','VALXSITE'))
#!need "add comments" ability via flag in tblRetrieve; AKdesignation needs whatever comment contains the site information, may need to change the parameter (manually need to add flags)

AKminesASSESS=addKEYS(tblRetrieve(Comments='Y',Parameters=c('IND_MINES','MINE'),Projects='AKEFO'),c('SITE_ID','DATE_COL','LOC_NAME'))
#? pipes, construction, liming, 'MAN_DREDGING','MAN_TREATMENT','HYDR'

write.csv(AKminesASSESS,'AKmine.csv')

##NorCal QC followup
#Nicole - All conductivity values to troubleshoot a low (6) value. 
ncond=tblRetrieve(Parameters=c('CONDUCTIVITY','CORRECTED'), Comments='Y',Projects='NorCal',Years=c('2013','2014'))
ncondPVT=addKEYS(cast(ncond,'UID+FLAG+COMMENT~PARAMETER',value='RESULT') #!comment/flag makes a separate pivot row...consider fixing if this functionaility is frequently desired (moving Comments addition to addKEYS?)
                 ,c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))#Include lat long for site proximity/mapping
write.csv(ncond,'NorCalConductivity_17Sept2014.csv')
write.csv(ncondPVT,'NorCalConductivityCorrected_17Sept2014.csv')#pivoted does not contain IND

#Scott - width and height checks: suspected fieldsheet flip flop and units issues
widhgt=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','BANKWID','BARWID'), Projects='NorCal',Years=c('2013','2014'))
widhgt2=tblRetrieve(Parameters=c('BANKHT','INCISED','WETWID','WETWIDTH','BANKWID','BARWID','BARWIDTH'), Projects='NorCal',Years=c('2013','2014'))
banks=tblRetrieve(Parameters=c('ANGLE','UNDERCUT'), Projects='NorCal',Years=c('2013','2014'))
banksnum=subset(banks,is.na(as.numeric(RESULT))==F);banksnum$RESULT=as.numeric(banksnum$RESULT)
widhgt=subset(widhgt,nchar(TRANSECT)==1 | substr(TRANSECT,1,1)=='X')

whPVT=cast(widhgt,'UID+TRANSECT~PARAMETER',value='RESULT')
bnkPVT=cast(banks,'UID+TRANSECT~PARAMETER+POINT',value='RESULT')      
tranPVT=addKEYS(merge(whPVT,bnkPVT,by=c('UID','TRANSECT'),all=T) ,c('SITE_ID','DATE_COL'))
wh2PVTavg=addKEYS(cast(rbind(banksnum,widhgt2),'UID~PARAMETER',value='RESULT',fun='mean')
                 ,c('SITE_ID','DATE_COL'))
View(subset(wh2PVTavg,BANKHT>INCISED))#!possible crossvalidation rule to scan for

write.csv(rbind(widhgt2,banks),'WidthHeightRaw_17Sept2014.csv')
write.csv(tranPVT,'WidthHeightPivot_17Sept2014.csv')
write.csv(wh2PVTavg,'WidthHeightAvg_17Sept2014.csv')

#Jennifer - missing data checks

#Jennifer - Slope checks
#!give crew lead, date, project, and protocol so we can look for systematic issues; these are also top priority sites for BR/TR slope comparison
#!query br/tr coordinates, pivot, addkeys for above
# select * from tblVERIFICATION where PARAMETER='project' and LEFT(uid,5) in (
#   43347,49602,56608,76596,95840,95840,60243,70773,25369,82402,97110,25360,28648,38339,42230,74084,84979,92568,22453,59619,67496,71247,73046,73046,95452,96164,10370,11805,12419--not full reach
#   ,25441,11625,11628,11832,12614,12619,12715--disconnected
#   --,74698,22828,16444,21013,25360,52508,23251,74173,31349--non transect name
#   ,95840,25441,73046,87601--otherwise odd
# )
# order by result


#!TN and TP updates, BMI sampleID updates

#checking for IND duplicates
# select * from(
#   select IND, COUNT(*)as cnt from(
#     select IND, 'v' as tbl, active, insertion,reason, parameter from tblVERIFICATION
#     union select IND, 'r' as tbl , active, insertion,reason, parameter from tblREACH
#     union select IND, 't' as tbl, active, insertion,reason, parameter from tblTRANSECT
#     union select IND, 'c' as tbl , active, insertion,reason, 'comment' parameter from tblcomments
#     union select IND, 'p' as tbl, active, insertion,reason, parameter  from tblpoint) as un
#   group by IND
# ) gr
# join (
#   select IND, 'v' as tbl, active, insertion,reason, parameter from tblVERIFICATION
#   union select IND, 'r' as tbl, active, insertion,reason, parameter from tblREACH
#   union select IND, 't' as tbl, active, insertion,reason, parameter from tblTRANSECT
#   union select IND, 'c' as tbl, active, insertion,reason, 'comment' as parameter from tblcomments
#   union select IND, 'p' as tbl, active, insertion,reason, parameter from tblpoint) as un2
# on un2.ind=gr.ind
# where cnt>1-- and ((REASON is not null and REASON <> 'NA') or (YEAR(INSERTION)=2014 and MONTH(INSERTION)>5))
# order by gr.ind, tbl


