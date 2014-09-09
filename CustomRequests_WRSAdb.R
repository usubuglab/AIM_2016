##Alaska Site Inventory
AKdesignation=addKEYS(tblRetrieve(Comments='Y',Parameters=c('SITE_ID'),Projects='AKEFO'),c('DATE_COL','LOC_NAME','LAT_DD','LON_DD','VALXSITE'))
#!need "add comments" ability via flag in tblRetrieve; AKdesignation needs whatever comment contains the site information, may need to change the parameter (manually need to add flags)

AKminesASSESS=addKEYS(tblRetrieve(Comments='Y',Parameters=c('IND_MINES','MINE'),Projects='AKEFO'),c('SITE_ID','DATE_COL','LOC_NAME'))
#? pipes, construction, liming, 'MAN_DREDGING','MAN_TREATMENT','HYDR'

write.csv(AKminesASSESS,'AKmine.csv')


#!TN and TP updates, BMI sampleID updates
