##Alaska Site Inventory
AKdesignation=addKEYS(tblRetrieve(Parameters=c('VALXSITE'),Projects='AKEFO'),c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD','VALXSITE'))
#!need "add comments" ability via flag in tblRetrieve; AKdesignation needs whatever comment contains the site information, may need to change the parameter (manually need to add flags)

AKminesASSESS=addKEYS(tblRetrieve(Parameters=c('IND_MINES','MAN_DREDGING','MAN_TREATMENT','MINE','HYDR'),Projects='AKEFO'),c('SITE_ID','DATE_COL','LOC_NAME'))