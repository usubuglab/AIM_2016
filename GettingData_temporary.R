tblRetrieve(Parameters='CONDUCTIVITY',Projects='NorCal',Years='2013')

WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','NTL','PTL'),Projects='NorCal')


WQtblK=addKEYS((tblRetrieve(Parameters=c('CONDUCTIVITY','NTL','PTL'),Projects='NorCal')),c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))



WQtbl=tblRetrieve(Parameters=c('CONDUCTIVITY','NTL','PTL'),Projects='NorCal')
WQpvt=cast(WQtbl,'UID~PARAMETER',value='RESULT')
WQfinal=addKEYS(WQpvt,c('SITE_ID','DATE_COL','LOC_NAME','LAT_DD','LON_DD'))

