 select table2.UID as FinalDataSubmitted, table1.* from 
 
 (select * from FieldSummary 
 where SiteID not in ('training1','calibration1','training2','calibration2','testorfake data') 
 and z_week>20 ) table1
 
 left join 
 
 (select SITE_ID,	DATE_COL,data_import.UID,LOC_NAME,PROJECT,PROTOCOL,VALXSITE,XSTATUS,CREW_LEADER,NAME1,NAME2,Z_FINALQA_COMMENT,SAMPLECOMMENT,ACCESSCOMMENT,Z_INDICATORS,REPEAT_VISIT,LAT_DD,LON_DD,LAT_DD_BR,LON_DD_BR,LAT_DD_TR,LON_DD_TR,	Z_DISTANCEFROMX,SLIDE_YN,AREA,BUG_METHOD,SAMPLER,JAR_NO,CONDUCTIVITY,PH,TEMPERATURE,TURBIDITY,TRCHLEN,PARTIAL_RCHLEN,POOL_COLLECT,POOLRCHLEN,SLOPE_COLLECT,SLPRCHLEN,AVGSLOPE,PCT_GRADE,BEAVER_FLOW_MOD	,BEAVER_SIGN,DEWATER
 from data_import

left JOin 
(SELECT     *

FROM       
  
   (SELECT       UID, PARAMETER, Result
                       FROM          tblreach where ACTIVE='TRUE') p PIVOT (min(Result) FOR Parameter IN (LAT_DD_BR,LON_DD_BR,LAT_DD_TR, LON_DD_TR, AREA, BUG_METHOD, SAMPLER,JAR_NO, CONDUCTIVITY,PH,TEMPERATURE , TURBIDITY , POOL_COLLECT, POOLRCHLEN, SLOPE_COLLECT,SLPRCHLEN,AVGSLOPE, PCT_GRADE,  BEAVER_FLOW_MOD, BEAVER_SIGN)) AS pvt
  
  
)pvt5
on data_import.UID=pvt5.UID) table2

on table1.pk_SampleEvent=table2.UID 

order by table1.project