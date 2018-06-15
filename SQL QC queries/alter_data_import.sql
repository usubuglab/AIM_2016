USE [WRSAdb]
GO

/****** Object:  View [dbo].[data_import]    Script Date: 04/12/2018 09:20:18 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


ALTER VIEW [dbo].[data_import] as

select SITE_ID, pvt3.UID, LOC_NAME, PROJECT,PROTOCOL,VALXSITE,XSTATUS, DATE_COL,CREW_LEADER,REPEAT_VISIT,LAT_DD,LON_DD,NAME1, NAME2, Z_INDICATORS,Z_FINALQA_COMMENT,SAMPLECOMMENT,ACCESSCOMMENT, SLIDE_YN,Z_DISTANCEFROMX,DEWATER, TRCHLEN,PARTIAL_RCHLEN


from 
(select SITE_ID, pvt2.UID, LOC_NAME, PROJECT,PROTOCOL,VALXSITE,XSTATUS, DATE_COL,CREW_LEADER,REPEAT_VISIT,LAT_DD,LON_DD,NAME1, NAME2, Z_INDICATORS,Z_FINALQA_COMMENT,SAMPLECOMMENT,SLIDE_YN,Z_DISTANCEFROMX,DEWATER, TRCHLEN,PARTIAL_RCHLEN
from
 
 (select tblCOMMENTS.UID, tblCOMMENTS.COmment SAMPLECOMMENT from tblcomments where ACTIVE='TRUE' and FLAG='sample')p10

right join 
 (SELECT     SITE_ID, pvt.UID, LOC_NAME, PROJECT,PROTOCOL,VALXSITE,XSTATUS, DATE_COL,CREW_LEADER,REPEAT_VISIT,LAT_DD,LON_DD,NAME1, NAME2, Z_INDICATORS,Z_FINALQA_COMMENT,SLIDE_YN,Z_DISTANCEFROMX,DEWATER, TRCHLEN,PARTIAL_RCHLEN

FROM (SELECT       UID, SAMPLE_TYPE, PARAMETER, Result
                       FROM          tblverification where ACTIVE='TRUE') p PIVOT (min(Result) FOR Parameter IN (CREW_LEADER,LOC_NAME, DATE_COL,ELEVATION,LAT_DD,LON_DD,NAME1,NAME2, PROJECT,PROTOCOL, RCHWIDTH,REPEAT_VISIT,SITE_ID,TRCHLEN, PARTIAL_RCHLEN, VALXSITE,XSTATUS, Z_INDICATORS, Z_FINALQA_COMMENT,SLIDE_YN,Z_DISTANCEFROMX,DEWATER)) AS pvt
  where year(DATE_COL)=2017 --and protocol !='Failed' 

 ) pvt2
 ON p10.UID=pvt2.UID) pvt3
 
 LEFT JOIN
(select tblCOMMENTS.UID, tblCOMMENTS.COMMENT ACCESSCOMMENT from tblCOMMENTS where ACTIVE='TRUE' and FLAG='ACCESS') pvt4 
 ON pvt3.UID=pvt4.UID
 

GO

