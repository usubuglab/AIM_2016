SELECT     UID, AVGSLOPE,LAT_DD_BR,LAT_DD_TR,LON_DD_BR,LON_DD_TR,PCT_GRADE,SLOPE_COLLECT,SLPRCHLEN

FROM         (SELECT       UID, SAMPLE_TYPE, PARAMETER, Result
                       FROM          tblreach where parameter in ('AVGSLOPE','LAT_DD_BR','LAT_DD_TR','LON_DD_BR','LON_DD_TR','PCT_GRADE','SLOPE_COLLECT','SLPRCHLEN') ) p PIVOT (min(Result) FOR parameter in (AVGSLOPE,LAT_DD_BR,LAT_DD_TR,LON_DD_BR,LON_DD_TR,PCT_GRADE,SLOPE_COLLECT,SLPRCHLEN)) AS pvt
					order by avgslope desc