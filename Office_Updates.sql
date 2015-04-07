
--query table to match ProbSurveyDB.Office_Updates
select '' as ID, [UID],[IND],
'' as transect, '' as point,
--transect, 
--point,
SAMPLE_TYPE, Parameter,
result, Flag
from tblverification
--from tblreach
--from tbltransect
--from tblpoint
where
UID=2043564548
RESULT like '%5153%'


select *
from tblverification
--from tblreach
--from tbltransect
--from tblpoint
where 
uid in (2043564548,
7469845862)

