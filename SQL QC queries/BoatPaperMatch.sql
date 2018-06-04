select * from tblverification where result='boatable'


select * from tblverification where UID in (
--12647,13525,41118442824902848,45352089416990941184, 70090082748373450752,144923414890316824586,178321731468591267840, 3776736633483858024444826,28089224615978200000000,9853542448978955600002--all param
--no photos, no comments, UID dup
26565414904257093632
--no photos, forms noted in comments, UID dup
,8394834999920013312,40624157704749674660240,8528795104932701705866286
--no photos, forms noted in comments, no UID dup
,104960567641218646026,3894129524427155046400
) --and (COMMENT like '%paper%' or COMMENT like '%form%')
and parameter='site_id'
order by result


select * from tblreach
where 
--RESULT in ('OT-LS-7012','OT-LS-7024','OT-LS-7024','OT-RM-7083','OT-RO-7095','OT-SS-7133','XN-RM-4072')
 UID in (4364885883638,26565414904257093632,12784)--OT-LS-7012--ipad9--switch 436 to 265; dates diff, use orig; use elev from orig; use gps_fix from orig
--UID in (8394834999920013312,22784934936124764160,12792)--OT-LS-7024--ipad9
 --UID in (40624157704749674660240,26206468843598077952,12781)--OT-RO-7095--ipad9
 --UID in (12780,92086947643678100000000,8528795104932701705866286)--OT-SS-7133--ipad9
 --UID in (12782,3894129524427155046400)--OT-RM-7083--ipad9
--UID in (104960567641218646026)--XN-RM-4072--ipad9
order by uid, parameter

select * from tblVERIFICATION where LEFT(uid,5)=32627

--22784934936124764160 BOATABLE WRSA14 OT-LS-7024

select * from tblverification where result like '%4086%'

select * from tblPOINT where SAMPLE_TYPE like 'slop%' and UID=8175761734091837440

select * from tblmetadatabin

--begin transaction
--update tblcomments
--set  OPERATION='U', insertion='10/6/14'
----set reason='SWJ: comments added to full dataset', OPERATION='OD', ACTIVE='FALSE', deprecation='10/6/14'
--where  IND IN (5591662,5591663,5591664,5591665,5591666,5591667,5591668,5591669,5591670,5591671,5591672,5591673,5591674
--)
----commit transaction