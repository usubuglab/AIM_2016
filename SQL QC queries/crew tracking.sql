
select * from (select * from

(select Project as project1,COUNT(SiteID) as SampledSites from FieldSummary 
 where SiteID not in ('training1','calibration1','training2','calibration2','testorfake data') 
 and z_week>20  and sitestatus like 'sampled%' group by project) t1 
 full join 
 (select Project as project2, COUNT(SiteID) as FailedSites from FieldSummary 
 where SiteID not in ('training1','calibration1','training2','calibration2','testorfake data') 
 and z_week>20  and sitestatus not like 'sampled%' group by project) t2 
 
 on t1.Project1=t2.project2) tbl
 
 full join 

 (select * from 
  
  (select Project as project3,COUNT(SiteID) as InterruptedSites from FieldSummary 
 where SiteID not in ('training1','calibration1','training2','calibration2','testorfake data') 
 and z_week>20  and designation like 'interrupted%' group by project) t3
 
 full join 
 (select Project as project4,COUNT(SiteID) as PartialSites from FieldSummary 
 where SiteID not in ('training1','calibration1','training2','calibration2','testorfake data') 
 and z_week>20  and designation like 'partially%' group by project) t4 
 
 on t3.Project3=t4.project4) tbl2
 on tbl2.project3=tbl.project1
 

