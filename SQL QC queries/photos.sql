select SITEID, Count (Distinct UID) CountUID, DataType, ToProcess FinalDataSubmittedButNotImportedYet,  Count(DataType) NumberOfBackups, MAx(convert(date, datesampled,101)) MaxDateSampled, max(DateModified) MaxDateModified, Max(convert(date,UploadDate,101))MostRecentUploadDate from appdata 
where year(uploaddate)=2018 and month(uploaddate)>=5 and Processed=0 and DataType='image'
group by SITEID, DataType, ToProcess, Processed
order by DataType,SiteID, ToProcess, Processed

