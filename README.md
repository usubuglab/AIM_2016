Aquatic AIM
====

BLM/NAMC Aquatic AIM data handling and analysis

Monitoring data are collected nationally to understand the status, condition, and trend of resources on BLM lands. Data are collected in accordance with the BLM Assessment, Inventory, and Monitoring (AIM) Strategy. The AIM Strategy specifies a probabilistic sampling design, standard core indicators and methods, electronic data capture and management, and integration with remote sensing. Attributes include the BLM aquatic core indicators: pH, conductivity, temperature, pool depth, length, frequency, streambed particles sizes, bank stability and cover, floodplain connectivity, large woody debris, macroinvertebrate biological integrity, ocular estimates of vegetative type, cover, and structure and canopy cover. In addition, the contingent indicators of total nitrogen and phosphorous, turbidity, bank angle, thalweg depth profile and quantitative vegetation estimates. Data were collected and managed by BLM Field Offices, BLM Districts, and/or affiliated field crews with support from the BLM National Operations Center. Data are stored in a centralized database (AquADat) at the BLM National Operations Center.

R scripts synced to GitHUb are of two types 1) handling and manipulating raw data 2) analysis and indicator computation.  
Data import scripts are not included on GitHub and can be found at Z:\buglab\Research Projects\AIM\Technology\App Import but for the most up to date versions talk to David Fowler to get his local copies. In brief, XML files are parsed into excel files and then imported into a SQL database.

All subsequent scripts pull from this SQL database. The database structure is adapted from EPA NRSA. It is a flat key-value structure. The main advantage of this structure is the flexibility to support protocol changes and multiple projects. The main disadvantage is that the data is difficult to visual for the beginning user (hence R scripts that have been setup to perform queries and pivot manipulation). Data could be pivoted in SQL and made into views that would act like tables however my understanding is that in order to do this we would need to use min and max functions which might miss duplicated data. Currently all R scripts use the “cast” function to pivot data, which warns the user if multiple values exist for a given parameter, UID, transect, or point. 

To pull data from this database, QC it, and compute indicators and subsequent condition and extent estimates, run scripts in the following order:

1. DBpasswordJC_doNOTgit- individual file (not on GitHub) contains passwords for accessing SQL database
2. Data Consumption_WRSAdb
  -creates a ODBC connection to SQL database
  -installs and calls important packages that are needed in all other scripts
  -Calls FNC_tblRetrievePVT_new, which sets up important xwalk and table retrieve functions. xwalk function is criticial for data import, missing data QC, and updatedatabase scripts. Table retrieve function pulls data from database using filters without having to understand database structure
  -Calls indicatorXwalk, which translates indicators from EPA or R code names to final AquADat names.
3. DataQA_WRSA- spits out questionable data in a series of csv files for further review
4. UpdateDatabase_WRSA- edits data in SQL database using edits in "Z:\buglab\Research Projects\AIM\Analysis\QC\Office_Updates.csv", which have been exported from "Z:\buglab\Research Projects\DatabaseDoNOTmove\ProbSurvey_DB_v28Aug14recover.accdb"
5. JC_IndicatorCalc- computes all Aquatic AIM indicators and combines them into one file
6. Indicator_Export
  -joins Aquatic AIM indicators with bug data and site information from design database
  -converts cryptic EPA indicator names to AquADat names and exports data in correct format for AquADat
7. SpSurvey_DesignWeights- calculates weights for a given analysis
8. SpSurvey_ExtentEstimates- takes weight output and Routput worksheet with conditions from benchmark tool to generate extent estimate
9. NC_ExtentFigures or NC_ExtentFigures_GoodPoor- creates report figures for 3 condition categories or 2 categories respectively

Other important R Scripts
- this folder contains scripts that are critical for documentation or preform critical tasks but aren't used on a regular basis

misc R scripts
- this folder contains scripts that are not critical, are still in development, or were only intended for one time tasks

