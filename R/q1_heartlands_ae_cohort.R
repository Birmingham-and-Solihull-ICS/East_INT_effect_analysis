
library(DBI)
library(tidyverse)

con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MLCSU-BI-SQL", 
                 Database = "EAT_Reporting_BSOL", Trusted_Connection = "True")


sql1 <- 
  "Select
  ArrivalMode,
  ArrivalModeDescription,
  AgeAtActivityDate,
  GenderCode,
  GenderDescription,
  IndexOfMultipleDeprivationDecile,
  EthnicCategoryCode,
  EthnicCategoryDescription,
  LSOA11,
  AcornHouseholdTypeCode,
  [AcornHouseholdTypeDescription],
  [AcornWellBeingTypeCode],
  [WellbeingAcornTypeDescription],
  [WellbeingAcornFullDescription],
  [CarehomeCode],
  [IsAdmittedAsInpatient],
  [AcuityCode],
  [AcuityECDSDescription],
  [ChiefComplaintCode],
  [ChiefComplaintDescription],
  [ChiefComplaintGroup],
  [ElapsedMinsArrivalToConclusion],
  'control' as [period]
  from [ECDSV2].[VwECDSAll]
  Where ProviderSiteCode = 'RRK97'
  and ActivityYearMonth between 202311 and 202402
  
  Union ALL
  
  Select ArrivalMode,
  ArrivalModeDescription,
  AgeAtActivityDate,
  GenderCode,
  GenderDescription,
  IndexOfMultipleDeprivationDecile,
  EthnicCategoryCode,
  EthnicCategoryDescription,
  LSOA11,
  AcornHouseholdTypeCode,
  [AcornHouseholdTypeDescription],
  [AcornWellBeingTypeCode],
  [WellbeingAcornTypeDescription],
  [WellbeingAcornFullDescription],
  [CarehomeCode],
  [IsAdmittedAsInpatient],
  [AcuityCode],
  [AcuityECDSDescription],
  [ChiefComplaintCode],
  [ChiefComplaintDescription],
  [ChiefComplaintGroup],
  [ElapsedMinsArrivalToConclusion],
  'int' as [period]
  from [ECDSV2].[VwECDSAll]
  Where ProviderSiteCode = 'RRK97'
  and ActivityYearMonth between 202411 and 202502
  "

ecds <- dbGetQuery(con, sql1)

# how many attendances in each period

ecds |> 
  group_by(period) |> 
  summarise(attendances = n())
# More attendances in the INT period, so not as simple as straigh numeric comparison 