
library(DBI)
library(tidyverse)
library(BSOLTheme)

theme_set(
  theme_bsol()
)


con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MLCSU-BI-SQL", 
                 Database = "EAT_Reporting_BSOL", Trusted_Connection = "True")


sql1 <- 
  "Select
  ArrivalMode,
  ArrivalModeDescription,
  ArrivalDate,
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
  and ArrivalDate between {d'2023-11-01'} and {d'2024-02-29'}
  
  Union ALL
  
  Select ArrivalMode,
  ArrivalModeDescription,
  ArrivalDate,
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
  and ArrivalDate between {d'2024-11-01'} and {d'2025-02-28'}
  "

ecds <- dbGetQuery(con, sql1)

# how many attendances in each period

ecds |> 
  group_by(period) |> 
  summarise(attendances = n())
# More attendances in the INT period, so not as simple as straight numeric comparison 

ecds |> 
  group_by(ArrivalDate, period) |> 
  summarise(attendances = n()) |> 
  ggplot(aes(y=attendances, x=ArrivalDate, colour=period, group=period))+
  geom_line()+
  scale_color_bsol()



ecds |> 
  group_by(period) |> 
  summarise(attendances = n())