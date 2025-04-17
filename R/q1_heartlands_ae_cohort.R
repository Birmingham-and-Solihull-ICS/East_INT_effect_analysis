
library(DBI)
library(tidyverse)
library(scales)
library(BSOLTheme)

theme_set(
  theme_bsol()
)


con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MLCSU-BI-SQL", 
                 Database = "EAT_Reporting_BSOL", Trusted_Connection = "True")


sql1 <- 
  "Select
  T2.Locality,
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
  from [ECDSV2].[VwECDSAll] T1
  LEFT JOIN EAT_Reporting_BSOL.Reference.BSOL_ICS_PracticeMapped T2
  ON T1.GPPracticeCode = T2.GPPracticeCode_Original and T2.ICS_2223 = 'BSOL' 
  Where ProviderSiteCode = 'RRK97'
  and ArrivalDate between {d'2023-11-01'} and {d'2024-02-29'}
  
  Union ALL
  
  Select 
  T2.Locality,
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
  'int' as [period]
  from [ECDSV2].[VwECDSAll] T1 
  LEFT JOIN EAT_Reporting_BSOL.Reference.BSOL_ICS_PracticeMapped T2
  ON T1.GPPracticeCode = T2.GPPracticeCode_Original and T2.ICS_2223 = 'BSOL' 
  Where ProviderSiteCode = 'RRK97'
  and ArrivalDate between {d'2024-11-01'} and {d'2025-02-28'}"


ecds <- dbGetQuery(con, sql1)

# reformat out of int64 to int
ecds$AgeAtActivityDate <- as.integer(ecds$AgeAtActivityDate)



# how many attendances in each period

ecds |> 
  group_by(period) |> 
  summarise(attendances = n())

# 


# More attendances in the INT period, so not as simple as straight numeric comparison 

ecds |> 
  group_by(ArrivalDate, period) |> 
  summarise(attendances = n()) |> 
  ggplot(aes(y=attendances, x=ArrivalDate, colour=period, group=period))+
  geom_line()+
  scale_color_bsol()


# Age at admission


ecds |> 
  ggplot(aes(x=AgeAtActivityDate, colour=period, fill=period, group=period))+
  geom_histogram(position = position_identity(), alpha=0.5)+
  scale_color_bsol()+
  scale_fill_bsol()


ecds |> 
  group_by(AgeAtActivityDate) |> 
  summarise(attendances = n()) |> 
  arrange(desc(AgeAtActivityDate))

summary(ecds$AgeAtActivityDate)



# Acuity
ecds |> 
  group_by(period, AcuityECDSDescription) |> 
  summarise(attendances = n()) |> 
  mutate(attendance_prc = ifelse(period == "int", attendances/54294, attendances/51481)) |> 
  ggplot(aes(y=attendance_prc, x= AcuityECDSDescription, fill = period))+
  geom_bar(position="dodge", stat="identity") +
  scale_fill_bsol()+
  scale_x_discrete("Acuity Description"
                   ,labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("Percentage of attendances", labels = percent)+
  theme(axis.text.x = element_text(size=6))

# Chief complaint
ecds |> 
  group_by(period, ChiefComplaintGroup) |> 
  summarise(attendances = n())  |> 
  ggplot(aes(y=attendances, x= ChiefComplaintGroup, fill = period))+
  geom_bar(position="dodge", stat="identity") +
  scale_fill_bsol()+
  scale_x_discrete("Chief Complaint Group"
                   ,labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("Attendances", labels = comma)+
  theme(axis.text.x = element_text(size=6))


# Arrival Mode
ecds |> 
  group_by(period, ArrivalModeDescription) |> 
  summarise(attendances = n())  |> 
  mutate(attendance_prc = ifelse(period == "int", attendances/54294, attendances/51481)) |> 
  ggplot(aes(y=attendance_prc, x= ArrivalModeDescription, fill = period))+
  geom_bar(position="dodge", stat="identity") +
  scale_fill_bsol()+
  scale_x_discrete("Arrival Mode",labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("Percentage of attendances", labels = percent)+
  theme(axis.text.x = element_text(size=6))

# Locality
ecds |> 
  group_by(period, Locality) |> 
  summarise(attendances = n())  |> 
  mutate(attendance_prc = ifelse(period == "int", attendances/54294, attendances/51481)) |> 
  ggplot(aes(y=attendance_prc, x= Locality, fill = period))+
  geom_bar(position="dodge", stat="identity") +
  scale_fill_bsol() +
  scale_x_discrete("Arrival Mode",labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("Percentage of attendances", labels = percent)
  


locality_adm <-
  ecds |> 
  group_by(period, Locality, IsAdmittedAsInpatient) |> 
  summarise(admissions = n())

locality_att <-
  ecds |> 
  group_by(period, Locality) |> 
  summarise(attendances = n())

locality_adm |> 
  filter(IsAdmittedAsInpatient==1) |> 
  left_join(locality_att) |> 
  mutate(admitted_prc = admissions / attendances) |>
  ggplot(aes(y=admitted_prc, x= Locality, fill = period))+
  geom_bar(position="dodge", stat="identity") +
  scale_fill_bsol() +
  scale_x_discrete("Arrival Mode",labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("Percentage of attendances", labels = percent)

locality_att |> 
  ggplot(aes(y=attendances, x= Locality, fill = period))+
  geom_bar(position="dodge", stat="identity") +
  scale_fill_bsol() +
  scale_x_discrete("Arrival Mode",labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("Attendances", labels = comma)


# first regression

# Number of events - min 10 per variable.


# acuity, age, locality, and chance of being admitted
library(ModelMetrics)

mod1 <- glm(IsAdmittedAsInpatient ~ AgeAtActivityDate +
              factor(GenderDescription) + 
              ##ArrivalModeDescription + EthnicCategoryDescription +
             #factor(AcuityECDSDescription) +
             Locality
            ,data=ecds
            , family = "binomial")

summary(mod1)

auc(mod1)

mod2 <- glm(IsAdmittedAsInpatient ~ AgeAtActivityDate +
              GenderDescription + 
              ArrivalModeDescription + 
              EthnicCategoryDescription +
              AcuityECDSDescription +
              Locality +
              period
            ,data=ecds
            , family = "binomial")

summary(mod2)

auc(mod2)
BIC(mod2)


mod3 <- glm(IsAdmittedAsInpatient ~ AgeAtActivityDate +
              GenderDescription + 
              ArrivalModeDescription + 
              #EthnicCategoryDescription +
              AcuityECDSDescription +
              Locality +
              ChiefComplaintGroup +
              period
            , data=ecds
            , family = "binomial")

summary(mod3)

auc(mod3)
BIC(mod3)

# Check overdispersion
sum(mod3$weights * mod3$residuals^2)/mod3$df.residual


# Mod 3 first's bias reduction
library(logistf)
mod3f <- logistf(IsAdmittedAsInpatient ~ AgeAtActivityDate +
              GenderDescription + 
              ArrivalModeDescription + 
              EthnicCategoryDescription +
              AcuityECDSDescription +
              Locality +
              ChiefComplaintGroup +
              period
            , data=ecds
            )

summary(mod3f)

auc(ecds$IsAdmittedAsInpatient, predict(mod3f, newdata = ecds))

auc(mod3f)
BIC(mod3f)

# Check overdispersion
sum(mod3f$weights * mod3f$residuals^2)/mod3f$df.residual


# Minimal

confint(mod3)

anova(mod3)



# Changes in the Locality distribution



library(nnet)

# Set east as baseline
ecds$Locality <- as.factor(ecds$Locality)
ecds$Locality <- relevel(ecds$Locality, ref="East")

mod1_loc <- multinom(Locality ~ IsAdmittedAsInpatient +
                AgeAtActivityDate +
                factor(GenderDescription) +
                  period
                ##ArrivalModeDescription + EthnicCategoryDescription +
                #factor(AcuityECDSDescription) +
                , data=ecds
                , family = "binomial")

summary(mod1_loc)

auc(mod1_loc)

z <- summary(mod1_loc)$coefficients/summary(mod1_loc)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod1_loc))



d_ecds <- data.frame(period = factor(c("control", "control", "int", "int"), levels = c("control","int"))
                     , IsAdmittedAsInpatient = 0
                   , AgeAtActivityDate = median(ecds$AgeAtActivityDate)
                   , GenderDescription=c("Female", "Male", "Female", "Male")
                   )

predict(mod1_loc, newdata = d_ecds, "probs")


# Now do it across the age range
Agedf <-  d_ecds[rep(seq_len(nrow(d_ecds)), times = 125), ]

Agedf$AgeAtActivityDate <- rep(seq(0,124,1), each = 4)

prds <- cbind(Agedf, predict(mod1_loc, newdata = Agedf, type = "probs"))

# Plot grid of intervention vs. control period for Male and female, looking at probability of
# being from a locality, by age.  Firstly for non-admitted patients

prds |> 
  pivot_longer(cols = -c(period, IsAdmittedAsInpatient, AgeAtActivityDate, GenderDescription)) |> 
  ggplot() +
  geom_line(aes(x=AgeAtActivityDate, y=value, col=name), linewidth = 1) +
  scale_colour_viridis_d()+
  labs(y = "Probability of patient being from Locality",
       x = "Age of patient",
       colour = "Locality") +
  facet_grid(rows = vars(GenderDescription), cols = vars(period))+
  theme_grey()



# Now for patients who were admitted
d_ecds2 <- data.frame(period = factor(c("control", "control", "int", "int"), levels = c("control","int"))
                     , IsAdmittedAsInpatient = 1
                     , AgeAtActivityDate = median(ecds$AgeAtActivityDate)
                     , GenderDescription=c("Female", "Male", "Female", "Male")
)

predict(mod1_loc, newdata = d_ecds2, "probs")

# Now do it across the age range
Agedf2 <-  d_ecds2[rep(seq_len(nrow(d_ecds2)), times = 125), ]

Agedf2$AgeAtActivityDate <- rep(seq(0,124,1), each = 4)

prds2 <- cbind(Agedf2, predict(mod1_loc, newdata = Agedf2, type = "probs"))

prds2 |> 
  pivot_longer(cols = -c(period, IsAdmittedAsInpatient, AgeAtActivityDate, GenderDescription)) |> 
  ggplot() +
  geom_line(aes(x=AgeAtActivityDate, y=value, col=name), linewidth = 1) +
  scale_colour_viridis_d()+
  labs(y = "Probability of patient being from Locality",
       x = "Age of patient",
       colour = "Locality") +
  facet_grid(rows = vars(GenderDescription), cols = vars(period))+
  theme_grey()
