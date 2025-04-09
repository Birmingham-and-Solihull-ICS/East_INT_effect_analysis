# BSOL East Locality Evaluation
## What effects does the INT service have on the patient flow?

# Overview

Integrated Neighbourhood Teams (INT) are being established in BSOL, with the aim of delivering appropriate care for patients in the right part of the system.  The East Locality INT is based at Washwood Health Healthcentre and has been piloting a new way of working with multidisciplinary teams sharing a service and deciding on treatment together.
The system has focussed on the 250 most frequent users of emergency care ('HISU' - 'high intensity service users'), and has flags in place for when these patients use the system.

## Proposed method of action

The proposal is that, with an INT service such as this, the system should see changes such as:

* Reduced ambulance conveyance of HISU patients
* Reduced emergency care usage for HISU patients
* Increased community care and/or GP usage for HISU patients
* Change in the types of patients seen in Heartlands A&E service
* Changes in the pattern of conditions seen in Heartlands A&E service

### Analytical questions

Comparing a pre-INT period (Nov-23 - Feb-24) against the INT period (Nov-24 - Feb-25):

1. Have the demographics of Heartlands A&E attenders changed?  Consider geography (LSOA -> Locality), age, acuity (ECDS4 - ), sex, deprivation and ethnicity.

    - Look at all Heartlands A&E attenders in CDS.
  
    - Consider factors: geography (LSOA -> Locality), age, sex, acuity of some sort (acuity of NEWS if available), deprivation and ethnicity.

2. Has community activity increased for HISU patients? 
  
    - Need a HISU/INT patient list to trace in systems
  
    -	What systems would this activity be recorded in? Community service? Ambulance? UEC? Mental Health?
  
    - Likely need to be done via join ICB / BCHC analyst using BCHC linked data.

3. Has emergency care activity decreased for HISU patients?
    
    - Again, dependent on patient list and needs joint ICB BCHC analyst
  
4. How has the case-mix of emergency admissions changed?  Age, Sex, ethnicity, deprivation, admitting condition, co-morbidity/frailty?
  
    - All emergency admissions in period.
  
    - Include age, sex, ethnicity, deprivation, admitting condition, co-morbidity/frailty?
  
    - Likely statistical model:  multiple correspondence analysis (and load factors on components), or if features are simple enough, logistic regression.




This repository is dual licensed under the [Open Government v3]([https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) & MIT. All code and outputs are subject to Crown Copyright.
