# Standardinzing the dataset to cope with the modifications in the input file ----
## the work is done here, given that the input file contains many problems with data classes and multiple information clustered in one cell. 
## the input file is changing frequently, so it is not feasible to develop a script to harness the data in the updated version, unless the formatting issues are solved first.

# libraries ----
library(tidyverse)
library(janitor)

# loading the dataset ----
ds_costs <- read_csv("./data/eLTER-SO-costs_standard-observations-detailed-costs_V18.csv")

# data standardization -----
ds_costs %>% 
## method prime ----
### Geosphere ----
#### SOGEO_001 
#### SOGEO_003 
#### SOGEO_167
#### SOGEO_048
#### SOGEO_155
### Hydrosphere ----
#### SOHYD_004
#### SOHYD_005
#### SOHYD_006
#### SOHYD_010
#### SOHYD_011
#### SOHYD_012
#### SOHYD_168
#### SOHYD_058
#### SOHYD_059
#### SOHYD_062
#### SOHYD_064
#### SOHYD_065
  # removing the SOHYD_065
  filter(!c(method == "prime" & code == "SOHYD_065")) %>%
### Sociosphere ----
#### SOSOC_029
# removing the SOSOC_029
  filter(!c(method == "prime" & code == "SOSOC_029")) %>%
#### SOSOC_031
  mutate(soBundles = replace(soBundles, method == "prime" & code == "SOSOC_031", "Agricultural production (detailed sub-categories for cropland, grassland, forest, fishery, in t/ha and year)")) %>% 
  mutate(measurementInterval = replace(measurementInterval, method == "prime" & code == "SOSOC_031", 1)) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_031", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_031", 2)) %>% 
#### SOSOC_030
  mutate(soBundles = replace(soBundles, method == "prime" & code == "SOSOC_030", "FNVA, FNVA/AWU, FFI, FFI/FWU, Farm worker wages")) %>% 
  mutate(measurementInterval = replace(measurementInterval, method == "prime" & code == "SOSOC_030", 1)) %>% 
  mutate(sensorType = replace(sensorType, method == "prime" & code == "SOSOC_030", "national statistics")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_030", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_030", 2)) %>% 
#### SOSOC_114
  mutate(soBundles = replace(soBundles, method == "prime" & code == "SOSOC_114", "Livestock numbers, breeds, + Feed/grazing management")) %>% 
  mutate(samplingType = replace(samplingType, method == "prime" & code == "SOSOC_114", "retrieval | sample")) %>% 
  mutate(measurementInterval = replace(measurementInterval, method == "prime" & code == "SOSOC_114", 1)) %>% 
  mutate(measurementIntervalUnit = replace(measurementIntervalUnit, method == "prime" & code == "SOSOC_114", "year")) %>% 
  mutate(measurementsPerYear = replace(measurementsPerYear, method == "prime" & code == "SOSOC_114", 1)) %>% 
  mutate(sensorType = replace(sensorType, method == "prime" & code == "SOSOC_114", "national statistics + site specific survey")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_114", 13)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_114", 11)) %>% 
#### SOSOC_032
  mutate(measurementInterval = replace(measurementInterval, method == "prime" & code == "SOSOC_032", 6)) %>% 
  mutate(measurementsPerYear = replace(measurementsPerYear, method == "prime" & code == "SOSOC_032", 1/6)) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_032", 9)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_032", 9)) %>% 
#### SOSOC_036
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_036", 1)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_036", 1)) %>% 
#### SOSOC_037
  mutate(soBundles = replace(soBundles, method == "prime" & code == "SOSOC_037", "Farm structure / land management / area statistics (incl. conv./organic; conv./cons./no tillage)")) %>% 
  mutate(samplingType = replace(samplingType, method == "prime" & code == "SOSOC_037", "retrieval | sample")) %>% 
  mutate(sensorType = replace(sensorType, method == "prime" & code == "SOSOC_037", "national statistics + site-specific survey")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_037", 13)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_037", 11)) %>% 
#### SOSOC_040
  mutate(samplingType = replace(samplingType, method == "prime" & code == "SOSOC_040", "retrieval | sample")) %>% 
  mutate(measurementInterval = replace(measurementInterval, method == "prime" & code == "SOSOC_040", 6)) %>% 
  mutate(measurementsPerYear = replace(measurementsPerYear, method == "prime" & code == "SOSOC_040", 1/6)) %>% 
  mutate(sensorType = replace(sensorType, method == "prime" & code == "SOSOC_040", "quantitative + expert judgement + site-specific survey")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_040", 13)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_040", 11)) %>% 
#### SOSOC_042
  mutate(measurementInterval = replace(measurementInterval, method == "prime" & code == "SOSOC_042", 1)) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_042", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_042", 2)) %>% 
#### SOSOC_043
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_043", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_043", 2)) %>% 
#### SOSOC_044
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_044", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_044", 2)) %>% 
#### SOSOC_045
  mutate(sensorType = replace(sensorType, method == "prime" & code == "SOSOC_045", "national census data + site-specific survey")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_045", 13)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_045", 11)) %>% 
#### SOSOC_183
  mutate(so = replace(so, method == "prime" & code == "SOSOC_183", "Resource use (MFA)")) %>% 
  mutate(soBundles = replace(soBundles, method == "prime" & code == "SOSOC_183", "DE, IMP, EXP, DPO, BI, DMC, DMI, PTB, NAS: biomass, metal ores, non-metallic minerals, fossil energy carriers (other products, waste, emissions)")) %>% 
  mutate(sensorType = replace(sensorType, method == "prime" & code == "SOSOC_183", "national statistics")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_183", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_183", 2)) %>% 
#### SOSOC_184
 mutate(soBundles = replace(soBundles, method == "prime" & code == "SOSOC_184", "CAP payments for direct support, rural development, market measures (€ total, avg. per beneficiary, avg. per ha, nr. of beneficiaries per total agric. holdings)")) %>% 
  mutate(sensorType = replace(sensorType, method == "prime" & code == "SOSOC_184", "national statistics")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "prime" & code == "SOSOC_184", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "prime" & code == "SOSOC_184", 2)) %>% 
## method basic -----
### Sociosphere ----
#### SOSOC_031
  mutate(soBundles = replace(soBundles, method == "basic" & code == "SOSOC_031", "Agricultural production (detailed sub-categories for cropland, grassland, forest, fishery, in t/ha and year)")) %>% 
#### SOSOC_030
  mutate(soBundles = replace(soBundles, method == "basic" & code == "SOSOC_030", "FNVA, FNVA/AWU, FFI, FFI/FWU, Farm worker wages")) %>% 
#### SOSOC_114
  mutate(samplingType = replace(samplingType, method == "basic" & code == "SOSOC_114", "retrieval | sample")) %>% 
  mutate(sensorType = replace(sensorType, method == "basic" & code == "SOSOC_114", "official statistics/ + expert judgement")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "basic" & code == "SOSOC_114", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "basic" & code == "SOSOC_114", 4)) %>% 
#### SOSOC_032
  mutate(measurementInterval = replace(measurementInterval, method == "basic" & code == "SOSOC_032", 3)) %>% 
  mutate(measurementsPerYear = replace(measurementsPerYear, method == "basic" & code == "SOSOC_032", 1/3)) %>% 
  mutate(sensorType = replace(sensorType, method == "basic" & code == "SOSOC_032", "expert judgement")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "basic" & code == "SOSOC_032", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "basic" & code == "SOSOC_032", 4)) %>% 
#### SOSOC_036
  mutate(samplingEffort = replace(samplingEffort, method == "basic" & code == "SOSOC_036", 1)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "basic" & code == "SOSOC_036", 1)) %>% 
#### SOSOC_037
  mutate(soBundles = replace(soBundles, method == "basic" & code == "SOSOC_037", "Farm structure / land management / area statistics (incl. conv./organic; conv./cons./no tillage)")) %>% 
  mutate(samplingType = replace(samplingType, method == "basic" & code == "SOSOC_037", "retrieval | sample")) %>% 
  mutate(measurementInterval = replace(measurementInterval, method == "basic" & code == "SOSOC_037", 1)) %>% 
  mutate(measurementsPerYear = replace(measurementsPerYear, method == "basic" & code == "SOSOC_037", 1)) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "basic" & code == "SOSOC_037", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "basic" & code == "SOSOC_037", 4)) %>% 
#### SOSOC_040
  mutate(samplingType = replace(samplingType, method == "basic" & code == "SOSOC_040", "retrieval | sample")) %>% 
  mutate(sensorType = replace(sensorType, method == "basic" & code == "SOSOC_040", "quantitative  (statistics, measurements) + expert judgement")) %>% 
  mutate(samplingEffort = replace(samplingEffort, method == "basic" & code == "SOSOC_040", 4)) %>% 
  mutate(processingRawDataEffort = replace(processingRawDataEffort, method == "basic" & code == "SOSOC_040", 4)) %>% 
#### SOSOC_042
#### SOSOC_043
  mutate(measurementInterval = replace(measurementInterval, method == "basic" & code == "SOSOC_043", 1)) %>% 
  mutate(measurementsPerYear = replace(measurementsPerYear, method == "basic" & code == "SOSOC_043", 1)) %>% 
#### SOSOC_044
  mutate(measurementInterval = replace(measurementInterval, method == "basic" & code == "SOSOC_044", 1)) %>% 
  mutate(measurementsPerYear = replace(measurementsPerYear, method == "basic" & code == "SOSOC_044", 1)) %>% 
#### SOSOC_045
  mutate(samplingType = replace(samplingType, method == "basic" & code == "SOSOC_045", "retrieval | sample")) %>% 
#### SOSOC_183
  mutate(soBundles = replace(soBundles, method == "basic" & code == "SOSOC_183", "DE, IMP, EXP, DPO, BI, DMC, DMI, PTB, NAS: biomass, metal ores, non-metallic minerals, fossil energy carriers (other products, waste, emissions)")) %>% 
#### SOSOC_184
  mutate(soBundles = replace(soBundles, method == "basic" & code == "SOSOC_184", "CAP payments for direct support, rural development, market measures (€ total, avg. per beneficiary, avg. per ha, nr. of beneficiaries per total agric. holdings)")) %>% 
# updating the totalHumanLabor after the changes
  mutate(totalHumanLabor = installationEffort + maintenanceEffort + samplingEffort + processingRawDataEffort)
  
  
  # distinct(testLabor)
  View()
#TODO correct the values of totalHumanLabor