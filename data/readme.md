# ECS25-0363 Data Supplement

This repository contains anonymized and processed datasets used in the analyses for the manuscript:

**"Trophic mismatch alters allocation to mass gain in female bighorn sheep (Ovis canadensis)"** (submitted to *Ecosphere*)

## Contents

- `ECS25-0363_unscaledDat.csv` — individual-level demographic and phenological data
- `ECS25-0363_massGainDat.csv` — subset used for mass gain analyses
- `ECS25-0363_breedingDat.csv` — subset used for breeding probability analyses

## Notes

- All individual IDs have been anonymized.
- Variables not used in the final analyses (e.g., lamb survival) have been excluded.
- Values have been rounded to 3 decimals for clarity.
- Original data were derived from long-term monitoring of the Ram Mountain bighorn sheep population (Alberta, Canada).

## Reproducibility

These datasets were exported from a processing script (`dataTidyingV2.R`) which:
- Loads the cleaned `.RData` object
- Anonymizes IDs
- Selects relevant columns
- Rounds numeric variables

## Study context - methods

Bighorn sheep are captured between 1 and 9 times a summer, with over 90% of females captured at least twice a summer, allowing for mass to be adjusted to June 5 (spring mass) and September 15 (fall mass). Mass adjustments are calculated using linear mixed-effects models with restricted maximum likelihood, with both intercept and slope allowed to vary for each individual. Enhanced Vegetation Index (EVI) green-up dates are extracted from data obtained by the Moderate Resolution Imaging Spectroradiometer (MODIS) on board NASA’s Terra and Aqua satellites, available every 8 days since 2000. Prior to modelling, all continuous variables were standardized and given a Z-score to bring them onto the same scale. Outliers (four observations with mismatch >65 days) were removed, as well as observations with missing data in explanatory variables.

## Variables 

Identifier and years 
ID: female ID, unique to each individual and may be repeated in the data frame
yr: focal year (time t), year of observation. All variables refer to observation in focal year unless otherwise specified
Age predictors
ewe_age_z: female age (years)
age_class: female age divided into 3 categories - prim (primiparous), adult (prime-aged, 3-7 years of age but not primiparous), senescent (over 7 years of age)
prim: categorical variable indicating whether a female is primiparous (1) or not (0)

Response variables 
repro_tp1: if a female did (1) or did not (0) reproduce the following year (t+1)
frs: female reproductive success in following year (t+1)
wean_tp1: if a lamb (born t+1) did (1) or did not (0) survive to weaning in the following year (t+1)
birthdate_tp1: in females who reproduced in the year following the focal year (t+1), their parturition date in Julian days
mismatch_tp1: in females who reproduced the year following the focal year (time t+1), mismatch between parturition and green-up in the following year

Other predictors 
pred: presence (1) or absence (0) of a specialist cougar in the area
first_pregnant: if a female was (1) or was not (0) pregnant at her first capture, indicating that her first mass measurement of the year was excluded from adjustment model

birthdate_z: parturition date, in Julian days (Jan 1 = Julian day 1)
median_bdate_z: median parturition date, in Julian days
greenup_date_z: spring green-up date based on Enhanced Vegetation Index (EVI)
mass_fall_tm1_z: female mass in fall of previous year (in kg, time t-1), adjusted to September 15 
mass_fall_z: female mass in fall of focal year (kg), adjusted to September 15 
mass_spring_z: female mass in spring of focal year (kg), adjusted to June 5 
greenup_mismatch_z: number of days between spring green-up and parturition date, with negative mismatch occurring when parturition is before spring green-up, and positive mismatch when parturition is after green-up, in days
code_sr: categorical variable of reproductive success - no lactation (0), neonatal mortality (1), lamb death during summer (2), lamb survived to weaning (3), lamb survived to weaning but overwinter survival unknown (4), lamb survived to 1 year of age (5), human intervention in reproduction (8, 9)
wean_mass_z: lamb weaning mass (kg) adjusted to September 15


